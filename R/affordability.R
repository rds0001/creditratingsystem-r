.cr_aff_frequencies <- c(weekly=52, biweekly=26, four_weekly=13, monthly=12,
                        quarterly=4, annual=1)
.cr_aff_scalar <- function(x,name,nonnegative=FALSE) {
  x <- .cr_numeric(x,name)
  if(length(x)!=1L || (nonnegative && x<0)) .cr_error("INVALID_AMOUNT",paste(name,"must be finite scalar",if(nonnegative) ">= 0" else ""))
  as.numeric(x)
}
.cr_aff_result <- function(value,...) {
  check <- function(x) {
    if(is.list(x)) { for(v in x) check(v) }
    else if(is.numeric(x) && any(!is.finite(x))) .cr_error("NUMERICAL_FAILURE","Affordability arithmetic exceeds finite range")
  }
  check(value);.cr_result(value,...)
}

stress_affordability <- function(baseline,scenario,context=NULL) {
  if(!.cr_object(baseline) || !.cr_object(scenario)) .cr_error("INVALID_INPUT","Baseline and scenario must be objects")
  if(!identical(scenario$schema_version,"1.0")) .cr_error("UNSUPPORTED_SCHEMA_VERSION","Unsupported stress scenario")
  sid <- .cr_aff_text(scenario$scenario_id,"scenario_id");currency <- .cr_aff_text(baseline$currency,"currency")
  cltv_scope <- baseline$cltv_scope
  if(length(cltv_scope)!=1L || !cltv_scope %in% c("all_contracts_secured_by_pool","not_applicable"))
    .cr_error("INVALID_INPUT","Declare all-contracts collateral pool scope or not_applicable")
  start <- .cr_date(baseline$as_of,"as_of");start_date <- as.Date(start)
  liquid <- .cr_aff_scalar(baseline$liquid_buffer,"liquid_buffer",TRUE)
  groups <- list()
  for(key in c("incomes","expenses","contracts","collateral")) {
    g <- list()
    for(row in .cr_aff_rows(baseline[[key]],key)) {
      rid <- .cr_aff_text(row$economic_id,"economic_id")
      if(rid %in% names(g)) .cr_error("DUPLICATE_ECONOMIC_ID","Reconcile baseline economic sources first")
      .cr_aff_text(row$currency,"currency");g[[rid]] <- row
      if(key=="incomes") {
        if(!identical(row$basis,"net")) .cr_error("INVALID_UNITS","Require normalized monthly net income")
        .cr_aff_scalar(row$monthly_amount,"monthly_amount")
      } else if(key=="expenses") .cr_aff_scalar(row$monthly_amount,"monthly_amount",TRUE)
      else if(key=="collateral") .cr_aff_scalar(row$value,"collateral value",TRUE)
    }
    groups[[key]] <- g
  }
  if(length(intersect(names(groups$expenses),names(groups$contracts)))) .cr_error("DOUBLE_COUNTING","Debt service also included in living expenses")
  schedules <- list()
  for(cid in names(groups$contracts)) {
    c <- groups$contracts[[cid]]
    principal <- .cr_aff_scalar(c$principal,"principal",TRUE)
    months <- .cr_aff_scalar(c$remaining_months,"remaining_months",TRUE)
    if(months<1 || months!=floor(months)) .cr_error("INVALID_INPUT","remaining_months must be positive integer")
    annual_rate <- .cr_aff_scalar(c$annual_rate,"annual_rate",TRUE)
    if(length(c$repayment)!=1L || !c$repayment %in% c("annuity","interest_only") ||
       length(c$rate_type)!=1L || !c$rate_type %in% c("fixed","reset")) .cr_error("INVALID_INPUT","Declare repayment and rate_type")
    balloon <- .cr_aff_scalar(c$balloon_amount,"balloon_amount",TRUE)
    if(balloon>principal || (c$repayment=="interest_only" && balloon!=principal)) .cr_error("INVALID_INPUT","Invalid terminal balloon")
    resets <- c$reset_dates
    if(!is.list(resets) || !is.null(names(resets)) || anyDuplicated(unlist(resets))) .cr_error("INVALID_INPUT","Declare unique reset_dates")
    for(d in resets) if(.cr_date(d,"reset_date")<=start) .cr_error("INVALID_DATE","Reset must follow baseline")
    if(c$rate_type=="fixed" && length(resets)) .cr_error("INVALID_INPUT","Fixed contracts have no resets")
    floor <- .cr_aff_scalar(c$rate_floor,"rate_floor",TRUE);cap <- .cr_aff_scalar(c$rate_cap,"rate_cap",TRUE)
    if(!(floor<=annual_rate && annual_rate<=cap)) .cr_error("INVALID_INPUT","Initial coupon outside floor/cap")
    schedules[[cid]] <- list(balance=principal,rate=annual_rate,payment=NULL,resets=sort(unlist(resets)))
  }
  periods <- .cr_aff_rows(scenario$periods,"periods")
  if(!length(periods)) .cr_error("EMPTY_INPUT","Scenario needs monthly periods")
  thresholds <- baseline$thresholds
  if(!.cr_object(thresholds) || !setequal(names(thresholds),c("minimum_residual","maximum_dsti","maximum_cltv")))
    .cr_error("INVALID_INPUT","Supply explicit residual, DSTI and CLTV thresholds")
  for(key in names(thresholds)) .cr_aff_scalar(thresholds[[key]],key,key!="minimum_residual")
  outputs <- list();first_exhaustion <- NULL;prior_date <- start
  for(index in seq_along(periods)) {
    row <- periods[[index]]
    total_month <- as.integer(format(start_date,"%Y"))*12+as.integer(format(start_date,"%m"))-1+index
    year <- total_month%/%12;month <- total_month%%12+1
    first <- as.Date(sprintf("%04d-%02d-01",year,month))
    next_first <- seq(first,by="month",length.out=2L)[2L]
    day <- min(as.integer(format(start_date,"%d")),as.integer(next_first-first))
    expected_date <- sprintf("%04d-%02d-%02d",year,month,day)
    if(!identical(row$date,expected_date)) .cr_error("INVALID_DATE","Scenario dates must be consecutive monthly anniversaries")
    fx <- row$fx_to_reporting
    currencies <- unique(c(currency,unlist(lapply(groups,function(g) vapply(g,function(r) r$currency,character(1))))))
    if(!.cr_object(fx) || !all(currencies %in% names(fx))) .cr_error("MISSING_SCENARIO_MAPPING","Every currency requires explicit FX")
    for(code in currencies) if(.cr_aff_scalar(fx[[code]],"FX",TRUE)<=0 || (code==currency && fx[[code]]!=1))
      .cr_error("INVALID_UNITS","Positive FX required; reporting FX is one")
    mappings <- c(incomes="income_factors",expenses="expense_factors",collateral="collateral_factors",contracts="rate_shifts")
    for(group in names(mappings)) {
      mapping <- mappings[[group]];factors <- row[[mapping]]
      if(!.cr_object(factors) || !setequal(names(factors),names(groups[[group]])))
        .cr_error("MISSING_SCENARIO_MAPPING",paste(mapping,"must cover every economic source exactly"))
      for(factor in factors) .cr_aff_scalar(factor,mapping,mapping!="rate_shifts")
    }
    income <- sum(vapply(names(groups$incomes),function(k) {
      c <- groups$incomes[[k]];c$monthly_amount*row$income_factors[[k]]*fx[[c$currency]]
    },numeric(1)))
    expenses <- sum(vapply(names(groups$expenses),function(k) {
      c <- groups$expenses[[k]];c$monthly_amount*row$expense_factors[[k]]*fx[[c$currency]]
    },numeric(1)))
    collateral_value <- sum(vapply(names(groups$collateral),function(k) {
      c <- groups$collateral[[k]];c$value*row$collateral_factors[[k]]*fx[[c$currency]]
    },numeric(1)))
    services <- 0;opening_debt <- 0;closing_debt <- 0;payments <- list()
    for(cid in names(groups$contracts)) {
      c <- groups$contracts[[cid]];state <- schedules[[cid]];balance <- state$balance;annual_rate <- state$rate
      if(index>c$remaining_months) {
        payments[[length(payments)+1L]] <- list(economic_id=cid,payment=0,interest=0,principal_repaid=0,closing_balance=0,annual_rate=annual_rate,reset=FALSE)
        next
      }
      opening_debt <- opening_debt+balance*fx[[c$currency]]
      reset <- any(prior_date<state$resets & state$resets<=expected_date)
      if(reset) {
        if(any(prior_date<state$resets & state$resets<expected_date)) .cr_error("INVALID_DATE","Reset must align with monthly payment date")
        annual_rate <- min(c$rate_cap,max(c$rate_floor,c$annual_rate+row$rate_shifts[[cid]]))
      }
      rate <- annual_rate/12;remaining <- c$remaining_months-index+1
      if(is.null(state$payment) || reset) {
        if(c$repayment=="interest_only") state$payment <- balance*rate
        else if(rate==0) state$payment <- (balance-c$balloon_amount)/remaining
        else {
          discount <- exp(-remaining*log1p(rate))
          state$payment <- (balance-c$balloon_amount*discount)*rate/(-expm1(-remaining*log1p(rate)))
        }
      }
      interest <- balance*rate;payment <- state$payment
      if(remaining==1) payment <- balance+interest
      principal_repaid <- payment-interest;closing <- balance-principal_repaid
      if(closing< -1e-9*max(1,c$principal)) .cr_error("NUMERICAL_FAILURE","Negative projected balance")
      closing <- max(0,closing);state$balance <- closing;state$rate <- annual_rate;schedules[[cid]] <- state
      services <- services+payment*fx[[c$currency]];closing_debt <- closing_debt+closing*fx[[c$currency]]
      payments[[length(payments)+1L]] <- list(economic_id=cid,payment=payment,interest=interest,
        principal_repaid=principal_repaid,closing_balance=closing,annual_rate=annual_rate,reset=reset)
    }
    residual <- income-expenses-services;liquid <- liquid+residual
    if(liquid<0 && is.null(first_exhaustion)) first_exhaustion <- expected_date
    dsti <- if(income<=0) NULL else services/income
    dti <- if(income<=0) NULL else opening_debt/(12*income)
    cltv <- if(cltv_scope=="not_applicable" || collateral_value<=0) NULL else closing_debt/collateral_value
    breaches <- list()
    if(residual<thresholds$minimum_residual) breaches[[length(breaches)+1L]] <- "RESIDUAL"
    if(is.null(dsti)) breaches[[length(breaches)+1L]] <- "NONPOSITIVE_INCOME"
    else if(dsti>thresholds$maximum_dsti) breaches[[length(breaches)+1L]] <- "DSTI"
    if(!is.null(cltv) && cltv>thresholds$maximum_cltv) breaches[[length(breaches)+1L]] <- "CLTV"
    if(cltv_scope!="not_applicable" && collateral_value<=0 && closing_debt>0) breaches[[length(breaches)+1L]] <- "ZERO_COLLATERAL_VALUE"
    outputs[[length(outputs)+1L]] <- list(date=expected_date,net_income=income,expenses=expenses,
      debt_service=services,residual=residual,liquid_buffer=liquid,opening_debt=opening_debt,closing_debt=closing_debt,
      collateral_value=collateral_value,dsti=dsti,annual_dti=dti,cltv=cltv,breaches=breaches,contracts=payments)
    prior_date <- expected_date
  }
  .cr_aff_result(list(scenario_id=sid,currency=currency,periods=outputs,first_liquidity_exhaustion=first_exhaustion,
    minimum_residual=min(vapply(outputs,function(x) x$residual,numeric(1))),
    scenario_hash=.cr_hash(scenario),baseline_hash=.cr_hash(baseline)),
    "affordability.stress_affordability",currency,context,
    limitations=c("Conditional monthly cashflow projection; no default probability, refinancing, prepayment or causal effect is inferred.",
      "CLTV requires the declared common all-contracts collateral pool; use combined_loan_to_value for legal lien coverage."))
}
.cr_aff_text <- function(x,name) { .cr_string(x,name); x }
.cr_aff_rows <- function(rows,name) {
  if(!is.list(rows) || !is.null(names(rows)) || !all(vapply(rows,.cr_object,logical(1))))
    .cr_error("INVALID_INPUT",paste(name,"must be an array of objects"))
  rows
}
.cr_aff_calendar <- function(calendar,period) {
  if(!.cr_object(calendar) || length(period)!=1L || !period %in% names(calendar))
    .cr_error("INVALID_UNITS","Supply payment calendar and target period")
  for(key in names(calendar)) {
    .cr_aff_text(key,"frequency")
    if(.cr_aff_scalar(calendar[[key]],"payments_per_year")<=0) .cr_error("INVALID_UNITS","Frequency counts must be positive")
  }
  calendar
}
.cr_aff_convert <- function(amount,frequency,calendar,period) {
  if(length(frequency)!=1L || !frequency %in% names(calendar)) .cr_error("INVALID_UNITS","Unknown payment frequency")
  value <- amount*(calendar[[frequency]]/calendar[[period]])
  if(!is.finite(value)) .cr_error("NUMERICAL_FAILURE","Period normalization overflowed")
  value
}

normalize_income_period <- function(components,target_period,target_basis,currency,
                                    payments_per_year,variable_income_factor,context=NULL) {
  .cr_aff_text(currency,"currency")
  if(length(target_basis)!=1L || !target_basis %in% c("net","gross")) .cr_error("INVALID_UNITS","Declare net/gross basis")
  calendar <- .cr_aff_calendar(payments_per_year,target_period)
  factor <- .cr_aff_scalar(variable_income_factor,"variable_income_factor",TRUE)
  if(factor>1) .cr_error("INVALID_INPUT","Variable inclusion factor must be in [0,1]")
  rows <- list();seen <- list();duplicates <- list()
  for(row in .cr_aff_rows(components,"components")) {
    eid <- .cr_aff_text(row$economic_id,"economic_id")
    if(!identical(row$currency,currency) || length(row$basis)!=1L || !row$basis %in% c("net","gross"))
      .cr_error("INVALID_UNITS","Income currency/basis mismatch")
    kind <- row$kind
    if(length(kind)!=1L || !kind %in% c("recurring","variable","internal_transfer")) .cr_error("INVALID_INPUT","Declare income kind")
    if(length(row$frequency)!=1L || !row$frequency %in% names(calendar)) .cr_error("INVALID_UNITS","Unknown income frequency")
    keys <- c("amount","frequency","basis","currency","kind","tax_amount")
    signature <- setNames(lapply(keys,function(k) row[[k]]),keys)
    if(eid %in% names(seen)) {
      if(!identical(.cr_hash(signature),seen[[eid]])) .cr_error("CONFLICTING_EVIDENCE","Conflicting income economics")
      duplicates[[length(duplicates)+1L]] <- eid; next
    }
    seen[[eid]] <- .cr_hash(signature)
    amount <- row$amount
    applied <- if(kind=="internal_transfer") 0 else if(kind=="variable") factor else 1
    if(kind=="internal_transfer") { normalized <- 0; status <- "EXCLUDED_TRANSFER" }
    else if(is.null(amount)) { normalized <- NULL; status <- "MISSING_AMOUNT" }
    else {
      amount <- .cr_aff_scalar(amount,"income amount")
      if(row$basis!=target_basis) {
        if(is.null(row$tax_amount)) {
          rows[[length(rows)+1L]] <- list(economic_id=eid,normalized_amount=NULL,status="MISSING_TAX_BRIDGE",inclusion_factor=applied)
          next
        }
        tax <- .cr_aff_scalar(row$tax_amount,"tax_amount",TRUE)
        amount <- amount+if(target_basis=="net") -tax else tax
      }
      if(amount<0) applied <- 1
      normalized <- .cr_aff_convert(amount,row$frequency,calendar,target_period)*applied
      status <- if(normalized<0) "NEGATIVE_INCOME" else if(normalized==0) "ZERO_INCOME" else "INCLUDED"
    }
    rows[[length(rows)+1L]] <- list(economic_id=eid,normalized_amount=normalized,status=status,inclusion_factor=applied)
  }
  missing <- lapply(Filter(function(r) is.null(r$normalized_amount),rows),function(r) r$economic_id)
  known <- sum(vapply(rows,function(r) if(is.null(r$normalized_amount)) 0 else r$normalized_amount,numeric(1)))
  .cr_aff_result(list(total_income=if(length(missing)) NULL else known,known_income=known,period=target_period,
    basis=target_basis,currency=currency,components=rows,missing_components=missing,duplicate_ids=duplicates),
    "affordability.normalize_income_period",currency,context,
    limitations="Payment counts and tax amounts are supplied inputs; no tax law is inferred.")
}

estimate_recurring_expenses <- function(expenses,target_period,currency,payments_per_year,
                                        household_profile,benchmarks=NULL,context=NULL) {
  .cr_aff_text(currency,"currency");calendar <- .cr_aff_calendar(payments_per_year,target_period)
  if(!.cr_object(household_profile)) .cr_error("INVALID_INPUT","Household profile required")
  profile_id <- .cr_aff_text(household_profile$profile_id,"profile_id")
  required <- household_profile$required_categories
  if(!is.list(required) || !is.null(names(required)) ||
     !all(vapply(required,function(x) is.character(x) && length(x)==1L && !is.na(x) && nzchar(x),logical(1))) || anyDuplicated(unlist(required)))
    .cr_error("INVALID_INPUT","Declare distinct required categories")
  groups <- list();excluded <- list()
  for(row in .cr_aff_rows(expenses,"expenses")) {
    eid <- .cr_aff_text(row$economic_id,"economic_id");category <- .cr_aff_text(row$category,"category");kind <- row$kind
    if(length(kind)!=1L || !kind %in% c("essential","discretionary","debt_service","internal_transfer"))
      .cr_error("INVALID_INPUT","Declare expense kind")
    if(!identical(row$currency,currency)) .cr_error("INVALID_UNITS","Expense currency mismatch")
    if(kind=="internal_transfer") {excluded[[length(excluded)+1L]] <- eid;next}
    amount <- if(is.null(row$amount)) NULL else .cr_aff_convert(.cr_aff_scalar(row$amount,"expense",TRUE),row$frequency,calendar,target_period)
    if(!eid %in% names(groups)) groups[[eid]] <- list(category=category,kind=kind,amounts=list())
    g <- groups[[eid]]
    if(g$category!=category || g$kind!=kind) .cr_error("CONFLICTING_EVIDENCE","Expense id changes meaning")
    g$amounts[length(g$amounts)+1L] <- list(amount);groups[[eid]] <- g
  }
  categories <- .cr_empty_object();debt <- 0;reconciled <- list();missing_ids <- list()
  for(eid in names(groups)) {
    g <- groups[[eid]];known <- unlist(g$amounts)
    amount <- if(length(known)) max(known) else NULL
    if(is.null(amount)) missing_ids[[length(missing_ids)+1L]] <- eid
    else if(g$kind=="debt_service") debt <- debt+amount
    else {
      entry <- categories[[g$category]]
      if(is.null(entry)) entry <- list(amount=0,kind=g$kind,benchmark_floor=NULL)
      if(entry$kind!=g$kind) .cr_error("CONFLICTING_EVIDENCE","Category changes expense kind")
      entry$amount <- entry$amount+amount;categories[[g$category]] <- entry
    }
    reconciled[[length(reconciled)+1L]] <- list(economic_id=eid,amount=amount,kind=g$kind,evidence_count=length(g$amounts))
  }
  used <- character()
  for(row in .cr_aff_rows(if(is.null(benchmarks)) list() else benchmarks,"benchmarks")) {
    if(!identical(row$profile_id,profile_id)) next
    category <- .cr_aff_text(row$category,"category")
    if(category %in% used || !identical(row$currency,currency) || length(row$kind)!=1L || !row$kind %in% c("essential","discretionary"))
      .cr_error("INVALID_INPUT","Ambiguous benchmark or currency/kind mismatch")
    used <- c(used,category)
    amount <- .cr_aff_convert(.cr_aff_scalar(row$amount,"benchmark amount",TRUE),row$frequency,calendar,target_period)
    entry <- categories[[category]]
    if(is.null(entry)) entry <- list(amount=0,kind=row$kind,benchmark_floor=NULL)
    if(entry$kind!=row$kind) .cr_error("CONFLICTING_EVIDENCE","Benchmark category kind mismatch")
    entry$amount <- max(entry$amount,amount);entry$benchmark_floor <- amount;categories[[category]] <- entry
  }
  missing_categories <- Filter(function(x) !x %in% names(categories),required)
  known_total <- sum(vapply(categories,function(x) x$amount,numeric(1)))
  .cr_aff_result(list(recurring_expenses=if(length(missing_categories)||length(missing_ids)) NULL else known_total,
    known_expenses=known_total,
    debt_service=if(any(vapply(missing_ids,function(k) groups[[k]]$kind=="debt_service",logical(1)))) NULL else debt,
    known_debt_service=debt,
    essential=sum(vapply(categories,function(x) if(x$kind=="essential") x$amount else 0,numeric(1))),
    discretionary=sum(vapply(categories,function(x) if(x$kind=="discretionary") x$amount else 0,numeric(1))),
    categories=categories,reconciled=reconciled,excluded_transfers=excluded,missing_categories=missing_categories,
    missing_obligations=missing_ids,period=target_period,currency=currency,profile_id=profile_id),
    "affordability.estimate_recurring_expenses",currency,context,
    limitations="Max reconciliation and benchmark floors are explicit estimation rules, not evidence of complete expenditure coverage.")
}

combined_loan_to_value <- function(claims,collateral,allocations,currency,context=NULL) {
  .cr_aff_text(currency,"currency");cs <- list();assets <- list()
  for(which in c("claims","collateral")) {
    key <- if(which=="claims") "claim_id" else "collateral_id"
    amount_name <- if(which=="claims") "amount" else "value"
    target <- list()
    for(row in .cr_aff_rows(if(which=="claims") claims else collateral,key)) {
      rid <- .cr_aff_text(row[[key]],key)
      if(rid %in% names(target) || !identical(row$currency,currency)) .cr_error("INVALID_INPUT","Duplicate identifier or currency mismatch")
      row[[amount_name]] <- .cr_aff_scalar(row[[amount_name]],amount_name,TRUE)
      if(which=="collateral") {
        .cr_date(row$valuation_date,"valuation_date")
        if(length(row$freshness)!=1L || !row$freshness %in% c("CURRENT","STALE")) .cr_error("INVALID_INPUT","Declare valuation freshness")
      }
      target[[rid]] <- row
    }
    if(which=="claims") cs <- target else assets <- target
  }
  by_asset <- list();used <- setNames(rep(0,length(cs)),names(cs));pairs <- list()
  for(row in .cr_aff_rows(allocations,"allocations")) {
    cid <- row$claim_id;aid <- row$collateral_id
    if(length(cid)!=1L || length(aid)!=1L || !cid %in% names(cs) || !aid %in% names(assets) ||
       any(vapply(pairs,function(p) identical(p,list(cid,aid)),logical(1))))
      .cr_error("INVALID_ALLOCATION","Unknown or repeated allocation")
    priority <- .cr_aff_scalar(row$priority,"priority",TRUE)
    if(priority!=floor(priority)) .cr_error("INVALID_ALLOCATION","Lien priority must be integer")
    amount <- .cr_aff_scalar(row$amount,"allocation amount",TRUE)
    pairs[[length(pairs)+1L]] <- list(cid,aid);used[[cid]] <- used[[cid]]+amount
    entries <- by_asset[[aid]];if(is.null(entries)) entries <- list()
    entries[[length(entries)+1L]] <- list(claim_id=cid,amount=amount,priority=priority);by_asset[[aid]] <- entries
  }
  for(cid in names(cs)) if(abs(used[[cid]]-cs[[cid]]$amount)>max(1e-12,1e-12*max(abs(used[[cid]]),abs(cs[[cid]]$amount))))
    .cr_error("INVALID_ALLOCATION","Allocations must partition each secured claim exactly once")
  rows <- list();coverage <- list()
  for(aid in sort(names(by_asset),method="radix")) {
    value <- assets[[aid]]$value;remaining <- value;entries <- by_asset[[aid]]
    total <- sum(vapply(entries,function(x) x$amount,numeric(1)));cumulative <- 0
    for(rank in sort(unique(vapply(entries,function(x) x$priority,numeric(1))))) {
      peers <- Filter(function(x) x$priority==rank,entries)
      peers <- peers[order(vapply(peers,function(x) x$claim_id,character(1)),method="radix")]
      mass <- sum(vapply(peers,function(x) x$amount,numeric(1)));secured <- min(remaining,mass);cumulative <- cumulative+mass
      for(x in peers) {
        allocated <- if(mass==0) 0 else secured*x$amount/mass
        coverage[[length(coverage)+1L]] <- c(x,list(collateral_id=aid,covered_amount=allocated,
          uncovered_amount=x$amount-allocated,cumulative_ltv=if(value==0) NULL else cumulative/value))
      }
      remaining <- remaining-secured
    }
    rows[[length(rows)+1L]] <- list(collateral_id=aid,cltv=if(value==0) NULL else total/value,
      value=value,claims=total,valuation_date=assets[[aid]]$valuation_date,freshness=assets[[aid]]$freshness,
      denominator_status=if(value==0) "ZERO_VALUE" else "APPLICABLE")
  }
  total_value <- sum(vapply(names(by_asset),function(k) assets[[k]]$value,numeric(1)))
  total_claims <- sum(vapply(cs,function(x) x$amount,numeric(1)))
  .cr_aff_result(list(cltv=if(total_value==0) NULL else total_claims/total_value,total_claims=total_claims,
    allocated_collateral_value=total_value,collateral=rows,priority_coverage=coverage,currency=currency),
    "affordability.combined_loan_to_value","ratio",context,
    limitations="Rank coverage is a static proportional waterfall without realization costs; it is not an LGD estimate.")
}
.cr_aff_units <- function(period, currency, basis="net") {
  if (!is.character(period) || length(period)!=1L || is.na(period) ||
      !period %in% names(.cr_aff_frequencies) || !is.character(currency) ||
      length(currency)!=1L || is.na(currency) || !nzchar(trimws(currency)) ||
      !is.character(basis) || length(basis)!=1L || is.na(basis) ||
      !basis %in% c("net","gross"))
    .cr_error("INVALID_UNITS","Declare frequency, currency and income basis")
}
.cr_aff_align <- function(...) {
  values <- list(...)
  for (nm in names(values)) .cr_numeric(values[[nm]],nm,vector_only=TRUE)
  sizes <- vapply(values,length,integer(1))
  if (length(unique(sizes[sizes!=1L]))>1L)
    .cr_error("INCOMPATIBLE_SHAPE","Only length-one expansion is supported")
  n <- max(sizes)
  lapply(values,function(v) if(length(v)==1L) rep(v,n) else v)
}
.cr_aff_ratios <- function(numerator,denominator) {
  ratio <- lapply(seq_along(numerator),function(i) {
    if(denominator[i]<=0) return(NULL)
    x <- numerator[i]/denominator[i]
    if(!is.finite(x)) .cr_error("NUMERICAL_FAILURE","Ratio exceeds finite range")
    x
  })
  status <- ifelse(denominator>0,"APPLICABLE","NONPOSITIVE_INCOME")
  list(ratio=if(length(ratio)==1L) ratio[[1L]] else ratio,
       denominator_status=status,applicable=denominator>0)
}
debt_to_income <- function(debt_stock,income,income_frequency,currency,ratio_period,
                           income_basis="net",context=NULL) {
  .cr_aff_units(income_frequency,currency,income_basis)
  if(!is.character(ratio_period) || length(ratio_period)!=1L || is.na(ratio_period) ||
     !ratio_period %in% c("annual","income_period"))
    .cr_error("INVALID_UNITS","ratio_period must be annual or income_period")
  a <- .cr_aff_align(debt_stock=debt_stock,income=income)
  if(any(a$debt_stock<0)) .cr_error("INVALID_AMOUNT","Debt stock must be nonnegative")
  factor <- if(ratio_period=="annual") unname(.cr_aff_frequencies[income_frequency]) else 1
  denominator <- a$income*factor
  if(any(!is.finite(denominator))) .cr_error("NUMERICAL_FAILURE","Annualized income exceeds finite range")
  .cr_result(.cr_aff_ratios(a$debt_stock,denominator),"affordability.debt_to_income","ratio",context,
    diagnostics=list(currency=currency,income_basis=income_basis,income_frequency=income_frequency,
                     ratio_period=ratio_period,annualization_factor=factor),
    limitations="Debt-to-income is not a default probability or debt-service ratio.")
}
debt_service_to_income <- function(debt_service,income,period,currency,income_basis="net",context=NULL) {
  .cr_aff_units(period,currency,income_basis)
  a <- .cr_aff_align(debt_service=debt_service,income=income)
  if(any(a$debt_service<0)) .cr_error("INVALID_AMOUNT","Debt service must be nonnegative")
  .cr_result(.cr_aff_ratios(a$debt_service,a$income),"affordability.debt_service_to_income","ratio",context,
    diagnostics=list(currency=currency,period=period,income_basis=income_basis),
    limitations="Income and debt service must cover the same period and debt scope.")
}
residual_income <- function(net_income,recurring_expenses,existing_debt_service,new_debt_service,
                            period,currency,context=NULL) {
  .cr_aff_units(period,currency)
  a <- .cr_aff_align(net_income=net_income,recurring_expenses=recurring_expenses,
                    existing_debt_service=existing_debt_service,new_debt_service=new_debt_service)
  if(any(unlist(a[names(a)!="net_income"])<0))
    .cr_error("INVALID_AMOUNT","Expenses and debt-service amounts must be nonnegative")
  remaining <- a$net_income-a$recurring_expenses-a$existing_debt_service-a$new_debt_service
  if(any(!is.finite(remaining))) .cr_error("NUMERICAL_FAILURE","Residual arithmetic exceeds finite range")
  .cr_result(list(residual=remaining,negative_buffer=remaining<0,minimum=min(remaining)),
    "affordability.residual_income",currency,context,
    diagnostics=list(period=period,currency=currency,components=a),
    limitations="Expense and service inputs must be reconciled disjoint totals.")
}
assess_affordability <- function(metrics,threshold_profile,evidence,context=NULL) {
  if(!.cr_object(metrics) || !.cr_object(threshold_profile) || !.cr_object(evidence))
    .cr_error("INVALID_INPUT","Metrics, profile and evidence must be objects")
  p <- threshold_profile
  if(!identical(p$schema_version,"1.0")) .cr_error("UNSUPPORTED_SCHEMA_VERSION","Unsupported affordability profile")
  for(nm in c("policy_id","version")) .cr_string(p[[nm]],nm,"INVALID_POLICY")
  required <- p$required_evidence
  if(is.character(required)) required <- as.list(required)
  if(!is.list(required) || !is.null(names(required)))
    .cr_error("INVALID_POLICY","required_evidence must be an array of names")
  for(x in required) .cr_string(x,"evidence name","INVALID_POLICY")
  required <- unlist(required,use.names=FALSE)
  if(anyDuplicated(required)) .cr_error("INVALID_POLICY","Evidence names must be distinct")
  rules <- p$rules
  if(!is.list(rules) || length(rules)==0L || !is.null(names(rules)))
    .cr_error("INVALID_POLICY","At least one explicit affordability rule is required")
  for(v in evidence) if(!is.logical(v) || length(v)!=1L || is.na(v))
    .cr_error("INVALID_INPUT","Evidence flags must be boolean")
  missing_evidence <- required[!vapply(required,function(k)identical(evidence[[k]],TRUE),logical(1))]
  trace <- list(); breaches <- missing <- seen <- character()
  for(rule in rules) {
    if(!.cr_object(rule) || !all(c("rule_id","metric","operator","threshold") %in% names(rule)))
      .cr_error("INVALID_POLICY","Incomplete affordability rule")
    rid <- .cr_string(rule$rule_id,"rule_id","INVALID_POLICY")
    metric <- .cr_string(rule$metric,"metric","INVALID_POLICY")
    op <- rule$operator
    if(rid %in% seen || !is.character(op) || length(op)!=1L || is.na(op) ||
       !op %in% c("<=","<",">=",">")) .cr_error("INVALID_POLICY","Invalid or duplicate affordability rule")
    seen <- c(seen,rid)
    threshold <- .cr_numeric(rule$threshold,"threshold",vector_only=TRUE)
    if(length(threshold)!=1L) .cr_error("INVALID_POLICY","Rule threshold must be scalar")
    value <- metrics[[metric]]
    if(is.null(value)) {
      missing <- unique(c(missing,metric)); passed <- headroom <- NULL
    } else {
      value <- .cr_numeric(value,metric,vector_only=TRUE)
      if(length(value)!=1L) .cr_error("INVALID_INPUT","Assessment metrics must be scalar")
      passed <- switch(op,"<="=value<=threshold,"<"=value<threshold,">="=value>=threshold,">"=value>threshold)
      headroom <- if(op %in% c("<=","<")) threshold-value else value-threshold
      if(!is.finite(headroom)) .cr_error("NUMERICAL_FAILURE","Headroom arithmetic exceeds finite range")
      if(!passed) breaches <- c(breaches,rid)
    }
    trace[[length(trace)+1L]] <- list(rule_id=rid,metric=metric,operator=op,threshold=threshold,
                                     value=value,passed=passed,headroom=headroom)
  }
  assessment <- if(length(missing) || length(missing_evidence)) "INSUFFICIENT_EVIDENCE" else
    if(length(breaches)) "NOT_AFFORDABLE" else "AFFORDABLE"
  .cr_result(list(assessment=assessment,rule_trace=trace,breaches=as.list(breaches),
    missing_metrics=as.list(missing),missing_evidence=as.list(missing_evidence),
    policy_id=p$policy_id,policy_version=p$version),"affordability.assess_affordability",
    "affordability_assessment",context,
    limitations="This assessment does not approve credit and does not alter a PD estimate.")
}
