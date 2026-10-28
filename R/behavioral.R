.cr_beh_result <- function(value,...) {
  check <- function(x) {
    if(is.list(x)) {for(v in x) check(v)}
    else if(is.numeric(x)&&any(!is.finite(x))) .cr_error("NUMERICAL_FAILURE","Behavioral output exceeds finite numeric range")
  }
  check(value);.cr_result(value,...)
}
# Dated account risk sets, payment allocation and empirical state dynamics.
.cr_beh_amount <- function(x,name,nonnegative=TRUE) {
  x <- .cr_numeric(x,name)
  if(length(x)!=1L || (nonnegative && x<0)) .cr_error("BEHAVIORAL_INPUT",paste(name,"requires finite scalar amount"))
  as.numeric(x)
}
.cr_beh_array <- function(x,name) {
  if(is.list(x) && is.null(names(x))) x <- unlist(x,use.names=FALSE)
  if(is.null(x)) x <- character()
  if(!is.character(x) || anyNA(x) || any(!nzchar(x)) || anyDuplicated(x))
    .cr_error("BEHAVIORAL_INPUT",paste(name,"requires distinct strings"))
  x
}
.cr_beh_month <- function(d,n,end=FALSE) {
  total <- as.integer(format(d,"%Y"))*12+as.integer(format(d,"%m"))-1+n
  y <- total%/%12;m <- total%%12+1
  first <- as.Date(sprintf("%04d-%02d-01",y,m));last <- seq(first,by="month",length.out=2L)[2L]-1
  day <- if(end) as.integer(format(last,"%d")) else min(as.integer(format(d,"%d")),as.integer(format(last,"%d")))
  as.Date(sprintf("%04d-%02d-%02d",y,m,day))
}
.cr_beh_events <- function(events) {
  rows <- .cr_data_rows(events,"events");.cr_data_unique(rows,"event_id","events")
  for(r in rows) {
    .cr_data_required(r,c("account_id","event_type","occurred_at","known_at"))
    if(.cr_data_date(r$known_at,"known_at")<.cr_data_date(r$occurred_at,"occurred_at"))
      .cr_error("DATA_DATE","Event knowledge precedes occurrence")
  }
  rows
}
.cr_beh_sort <- function(rows,keys) {
  if(!length(rows)) return(rows)
  cols <- lapply(keys,function(k) vapply(rows,function(r) r[[k]],character(1)))
  rows[do.call(order,c(cols,list(method="radix")))]
}

compute_days_past_due <- function(obligations,payments,as_of,known_at,
                                  allocation_policy="fifo",currency,context=NULL) {
  anchor <- .cr_data_date(as_of,"as_of");cutoff <- .cr_data_date(known_at,"known_at")
  if(length(allocation_policy)!=1L || !allocation_policy %in% c("fifo","explicit") ||
     !is.character(currency) || length(currency)!=1L || !nzchar(currency))
    .cr_error("BEHAVIORAL_INPUT","Declare allocation policy and currency")
  obs <- .cr_data_rows(obligations,"obligations");pays <- .cr_data_rows(payments,"payments")
  .cr_data_unique(obs,"obligation_id","obligations");.cr_data_unique(pays,"payment_id","payments")
  accounts <- character()
  for(r in obs) {
    .cr_data_required(r,c("account_id","due_at","known_at","amount","currency"))
    .cr_data_date(r$due_at,"due_at");.cr_data_date(r$known_at,"known_at");.cr_beh_amount(r$amount,"obligation amount")
    accounts <- c(accounts,r$account_id)
    if(!identical(r$currency,currency)) .cr_error("BEHAVIORAL_CURRENCY","Currency mismatch")
  }
  for(r in pays) {
    .cr_data_required(r,c("account_id","paid_at","known_at","amount","currency","kind"))
    if(.cr_data_date(r$known_at,"known_at")<.cr_data_date(r$paid_at,"paid_at")) .cr_error("DATA_DATE","Payment knowledge precedes occurrence")
    .cr_beh_amount(r$amount,"payment amount");accounts <- c(accounts,r$account_id)
    if(!identical(r$currency,currency)) .cr_error("BEHAVIORAL_CURRENCY","Currency mismatch")
    if(!r$kind %in% c("payment","reversal")) .cr_error("BEHAVIORAL_INPUT","Unknown payment kind")
  }
  accounts <- unique(accounts)
  if(length(accounts)>1L) .cr_error("BEHAVIORAL_INPUT","DPD requires one account")
  due <- .cr_beh_sort(Filter(function(r) .cr_data_date(r$due_at,"due_at")<=anchor && .cr_data_date(r$known_at,"known_at")<=cutoff,obs),c("due_at","obligation_id"))
  visible <- Filter(function(r) .cr_data_date(r$paid_at,"paid_at")<=anchor && .cr_data_date(r$known_at,"known_at")<=cutoff,pays)
  credits <- list()
  for(r in visible) if(r$kind=="payment") {r$net <- as.numeric(r$amount);credits[[r$payment_id]] <- r}
  for(r in visible) if(r$kind=="reversal") {
    original <- if(length(r$reverses_payment_id)==1L) credits[[r$reverses_payment_id]] else NULL
    if(is.null(original) || original$paid_at>r$paid_at) .cr_error("BEHAVIORAL_REVERSAL","Reversal lacks earlier visible payment")
    original$net <- original$net-r$amount
    if(original$net<0) .cr_error("BEHAVIORAL_REVERSAL","Reversals exceed original payment")
    credits[[r$reverses_payment_id]] <- original
  }
  available <- sum(vapply(credits,function(r) r$net,numeric(1)));pool <- available;rows <- list()
  ids <- vapply(obs,function(r) r$obligation_id,character(1))
  if(allocation_policy=="explicit" && any(vapply(credits,function(r) length(r$obligation_id)!=1L || !r$obligation_id %in% ids,logical(1))))
    .cr_error("BEHAVIORAL_ALLOCATION","Explicit payments require known obligation ids")
  for(r in due) {
    funds <- if(allocation_policy=="fifo") pool else sum(vapply(credits,function(p) if(p$obligation_id==r$obligation_id) p$net else 0,numeric(1)))
    applied <- min(r$amount,funds)
    if(allocation_policy=="fifo") pool <- pool-applied
    unpaid <- r$amount-applied
    rows[[length(rows)+1L]] <- list(obligation_id=r$obligation_id,due_at=r$due_at,amount=as.numeric(r$amount),
      applied=applied,unpaid=unpaid,dpd=if(unpaid>0) as.integer(anchor-.cr_data_date(r$due_at,"due_at")) else 0L)
  }
  open_rows <- Filter(function(r) r$unpaid>0,rows)
  past_due <- sum(vapply(rows,function(r) if(r$due_at<as_of) r$unpaid else 0,numeric(1)))
  .cr_beh_result(list(account_id=if(length(accounts)) accounts[1] else NULL,as_of=as_of,known_at=known_at,
    dpd=if(length(open_rows)) max(vapply(open_rows,function(r) r$dpd,numeric(1))) else 0L,
    past_due_amount=past_due,unpaid_due_amount=sum(vapply(rows,function(r) r$unpaid,numeric(1))),
    oldest_unpaid_obligation=if(length(open_rows)) open_rows[[1]]$obligation_id else NULL,
    obligations=rows,net_payments=available,unallocated_payments=available-sum(vapply(rows,function(r) r$applied,numeric(1))),currency=currency),
    "behavioral.compute_days_past_due","days",context,
    limitations="No materiality or grace threshold is applied; obligations must represent the historical contractual schedule.")
}

assign_delinquency_state <- function(dpd,past_due_amount,exposure,events,state_profile,context=NULL) {
  .cr_data_days(dpd,"dpd");arrears <- .cr_beh_amount(past_due_amount,"past_due_amount");ead <- .cr_beh_amount(exposure,"exposure")
  bool <- function(x) is.logical(x)&&length(x)==1L&&!is.na(x)
  if(!.cr_object(events) || !all(vapply(events,bool,logical(1))) || !.cr_object(state_profile))
    .cr_error("BEHAVIORAL_PROFILE","Supply event flags and state profile")
  p <- state_profile
  if(!identical(p$schema_version,"1.0")) .cr_error("UNSUPPORTED_SCHEMA_VERSION","Unsupported state profile")
  .cr_data_required(p,c("profile_id","immaterial_state","materiality","buckets","event_priority"))
  material <- p$materiality
  if(!.cr_object(material)||length(material$logic)!=1L||!material$logic %in% c("and","or")) .cr_error("BEHAVIORAL_PROFILE","Declare materiality logic")
  absolute <- .cr_beh_amount(material$absolute,"absolute materiality");relative <- .cr_beh_amount(material$relative,"relative materiality")
  relative_met <- if(ead==0) arrears>0 else arrears/ead>=relative
  tests <- c(arrears>=absolute,relative_met)
  is_material <- arrears>0 && if(material$logic=="and") all(tests) else any(tests)
  buckets <- .cr_data_rows(p$buckets,"buckets");priorities <- .cr_data_rows(p$event_priority,"event_priority")
  lower <- 0;states <- character()
  for(i in seq_along(buckets)) {
    b <- buckets[[i]];.cr_data_required(b,c("state","lower"));.cr_data_days(b$lower,"lower");upper <- b$upper
    if(b$state %in% states || b$lower!=lower || (is.null(upper)&&i!=length(buckets))) .cr_error("BEHAVIORAL_PROFILE","Buckets must be distinct and gap-free")
    if(!is.null(upper)) {.cr_data_days(upper,"upper");if(upper<=lower) .cr_error("BEHAVIORAL_PROFILE","Empty bucket")}
    states <- c(states,b$state);lower <- upper
  }
  if(!length(buckets)||!is.null(buckets[[length(buckets)]]$upper)) .cr_error("BEHAVIORAL_PROFILE","Final bucket is unbounded")
  seen <- character()
  for(r in priorities) {
    .cr_data_required(r,c("event","state","at_risk","absorbing"))
    if(r$event %in% seen || !bool(r$at_risk)||!bool(r$absorbing)) .cr_error("BEHAVIORAL_PROFILE","Invalid event priority")
    seen <- c(seen,r$event)
  }
  if(length(setdiff(names(events),seen))) .cr_error("BEHAVIORAL_PROFILE","Every event requires declared precedence")
  for(r in priorities) if(isTRUE(events[[r$event]]))
    return(.cr_beh_result(list(state=r$state,trigger=r$event,material=is_material,at_risk=r$at_risk,
      absorbing=r$absorbing,profile_id=p$profile_id),"behavioral.assign_delinquency_state","state",context))
  rule <- p$dpd_default_rule
  if(!is.null(rule)) {
    .cr_data_required(rule,c("minimum_dpd","state","material_required"));.cr_data_days(rule$minimum_dpd,"minimum_dpd")
    if(!bool(rule$material_required)) .cr_error("BEHAVIORAL_PROFILE","material_required must be boolean")
    if(dpd>=rule$minimum_dpd && (is_material || !rule$material_required))
      return(.cr_beh_result(list(state=rule$state,trigger="DECLARED_DPD_DEFAULT",material=is_material,
        at_risk=FALSE,absorbing=TRUE,profile_id=p$profile_id),"behavioral.assign_delinquency_state","state",context))
  }
  state <- p$immaterial_state
  if(is_material) for(b in buckets) if(b$lower<=dpd && (is.null(b$upper)||dpd<b$upper)) {state <- b$state;break}
  .cr_beh_result(list(state=state,trigger=if(is_material) "DPD_BUCKET" else "IMMATERIAL",material=is_material,
    at_risk=TRUE,absorbing=FALSE,profile_id=p$profile_id),"behavioral.assign_delinquency_state","state",context,
    limitations="A delinquency bucket is not a regulatory default definition.")
}

build_account_month_panel <- function(accounts,statements,account_events,observation_dates,
                                      reporting_lag_days=0,obligations=NULL,payments=NULL,context=NULL) {
  acc <- .cr_data_rows(accounts,"accounts");.cr_data_unique(acc,"account_id","accounts")
  stmt <- .cr_data_rows(statements,"statements");.cr_data_unique(stmt,"statement_id","statements")
  events <- .cr_beh_events(account_events);dates <- .cr_beh_array(observation_dates,"observation_dates")
  lag <- .cr_data_days(reporting_lag_days,"reporting_lag_days")
  for(d in dates) if(.cr_data_date(d,"observation_date")!=.cr_beh_month(.cr_data_date(d,"observation_date"),0,TRUE))
    .cr_error("DATA_DATE","Panel observations must be calendar month ends")
  aids <- vapply(acc,function(a) a$account_id,character(1))
  for(a in acc) {
    .cr_data_required(a,c("customer_id","product_id","opened_at","known_at","currency"))
    if(.cr_data_date(a$known_at,"known_at")<.cr_data_date(a$opened_at,"opened_at")) .cr_error("DATA_DATE","Account known before opening")
  }
  for(s in stmt) {
    .cr_data_required(s,c("account_id","observed_at","known_at","balance","credit_limit","dpd","past_due_amount","currency"))
    if(!s$account_id %in% aids) .cr_error("BEHAVIORAL_ACCOUNT","Unknown statement account")
    if(.cr_data_date(s$known_at,"known_at")<.cr_data_date(s$observed_at,"observed_at")) .cr_error("DATA_DATE","Statement known before observation")
    for(k in c("balance","credit_limit","past_due_amount")) .cr_beh_amount(s[[k]],k)
    .cr_data_days(s$dpd,"dpd")
  }
  for(e in events) if(!e$account_id %in% aids || !e$event_type %in% c("ACTIVE","DEFAULT","CHARGEOFF","CURED","CLOSED","PREPAID","SOLD"))
    .cr_error("BEHAVIORAL_EVENT","Unknown account or lifecycle event")
  if(is.null(obligations)!=is.null(payments)) .cr_error("BEHAVIORAL_INPUT","Supply obligations and payments together")
  obs <- if(is.null(obligations)) NULL else .cr_data_rows(obligations,"obligations")
  pays <- if(is.null(payments)) NULL else .cr_data_rows(payments,"payments")
  output <- list()
  for(anchor in sort(dates,method="radix")) {
    cutoff <- format(.cr_data_date(anchor,"date")-lag,"%Y-%m-%d")
    for(a in .cr_beh_sort(acc,"account_id")) {
      if(a$opened_at>anchor || a$known_at>cutoff) next
      cid <- a$account_id
      history <- .cr_beh_sort(Filter(function(e) e$account_id==cid && e$occurred_at<=anchor && e$known_at<=cutoff,events),c("occurred_at","known_at","event_id"))
      if(anyDuplicated(vapply(history,function(e) paste(e$occurred_at,e$known_at),character(1)))) .cr_error("BEHAVIORAL_AMBIGUITY","Simultaneous lifecycle events need resolution")
      terminal <- Filter(function(e) e$event_type %in% c("CLOSED","PREPAID","SOLD"),history)
      if(length(terminal) && any(vapply(history,function(e) e$occurred_at>terminal[[1]]$occurred_at && e$event_type %in% c("ACTIVE","CURED"),logical(1))))
        .cr_error("BEHAVIORAL_EVENT","Legal termination cannot be silently reopened")
      lifecycle <- if(length(terminal)) terminal[[1]]$event_type else if(length(history)) history[[length(history)]]$event_type else "ACTIVE"
      ever_default <- any(vapply(history,function(e) e$event_type %in% c("DEFAULT","CHARGEOFF"),logical(1)))
      available <- .cr_beh_sort(Filter(function(s) s$account_id==cid && s$observed_at<=anchor && s$known_at<=cutoff,stmt),c("observed_at","known_at","statement_id"))
      s <- if(length(available)) available[[length(available)]] else NULL
      if(length(available)>1L) {
        previous <- available[[length(available)-1L]]
        if(s$observed_at==previous$observed_at && s$known_at==previous$known_at) .cr_error("BEHAVIORAL_AMBIGUITY","Ambiguous statement revision")
      }
      if(!is.null(s)&&s$currency!=a$currency) .cr_error("BEHAVIORAL_CURRENCY","Statement currency mismatch")
      keys <- c("balance","credit_limit","dpd","past_due_amount")
      features <- setNames(lapply(keys,function(k) if(is.null(s)) NULL else s[[k]]),keys)
      if(!is.null(obs)) {
        dpd <- compute_days_past_due(Filter(function(r) r$account_id==cid,obs),Filter(function(r) r$account_id==cid,pays),anchor,cutoff,currency=a$currency)$value
        features$dpd <- dpd$dpd;features$past_due_amount <- dpd$past_due_amount
      }
      features["utilization"] <- list(if(is.null(features$balance)||is.null(features$credit_limit)||features$credit_limit==0) NULL else features$balance/features$credit_limit)
      opened <- .cr_data_date(a$opened_at,"opened_at");adate <- .cr_data_date(anchor,"date")
      mob <- (as.integer(format(adate,"%Y"))-as.integer(format(opened,"%Y")))*12+as.integer(format(adate,"%m"))-as.integer(format(opened,"%m"))
      row <- list(account_id=cid,customer_id=a$customer_id,product_id=a$product_id,as_of=anchor,known_at=cutoff,
        opened_at=a$opened_at,currency=a$currency,months_on_book=mob,lifecycle_state=lifecycle,state=lifecycle,
        ever_default=ever_default,at_risk=lifecycle %in% c("ACTIVE","CURED") && !ever_default,features=features,
        statement_id=if(is.null(s)) NULL else s$statement_id,statement_observed_at=if(is.null(s)) NULL else s$observed_at,
        source_age_days=if(is.null(s)) NULL else as.integer(adate-.cr_data_date(s$observed_at,"observed_at")),
        source_complete=all(vapply(features[keys],function(v) !is.null(v),logical(1))))
      row$snapshot_hash <- .cr_hash(row);output[[length(output)+1L]] <- row
    }
  }
  customer_groups <- list()
  for(r in output) {
    k <- .cr_data_key(r,c("customer_id","as_of","currency"))
    customer_groups[[k]] <- c(customer_groups[[k]],list(r))
  }
  customers <- lapply(customer_groups,function(rows) {
    r <- rows[[1]];active <- Filter(function(x) !x$lifecycle_state %in% c("CLOSED","PREPAID","SOLD"),rows)
    balances <- lapply(active,function(x) x$features$balance)
    list(customer_id=r$customer_id,as_of=r$as_of,currency=r$currency,account_count=length(rows),
      at_risk_accounts=sum(vapply(rows,function(x) x$at_risk,logical(1))),
      active_balance=if(any(vapply(balances,is.null,logical(1)))) NULL else sum(unlist(balances)))
  })
  customers <- unname(.cr_beh_sort(customers,c("customer_id","as_of","currency")))
  .cr_beh_result(list(accounts=output,customers=customers),"behavioral.build_account_month_panel","panel",context,
    limitations=c("Features preserve reporting cutoffs; target construction uses separate future event evidence.",
      "Customer balance aggregation is not a customer PD. Closed accounts remain visible for lifecycle/recovery context."))
}

.cr_beh_coverage <- function(coverage,cutoff) {
  rows <- .cr_data_rows(coverage,"coverage");.cr_data_unique(rows,"account_id","coverage");output <- list()
  for(r in rows) {
    .cr_data_required(r,c("observed_through","known_at"))
    end <- .cr_data_date(r$observed_through,"observed_through");known <- .cr_data_date(r$known_at,"known_at")
    if(known<end) .cr_error("DATA_DATE","Completeness known before observation end")
    if(known<=cutoff) output[[r$account_id]] <- min(end,cutoff)
  }
  output
}

build_behavioral_targets <- function(panel,events,horizon_months,target_event,competing_events,
                                     exit_treatment,evaluation_cutoff,coverage,context=NULL) {
  rows <- .cr_data_rows(panel,"panel");ev <- .cr_beh_events(events)
  horizon <- .cr_data_days(horizon_months,"horizon_months",1)
  competing <- .cr_beh_array(competing_events,"competing_events")
  .cr_beh_array(list(target_event),"target_event")
  if(target_event %in% competing || !exit_treatment %in% c("competing","censor")) .cr_error("BEHAVIORAL_TARGET","Declare distinct target and exit treatment")
  cutoff <- .cr_data_date(evaluation_cutoff,"evaluation_cutoff");complete <- .cr_beh_coverage(coverage,cutoff)
  output <- list();seen <- character()
  for(r in rows) {
    .cr_data_required(r,c("account_id","customer_id","as_of","at_risk","ever_default"))
    key <- .cr_data_key(r,c("account_id","as_of"))
    if(key %in% seen) .cr_error("DATA_DUPLICATE","Duplicate account snapshot")
    seen <- c(seen,key);anchor <- .cr_data_date(r$as_of,"as_of");end <- .cr_beh_month(anchor,horizon)
    history <- .cr_beh_sort(Filter(function(e) e$account_id==r$account_id && .cr_data_date(e$known_at,"known_at")<=cutoff,ev),c("occurred_at","event_id"))
    prior <- any(vapply(history,function(e) e$event_type==target_event && .cr_data_date(e$occurred_at,"occurred_at")<=anchor,logical(1)))
    eligible <- isTRUE(r$at_risk)&&!r$ever_default&&!prior&&anchor<=cutoff
    future <- Filter(function(e) anchor<.cr_data_date(e$occurred_at,"occurred_at") && .cr_data_date(e$occurred_at,"occurred_at")<=min(end,cutoff) && e$event_type %in% c(target_event,competing),history)
    if(length(future)&&any(vapply(future,function(e) e$occurred_at==future[[1]]$occurred_at && e$event_type!=future[[1]]$event_type,logical(1))))
      .cr_error("BEHAVIORAL_AMBIGUITY","Same-day competing outcomes need ordering")
    first <- if(length(future)) future[[1]] else NULL;observed <- complete[[r$account_id]]
    label <- NULL;status <- "NOT_AT_RISK"
    if(eligible) {
      if(!is.null(first)&&first$event_type==target_event) {label <- 1L;status <- "EVENT"}
      else if(!is.null(first)) {label <- if(exit_treatment=="competing") 0L else NULL;status <- if(exit_treatment=="competing") "COMPETING_EVENT" else "CENSORED_EXIT"}
      else if(!is.null(observed)&&observed>=end) {label <- 0L;status <- "MATURE_NO_EVENT"}
      else status <- if(end<=cutoff) "OUTCOME_PENDING" else "IMMATURE"
    }
    stop_date <- if(!is.null(observed)) min(end,cutoff,observed) else min(end,cutoff)
    if(!is.null(first)) stop_date <- .cr_data_date(first$occurred_at,"occurred_at")
    output[[length(output)+1L]] <- list(account_id=r$account_id,customer_id=r$customer_id,as_of=r$as_of,
      horizon_end=format(end,"%Y-%m-%d"),target_event=target_event,label=label,eligible=eligible,status=status,
      first_event_id=if(!is.null(first)&&eligible) first$event_id else NULL,
      first_event_type=if(!is.null(first)&&eligible) first$event_type else NULL,
      followup_days=if(!is.null(observed)||!is.null(first)) max(0,as.integer(stop_date-anchor)) else NULL,calendar_mature=end<=cutoff,
      outcome_complete=!is.null(label),evaluation_cutoff=evaluation_cutoff)
  }
  .cr_beh_result(output,"behavioral.build_behavioral_targets","labels",context,
    limitations=c("Overlapping account windows and customers are dependent; isolate customers/groups and time in fitting and evaluation.",
      "A competing-exit zero means no first target before that exit, not hypothetical continuing-account performance."))
}

estimate_retail_roll_rates <- function(panel,states,from_date,to_date,group_by,weighting,
                                       missing_successor,context=NULL) {
  states <- .cr_beh_array(states,"states");groups <- .cr_beh_array(group_by,"group_by")
  if(!length(states)||!weighting %in% c("count","opening_balance")||!missing_successor %in% c("error","censor"))
    .cr_error("BEHAVIORAL_INPUT","Declare states, weighting and missing successor policy")
  if(.cr_data_date(from_date,"from_date")>=.cr_data_date(to_date,"to_date")) .cr_error("DATA_DATE","Dates must increase")
  rows <- .cr_data_rows(panel,"panel");lookup <- list();buckets <- list();n <- length(states)
  for(r in rows) {
    .cr_data_required(r,c("account_id","as_of","state"));key <- .cr_data_key(r,c("account_id","as_of"))
    if(key %in% names(lookup)) .cr_error("DATA_DUPLICATE","Duplicate state snapshot")
    if(!r$state %in% states) .cr_error("BEHAVIORAL_STATE","Unknown state")
    lookup[[key]] <- r
  }
  for(r in rows) {
    if(r$as_of!=from_date) next
    for(g in groups) .cr_data_required(r,g)
    group <- if(length(groups)) setNames(lapply(groups,function(g) r[[g]]),groups) else .cr_empty_object()
    if(any(vapply(group,function(x) length(x)!=1L || !(is.character(x)||is.numeric(x)),logical(1)))) .cr_error("BEHAVIORAL_INPUT","Grouping values must be scalar")
    key <- .cr_hash(group);b <- buckets[[key]]
    if(is.null(b)) b <- list(group=group,counts=matrix(0L,n,n),mass=matrix(0,n,n),origin_count=rep(0L,n),
      origin_mass=rep(0,n),censored_count=rep(0L,n),censored_mass=rep(0,n))
    i <- match(r$state,states)
    balance <- if(!is.null(r$balance)) r$balance else r$features$balance
    weight <- if(weighting=="count") 1 else .cr_beh_amount(balance,"opening balance")
    b$origin_count[i] <- b$origin_count[i]+1L;b$origin_mass[i] <- b$origin_mass[i]+weight
    dest_key <- .cr_data_key(list(account_id=r$account_id,as_of=to_date),c("account_id","as_of"));successor <- lookup[[dest_key]]
    if(is.null(successor)) {
      if(missing_successor=="error") .cr_error("MISSING_SUCCESSOR","Explicit destination required, including exits")
      b$censored_count[i] <- b$censored_count[i]+1L;b$censored_mass[i] <- b$censored_mass[i]+weight
    } else {
      j <- match(successor$state,states);b$counts[i,j] <- b$counts[i,j]+1L;b$mass[i,j] <- b$mass[i,j]+weight
    }
    buckets[[key]] <- b
  }
  output <- unname(lapply(buckets,function(b) {
    observed <- rowSums(b$mass)
    probabilities <- lapply(seq_len(n),function(i) if(observed[i]==0) rep(list(NULL),n) else as.list(b$mass[i,]/observed[i]))
    list(group=b$group,states=as.list(states),transition_matrix=probabilities,
      counts=lapply(seq_len(n),function(i) as.list(b$counts[i,])),mass=lapply(seq_len(n),function(i) as.list(b$mass[i,])),
      origin_count=as.list(b$origin_count),origin_mass=as.list(b$origin_mass),
      censored_count=as.list(b$censored_count),censored_mass=as.list(b$censored_mass),
      observed_mass=as.list(observed),empty_rows=as.list(states[observed==0]))
  }))
  .cr_beh_result(list(from_date=from_date,to_date=to_date,weighting=weighting,groups=output),
    "behavioral.estimate_retail_roll_rates","transition_estimate",context,
    limitations=c("Censor mode estimates transitions conditional on observed successors, not an unbiased full-cohort matrix.",
      "Opening-balance weights measure monetary flows, not account probabilities; zero balances retain origin counts."))
}

.cr_beh_matrix <- function(x,name) {
  if(is.list(x)&&is.null(names(x))) {
    if(!length(x)) .cr_error("BEHAVIORAL_INPUT",paste(name,"matrix required"))
    nr <- length(x);nc <- length(x[[1]])
    if(any(vapply(x,length,integer(1))!=nc)) .cr_error("INCOMPATIBLE_SHAPE","Ragged matrix")
    for(row in x) for(value in row) .cr_beh_amount(value,name,FALSE)
    x <- matrix(unlist(x),nrow=nr,ncol=nc,byrow=TRUE)
  }
  if(!is.matrix(x)||!is.numeric(x)||any(!is.finite(x))) .cr_error("BEHAVIORAL_INPUT",paste(name,"finite numeric matrix required"))
  x
}
project_retail_roll_rates <- function(initial_distribution,transition_matrices,states,
                                      default_states,closure_states,exposure_by_period,context=NULL) {
  states <- .cr_beh_array(states,"states");defaults <- .cr_beh_array(default_states,"default_states");closures <- .cr_beh_array(closure_states,"closure_states")
  if(!length(states)||!length(defaults)||!all(c(defaults,closures) %in% states)||length(intersect(defaults,closures)))
    .cr_error("BEHAVIORAL_STATE","Declare disjoint default and closure states")
  if(is.list(initial_distribution)) initial_distribution <- unlist(initial_distribution)
  v <- .cr_numeric(initial_distribution,"initial_distribution",vector_only=TRUE);n <- length(states)
  if(length(v)!=n||any(v<0)||abs(sum(v)-1)>1e-12) .cr_error("INVALID_PROBABILITY","Initial distribution must sum to one")
  if(!is.list(transition_matrices)||!length(transition_matrices)) .cr_error("BEHAVIORAL_INPUT","Supply matrix for each period")
  exposure <- .cr_beh_matrix(exposure_by_period,"exposure_by_period")
  if(!identical(dim(exposure),c(length(transition_matrices),n))||any(exposure<0)) .cr_error("INCOMPATIBLE_SHAPE","Exposure requires period/state amounts")
  default_idx <- match(defaults,states);absorbing <- match(c(defaults,closures),states)
  prior_default <- sum(v[default_idx]);initial_default <- prior_default;output <- list()
  for(t in seq_along(transition_matrices)) {
    p <- .cr_beh_matrix(transition_matrices[[t]],"transition_matrix")
    if(!identical(dim(p),c(n,n))||any(p<0)||any(abs(rowSums(p)-1)>1e-12)) .cr_error("INVALID_PROBABILITY","Transition rows must sum to one")
    for(i in absorbing) if(!all(p[i,]==diag(n)[i,])) .cr_error("BEHAVIORAL_STATE","First-default/closure states must be absorbing")
    if(any(exposure[t,match(closures,states)]!=0)) .cr_error("BEHAVIORAL_EXPOSURE","Closed states must have zero exposure")
    flows <- v*p;v <- as.numeric(v %*% p)
    cumulative <- sum(v[default_idx]);marginal <- cumulative-prior_default
    output[[length(output)+1L]] <- list(period=t,state_probabilities=as.list(v),
      probability_flows=lapply(seq_len(n),function(i) as.list(flows[i,])),marginal_default=marginal,
      balance_flows=lapply(seq_len(n),function(i) as.list(flows[i,]*exposure[t,])),
      cumulative_default=cumulative,new_cumulative_default=cumulative-initial_default,
      state_exposure=as.list(exposure[t,]),expected_balance=sum(v*exposure[t,]),expected_balance_by_state=as.list(v*exposure[t,]))
    prior_default <- cumulative
  }
  .cr_beh_result(list(states=as.list(states),initial_default_mass=initial_default,periods=output),
    "behavioral.project_retail_roll_rates","projection",context,
    limitations=c("Exposure amounts are supplied separately for each state/period and must incorporate amortization, drawdowns and maturity.",
      "Default is absorbing for first-event probability; operational cure needs a separate state model."))
}

predict_behavioral_risk <- function(snapshot,model,target,calibrator=NULL,offset=NULL,context=NULL) {
  s <- if(inherits(snapshot,"CalcResult")) snapshot$value else snapshot
  if(!.cr_object(s)) .cr_error("BEHAVIORAL_INPUT","Snapshot must be object")
  payload <- s;digest <- payload$snapshot_hash;payload$snapshot_hash <- NULL
  if(!identical(digest,.cr_hash(payload))) .cr_error("INTEGRITY_ERROR","Snapshot differs from hash")
  m <- if(inherits(model,"FitResult")) model$model else if(inherits(model,"CalcResult")) model$value else model
  m <- .cr_model(m);contract <- m$behavioral_contract
  if(.cr_data_date(m$training$known_at,"training.known_at")>.cr_data_date(s$known_at,"snapshot.known_at"))
    .cr_error("BEHAVIORAL_LEAKAGE","Model training unavailable at snapshot cutoff")
  if(!.cr_object(contract)||!identical(contract$risk_unit,"account")||!identical(contract$event_policy,"first_default"))
    .cr_error("BEHAVIORAL_MODEL","Require account/first_default model contract")
  if(!s$product_id %in% unlist(contract$product_ids)||!s$lifecycle_state %in% unlist(contract$allowed_states))
    .cr_error("BEHAVIORAL_MODEL","Snapshot outside product/state support")
  if(!identical(s$at_risk,TRUE)||!identical(s$ever_default,FALSE)) .cr_error("BEHAVIORAL_RISK_SET","Default/terminated account requires separate model")
  max_age <- .cr_data_days(contract$maximum_source_age_days,"maximum_source_age_days")
  if(is.null(s$source_age_days)||s$source_age_days>max_age) .cr_error("BEHAVIORAL_STALE","Evidence missing or stale")
  if(!.cr_object(target)||!identical(target$probability_measure,"PHYSICAL")) .cr_error("BEHAVIORAL_TARGET","Physical target required")
  features <- s$features
  x <- matrix(vapply(m$feature_order,function(k) .cr_beh_amount(features[[k]],k,FALSE),numeric(1)),nrow=1)
  raw <- as.numeric(predict_pd(m,x,unlist(m$feature_order,use.names=FALSE),target,offset,context)$value[1]);pd <- raw
  if(!is.null(calibrator)) {
    artifact <- if(inherits(calibrator,"CalcResult")) calibrator$value else calibrator
    if(.cr_object(artifact)&&.cr_data_date(artifact$metadata$calibration$known_at,"calibration.known_at")>.cr_data_date(s$known_at,"snapshot.known_at"))
      .cr_error("BEHAVIORAL_LEAKAGE","Calibration unavailable at snapshot cutoff")
    if(.cr_object(artifact)&&identical(artifact$calibration_type,"isotonic")&&!identical(artifact$metadata$input_scale,"probability"))
      .cr_error("BEHAVIORAL_CALIBRATION","Isotonic calibration needs probability input scale")
    pd <- as.numeric(apply_calibrator(calibrator,c(raw),target,m$model_version,context)$value[1])
  }
  .cr_beh_result(list(account_id=s$account_id,customer_id=s$customer_id,as_of=s$as_of,known_at=s$known_at,
    target=target,pd=pd,unadjusted_pd=raw,calibrated=!is.null(calibrator),model_id=m$model_id,model_version=m$model_version,
    snapshot_hash=digest,source_complete=s$source_complete),"behavioral.predict_behavioral_risk","probability",context,
    limitations="Zero account balance does not imply zero PD; action thresholds and future EAD are separate.")
}

build_vintage_performance <- function(accounts,panel,events,months_on_book,evaluation_cutoff,
                                       coverage,default_event,delinquency_states,exit_events,context=NULL) {
  acc <- .cr_data_rows(accounts,"accounts");.cr_data_unique(acc,"account_id","accounts")
  rows <- .cr_data_rows(panel,"panel");ev <- .cr_beh_events(events)
  ages <- unlist(months_on_book)
  if(!length(ages)||anyDuplicated(ages)) .cr_error("BEHAVIORAL_INPUT","Supply distinct MOB values")
  for(n in ages) .cr_data_days(n,"months_on_book")
  delinquent <- .cr_beh_array(delinquency_states,"delinquency_states");exits <- .cr_beh_array(exit_events,"exit_events")
  .cr_beh_array(list(default_event),"default_event")
  if(default_event %in% exits) .cr_error("BEHAVIORAL_TARGET","Default and exits must differ")
  cutoff <- .cr_data_date(evaluation_cutoff,"evaluation_cutoff");complete <- .cr_beh_coverage(coverage,cutoff);lookup <- list()
  for(r in rows) {
    .cr_data_required(r,c("account_id","as_of","state"));key <- .cr_data_key(r,c("account_id","as_of"))
    if(key %in% names(lookup)) .cr_error("DATA_DUPLICATE","Duplicate vintage snapshot")
    lookup[[key]] <- r
  }
  cohorts <- list()
  for(a in acc) {
    .cr_data_required(a,c("opened_at","known_at","original_balance","currency"));opened <- .cr_data_date(a$opened_at,"opened_at")
    if(.cr_data_date(a$known_at,"known_at")>cutoff||opened>cutoff) next
    .cr_beh_amount(a$original_balance,"original_balance")
    key <- .cr_data_key(list(cohort=substr(a$opened_at,1,7),currency=a$currency),c("cohort","currency"))
    cohorts[[key]] <- c(cohorts[[key]],list(a))
  }
  cohort_order <- order(vapply(cohorts,function(x) substr(x[[1]]$opened_at,1,7),character(1)),vapply(cohorts,function(x) x[[1]]$currency,character(1)),method="radix")
  output <- list()
  for(members in cohorts[cohort_order]) {
    cohort <- substr(members[[1]]$opened_at,1,7);currency <- members[[1]]$currency
    for(age in sort(ages)) {
      end <- .cr_beh_month(as.Date(paste0(cohort,"-01")),age,TRUE);prev <- .cr_beh_month(as.Date(paste0(cohort,"-01")),age-1,TRUE)
      counts <- list(defaults=0L,incidence=0L,exits=0L,complete_accounts=0L,stock_observed=0L,delinquent_stock=0L);losses <- 0
      for(a in members) {
        hist <- .cr_beh_sort(Filter(function(e) e$account_id==a$account_id && e$occurred_at>=a$opened_at &&
          .cr_data_date(e$occurred_at,"occurred_at")<=min(end,cutoff) && .cr_data_date(e$known_at,"known_at")<=cutoff,ev),c("occurred_at","event_id"))
        firsts <- Filter(function(e) e$event_type %in% c(default_event,exits),hist)
        first <- if(length(firsts)) firsts[[1]] else NULL
        if(!is.null(first)&&any(vapply(firsts,function(e) e$occurred_at==first$occurred_at && e$event_type!=first$event_type,logical(1))))
          .cr_error("BEHAVIORAL_AMBIGUITY","Same-day default/exit requires ordering")
        if(!is.null(first)) {
          counts$complete_accounts <- counts$complete_accounts+1L
          if(first$event_type==default_event) {counts$defaults <- counts$defaults+1L;counts$incidence <- counts$incidence+as.integer(.cr_data_date(first$occurred_at,"occurred_at")>prev)}
          else counts$exits <- counts$exits+1L
        } else if(!is.null(complete[[a$account_id]])&&complete[[a$account_id]]>=end) counts$complete_accounts <- counts$complete_accounts+1L
        key <- .cr_data_key(list(account_id=a$account_id,as_of=format(end,"%Y-%m-%d")),c("account_id","as_of"));stock <- lookup[[key]]
        if(!is.null(stock)) {
          known <- if(is.null(stock$known_at)) stock$as_of else stock$known_at
          if(.cr_data_date(known,"known_at")<=cutoff) {
            counts$stock_observed <- counts$stock_observed+1L;counts$delinquent_stock <- counts$delinquent_stock+as.integer(stock$state %in% delinquent)
          }
        }
        for(e in hist) if(e$event_type %in% c("LOSS","RECOVERY")) {
          if(!identical(e$currency,currency)) .cr_error("BEHAVIORAL_CURRENCY","Loss currency mismatch")
          losses <- losses+.cr_beh_amount(e$amount,"loss cashflow")*(if(e$event_type=="LOSS") 1 else -1)
        }
      }
      n <- length(members);mature <- end<=cutoff&&counts$complete_accounts==n
      denom <- sum(vapply(members,function(a) a$original_balance,numeric(1)))
      output[[length(output)+1L]] <- c(list(cohort=cohort,currency=currency,months_on_book=age,period_end=format(end,"%Y-%m-%d"),accounts=n,original_balance=denom),counts,
        list(mature=mature,cumulative_default_rate=if(mature) counts$defaults/n else NULL,
          incidence_rate=if(mature) counts$incidence/n else NULL,
          delinquency_stock_rate=if(end<=cutoff&&counts$stock_observed==n) counts$delinquent_stock/n else NULL,
          observed_net_loss=losses,net_loss_rate=if(mature&&denom>0) losses/denom else NULL))
    }
  }
  .cr_beh_result(output,"behavioral.build_vintage_performance","vintage_table",context,
    limitations=c("Cohort denominators are fixed origination counts; first competing exit ends loan risk, not hypothetical continuing-customer risk.",
      "Loss rates use supplied observed cashflows and do not infer ultimate workout loss.",
      "Age + cohort = calendar period; this descriptive table does not identify independent causal effects."))
}
