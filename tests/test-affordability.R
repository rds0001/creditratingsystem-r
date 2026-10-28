library(creditratingsystem)
error_aff <- function(expr,code) {
  e <- tryCatch(force(expr),error=identity)
  stopifnot(inherits(e,"creditratingsystem_error"),identical(e$code,code))
}
calendar_aff <- list(weekly=52,monthly=12,annual=1)
salary_aff <- list(economic_id="salary",amount=1000,frequency="weekly",basis="gross",
  currency="EUR",kind="recurring",tax_amount=200)
loss_aff <- list(economic_id="loss",amount=-100,frequency="monthly",basis="net",currency="EUR",kind="variable")
income_aff <- normalize_income_period(list(salary_aff,salary_aff,loss_aff),"monthly","net","EUR",calendar_aff,.5)$value
stopifnot(abs(income_aff$total_income-(800*52/12-100))<1e-10,
  identical(income_aff$duplicate_ids,list("salary")),income_aff$components[[2]]$normalized_amount== -100)
salary_missing_aff <- salary_aff;salary_missing_aff$tax_amount <- NULL
stopifnot(is.null(normalize_income_period(list(salary_missing_aff),"monthly","net","EUR",calendar_aff,1)$value$total_income))
salary_conflict_aff <- salary_aff;salary_conflict_aff$amount <- 900
error_aff(normalize_income_period(list(salary_aff,salary_conflict_aff),"monthly","net","EUR",calendar_aff,1),"CONFLICTING_EVIDENCE")
insurance_aff <- list(economic_id="insurance",category="insurance",amount=1200,frequency="annual",currency="EUR",kind="essential")
insurance2_aff <- insurance_aff;insurance2_aff$amount <- 110;insurance2_aff$frequency <- "monthly"
loan_aff <- list(economic_id="loan",category="loan",amount=500,frequency="monthly",currency="EUR",kind="debt_service")
own_aff <- list(economic_id="transfer",category="transfer",amount=900,frequency="monthly",currency="EUR",kind="internal_transfer")
profile_aff <- list(profile_id="two_adults",required_categories=list("insurance","food"))
benchmark_aff <- list(profile_id="two_adults",category="food",amount=300,frequency="monthly",currency="EUR",kind="essential")
expenses_aff <- estimate_recurring_expenses(list(insurance_aff,insurance2_aff,loan_aff,loan_aff,own_aff),"monthly","EUR",calendar_aff,profile_aff,list(benchmark_aff))$value
stopifnot(expenses_aff$recurring_expenses==410,expenses_aff$debt_service==500,identical(expenses_aff$excluded_transfers,list("transfer")))
stopifnot(is.null(estimate_recurring_expenses(list(insurance_aff),"monthly","EUR",calendar_aff,profile_aff)$value$recurring_expenses))
claims_aff <- list(list(claim_id="senior",amount=80,currency="EUR"),list(claim_id="junior",amount=20,currency="EUR"))
asset_aff <- list(list(collateral_id="house",value=125,currency="EUR",valuation_date="2024-01-01",freshness="CURRENT"))
allocation_aff <- list(list(claim_id="senior",collateral_id="house",amount=80,priority=1),list(claim_id="junior",collateral_id="house",amount=20,priority=2))
stopifnot(combined_loan_to_value(claims_aff,asset_aff,allocation_aff,"EUR")$value$cltv==.8)
asset_aff[[1]]$value <- 90
coverage_aff <- combined_loan_to_value(claims_aff,asset_aff,allocation_aff,"EUR")$value$priority_coverage
stopifnot(coverage_aff[[1]]$covered_amount==80,coverage_aff[[2]]$covered_amount==10)
asset_aff[[1]]$value <- 0
stopifnot(is.null(combined_loan_to_value(claims_aff,asset_aff,allocation_aff,"EUR")$value$cltv))
bad_allocation_aff <- allocation_aff;bad_allocation_aff[[1]]$amount <- 81
error_aff(combined_loan_to_value(claims_aff,asset_aff,bad_allocation_aff,"EUR"),"INVALID_ALLOCATION")
baseline_aff <- list(as_of="2024-01-31",currency="EUR",liquid_buffer=500,
  cltv_scope="all_contracts_secured_by_pool",
  incomes=list(list(economic_id="salary",monthly_amount=1500,basis="net",currency="EUR")),
  expenses=list(list(economic_id="living",monthly_amount=500,currency="EUR")),
  contracts=list(list(economic_id="loan",principal=1200,remaining_months=3,annual_rate=0,
    repayment="annuity",balloon_amount=600,rate_type="reset",reset_dates=list("2024-03-31"),
    rate_floor=0,rate_cap=.24,currency="EUR")),
  collateral=list(list(economic_id="car",value=1500,currency="EUR")),
  thresholds=list(minimum_residual=0,maximum_dsti=.4,maximum_cltv=.9))
dates_aff <- c("2024-02-29","2024-03-31","2024-04-30")
scenario_aff <- list(schema_version="1.0",scenario_id="joint-stress",periods=lapply(seq_along(dates_aff),function(i)
  list(date=dates_aff[i],income_factors=list(salary=c(1,.5,0)[i]),expense_factors=list(living=1.1),
    collateral_factors=list(car=.8),rate_shifts=list(loan=.12),fx_to_reporting=list(EUR=1))))
stress_aff <- stress_affordability(baseline_aff,scenario_aff)$value
payment_aff <- (1000*1.01^2-600)/2.01
stopifnot(stress_aff$periods[[1]]$debt_service==200,stress_aff$periods[[1]]$residual==750,
  abs(stress_aff$periods[[2]]$debt_service-payment_aff)<1e-10,
  abs(stress_aff$periods[[3]]$debt_service-payment_aff-600)<1e-10,
  stress_aff$periods[[3]]$closing_debt==0,is.null(stress_aff$periods[[3]]$dsti),
  stress_aff$first_liquidity_exhaustion=="2024-04-30")
fixed_aff <- baseline_aff
fixed_aff$contracts[[1]]$rate_type <- "fixed";fixed_aff$contracts[[1]]$reset_dates <- list()
fixed_aff$contracts[[1]]$repayment <- "interest_only";fixed_aff$contracts[[1]]$balloon_amount <- 1200
fixed_aff$contracts[[1]]$annual_rate <- .12;fixed_aff$contracts[[1]]$currency <- "USD"
fx_scenario_aff <- scenario_aff
fx_scenario_aff$periods <- lapply(fx_scenario_aff$periods,function(p) {p$fx_to_reporting$USD <- 2;p})
fixed_path_aff <- stress_affordability(fixed_aff,fx_scenario_aff)$value$periods
stopifnot(identical(vapply(fixed_path_aff,function(p) p$debt_service,numeric(1)),c(24,24,2424)))
bad_aff <- baseline_aff;bad_aff$expenses[[1]]$economic_id <- "loan"
error_aff(stress_affordability(bad_aff,scenario_aff),"DOUBLE_COUNTING")
bad_aff <- baseline_aff;bad_aff$contracts[[1]]$reset_dates <- list("2024-03-15")
error_aff(stress_affordability(bad_aff,scenario_aff),"INVALID_DATE")
stopifnot(debt_to_income(60000,2500,"monthly","EUR","annual")$value$ratio==2,
          debt_service_to_income(900,3000,"monthly","EUR")$value$ratio==0.3)
x <- debt_service_to_income(100,c(0,-10,200),"monthly","EUR")$value
stopifnot(identical(x$ratio,list(NULL,NULL,0.5)),identical(x$applicable,c(FALSE,FALSE,TRUE)))
x <- residual_income(c(3000,500),1000,900,200,"monthly","EUR")$value
stopifnot(identical(x$residual,c(900,-1600)),x$minimum == -1600)
p <- list(schema_version="1.0",policy_id="illustrative",version="1",
  required_evidence=list("income_verified"),rules=list(
    list(rule_id="dsti",metric="dsti",operator="<=",threshold=0.3),
    list(rule_id="buffer",metric="residual",operator=">",threshold=0)))
stopifnot(assess_affordability(list(dsti=0.3,residual=1),p,list(income_verified=TRUE))$value$assessment=="AFFORDABLE")
x <- assess_affordability(list(dsti=0.3,residual=0),p,list(income_verified=TRUE))$value
stopifnot(x$assessment=="NOT_AFFORDABLE",identical(x$breaches,list("buffer")),x$rule_trace[[2]]$headroom==0)
x <- assess_affordability(list(dsti=NULL,residual=-1),p,list(income_verified=FALSE))$value
stopifnot(x$assessment=="INSUFFICIENT_EVIDENCE",identical(x$missing_metrics,list("dsti")))
e <- tryCatch(residual_income(c(1,2),c(1,2,3),1,1,"monthly","EUR"),creditratingsystem_error=identity)
stopifnot(e$code=="INCOMPATIBLE_SHAPE")
e <- tryCatch(debt_service_to_income(-100,200,"monthly","EUR"),creditratingsystem_error=identity)
stopifnot(e$code=="INVALID_AMOUNT")
