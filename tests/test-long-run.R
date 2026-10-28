library(creditratingsystem)
target <- list(event="default",default_definition="90dpd_or_utp",horizon=1,time_unit="years",probability_measure="PHYSICAL",conditioning="observed_cohorts")
representative <- list(assessment="unknown",rationale="Synthetic test",reference_period="2020-2022")
cohorts <- list(list(cohort_id="a",period_start="2020-01-01",period_end="2021-01-01",known_at="2021-02-01",defaults=1,denominator=10,mature=TRUE,default_definition=target$default_definition),list(cohort_id="b",period_start="2021-01-01",period_end="2022-01-01",known_at="2022-02-01",defaults=2,denominator=100,mature=TRUE,default_definition=target$default_definition))
estimate <- function(rows=cohorts,weighting="annual_mean",uncertainty="none",dependence="unspecified") estimate_long_run_pd(rows,c("a","b"),"2023-01-01",weighting,uncertainty,dependence,representative,target)
stopifnot(abs(estimate()$value$pd-.06)<1e-14,abs(estimate(weighting="pooled_obligor")$value$pd-3/110)<1e-14,
 !estimate()$diagnostics$automatically_ttc,is.null(estimate()$value$lower))
zero <- cohorts; zero[[1]]$defaults <- zero[[2]]$defaults <- 0
exact <- estimate(zero,"pooled_obligor","clopper_pearson","independent_homogeneous_obligors")
stopifnot(exact$value$lower==0,abs(exact$value$upper-(1-.025^(1/110)))<1e-14)
interval <- estimate(uncertainty="year_t",dependence="independent_years")
stopifnot(interval$value$lower==0,abs(interval$value$upper-(.06+.04*tan(pi*.475)))<1e-10)
# Independent native reference implementation for a nonzero exact interval.
exact <- estimate(weighting="pooled_obligor",uncertainty="clopper_pearson",dependence="independent_homogeneous_obligors")
stopifnot(max(abs(c(exact$value$lower,exact$value$upper)-binom.test(3,110)$conf.int))<1e-14)
expect_code <- function(expr,code) {
 actual <- tryCatch({force(expr);"NO_ERROR"},creditratingsystem_error=function(e)e$code)
 stopifnot(identical(actual,code))
}
bad <- cohorts; bad[[1]]$mature <- FALSE
expect_code(estimate(bad),"IMMATURE_COHORT")
bad <- cohorts; bad[[1]]$defaults <- .5
expect_code(estimate(bad),"INVALID_COUNTS")
expect_code(estimate(uncertainty="year_t"),"INCOMPATIBLE_UNCERTAINTY")
