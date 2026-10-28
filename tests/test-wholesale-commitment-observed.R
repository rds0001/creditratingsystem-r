library(creditratingsystem)
source(system.file('examples/wholesale_commitment_observed.R',package='creditratingsystem'))
data<-jsonlite::fromJSON(system.file('extdata/wholesale_commitment_observed.json',package='creditratingsystem'),simplifyVector=FALSE)
v<-run_wholesale_commitment_observed(data)
for(i in seq_along(v$cohorts)){j<-v$cohorts[[i]]$journeys[[1]];o<-j$default_backtest$overall;stopifnot(o$observed_count==20,o$observed_defaults==c(2,6)[i],abs(j$closed_workout$economic_lgd-j$independent_oracle$lgd)<1e-12,is.null(j$incomplete_workout$economic_lgd),sum(vapply(j$windows,function(x)x$censored,logical(1)))==2)}
bad<-data;bad$cohorts[[1]]$journeys[[1]]$monitoring$predictions[[1]]$pd<-.9
stopifnot(inherits(try(run_wholesale_commitment_observed(bad,verify_reference=FALSE),silent=TRUE),'try-error'))
bad<-data;bad$cohorts[[2]]$cohort_contract$horizon_days<-730
stopifnot(inherits(try(run_wholesale_commitment_observed(bad,verify_reference=FALSE),silent=TRUE),'try-error'))
