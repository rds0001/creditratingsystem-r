library(creditratingsystem)
source(system.file('examples/specialized_observed_lifecycle.R',package='creditratingsystem'))
r<-run_specialized_observed_lifecycle()
stopifnot(length(r$journeys)==5)
for(row in r$journeys) {
  zero<-Filter(function(w)!is.null(w$drawn_balance_at_origin)&&w$drawn_balance_at_origin==0,row$windows)
  stopifnot(length(zero)==1,zero[[1]]$eligible,zero[[1]]$target==0,!is.null(row$closed_workout$economic_lgd),is.null(row$incomplete_workout$economic_lgd))
}
