library(creditratingsystem)
source(system.file('examples/specialized_open_workout.R',package='creditratingsystem'))
r<-run_specialized_open_workout()
stopifnot(length(r$journeys)==5,length(unique(vapply(r$journeys,function(row)row$expected_final_lgd,numeric(1))))==5,all(vapply(r$journeys,function(row)row$ratio_status=='DEFINED',logical(1))))
