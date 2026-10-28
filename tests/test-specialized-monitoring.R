library(creditratingsystem)
source(system.file('examples/specialized_monitoring.R',package='creditratingsystem'))
r<-run_specialized_monitoring()
stopifnot(length(r$journeys)==5)
for(row in r$journeys) {
  stopifnot(all(vapply(row$residual_sources,function(x)x$eligible_count,numeric(1))==20))
  curve<-row$multistate$curves[[1]];stopifnot(curve$first_hit_probability[[3]]>0,curve$state_occupancy[[3]][[2]]==0)
}
