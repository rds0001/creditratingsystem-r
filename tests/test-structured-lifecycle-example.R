if(!exists('fit_cause_specific_hazards'))library(creditratingsystem)
lifecycle_example_file<-system.file('examples/structured_lifecycle.R',package='creditratingsystem');lifecycle_data_file<-system.file('extdata/structured_lifecycle.json',package='creditratingsystem')
if(!nzchar(lifecycle_example_file))lifecycle_example_file<-'creditratingsystem-r/examples/structured_lifecycle.R'
if(!nzchar(lifecycle_data_file))lifecycle_data_file<-'creditratingsystem-r/inst/extdata/structured_lifecycle.json'
source(lifecycle_example_file);lifecycle_data<-jsonlite::fromJSON(lifecycle_data_file,simplifyVector=FALSE);lifecycle_result<-run_structured_lifecycle(lifecycle_data)
for(case in lifecycle_result$variants){
 curve<-case$competing_curves$curves[[1]];stopifnot(abs(curve$cumulative_incidence$exit[[2]]-(1-exp(-.5))/2)<1e-12,abs(curve$survival[[2]]+curve$cumulative_incidence$exit[[2]]+curve$cumulative_incidence$default[[2]]-1)<1e-12,case$transition_curves$curves[[1]]$first_hit_probability[[5]]==.5,case$restructuring_event$state$processed[[2]]$event$payload$execution=='PROPOSAL_ONLY')
}
lifecycle_data$training$known_at<-'2024-01-17'
stopifnot(grepl('before the decision',tryCatch({run_structured_lifecycle(lifecycle_data);'NO_ERROR'},error=function(e)conditionMessage(e))))
