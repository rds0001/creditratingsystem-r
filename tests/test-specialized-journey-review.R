library(creditratingsystem)
source(system.file('examples/specialized_journey_review.R',package='creditratingsystem'))
d<-jsonlite::fromJSON(system.file('extdata/specialized_journey_review.json',package='creditratingsystem'),simplifyVector=FALSE)
v<-.specialized_kernel_convergence(d$cases[[1]],d$convergence);m<-v$result$metrics
stopifnot(length(m$sampling)==4,length(m$paired_grid_differences)==2,all(vapply(m$paired_grid_differences,function(x)x$estimate==0,logical(1))),!v$result$discretization_bias_identified)
if(Sys.getenv('CREDITRATINGSYSTEM_EXTENDED_TESTS')=='true'){report<-run_specialized_journey_review();stopifnot(length(report$cases)==5,all(vapply(report$cases,function(c)c$replay$decision$status=='PASS'&&c$negative_replay$decision$status=='BLOCKED',logical(1))))}
