library(creditratingsystem)
source(system.file('examples/wholesale_sector_monitoring.R',package='creditratingsystem'))
data<-jsonlite::fromJSON(system.file('extdata/wholesale_sector_monitoring.json',package='creditratingsystem'),simplifyVector=FALSE)
rows<-run_wholesale_sector_monitoring(data)$journeys
for(i in seq_along(rows)) {
 row<-rows[[i]];curve<-row$multistate$curves[[1]];p<-c(1,2,3,2,1,1,2)[i]/20
 stopifnot(max(abs(unlist(curve$first_hit_probability)-c(0,p,p,p)))<1e-12,max(abs(vapply(curve$state_occupancy,function(x)x[[2]],numeric(1))-c(0,p,0,p)))<1e-12,all(vapply(row$residual_sources,function(x)x$eligible_count,numeric(1))==20),all(vapply(row$residual_sources,function(x)x$evaluation_cutoff,character(1))>'2025-01-01'),max(abs(unlist(row$pd_drift$statistic_path)-unlist(row$independent_ewma)))<1e-12)
}
bad<-data;bad$journeys[[1]]$cohorts[[2]]$predictions[[1]]$pd<-.9
error<-tryCatch({run_wholesale_sector_monitoring(bad,verify_reference=FALSE);NULL},error=function(e)e)
stopifnot(inherits(error,'error'),grepl('pinned model',conditionMessage(error)))
