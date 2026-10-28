library(creditratingsystem)
source(system.file('examples/wholesale_commitment_review.R',package='creditratingsystem'))
data<-jsonlite::fromJSON(system.file('extdata/wholesale_commitment_review.json',package='creditratingsystem'),simplifyVector=FALSE)
v<-run_wholesale_commitment_review(data)
for(i in seq_along(v$review$journeys)){row<-v$review$journeys[[i]];d<-c(2,6)[i];e<-c(4,2)[i];stopifnot(abs(row$independent_oracle$competing$default_incidence-(1-exp(-(d+e)/20))*d/(d+e))<1e-12,length(row$treatment$candidates)==2)}
for(row in v$monitoring$journeys){curve<-row$multistate$curves[[1]];stopifnot(all(abs(unlist(curve$first_hit_probability)[2:4]-curve$first_hit_probability[[2]])<1e-12),curve$state_occupancy[[3]][[2]]==0,all(abs(unlist(row$pd_drift$statistic_path)-unlist(row$independent_ewma))<1e-12))}
bad<-data;bad$monitoring$journeys[[1]]$cohorts[[1]]$predictions[[1]]$pd<-.99
stopifnot(inherits(try(run_wholesale_commitment_review(bad,verify_reference=FALSE),silent=TRUE),'try-error'))
