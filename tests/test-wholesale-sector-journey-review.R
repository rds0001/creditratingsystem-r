library(creditratingsystem)
source(system.file('examples/wholesale_sector_journey_review.R',package='creditratingsystem'))
d<-jsonlite::fromJSON(system.file('extdata/wholesale_sector_journey_review.json',package='creditratingsystem'),simplifyVector=FALSE)
if(Sys.getenv('CREDITRATINGSYSTEM_EXTENDED_TESTS')!='true')d$governance$cases<-Filter(function(c)c$product%in%c('SOVEREIGN_DEBT','PUBLIC_SERVICE_FINANCE'),d$governance$cases)
v<-run_wholesale_sector_journey_review(d)
stopifnot(v$lineage$decision$status=='PASS')
for(c in v$cases){stopifnot(c$replay$decision$status=='PASS',c$negative_replay$decision$status=='BLOCKED',c$rating_benchmark$applicability=='NOT_APPLICABLE');if(c$product=='SOVEREIGN_DEBT')stopifnot(c$original_target$event=='pre_transfer_sovereign_payment_default');if(!is.null(c$prediction_review)){q<-c$prediction_review;p<-q$raw_pd;stopifnot(abs(q$benchmark$metrics$by_model$raw$brier-(.1*(1-p)^2+.9*p*p))<1e-12,q$override$metrics$overridden_cases==0)}}
