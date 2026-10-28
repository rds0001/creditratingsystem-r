library(creditratingsystem)
source(system.file('examples/wholesale_sme_journey_review.R',package='creditratingsystem'))
d<-jsonlite::fromJSON(system.file('extdata/wholesale_sme_journey_review.json',package='creditratingsystem'),simplifyVector=FALSE)
if(Sys.getenv('CREDITRATINGSYSTEM_EXTENDED_TESTS')!='true')d$cases<-d$cases[1]
v<-run_wholesale_sme_journey_review(d)
for(c in v$cases)stopifnot(c$replay$decision$status=='PASS',c$negative_replay$decision$status=='BLOCKED',c$prediction_review$override$metrics$overridden_cases==0,abs(c$prediction_review$benchmark$metrics$by_model$raw$brier-c$observed_brier_oracle)<1e-12)
