library(creditratingsystem)
source(system.file('examples/specialized_governance.R',package='creditratingsystem'))
d<-jsonlite::fromJSON(system.file('extdata/specialized_governance.json',package='creditratingsystem'),simplifyVector=FALSE)
if(!identical(tolower(Sys.getenv('CREDITRATINGSYSTEM_EXTENDED_TESTS')),'true'))d$journeys<-d$journeys[1]
r<-run_specialized_governance(d)
for(row in r$journeys)stopifnot(row$replay=='REPRODUCED',row$replay_after_later_fit=='REPRODUCED',row$tampered_replay=='NOT_REPRODUCIBLE',row$observed_servicing$default_days_past_due>90)
