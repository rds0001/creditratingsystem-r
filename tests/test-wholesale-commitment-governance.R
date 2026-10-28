library(creditratingsystem)
source(system.file('examples/wholesale_commitment_governance.R',package='creditratingsystem'))
data<-jsonlite::fromJSON(system.file('extdata/wholesale_commitment_governance.json',package='creditratingsystem'),simplifyVector=FALSE)
if(tolower(Sys.getenv('CREDITRATINGSYSTEM_EXTENDED_TESTS'))!='true')data$cases<-data$cases[c(1,7)]
v<-run_wholesale_commitment_governance(data,return_audit_artifacts=TRUE)
for(i in seq_along(data$cases)){
 case<-data$cases[[i]];row<-v$report$cases[[i]];events<-v$audits[[i]]$requests[[2]]$input$events
 default<-Filter(function(x)x$event_id=='credit-default',events)[[1]]
 stopifnot(row$origination_decision==case$reference_action,row$replay=='REPRODUCED',row$replay_after_new_champion=='REPRODUCED',row$origination_pd==row$historical_pd,row$tampered_replay=='NOT_REPRODUCIBLE',default$occurred_at==case$observed$default_at)
 if(case$product=='LEVERAGED_LOAN')stopifnot(default$occurred_at=='2026-01-01')else stopifnot(length(case$calibrator_arguments$groups)==1,default$occurred_at=='2024-01-31')
}
bad<-data;bad$cases<-bad$cases[1];bad$cases[[1]]$timeline$default_known<-'2023-01-01'
stopifnot(inherits(try(run_wholesale_commitment_governance(bad,verify_reference=FALSE),silent=TRUE),'try-error'))
