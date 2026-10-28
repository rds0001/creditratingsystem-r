library(creditratingsystem)
source(system.file('examples/wholesale_sector_governance.R',package='creditratingsystem'))
data<-jsonlite::fromJSON(system.file('extdata/wholesale_sector_governance.json',package='creditratingsystem'),simplifyVector=FALSE)
if(tolower(Sys.getenv('CREDITRATINGSYSTEM_EXTENDED_TESTS'))!='true')data$cases<-data$cases[2]
r<-run_wholesale_sector_governance(data)$cases
for(row in r)stopifnot(row$origination_pd==row$historical_pd,row$replay=='REPRODUCED',row$replay_after_new_champion=='REPRODUCED',row$tampered_replay=='NOT_REPRODUCIBLE',identical(unlist(row$event_statuses),c('APPLIED','DUPLICATE','APPLIED','APPLIED','APPLIED')),row$final_state=='SUPERSEDED',row$origination_decision==if(row$credit_gate_inputs$available_unrestricted_cashflow>=row$credit_gate_inputs$debt_service_due)'APPROVE'else'REFER')
