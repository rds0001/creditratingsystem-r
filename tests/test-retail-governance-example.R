if(!exists('predict_application_risk'))library(creditratingsystem)
retail_governance_example<-system.file('examples/retail_governance.R',package='creditratingsystem');retail_governance_data<-system.file('extdata/retail_governance.json',package='creditratingsystem')
if(!nzchar(retail_governance_example))retail_governance_example<-'creditratingsystem-r/examples/retail_governance.R'
if(!nzchar(retail_governance_data))retail_governance_data<-'creditratingsystem-r/inst/extdata/retail_governance.json'
source(retail_governance_example);rg_data<-jsonlite::fromJSON(retail_governance_data,simplifyVector=FALSE)
expected<-list(MORTGAGE=6000,HELOC=c(1600,1600,-38400),CONSOLIDATION=250,EDUCATION=c(500,500,600),AUTO_BALLOON=c(7000,-2750),OPERATING_LEASE=200,CREDIT_CARD=350,CHARGE_CARD=-600,OVERDRAFT=-300,BNPL=c(-50,-50,0),MICRO=c(-200,-200,700,-200),OWNER_BUSINESS=15)
for(case in rg_data$cases){e<-retail_contractual_service(case);b<-case$budget;for(k in c('net_income','recurring_expenses','existing_debt_service'))b[[k]]<-unlist(b[[k]]);v<-do.call(residual_income,c(b,list(new_debt_service=unlist(e$service))))$value;stopifnot(max(abs(unlist(v$residual)-expected[[case$kind]]))<1e-10);if(case$kind=='OWNER_BUSINESS')stopifnot(e$aux$business_after_draw==40);if(case$kind=='OPERATING_LEASE')stopifnot(e$aux$lessor_residual_excluded==12000)}
rg_result<-run_retail_governance(rg_data,product_ids=c('RETAIL_CREDIT_CARD','RETAIL_CHARGE_CARD'))$variants
stopifnot(rg_result[[1]]$decision$action=='APPROVE',rg_result[[2]]$decision$action=='DECLINE',abs(rg_result[[1]]$application_risk$pd-rg_result[[2]]$application_risk$pd)<1e-12)
for(case in rg_result)stopifnot(case$behavioral_replay$status=='REPRODUCED',case$missing_evidence_decision$action!='APPROVE',case$monitoring[[1]]$status=='INSUFFICIENT_INFORMATION',case$exit_assessment$state=='SUPERSEDED',!case$restructuring_proposal_event$state$processed[[2]]$event$payload$contract_changed)
rg_bad<-rg_data;rg_bad$cases<-rg_bad$cases['RETAIL_CREDIT_CARD'];rg_bad$cases[[1]]$income_evidence$known_at<-'2025-01-02'
stopifnot(grepl('not known',tryCatch({run_retail_governance(rg_bad);'NO_ERROR'},error=function(e)conditionMessage(e))))
rg_bad_case<-rg_data$cases$RETAIL_SMALL_BUSINESS_FINANCE;rg_bad_case$budget$net_income<-list(300)
stopifnot(grepl('once',tryCatch({retail_contractual_service(rg_bad_case);'NO_ERROR'},error=function(e)conditionMessage(e))))
