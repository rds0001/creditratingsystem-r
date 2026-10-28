# Explicit draw and repayment-period limit calendars and legal lien ranks.
if(!exists("exposure_schedule"))library(creditratingsystem)
run_home_equity_variants<-function(data=NULL,verify_reference=TRUE){
 if(is.null(data))data<-jsonlite::fromJSON(system.file("extdata/home_equity_variants.json",package="creditratingsystem"),simplifyVector=FALSE)
 a<-data$exposure;a$annual_interest_rates<-unlist(a$annual_interest_rates)
 exposure<-do.call(exposure_schedule,a)$value
 a$events<-c(a$events,list(data$forbidden_draw))
 restriction<-tryCatch({do.call(exposure_schedule,a);"DRAW_SUCCEEDED"},creditratingsystem_error=function(e)e$code)
 collateral<-lapply(data$collateral,function(a)do.call(combined_loan_to_value,a)$value)
 recovery<-lapply(data$recovery,function(a)do.call(simulate_recovery_waterfall,a)$value)
 if(verify_reference){
  stopifnot(all(vapply(exposure$schedule,function(x)x$conditional_ead,numeric(1))==c(62400,50000)),all(vapply(exposure$schedule,function(x)x$undrawn_commitment,numeric(1))==0),restriction=="EXPOSURE_LIMIT",all(vapply(collateral,function(x)x$cltv,numeric(1))==.9))
  for(name in names(recovery))stopifnot(Filter(function(x)x$claim_id=="home_equity",recovery[[name]]$claim_results)[[1]]$lgd==if(name=="first_lien")0 else .2)
 }
 list(data_origin=data$data_origin,product=data$product,exposure=exposure,repayment_draw_restriction=restriction,collateral=collateral,recovery=recovery)
}
