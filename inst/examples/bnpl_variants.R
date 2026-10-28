# Allocated purchase plans, effective returns and actual short observation spans.
if(!exists('build_bnpl_obligation_features'))library(creditratingsystem)
run_bnpl_variants <- function(data=NULL,verify_reference=TRUE) {
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/bnpl_variants.json',package='creditratingsystem'),simplifyVector=FALSE)
 cases<-lapply(data$cases,function(args) {
  plans<-do.call(build_bnpl_obligation_features,args)$value
  obligations<-lapply(plans$installments,function(x)list(obligation_id=x$installment_id,account_id='customer-plan-pool',due_at=x$due_at,known_at=args$information_cutoff,amount=x$outstanding,currency=args$currency))
  dpd<-compute_days_past_due(obligations,list(),as_of=args$as_of,known_at=args$information_cutoff,currency=args$currency)$value
  list(plans=plans,net_obligation_dpd=dpd)
 })
 a<-data$observation;a$horizon_days<-20;short<-do.call(build_observation_windows,a)$value
 a$horizon_days<-365;annual<-do.call(build_observation_windows,a)$value
 if(verify_reference) {
  stopifnot(cases$pay_in_installments$plans$total_outstanding==75,cases$pay_in_installments$net_obligation_dpd$past_due_amount==25,cases$pay_in_installments$net_obligation_dpd$dpd==2,cases$pay_later$plans$total_outstanding==100,cases$pay_later$net_obligation_dpd$dpd==0)
  repeat_plan<-cases$repeat_purchases$plans
  stopifnot(repeat_plan$total_outstanding==140,repeat_plan$known_near_term_due==65,repeat_plan$active_plan_count==2,repeat_plan$repeat_purchase_count==1)
  stopifnot(cases$effective_return$plans$total_outstanding==0,cases$effective_return$net_obligation_dpd$dpd==0,cases$completed_short_plan$plans$active_plan_count==0,cases$completed_short_plan$plans$total_outstanding==0)
  stopifnot(short[[1]]$target==0,short[[1]]$observed_until=='2025-01-21',is.null(annual[[1]]$target),annual[[1]]$observed_until=='2025-01-22',annual[[1]]$label_status=='exit')
 }
 list(data_origin=data$data_origin,product=data$product,cases=cases,twenty_day_observation=short,annual_observation=annual)
}
