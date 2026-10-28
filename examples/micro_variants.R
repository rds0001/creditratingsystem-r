# Weekly contractual dates, seasonal deficits and a shared aggregate guarantee.
if(!exists('build_debt_schedule'))library(creditratingsystem)
run_micro_variants <- function(data=NULL,verify_reference=TRUE) {
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/micro_variants.json',package='creditratingsystem'),simplifyVector=FALSE)
 debt<-do.call(build_debt_schedule,data$debt)$value
 obligations<-lapply(seq_along(debt$schedule),function(i){row<-debt$schedule[[i]];list(obligation_id=paste0('week',i),account_id='weekly',due_at=row$payment_date,known_at=data$debt$calendar[[1]]$start_date,amount=row$net_payment_due,currency='EUR')})
 due<-compute_days_past_due(obligations,data$payments,as_of=data$observed_at,known_at=data$observed_at,currency='EUR')$value
 a<-data$residual;for(k in c('net_income','recurring_expenses','existing_debt_service','new_debt_service'))a[[k]]<-unlist(a[[k]])
 residual<-do.call(residual_income,a)$value
 graph<-do.call(build_entity_graph,data$graph)
 groups<-assign_default_groups(graph,data$default_policy,data$facility_mappings)$value
 assignments<-setNames(lapply(groups$assignments,`[[`,'group_id'),vapply(groups$assignments,`[[`,character(1),'entity_id'))
 event_groups<-unlist(assignments[unlist(data$observed_default_entities)])
 membership_default<-lapply(assignments,function(x)x%in%event_groups)
 allocation<-do.call(allocate_guarantee_coverage,data$allocation)
 protected<-lapply(data$guarantee_risks,function(a){a$coverage_allocation<-allocation;do.call(apply_guarantee_risk,a)$value})
 if(verify_reference) {
  stopifnot(all(vapply(debt$schedule,function(x)x$net_payment_due,numeric(1))==rep(100,4)),due$dpd==5,due$unpaid_due_amount==100)
  stopifnot(all(unlist(residual$residual)==rep(c(-200,-200,700),4)),sum(unlist(residual$residual))/12==100)
  stopifnot(membership_default$memberA,!membership_default$memberB,!membership_default$group)
  stopifnot(all(vapply(allocation$value$allocations,function(x)x$covered_amount,numeric(1))==c(80,20)),abs(protected$memberA$expected_loss_after-1.6)<1e-12,abs(protected$memberB$expected_loss_after-6.4)<1e-12,all(vapply(protected,function(x)x$pd,numeric(1))==.1))
 }
 list(data_origin=data$data_origin,product=data$product,weekly_schedule=debt,weekly_delinquency=due,seasonal_residual=residual,default_groups=groups,membership_default=membership_default,guarantee_allocation=allocation$value,protected_claims=protected)
}
