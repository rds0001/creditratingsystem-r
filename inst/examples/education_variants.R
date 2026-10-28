# Explicit educational loan payment and guarantor conventions.
if(!exists("simulate_repayment_arrangement"))library(creditratingsystem)
run_education_variants<-function(data=NULL,verify_reference=TRUE){
 if(is.null(data))data<-jsonlite::fromJSON(system.file("extdata/education_variants.json",package="creditratingsystem"),simplifyVector=FALSE)
 rule<-data$income_rule
 if(!is.finite(rule$excess_rate)||rule$excess_rate<0||rule$excess_rate>1||!is.finite(rule$monthly_threshold)||rule$monthly_threshold<0||any(!is.finite(unlist(rule$income))))stop("Invalid supplied excess-income rule")
 contingent<-data$contingent;contingent$candidate$payments<-pmax(unlist(rule$income)-rule$monthly_threshold,0)*rule$excess_rate
 project<-function(a){
  a$discount_factors<-unlist(a$discount_factors)
  for(n in c("baseline","candidate"))for(k in c("payments","period_interest_rates","new_fees","servicing_costs","receipt_fractions"))a[[n]][[k]]<-unlist(a[[n]][[k]])
  a$affordability_paths<-lapply(a$affordability_paths,function(x){x$available_cash<-unlist(x$available_cash);x})
  do.call(simulate_repayment_arrangement,a)$value
 }
 plans<-lapply(list(grace_period=data$grace,deferred_payment=data$deferred,income_contingent=contingent),project)
 due<-do.call(compute_days_past_due,data$no_due)$value
 allocation<-do.call(allocate_guarantee_coverage,data$guarantee_allocation)
 protected<-do.call(apply_guarantee_risk,c(data$guarantee_loss,list(coverage_allocation=allocation)))$value
 ineffective<-data$guarantee_allocation;ineffective$guarantees[[1]]$legally_effective<-FALSE
 unprotected<-do.call(apply_guarantee_risk,c(data$guarantee_loss,list(coverage_allocation=do.call(allocate_guarantee_coverage,ineffective))))$value
 joint<-do.call(assemble_joint_application,data$joint_application)$value
 if(verify_reference){
  stopifnot(due$dpd==0,due$unpaid_due_amount==0,plans$grace_period$candidate$remaining_claim==12000,abs(plans$deferred_payment$candidate$remaining_claim-12241.2)<1e-8,all(vapply(plans$income_contingent$candidate$schedule,function(x)x$scheduled_payment,numeric(1))==c(0,100)),plans$income_contingent$candidate$remaining_claim==11900,protected$covered_amount==3000,protected$pd==.1,abs(protected$expected_loss_after-960)<1e-9,unprotected$expected_loss_after==1200,joint$monthly_income==1500)
 }
 list(data_origin=data$data_origin,product=data$product,income_rule=rule,plans=plans,grace_delinquency=due,guarantee=protected,ineffective_guarantee=unprotected,joint_application=joint)
}
