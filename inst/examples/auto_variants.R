# Same dated loan, physical asset and capped guarantee inputs, native execution.
if(!exists("build_debt_schedule"))library(creditratingsystem)
run_auto_variants<-function(data=NULL,verify_reference=TRUE){
 if(is.null(data))data<-jsonlite::fromJSON(system.file("extdata/auto_variants.json",package="creditratingsystem"),simplifyVector=FALSE)
 schedules<-lapply(data$debt,function(a)do.call(build_debt_schedule,a)$value)
 asset<-do.call(simulate_asset_values,data$asset)$value;gross<-asset$paths[[1]]$asset_value
 eads<-lapply(schedules,function(v){row<-tail(v$schedule,1)[[1]];row$opening_principal+row$interest_due})
 recoveries<-list()
 for(name in names(eads)){
  ead<-eads[[name]];a<-data$recovery
  a$claims<-list(list(scenario_id="sale",default_event_id="default",subject_id="borrower",claim_id="auto",pool_id="car",currency="EUR",ead_at_default=ead,legal_balance=ead,priority=1,deficit_cost_share=1))
  a$realization_paths[[1]]$gross_value<-gross
  recoveries[[name]]<-do.call(simulate_recovery_waterfall,a)$value
  if(name=="balloon"){
   a$realization_paths[[1]]$cost<-0;recoveries$balloon_no_cost<-do.call(simulate_recovery_waterfall,a)$value
  }
 }
 debt<-eads$balloon;loss<-recoveries$balloon$claim_results[[1]]$economic_loss
 a<-data$guarantee_allocation
 a$claims<-list(list(claim_id="auto",entity_id="borrower",amount=debt,currency="EUR",due_at="2023-01-01",known_at="2021-01-01"))
 allocated<-do.call(allocate_guarantee_coverage,a)
 net<-gross-data$recovery$realization_paths[[1]]$cost;gap<-max(data$net_realization_floor-net,0)
 covered<-sum(vapply(allocated$value$allocations,function(x)x$covered_amount,numeric(1)))
 risk<-data$guarantee_loss
 for(name in names(risk$loss_contract$states))risk$loss_contract$states[[name]]$loss_before_guarantee<-loss
 denominator<-min(covered,max(loss,0))
 risk$loss_contract$states$borrower_only$payout_fraction<-if(denominator>0)min(gap,denominator)/denominator else 0
 protection<-do.call(apply_guarantee_risk,c(risk,list(coverage_allocation=allocated)))$value
 a$guarantees[[1]]$legally_effective<-FALSE
 ineffective<-do.call(apply_guarantee_risk,c(risk,list(coverage_allocation=do.call(allocate_guarantee_coverage,a))))$value
 states<-protection$states;bpd<-risk$borrower_risk$pd;gpd<-risk$guarantor_risk$pd;joint<-risk$joint_default_probability
 paths<-expected_loss_from_paths(default_indicator=matrix(c(1,1,0,0),ncol=1),lgd_paths=matrix(c(states[[1]]$loss_after_guarantee/debt,states[[2]]$loss_after_guarantee/debt,0,0),ncol=1),ead_paths=matrix(c(debt,debt,0,0),ncol=1),scenario_weights=c(bpd-joint,joint,gpd-joint,1-bpd-gpd+joint),discount_factors=1,scenario_ids=c("borrower_only","joint","dealer_only","neither"),time_grid=2,currency="EUR",probability_measure="PHYSICAL")$value
 if(verify_reference)stopifnot(eads$standard_amortizing==10500,eads$balloon==15750,gross==10000,loss==6750,recoveries$balloon_no_cost$claim_results[[1]]$economic_loss==5750,abs(protection$expected_loss_after-435)<1e-9,abs(ineffective$expected_loss_after-675)<1e-9,abs(paths$expected_loss-435)<1e-9)
 list(data_origin=data$data_origin,product=data$product,schedules=schedules,asset=asset,ead_before_final_payment=eads,recoveries=recoveries,guarantee=protection,ineffective_guarantee=ineffective,joint_loss_paths=paths)
}
