# Dated retail cash ledgers, conditional proposals and observed competing exits.
if(!exists('compute_days_past_due'))library(creditratingsystem)
run_retail_transitions<-function(data=NULL,verify_reference=TRUE){
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/retail_transitions.json',package='creditratingsystem'),simplifyVector=FALSE)
 output<-list()
 for(product in names(data$cases)){
  z<-data$cases[[product]];aid<-z$account_id;origin<-z$arrangement$contract$as_of
  arrears<-do.call(compute_days_past_due,z$delinquency)$value
  cured<-do.call(compute_days_past_due,z$cured)$value
  state<-assign_delinquency_state(arrears$dpd,arrears$past_due_amount,z$opening_claim,list(closed=FALSE,confirmed_default=FALSE),data$state_profile)$value
  clean_state<-assign_delinquency_state(cured$dpd,cured$past_due_amount,z$opening_claim,list(closed=FALSE,confirmed_default=FALSE),data$state_profile)$value
  default_control<-assign_delinquency_state(0,0,z$opening_claim,list(closed=FALSE,confirmed_default=TRUE),data$state_profile)$value
  late_known<-z$cured;late_known$payments[[length(late_known$payments)]]$known_at<-z$collections$evaluation_cutoff
  unconfirmed<-do.call(compute_days_past_due,late_known)$value
  a<-z$arrangement;a$discount_factors<-unlist(a$discount_factors)
  for(n in c('baseline','candidate'))for(k in c('payments','period_interest_rates','new_fees','servicing_costs','receipt_fractions'))a[[n]][[k]]<-unlist(a[[n]][[k]])
  a$affordability_paths<-lapply(a$affordability_paths,function(x){x$available_cash<-unlist(x$available_cash);x})
  arrangement<-do.call(simulate_repayment_arrangement,a)$value
  plan<-arrangement$candidate;dates<-z$arrangement$period_end_dates;contract<-z$arrangement$contract;events<-list()
  for(i in seq_along(plan$schedule)){
   row<-plan$schedule[[i]]
   for(j in 1:2)events[[length(events)+1L]]<-list(event_id=paste0('proposal-',i-1L,'-',j-1L),event_date=row$period_end,known_at=origin,sequence=j-1L,kind=c('INTEREST_PAYMENT','PRINCIPAL_REPAYMENT')[[j]],amount=c(row$paid_interest,row$paid_principal)[[j]])
  }
  ea<-list(contract=list(facility_id=aid,currency='EUR',as_of=origin,drawn_principal=contract$principal,credit_limit=contract$principal,accrued_interest=contract$accrued_interest),events=events,annual_interest_rates=c(0,0,0),period_end_dates=dates,day_count='ACT/365F',allow_overlimit=FALSE,currency='EUR')
  before<-do.call(exposure_schedule,c(ea,list(default_timing='BEFORE_PERIOD_END_EVENTS')))$value
  after<-do.call(exposure_schedule,c(ea,list(default_timing='AFTER_PERIOD_END_EVENTS')))$value
  affordable<-all(unlist(plan$affordability[[1]]$residual_cash)>=0)
  flows<-lapply(seq_along(plan$schedule),function(i){row<-plan$schedule[[i]];list(cashflow_id=paste0('period-',i-1L),date=row$period_end,expected_recovery=row$received_payment,expected_cost=0,discount_factor=z$arrangement$discount_factors[[i]])})
  treatment<-list(treatment_id='proposed',version='1',known_at=origin,subject_id=aid,currency='EUR',action='ARRANGEMENT',prediction_reference='Explicit conditional proposal receipts, not causal response estimates',support_status='SUPPORTED',affordability_status=if(affordable)'AFFORDABLE'else'UNAFFORDABLE',eligibility_status='ELIGIBLE',uptake_probability=.5,offer_cost=0,accepted_upfront_cost=0,rejected_present_value=arrangement$baseline$npv,cashflows=flows)
  profile<-list(profile_id=paste0(product,'-proposal'),version='1',known_at=origin,currency='EUR',subject_id=aid,baseline_id='contractual',baseline_present_value=arrangement$baseline$npv,identification='CONDITIONAL_SCENARIO_NOT_CAUSAL',allowed_actions=list('ARRANGEMENT'))
  evaluation<-evaluate_treatment_candidates(list(treatment),profile,as_of=origin)$value
  episodes<-do.call(build_collections_episodes,z$collections)
  labels<-build_cure_targets(episodes,horizon_days=30,redefault_horizon_days=90,evaluation_cutoff=z$collections$evaluation_cutoff)$value
  windows<-do.call(build_observation_windows,z$windows)$value
  clean_date<-z$cured$as_of
  panel<-list(list(account_id=aid,as_of=origin,state='ARREARS',balance=z$opening_claim,product=product),list(account_id=aid,as_of=clean_date,state='CLEAN',balance=max(0,z$opening_claim-arrears$past_due_amount),product=product),list(account_id=paste0(aid,'-missing'),as_of=origin,state='ARREARS',balance=z$opening_claim,product=product))
  roll<-estimate_retail_roll_rates(panel,states=list('ARREARS','CLEAN','CLOSED'),from_date=origin,to_date=clean_date,group_by=list('product'),weighting='count',missing_successor='censor')$value
  dep<-list(list(assessment_id=paste0(aid,'-assessment'),dependency_type='account',dependency_id=aid))
  event<-list(event_id=paste0(aid,'-delinquency'),revision='1',occurred_at=origin,known_at=origin,dependencies=list(list(type='account',id=aid)),payload=list(dpd=arrears$dpd,unpaid=arrears$past_due_amount))
  impact<-apply_assessment_event(NULL,event,dep,origin)$value
  repeated<-apply_assessment_event(impact$state,event,dep,origin)$value
  history<-list(list(entity_id=aid,start=0,stop=14,from_state='arrears',to_state='clean'),list(entity_id=aid,start=14,stop=60,from_state='clean',to_state='exit'),list(entity_id=paste0(aid,'-control'),start=0,stop=100,from_state='arrears',to_state=NULL))
  target<-list(event='exit',horizon=60,time_unit='days',probability_measure='PHYSICAL',conditioning='in_arrears_at_landmark')
  meta<-list(target=target,training=list(data_fingerprint=paste0(product,'-observed-state-history'),split_id='descriptive_history',known_at=z$collections$evaluation_cutoff,data_origin='SYNTHETIC'),model_id=paste0(product,'-transitions'),model_version='1')
  multistate<-do.call(estimate_multistate_transitions,c(list(event_history=history,states=list('arrears','clean','exit'),allowed_transitions=list(list('arrears','clean'),list('clean','exit')),initial_state='arrears'),meta))
  curves<-predict_event_curves(multistate,list(),c(0,14,60),feature_order=list(),target=target,event_states=list('exit'))$value
  if(verify_reference){
   e<-z$expected
   stopifnot(arrears$dpd==e$dpd,arrears$past_due_amount==e$past_due,cured$dpd==0,cured$past_due_amount==0,unconfirmed$past_due_amount==e$past_due,unconfirmed$dpd==44)
   stopifnot(state$state=='ARREARS',state$at_risk,clean_state$state=='CLEAN',default_control$state=='DEFAULT',!default_control$at_risk)
   stopifnot(!arrangement$contract_changed,!arrangement$default_status_changed,abs(plan$npv-e$proposed_npv)<1e-9,abs(plan$remaining_claim-e$proposed_remaining_claim)<1e-9)
   stopifnot(abs(sum(vapply(plan$schedule,function(x)x$received_payment,numeric(1)))+plan$remaining_claim-z$opening_claim)<1e-9,affordable==e$candidate_affordable,evaluation$candidates[[1]]$feasible==affordable,abs(evaluation$candidates[[1]]$expected_present_value-(e$proposed_npv+e$baseline_npv)/2)<1e-9)
   for(i in seq_along(plan$schedule))stopifnot(abs(before$schedule[[i]]$conditional_ead-after$schedule[[i]]$conditional_ead-plan$schedule[[i]]$received_payment)<1e-9)
   stopifnot(labels[[1]]$cure_at==e$cure_date,labels[[1]]$cure_label==1,is.null(labels[[1]]$redefault_label),labels[[1]]$redefault_status=='CENSORED_EXIT',is.null(windows[[1]]$target),windows[[2]]$target==1,windows[[3]]$target==0)
   stopifnot(identical(as.numeric(unlist(roll$groups[[1]]$origin_count)),c(2,0,0)),identical(as.numeric(unlist(roll$groups[[1]]$censored_count)),c(1,0,0)),identical(as.numeric(unlist(roll$groups[[1]]$transition_matrix[[1]])),c(0,1,0)))
   stopifnot(repeated$status=='DUPLICATE',identical(as.numeric(unlist(curves$curves[[1]]$first_hit_probability)),c(0,0,.5)))
  }
  output[[product]]<-list(financial_convention=z$financial_convention,delinquency=arrears,delinquency_state=state,clean_state=clean_state,confirmed_default_control=default_control,confirmed_cure=cured,unconfirmed_receipt=unconfirmed,arrangement=arrangement,conditional_ead_before=before,conditional_ead_after=after,treatment=evaluation,cure_targets=labels,observation_windows=windows,roll_rates=roll,assessment_event=impact,duplicate_event_status=repeated$status,multistate=curves)
 }
 list(data_origin='SYNTHETIC',products=output,interpretation='Actual observed cure and exit are separate from proposed cashflow scenarios; censored exits are not hypothetical nondefaults.')
}
