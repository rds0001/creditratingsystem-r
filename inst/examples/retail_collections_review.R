# One observed default claim and an unaccepted plan; prior models are observational.
if(!exists('build_collections_episodes',mode='function'))library(creditratingsystem)
run_retail_collections_review<-function(data=NULL,verify_reference=TRUE,return_artifacts=FALSE){
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/retail_collections_review.json',package='creditratingsystem'),simplifyVector=FALSE)
 d<-data;target<-d$target;train<-d$training;prior<-d$prior;origin<-d$prior_origin;day<-function(t)as.numeric(as.Date(t)-as.Date(origin))
 if(max(vapply(prior$coverage,function(r)r$observed_through,''))>train$known_at||train$known_at>d$valuation_date)stop('Model requires genuinely prior observation support')
 previous<-do.call(build_collections_episodes,prior);targets<-build_cure_targets(previous,horizon_days=90,redefault_horizon_days=90,evaluation_cutoff=prior$evaluation_cutoff)$value
 times<-numeric();events<-list();panel<-list();transitions<-list()
 for(coverage in prior$coverage){
  aid<-coverage$account_id;label<-Filter(function(t)t$account_id==aid,targets)[[1]];end<-day(coverage$observed_through);history<-Filter(function(r)r$account_id==aid,prior$states);exits<-Filter(function(r)r$state=='CLOSED',history)
  cured<-identical(label$cure_label,1)||identical(label$cure_label,1L);t<-if(cured)day(label$cure_at)else if(length(exits))day(exits[[1]]$occurred_at)else end;event<-if(cured)'cure'else if(length(exits))'exit'else NULL
  times<-c(times,t);events[length(events)+1]<-list(event);panel[[length(panel)+1]]<-list(entity_id=aid,start=0,stop=t,event=as.integer(!is.null(event)),covariate_time=0)
  state<-'D';start<-0;changes<-list()
  if(identical(event,'exit'))changes<-list(list(time=t,state='X'))
  if(identical(event,'cure')){changes<-list(list(time=t,state='C'));if(isTRUE(label$redefault_label==1))changes[[2]]<-list(time=day(label$redefault_at),state='R')}
  for(change in changes){transitions[[length(transitions)+1]]<-list(entity_id=aid,start=start,stop=change$time,from_state=state,to_state=change$state);start<-change$time;state<-change$state}
  if(start<end)transitions[[length(transitions)+1]]<-list(entity_id=aid,start=start,stop=end,from_state=state,to_state=NULL)
 }
 cause<-fit_cause_specific_hazards(panel,matrix(numeric(),5,0),events,feature_order=character(),causes=c('cure','exit'),ties='breslow',target=target,training=train,model_id='prior-cure-cause',model_version='1')
 fine<-fit_fine_gray(times,events,matrix(numeric(),5,0),cause_of_interest='cure',feature_order=character(),censoring_spec=list(estimator='kaplan_meier',independence_assumption='independent_within_censoring_group'),target=target,training=train,model_id='prior-cure-fine',model_version='1')
 path<-list(list(path_id='first-default',start=0,stop=90,covariates=list(),stratum='all',covariate_time=0))
 curves<-lapply(list(cause_specific=cause,fine_gray=fine),function(m)predict_event_curves(m,path,c(15,45,60,90),feature_order=character(),target=target)$value$curves[[1]])
 aj<-estimate_multistate_transitions(transitions,states=c('D','C','R','X'),allowed_transitions=list(c('D','C'),c('D','X'),c('C','R')),initial_state='D',target=target,training=train,model_id='prior-cure-redefault',model_version='1')
 model<-aj$model;model$collections_contract<-list(cure_states=list('C'),default_states=list('D','R'),supported_actions=list('observed-offer'),supported_initial_states=list('D'),action_interpretation='observational_conditioning')
 observed<-do.call(build_collections_episodes,d$heldout);actual<-build_cure_targets(observed,horizon_days=60,redefault_horizon_days=60,evaluation_cutoff=d$heldout$evaluation_cutoff)$value
 missing<-d$heldout;missing$coverage<-list();uncertain<-do.call(build_collections_episodes,missing);unknown<-build_cure_targets(uncertain,horizon_days=60,redefault_horizon_days=60,evaluation_cutoff=missing$evaluation_cutoff)$value
 ah<-d$action_history;ah$required_features<-unlist(ah$required_features);action<-do.call(build_collections_action_history,ah)$value
 w<-d$open_workout;episode<-w$episode;default<-Filter(function(r)r$state=='DEFAULT',d$heldout$states)[[1]];asof<-d$valuation_date
 if(episode$episode_id!=paste0(default$account_id,'-first-default')||any(vapply(d$action_history$actions,function(a)a$account_id!=default$account_id,TRUE))||episode$default_date!=default$occurred_at||any(vapply(actual,function(t)!is.null(t$cure_at)&&t$cure_at<=asof,TRUE))||asof!=d$action_history$actions[[1]]$occurred_at)stop('Offer and open valuation must precede observed cure on the same default claim')
 residual<-episode$residual_exposure_at_reference;cash<-sum(vapply(Filter(function(r)r$kind=='RECOVERY'&&r$known_at<=asof,w$cashflows),function(r)r$amount,0))
 if(abs(episode$ead_at_default-cash-residual)>1e-10)stop('One residual claim must reconcile prior actual recoveries')
 money<-matrix(0,5,4);money[5,]<-c(.25*residual,residual,.5*residual,.25*residual)
 prediction<-predict_cure_and_redefault(list(episode_id=episode$episode_id,state='D',model_time=as.numeric(as.Date(asof)-as.Date(default$occurred_at)),known_at=asof,ever_default=TRUE),model,list(action_id='observed-offer',assumption_reference='Prior observational support, no causal action effect'),c(0,30,45,60,75),money,target=target,currency='EUR')$value
 p_cure<-tail(unlist(prediction$sustained_cure_probability),1);p_red<-tail(unlist(prediction$cure_then_redefault_probability),1);probs<-c(p_cure-p_red,p_red,1-p_cure)
 costs<-vapply(d$period_end_dates,function(t)sum(vapply(Filter(function(f)f$cashflow_date==t&&f$known_at<=asof,w$future_costs),function(f)f$amount,0)),0)
 base<-list(plan_id='existing',payments=if(is.null(d$baseline_payments))c(residual/2,residual/2,0)else unlist(d$baseline_payments),period_interest_rates=c(0,0,0),new_fees=c(0,0,0),servicing_costs=costs,receipt_fractions=c(1,1,1),capitalize_unpaid_interest=FALSE)
 candidate<-base;candidate$plan_id<-'deferred-offer';candidate$payments<-if(is.null(d$candidate_payments))c(0,residual/2,residual/2)else unlist(d$candidate_payments)
 args<-list(contract=list(currency='EUR',principal=residual,principal_arrears=if(is.null(d$principal_arrears))20 else d$principal_arrears,accrued_interest=0,fees=0,as_of=asof),baseline=base,candidate=candidate,affordability_paths=list(list(path_id='observed-preoffer-budget',currency='EUR',available_cash=if(is.null(d$available_cash))c(20,50,50)else unlist(d$available_cash))),discount_factors=unlist(d$discount_factors),period_end_dates=unlist(d$period_end_dates),currency='EUR')
 arrangement<-do.call(simulate_repayment_arrangement,args)$value;scenarios<-list();sids<-c('cure-no-redefault','cure-then-redefault','no-cure')
 for(i in seq_along(probs)){
  sid<-sids[i];q<-c(1,.5,.25)[i];a<-args;a$candidate$receipt_fractions<-rep(q,3);schedule<-do.call(simulate_repayment_arrangement,a)$value$candidate$schedule;flows<-list()
  for(j in seq_along(schedule)){flows[[length(flows)+1]]<-list(cashflow_id=paste0(sid,'-receipt-',j-1),kind='RECOVERY',period_end=d$period_end_dates[[j]],amount=schedule[[j]]$received_payment);flows[[length(flows)+1]]<-list(cashflow_id=paste0(sid,'-cost-',j-1),kind='COST',period_end=d$period_end_dates[[j]],amount=costs[j])}
  scenarios[[i]]<-list(scenario_id=sid,probability=probs[i],cashflows=flows)
 }
 offered<-list(action_id='offered-plan',currency='EUR',uptake_probability=d$uptake_probability,accepted_scenarios=scenarios,declined_scenarios=list(list(scenario_id='declined',probability=1,cashflows=list(list(cashflow_id='decline-receipt',kind='RECOVERY',period_end=tail(unlist(d$period_end_dates),1),amount=d$declined_recovery)))),offer_cost=d$offer_cost,support_status='SYNTHETIC_ASSUMPTION',assumption_reference='Explicit cash conditional on prior observational cure paths; uptake and decline cash are assumptions')
 value<-expected_collections_value(list(offered),unlist(d$discount_factors),ead=residual,currency='EUR',period_end_dates=unlist(d$period_end_dates))$value[[1]];declined<-offered;declined$uptake_probability<-0
 no_uptake<-expected_collections_value(list(declined),unlist(d$discount_factors),ead=residual,currency='EUR',period_end_dates=unlist(d$period_end_dates))$value[[1]]
 open_value<-estimate_open_workout_lgd(list(episode),w$cashflows,d$residual_model,matrix(numeric(),1,0),list(w$valuation),w$future_costs,feature_order=character(),expected_target=d$recovery_target,stage_spec=d$stage_spec,as_of=asof,currency='EUR',extrapolation='parametric')$value
 out<-list(data_origin=d$data_origin,prior_cure_targets=targets,prior_first_cure_forecasts=curves,prior_transition_rows=transitions,observed_episodes=observed$value,cure_targets=actual,missing_probation_targets=unknown,action_history=action,landmark_prediction=prediction,exclusive_history_probabilities=as.list(probs),repayment_arrangement=arrangement,action_value=value,no_uptake_value=no_uptake,remaining_claim=residual,prior_discounted_recovery=cash,ultimate_assumption_based_lgd=1-(cash+value$expected_net_recovery)/episode$ead_at_default,open_workout=open_value,raw_artifacts_available=TRUE)
 if(verify_reference){stopifnot(abs(prediction$expected_recovery-7/12*residual)<1e-10,unlist(arrangement$baseline$affordability[[1]]$residual_cash)[1]<0,min(unlist(arrangement$candidate$affordability[[1]]$residual_cash))>=0,max(abs(probs-1/3))<1e-12,max(abs(unlist(curves$cause_specific$cumulative_incidence$cure)-c(0,exp(-.2)*(1-exp(-1/3)),rep(exp(-.2)*(1-exp(-5/6)),2))))<1e-12,abs(value$expected_net_recovery-(d$uptake_probability*(7/12*residual-sum(costs))+(1-d$uptake_probability)*d$declined_recovery-d$offer_cost))<1e-10,no_uptake$expected_net_recovery==d$declined_recovery-d$offer_cost,identical(vapply(observed$value$episodes,function(e)e$status,''),c('CURED','OPEN')),actual[[1]]$redefault_label==1,is.null(unknown[[1]]$cure_label),!action$rows[[1]]$accepted,!action$rows[[1]]$performed,action$rows[[1]]$preaction_features$available_cash==50,!arrangement$contract_changed,!arrangement$default_status_changed)}
 if(return_artifacts)out$raw_artifacts<-list(cause_model=cause$model,fine_gray_model=fine$model,multistate_model=model,action_value_input=offered,arrangement_input=args)
 out
}
