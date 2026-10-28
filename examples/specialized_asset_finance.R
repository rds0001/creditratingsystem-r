# Explicit physical cash-pool contracts for sixteen specialized-finance variants.
if(!exists('fit_var'))library(creditratingsystem)
.specialized_close<-function(a,b)stopifnot(abs(a-b)<=1e-10+1e-10*abs(b))
.specialized_ledger_oracle<-function(case,scenario,price,transaction){
 principal<-case$principal;interest<-0;reserve<-case$reserve;arrears<-0;oparrears<-0;previous_material<-FALSE;clock<-0;expected<-list();default_date<-NULL;loss<-0;ead_default<-0
 origin<-as.Date('2023-01-01');previous<-origin;dates<-c('2024-01-01','2025-01-01')
 for(i in 1:2){
  when<-as.Date(dates[i]);multiplier<-round(1-case$floating_share[[i]]+case$floating_share[[i]]*price[i],12)
  cash<-case$revenue[[i]]*multiplier-case$fixed_cost[[i]]-case$capex[[i]]
  for(j in seq_along(case$contract_amounts))if(scenario$counterparty_stress&&!(j-1L)%in%unlist(case$safe_counterparty_indices))cash<-cash-case$contract_amounts[[j]][[i]]
  coupon<-case$principal*.1*as.numeric(when-previous)/365;due<-coupon+if(i==2)case$principal else 0;due<-due+arrears
  opening_reserve<-reserve;opening_oparrears<-oparrears;available<-max(cash,0)+reserve;opdue<-max(-cash,0)+oparrears;paid_op<-min(available,opdue);available<-available-paid_op;oparrears<-opdue-paid_op
  payment<-min(available,due);available<-available-payment;arrears<-due-payment;cash_consumed<-max(paid_op+payment-max(cash,0),0);reserve<-max(opening_reserve-cash_consumed,0)
  interest<-interest+coupon;paid_interest<-min(interest,payment);interest<-interest-paid_interest;paid_principal<-payment-paid_interest;principal<-principal-paid_principal;ead<-principal+interest
  material<-arrears>1&&(if(ead==0)0 else arrears/ead)>.01;clock<-if(material&&previous_material)clock+as.numeric(when-previous)else 0
  completion<-if(is.null(case$completion_multiplier))1 else case$completion_multiplier[[i]];market<-round(price[i]*completion,12);elapsed<-as.numeric(when-origin)/365
  assets<-vapply(seq_along(case$asset_values),function(j){ageing<-if(isTRUE(case$obsolescence))exp(-.35*elapsed)else max(case$life-elapsed,0)/case$life;case$salvage[[j]]+(case$asset_values[[j]]-case$salvage[[j]])*ageing},numeric(1))*market
  buffer<-max(opening_reserve-opening_oparrears,0);utp<-cash+buffer<due&&ead>sum(assets);default<-clock>90||utp
  expected[[i]]<-list(date=dates[i],cfads=cash,principal_paid=paid_principal,interest_paid=paid_interest,ead=ead,reserve=reserve,arrears=arrears,clock=clock,assets=assets)
  if(default){default_date<-dates[i];ead_default<-ead;net<-sum(assets)+reserve-sum(unlist(case$recovery_cost));paid<-min(ead,max(net,0));df<-1.05^(-case$delay/365);stopifnot(reserve==0);loss<-ead-paid*df+max(-net,0)*df;break}
  previous<-when;previous_material<-material
 }
 path<-Filter(function(p)p$scenario_id==scenario$scenario_id,transaction$paths)[[1]];stopifnot(identical(path$default_event$first_default_date,default_date),length(path$going_concern_ledger)==length(expected));.specialized_close(path$ead_at_default,ead_default)
 for(i in seq_along(expected)){
  actual<-path$going_concern_ledger[[i]];want<-expected[[i]];.specialized_close(actual$cfads,want$cfads);.specialized_close(actual$liquidity$closing_reserve,want$reserve)
  for(k in c('principal_paid','interest_paid','ead')).specialized_close(actual$facilities[[1]][[k]],want[[k]])
  .specialized_close(actual$material_arrears,want$arrears);stopifnot(actual$material_days_past_due==want$clock);stopifnot(max(abs(unlist(actual$asset_values)-want$assets))<1e-8)
 }
 actual_loss<-sum(vapply(Filter(function(x)x$scenario_id==scenario$scenario_id,transaction$recovery$claim_results),function(x)x$economic_loss,numeric(1)));.specialized_close(actual_loss,loss)
 list(defaulted=!is.null(default_date),loss=loss)
}
.specialized_guarantee<-function(case,transaction,cube,convention,verify){
 if(is.null(case$guarantee_cap))return(NULL)
 key<-case$case_id;guarantor<-paste0(key,'-guarantor');origin<-cube$origin;claim_id<-paste0(key,'-claim');rows<-list()
 for(s in seq_along(cube$scenario_ids)){
  sid<-cube$scenario_ids[[s]];weight<-cube$weights[[s]];path<-Filter(function(p)p$scenario_id==sid,transaction$paths)[[1]]
  if(!path$default_event$defaulted)next
  due<-path$default_event$first_default_date;q<-convention$guarantor_cumulative_pd[[due]]
  loss<-sum(vapply(Filter(function(x)x$scenario_id==sid,transaction$recovery$claim_results),function(x)x$economic_loss,numeric(1)))
  claim<-list(claim_id=claim_id,entity_id=key,amount=path$ead_at_default,currency='EUR',due_at=due,known_at=origin)
  expiry<-if(case$variant=='vendor_supported')convention$vendor_valid_until else convention$other_valid_until
  guarantee<-list(guarantee_id=paste0(key,'-guarantee'),guarantor_id=guarantor,currency='EUR',cap=case$guarantee_cap,cap_used=0,coverage_fraction=1,covered_claim_ids=list(claim_id),valid_from=origin,valid_until=expiry,known_at=origin,legally_effective=TRUE,evidence_reference='Synthetic dated capped contract')
  policy<-list(policy_id=paste0(key,'-allocation'),version='1',guarantee_priority=list(guarantee$guarantee_id),claim_priority=list(claim_id),cap_convention='non_reinstating_total')
  allocation<-allocate_guarantee_coverage(list(guarantee),list(claim),policy,as_of=origin,known_at=origin,currency='EUR')
  target<-list(event='default',horizon=if(due=='2024-01-01')1 else 2,time_unit='years',probability_measure='PHYSICAL',conditioning='given_macro_lessee_path')
  bt<-target;bt$exposure_id<-claim_id;borrower<-list(entity_id=key,pd=1,stage='PRE_TRANSFER',target=bt,applied_effects=list());supporter<-list(entity_id=guarantor,pd=q,stage='STANDALONE',target=target,applied_effects=list())
  joint<-joint_default_probability(1,q,event_order=c(key,guarantor),joint_probability=q)$value
  contract<-list(effect_id=paste0(key,'-protection'),currency='EUR',assumption_reference=convention$dependence,stage='PRE_GUARANTEE',valuation_date=due,states=list(borrower_only=list(loss_before_guarantee=loss,payout_fraction=1,evidence_reference='Capped claim paid if guarantor survives'),joint_default=list(loss_before_guarantee=loss,payout_fraction=0,evidence_reference='No payment on guarantor default')))
  adjusted<-apply_guarantee_risk(borrower,supporter,allocation,claim_id,contract,joint_default_probability=joint)$value
  paid<-if(due<expiry)min(case$guarantee_cap,max(loss,0))else 0
  states<-list(list(state_id='survives',events=list(borrower_default=TRUE,guarantor_default=FALSE),probability=1-q,conditional_loss=loss-paid),list(state_id='fails',events=list(borrower_default=TRUE,guarantor_default=TRUE),probability=q,conditional_loss=loss))
  states<-c(states,lapply(c(FALSE,TRUE),function(g)list(state_id=paste0('no_borrower_',if(g)'True'else'False'),events=list(borrower_default=FALSE,guarantor_default=g),probability=0,conditional_loss=0)))
  exclusive<-event_conditioned_loss(states,event_names=c('borrower_default','guarantor_default'),currency='EUR',probability_measure='PHYSICAL')$value
  if(verify){.specialized_close(adjusted$expected_loss_after,loss-(1-q)*paid);.specialized_close(exclusive$expected_loss,adjusted$expected_loss_after);stopifnot(adjusted$pd==1);if(case$variant=='vendor_supported')stopifnot(adjusted$covered_amount==0)}
  rows[[length(rows)+1L]]<-list(scenario_id=sid,weight=weight,claim_due=due,guarantee_expiry=expiry,covered_amount=adjusted$covered_amount,guarantor_pd=q,joint_default_probability=joint,loss_before=loss,conditional_expected_loss_after=adjusted$expected_loss_after,conditional_borrower_pd=adjusted$pd)
 }
 after<-sum(vapply(rows,function(x)x$weight*x$conditional_expected_loss_after,numeric(1)));if(verify)stopifnot(after<=transaction$risk$expected_loss+1e-10)
 list(contract=convention,paths=rows,borrower_pd=transaction$risk$pd,expected_loss_before=transaction$risk$expected_loss,expected_loss_after=after)
}

run_specialized_asset_finance<-function(data=NULL,verify_reference=TRUE){
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/specialized_asset_finance.json',package='creditratingsystem'),simplifyVector=FALSE)
 lt<-data$lessee_training;tr<-lt$score_training;ca<-lt$calibration
 lessee_model<-fit_scorecard(do.call(rbind,lapply(tr$X,unlist)),unlist(tr$y),feature_order=unlist(tr$feature_order),fit_spec=tr$fit_spec,weights=unlist(tr$weights),target=lt$target,training=tr$training,model_id='equipment-lessee',model_version='1',tolerance=1e-13)
 lessee_cal<-calibrate_logistic(unlist(ca$values),unlist(ca$outcomes),unlist(ca$weights),metadata=ca$metadata)
 lessee_raw<-score_scorecard(lessee_model,matrix(lt$application_leverage,ncol=1),feature_order=unlist(tr$feature_order),expected_target=lt$target)$value$pd
 lessee_pd<-apply_calibrator(lessee_cal,lessee_raw,target=lt$target,base_model_version='1')$value[1]
 if(verify_reference)stopifnot(abs(lessee_pd-.1)<1e-8)
 lessee_pd<-round(lessee_pd,8)
 model<-fit_var(data$macro_history,lags=1,training=data$macro_training,target=data$macro_target,model_id='specialized-macro',model_version='1',covariance_estimator='mle')
 macro<-simulate_macro_scenarios(model,data$innovations)
 satellite<-do.call(fit_satellite_model,c(list(target_series=data$satellite_records,macro_history=data$macro_history),data$satellite_arguments))
 state<-list(as_of=macro$value$origin,known_at=macro$value$information_cutoff,mode='absolute',macro_history=list(list(0)),residual_state=0)
 noise<-list(kind='STANDARD_NORMAL',scenario_ids=macro$value$scenario_ids,time_grid=macro$value$time_grid,values=rep(list(list(0,0)),length(macro$value$scenario_ids)))
 price<-apply_satellite_model(satellite,macro,noise,state)$value
 ttc<-simulate_macro_scenarios(model,data$ttc_innovations,sampling_contract=data$ttc_sampling_contract)$value
 cube<-list(schema_version='1.0',kind='ScenarioCube',axes=list('scenario','time','factor'),scenario_ids=macro$value$scenario_ids,time_grid=macro$value$time_grid,weights=macro$value$weights,origin=macro$value$origin,frequency='annual',probability_measure='PHYSICAL',sampling_contract=list(law='FORECAST_CONDITIONAL',conditioning_scope='PIT',quadrature='Three-point Gaussian innovation tensor crossed with declared common counterparty Bernoulli state; fixed weights, no IID confidence interval.'),factor_ids=list(),units=list(),values=rep(list(list(list(),list())),length(macro$value$scenario_ids)))
 for(case in data$cases){
  key<-case$case_id;cube$factor_ids<-c(cube$factor_ids,list(paste0(key,'-revenue'),paste0(key,'-asset')));cube$units<-c(cube$units,list('relative_index','relative_index'))
  for(s in seq_along(cube$scenario_ids))for(t in 1:2){p<-price$values[[s]][[t]][[1]];completion<-if(is.null(case$completion_multiplier))1 else case$completion_multiplier[[t]];cube$values[[s]][[t]]<-c(cube$values[[s]][[t]],list(round(1-case$floating_share[[t]]+case$floating_share[[t]]*p,12),round(p*completion,12)))}
 }
 parameter_snapshot<-data$parameter_snapshot
 for(case in data$cases)if(case$product=='EQUIPMENT_LEASING')for(i in seq_along(parameter_snapshot$parameters[[case$case_id]]$counterparties$paths))if(parameter_snapshot$parameters[[case$case_id]]$counterparties$paths[[i]]$period_end=='2024-01-01')parameter_snapshot$parameters[[case$case_id]]$counterparties$paths[[i]]$conditional_pd<-lessee_pd
 recipes<-lapply(data$cases,function(c)list(transaction_id=c$case_id,version='1',subject_id=c$case_id,parameter_id=c$case_id,calendar=data$calendar,parameter_hash=content_fingerprint(parameter_snapshot$parameters[[c$case_id]])$value,cube_hash=content_fingerprint(cube)$value))
 execution<-simulate_transactions(recipes,cube,data$entity_innovations,parameter_snapshot)$value;variants<-list()
 for(case in data$cases){
  key<-case$case_id;tx<-Filter(function(t)t$transaction_id==key,execution$transactions)[[1]]
  facts<-list(product=case$product,variant=case$variant,phase=case$phase,contracts_verified=TRUE)
  snapshot<-list(subject_id=key,subject_type='facility',as_of=cube$origin,known_at=cube$origin,attributes=lapply(facts,function(v)list(value=v,observed_at=cube$origin,known_at=cube$origin)))
  rule<-list(rule_id=key,profile_id=key,profile_version='1',priority=1,effective_from=cube$origin,effective_to=NULL,known_at=cube$origin,conditions=lapply(names(facts),function(k)list(field=k,operator='eq',value=facts[[k]])))
  route<-route_rating_profile(snapshot,list(rule),cube$origin,cube$origin)$value;snapshot$attributes$contracts_verified$value<-FALSE
  negative<-route_rating_profile(snapshot,list(rule),cube$origin,cube$origin)$value;default<-lgd<-ead<-matrix(0,length(cube$scenario_ids),2)
  for(s in seq_along(cube$scenario_ids)){
   sid<-cube$scenario_ids[[s]];path<-Filter(function(p)p$scenario_id==sid,tx$paths)[[1]]
   if(path$default_event$defaulted){t<-match(path$default_event$first_default_date,vapply(data$calendar,function(p)p$end_date,character(1)));default[s,t]<-1;ead[s,t]<-path$ead_at_default;loss<-sum(vapply(Filter(function(r)r$scenario_id==sid,tx$recovery$claim_results),function(r)r$economic_loss,numeric(1)));lgd[s,t]<-loss/ead[s,t]}
  }
  loss<-expected_loss_from_paths(default,lgd,ead,unlist(cube$weights),c(1,1),scenario_ids=unlist(cube$scenario_ids),time_grid=unlist(cube$time_grid),currency='EUR',probability_measure='PHYSICAL')$value
  if(verify_reference){
   stopifnot(route$status=='ROUTED',negative$status=='NO_ROUTE')
   checks<-lapply(seq_along(data$scenario_contract),function(s).specialized_ledger_oracle(case,data$scenario_contract[[s]],vapply(price$values[[s]],function(v)v[[1]],numeric(1)),tx))
   weights<-vapply(data$scenario_contract,function(sc)sc$weight,numeric(1));.specialized_close(tx$risk$pd,sum(weights*vapply(checks,function(v)as.numeric(v$defaulted),numeric(1))));.specialized_close(tx$risk$expected_loss,sum(weights*vapply(checks,function(v)v$loss,numeric(1))));.specialized_close(loss$expected_loss,tx$risk$expected_loss)
  }
  protection<-.specialized_guarantee(case,tx,cube,data$guarantee_convention,verify_reference)
  variants[[key]]<-list(guarantee=protection,lessee_pd=if(case$product=='EQUIPMENT_LEASING')lessee_pd else NULL,product=case$product,variant=case$variant,profile_id=route$profile_id,missing_contract_status=negative$status,transaction=tx,joint_path_loss=loss)
 }
 moments<-list()
 for(t in 1:2){v<-vapply(ttc$values,function(v)v[[t]][[1]],numeric(1));w<-unlist(ttc$weights);mean<-sum(w*v);variance<-sum(w*(v-mean)^2);moments[[t]]<-list(mean=mean,variance=variance);if(verify_reference){.specialized_close(mean,-3/35);.specialized_close(variance,data$hidden_truth$stationary_variance)}}
 p<-model$model$parameters
 if(verify_reference){
  .specialized_close(p$intercept[[1]],-1/7);.specialized_close(p$lag_coefficients[[1]][[1]][[1]],-2/3);.specialized_close(p$covariance[[1]][[1]],data$hidden_truth$innovation_variance)
  for(s in seq_along(data$scenario_contract)){sc<-data$scenario_contract[[s]];first<--1/7+sqrt(data$hidden_truth$innovation_variance)*sc$innovations[[1]][[1]];.specialized_close(price$values[[s]][[1]][[1]],1);.specialized_close(price$values[[s]][[2]][[1]],exp(.2*first))}
 }
 list(data_origin='SYNTHETIC',scenario_cube_hash=execution$scenario_cube_hash,parameter_snapshot_hash=execution$parameter_snapshot_hash,innovation_hash=execution$innovation_hash,scenario_precision_contract=data$scenario_precision_contract,pit_sampling_contract=cube$sampling_contract,ttc_sampling_contract=ttc$sampling_contract,stationary_moments=moments,macro_parameters=p,satellite_parameters=satellite$model$parameters,variants=variants)
}
