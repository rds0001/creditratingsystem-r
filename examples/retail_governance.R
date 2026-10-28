retail_contractual_service<-function(case){
 t<-case$terms;kind<-case$kind;aux<-list()
 if(kind=='MORTGAGE')service<-t$principal*t$annual_rate+t$annual_principal
 else if(kind=='HELOC')service<-t$drawn*t$annual_rate/12+unlist(t$monthly_principal)
 else if(kind=='CONSOLIDATION'){
  service<-t$new_payment;aux$old_service_still_counted<-sum(unlist(t$old_payments))
  if(!identical(as.numeric(unlist(case$budget$existing_debt_service)),as.numeric(sum(unlist(t$old_payments)))))stop('Unsettled old facilities must remain in application capacity')
 }else if(kind=='EDUCATION')service<-unlist(t$monthly_dues)
 else if(kind=='AUTO_BALLOON'){
  remaining<-t$principal;service<-numeric();for(principal in unlist(t$annual_principal)){service<-c(service,principal+remaining*t$annual_rate);remaining<-remaining-principal};aux$remaining_principal<-remaining
 }else if(kind=='OPERATING_LEASE'){
  if(t$customer_bears_residual)stop('Operating-lease reference requires a separate residual liability contract');service<-t$rent;aux$lessor_residual_excluded<-t$lessor_residual_value
 }else if(kind=='CREDIT_CARD'){service<-max(t$minimum_floor,t$minimum_fraction*t$statement_balance);aux$full_repayment_scenario<-t$statement_balance}
 else if(kind=='CHARGE_CARD'){if(t$deferred)stop('Deferred charge requires its own dated due schedule');service<-t$statement_balance}
 else if(kind=='OVERDRAFT'){if(t$demand_known_at>'2025-01-01'||t$demand_due_at<'2025-01-01')stop('Demand must be known and prospective');service<-t$demand_amount}
 else if(kind=='BNPL')service<-vapply(t$weekly_plan_dues,function(x)sum(unlist(x)),numeric(1))
 else if(kind=='MICRO')service<-unlist(t$weekly_dues)
 else if(kind=='OWNER_BUSINESS'){
  service<-t$new_payment;aux$business_after_draw<-t$sales-t$operating_cost-t$tax-t$capex-t$business_debt-t$owner_draw
  if(!identical(as.numeric(unlist(case$budget$net_income)),as.numeric(t$owner_draw+t$other_household_income)))stop('Owner transfer must enter household income once')
 }else stop('Unsupported contractual payment type')
 list(service=as.list(service),aux=if(length(aux))aux else structure(list(),names=character()))
}
run_retail_governance<-function(data=NULL,verify_reference=TRUE,product_ids=NULL){
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/retail_governance.json',package='creditratingsystem'),simplifyVector=FALSE)
 D<-'2025-01-01';P<-'2025-01-31';R<-'2025-02-28';out<-list();models<-list(data$application_model,data$behavioral_model);registry<-NULL
 for(model in models){
  if(model$training$known_at>=D)stop('Risk model must be frozen before application date')
  registry<-register_rating_model_version(registry,list(model=model,dependencies=structure(list(),names=character()),configuration=list(population='explicit pooled synthetic illustration')),list(model_id=model$model_id,model_version='1',registered_at='2024-12-31'))$value
 }
 hashes<-setNames(lapply(registry$entries,function(e)e$model_hash),vapply(registry$entries,function(e)e$model_id,character(1)));appmodel<-models[[1]];bmodel<-models[[2]];policy<-compile_credit_decision_policy(data$policy)$value
 for(product in names(data$cases)){
  if(!is.null(product_ids)&&!product%in%product_ids)next
  c<-data$cases[[product]]
  if(c$term_evidence$known_at>D||c$income_evidence$known_at>D)stop('Capacity evidence not known at application')
  economic<-retail_contractual_service(c);service<-economic$service;aux<-economic$aux;budget<-c$budget
  for(k in c('net_income','recurring_expenses','existing_debt_service'))budget[[k]]<-unlist(budget[[k]])
  residual<-do.call(residual_income,c(budget,list(new_debt_service=unlist(service))))$value
  evidence<-list(income_verified=c$income_evidence$income_verified,terms_verified=TRUE)
  capacity<-assess_affordability(list(minimum_residual=residual$minimum),data$affordability_profile,evidence)$value
  no_evidence<-assess_affordability(list(minimum_residual=residual$minimum),data$affordability_profile,list(income_verified=FALSE,terms_verified=TRUE))$value
  aid<-paste0(product,'-application');sid<-paste0(product,'-customer');account<-paste0(product,'-existing-account')
  app<-list(application_id=aid,customer_id=sid,product_id=product,population_id='synthetic_pooled_new_customer',application_at=D,information_cutoff=D,requested_terms=c$requested_terms)
  features<-c$application_features
  snapshot<-build_application_snapshot(list(app),features,required_features='recent_arrears')$value[[1]];risk<-predict_application_risk(snapshot,appmodel,data$application_target)$value
  subject<-list(subject_id=sid,subject_type='customer');subjects<-build_rating_subjects(list(customer=list(list(subject_id=sid,version='1',effective_from=D,effective_to=NULL,known_at=D,attributes=list(product=product)))),list(),D,D)$value
  rs<-c(subject,list(as_of=D,known_at=D,attributes=list(product=list(value=product,observed_at=D,known_at=D),recent_arrears=list(value=c$recent_arrears,observed_at='2024-12-30',known_at='2024-12-31'))))
  rules<-list(list(rule_id=product,profile_id=paste0(product,'-application'),profile_version='1',priority=1,effective_from=D,effective_to=NULL,known_at='2024-12-31',conditions=list(list(field='product',operator='eq',value=product))))
  route<-route_rating_profile(rs,rules,D,D)$value
  applicability<-assess_rating_applicability(rs,list(contract_id=paste0(product,'-support'),version='1',required_features=list('product','recent_arrears'),allowed_values=list(product=list(product)),support_bounds=list(recent_arrears=list(0,1)),maximum_age_days=list(recent_arrears=31)),D,D)$value
  mh<-hashes[[appmodel$model_id]];assignment<-list(list(assignment_id=paste0(product,'-champion'),profile_id=route$profile_id,purpose='application',model_id=appmodel$model_id,model_version='1',model_hash=mh,effective_from=D,effective_to='2027-01-01',known_at='2024-12-31'))
  selected<-select_effective_model(registry,assignment,route$profile_id,'application',R,R)$value
  request<-list(row_id=aid,known_at=D,model_id=appmodel$model_id,model_version='1',model_hash=mh,recipe=list(method='application_risk',version='1.0',parameters=list(target=data$application_target)),input=list(snapshot=snapshot))
  batch<-rate_subject_batch(list(request),registry,paste0(product,'-batch'),D)$value
  binding<-list(profile_id=route$profile_id,profile_version='1',model_id=appmodel$model_id,model_version='1',model_hash=mh,input_hash=batch$rows[[1]]$input_hash)
  components<-list(c(list(component_id=paste0(product,'-pd'),quantity='PD',value=risk$pd),subject,list(variant='application_given_funding',definition=data$application_target)))
  assessment<-compose_risk_assessment(paste0(aid,'-assessment'),subject,components,binding,D,D,D)$value;dates<-list(as_of=D,known_at=D)
  decision<-apply_credit_decision_policy(policy,c(risk,dates,list(freshness='CURRENT')),c(list(status=capacity$assessment),dates),c(list(status='ELIGIBLE'),dates),c(list(status='CLEAR'),dates),as_of=D,known_at=D)$value
  missing_decision<-apply_credit_decision_policy(policy,c(risk,dates,list(freshness='CURRENT')),c(list(status=no_evidence$assessment),dates),c(list(status='ELIGIBLE'),dates),c(list(status='CLEAR'),dates),as_of=D,known_at=D)$value
  history<-do.call(build_payment_history_features,c$payment_history)$value
  panel_args<-c$historical_account;panel_args$observation_dates<-unlist(panel_args$observation_dates)
  panel<-do.call(build_account_month_panel,panel_args)$value$accounts
  if(is.null(history$features$max_dpd)||panel[[length(panel)]]$features$dpd!=history$features$max_dpd||abs(panel[[length(panel)]]$features$past_due_amount-history$daily_history[[length(history$daily_history)]]$unpaid_due_amount)>1e-10)stop('Statement arrears must reconcile to the dated payment ledger')
  behavior<-lapply(panel,function(s)predict_behavioral_risk(s,bmodel,data$behavioral_target)$value)
  breqs<-lapply(panel,function(s)list(row_id=paste0(account,'-',s$as_of),known_at=s$known_at,model_id=bmodel$model_id,model_version='1',model_hash=hashes[[bmodel$model_id]],recipe=list(method='behavioral_risk',version='1.0',parameters=list(target=data$behavioral_target)),input=list(snapshot=s)))
  bbatch<-rate_subject_batch(breqs,registry,paste0(product,'-behavioral-batch'),R)$value
  bs<-list(subject_id=account,subject_type='facility');bcomponents<-list(c(list(component_id=paste0(account,'-pd'),quantity='PD',value=behavior[[1]]$pd),bs,list(variant='active_account',definition=data$behavioral_target)))
  bbinding<-list(profile_id=paste0(product,'-behavioral'),profile_version='1',model_id=bmodel$model_id,model_version='1',model_hash=hashes[[bmodel$model_id]],input_hash=bbatch$rows[[1]]$input_hash)
  performing<-compose_risk_assessment(paste0(account,'-assessment'),bs,bcomponents,bbinding,P,P,P)$value
  pool<-list();for(i in 1:2){label<-c('application','performing')[i];score<-c(risk$pd,behavior[[1]]$pd)[i];date<-c(D,P)[i];row<-list(observation_id=paste0(product,'-',label),index_date=date,feature_observed_at=date,feature_known_at=date,score=score,product=product,borrower_type='person',facility_type=c$kind,delinquency_state='not_defaulted');pool[[i]]<-assign_retail_risk_pool(list(row),data$pool_model,as_of=date,information_cutoff=date,expected_target=data$pool_target,uncovered_policy='error')$value}
  trigger<-list(event_id=paste0(account,'-delay'),subject_id=account,type='payment_delay',occurred_at=R,known_at=R)
  freshness_policy<-list(maximum_age_days=60,maximum_information_age_days=60,trigger_types=list('payment_delay'))
  fresh<-check_assessment_freshness(performing,freshness_policy,list(),R,R)$value;stale<-check_assessment_freshness(performing,freshness_policy,list(trigger),R,R)$value
  deps<-list(list(assessment_id=performing$assessment_id,dependency_type='facility',dependency_id=account));event<-list(event_id=trigger$event_id,revision='1',occurred_at=R,known_at=R,dependencies=list(list(type='facility',id=account)),payload=list(dpd=history$features$max_dpd))
  impact<-apply_assessment_event(NULL,event,deps,R)$value;proposal<-event;proposal$event_id<-paste0(account,'-proposal');proposal$payload<-list(action='PROPOSED_REPAYMENT_REVIEW',contract_changed=FALSE,default_status_changed=FALSE);proposal_impact<-apply_assessment_event(impact$state,proposal,deps,R)$value
  target<-list(event='default',probability_measure='PHYSICAL',default_definition='DECLARED_FIRST_DEFAULT',unit='account',horizon_days=365,default_grade='D')
  forecast<-list(forecast_id=paste0(account,'-forecast'),case_id=account,entity_id=account,cluster_id=sid,cohort='2025-01',segment=product,model_id=bmodel$model_id,model_version='1',forecast_at=P,known_at=P,pd=behavior[[1]]$pd,target=target,origin_grade='P',override=FALSE,prediction_origin='OUT_OF_TIME')
  cohort<-build_monitoring_cohorts(list(forecast),c$monitoring_events,c$monitoring_coverage,target_contract=target,evaluation_cutoff='2026-02-01')$value
  monitor<-monitor_rating_cohort(cohort,group_by='segment',reference_window=list(start=P,end=P),minimum_count=30,uncertainty='none')$value
  permutation_grid<-as.matrix(expand.grid(rep(list(0:3),4)));permutation_grid<-permutation_grid[apply(permutation_grid,1,function(x)length(unique(x))==4),,drop=FALSE]
  drift<-detect_credit_feature_drift(list(list(dpd=0),list(dpd=0)),list(list(dpd=0),list(dpd=history$features$max_dpd)),feature_order='dpd',method='ks',permutations=permutation_grid,multiple_testing='none',dependence_assumption='exchangeable_rows')$value
  nullspec<-list(model_id=bmodel$model_id,model_version='1',calibration_id='arithmetic-zero-reference',residual_scale='observed_minus_expected_rate',dependence='prespecified_dependent',probability_measure='PHYSICAL',calibrated_before='2025-01-30',validation_paths_independent=TRUE)
  calibration<-detect_pd_calibration_drift(list(list(time='2026-02-01',value=monitor[[1]]$metrics$calibration_error,mature=TRUE)),method='ewma',parameters=list(smoothing=.5),null_paths=matrix(0,19,1),null_spec=nullspec,alpha=.05,validation_null_paths=matrix(0,20,1))$value
  transitions<-list(list(from_state='DRAFT',to_state='FINAL',event_type='approve',required_gates=list('review')),list(from_state='FINAL',to_state='SUPERSEDED',event_type='close',required_gates=list('closed')))
  finalized<-transition_assessment_state(performing,list(event_id=paste0(account,'-final'),type='approve',occurred_at=P,known_at=P,reason='Separate historical account assessment reviewed',gates=list(review=TRUE)),transitions)$value
  exited<-transition_assessment_state(finalized,list(event_id=paste0(account,'-closed'),type='close',occurred_at='2026-02-01',known_at='2026-02-01',reason='Historical account settlement completed',gates=list(closed=TRUE)),transitions)$value
  newer<-appmodel;newer$model_version<-'2';newer$parameters$coefficients[[1]]<-newer$parameters$coefficients[[1]]+1
  later<-register_rating_model_version(registry,list(model=newer,dependencies=structure(list(),names=character()),configuration=structure(list(),names=character())),list(model_id=appmodel$model_id,model_version='2',registered_at='2025-02-01'))$value
  replay<-replay_assessment_history(list(request),later,batch$rows,D)$value
  breplay<-replay_assessment_history(breqs,later,bbatch$rows,R)$value
  out[[product]]<-list(contractual_service=service,contract_note=c$contract_note,economic_details=aux,residual_income=residual,capacity=capacity,missing_evidence_capacity=no_evidence,subjects=subjects,route=route,applicability=applicability,snapshot=snapshot,application_risk=risk,selected_model=selected,assessment=assessment,decision=decision,missing_evidence_decision=missing_decision,payment_history=history,account_panel=panel,behavioral_risk=behavior,performing_assessment=performing,pool_assignments=pool,freshness=fresh,triggered_freshness=stale,event=impact,restructuring_proposal_event=proposal_impact,cohort=cohort,monitoring=monitor,feature_drift=drift,calibration_drift=calibration,exit_assessment=exited,replay=replay,behavioral_replay=breplay)
  if(verify_reference){
   stopifnot(abs(max(unlist(service))-c$reference_due)<1e-10,abs(risk$pd-.02)<1e-8,abs(behavior[[1]]$pd-.01)<1e-8,abs(behavior[[2]]$pd-1/(1+99/(11^(28/30))))<1e-8,history$features$max_dpd==28,fresh$status=='CURRENT',stale$status=='STALE',decision$action==if(residual$minimum>=0)'APPROVE'else'DECLINE',missing_decision$action!='APPROVE',monitor[[1]]$status=='INSUFFICIENT_INFORMATION',monitor[[1]]$metrics$n==1,replay$status=='REPRODUCED',exited$state=='SUPERSEDED')
  }
 }
 list(data_origin=data$data_origin,version=data$version,product=data$product,variants=out)
}
