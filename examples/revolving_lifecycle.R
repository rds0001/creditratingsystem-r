# Four dated revolving account journeys; native fits and observed workout ledgers.
library(creditratingsystem)
.rl_origin<-function(case,product){
 t<-case$application_target;tr<-case$application_training;ca<-case$application_calibration
 X<-function(z)do.call(rbind,lapply(z,unlist))
 fit<-fit_binary_glm(X(tr$X),unlist(tr$y),weights=unlist(tr$weights),link='logit',feature_order='recent_arrears',fit_intercept=TRUE,target=t,training=tr$metadata,model_id=paste0(product,'-application'),model_version='1')
 raw<-predict_pd(fit,X(ca$X),feature_order='recent_arrears',expected_target=t)
 calibrator<-calibrate_logistic(unlist(raw$value),unlist(ca$y),weights=unlist(ca$weights),metadata=ca$metadata)
 calibrated<-apply_calibrator(calibrator$value,unlist(raw$value),target=t,base_model_version='1')$value;stopifnot(abs(unlist(calibrated)[1]-case$expected_application_pd)<1e-8)
 model<-fit$model;model$application_contract<-list(product_ids=list(product),population_ids=list(paste0(product,'-new-funded')),performance_origin='first_draw_at',term_stage='requested',term_features=structure(list(),names=character()));model$calibration_artifact<-calibrator$value
 features<-lapply(case$features,function(r){r$source_id<-paste0(product,'-prior-bureau');r})
 snapshots<-build_application_snapshot(case$applications,features,required_features='recent_arrears')$value
 risks<-lapply(snapshots,function(s)predict_application_risk(s,model,t,calibrator=calibrator$value)$value)
 policy<-compile_credit_decision_policy(case$application_policy)
 due<-if(case$kind=='CARD')30 else if(case$kind%in%c('CHARGE','OVERDRAFT'))600 else 1000
 residual<-residual_income(case$monthly_income,case$monthly_expenses,case$existing_debt_service,due,period='monthly',currency='EUR')$value
 ratio<-debt_service_to_income(case$existing_debt_service+due,case$monthly_income,period='monthly',currency='EUR')$value
 capacities<-.rl_capacity(case,product)
 dated<-list(as_of='2024-01-01',known_at='2024-01-01')
 decisions<-Map(function(risk,cap)apply_credit_decision_policy(policy,c(risk,list(freshness='CURRENT'),dated),c(list(status=cap$assessment$assessment),dated),c(list(status='ELIGIBLE'),dated),c(list(status='CLEAR'),dated),as_of=dated$as_of,known_at=dated$known_at)$value,risks,capacities)
 defaults<-lapply(Filter(function(e)e$event_type=='DEFAULT',case$events),function(e)list(event_id=e$event_id,facility_id=e$account_id,default_at=e$occurred_at,known_at=e$known_at))
 exits<-lapply(Filter(function(e)e$event_type=='CLOSED',case$events),function(e)list(event_id=e$event_id,facility_id=e$account_id,exit_at=e$occurred_at,known_at=e$known_at,exit_type='closed'))
 targets<-build_application_targets(case$applications,case$decisions,case$bookings,defaults,exits,365,'2025-01-02')$value
 stopifnot(all(vapply(risks,function(r)abs(r$pd-case$expected_application_pd)<1e-8,TRUE)),all(vapply(decisions,function(d)d$action=='APPROVE',TRUE)))
 list(model=model,calibrator=calibrator$value,snapshots=snapshots,risks=risks,policy=policy$value,decisions=decisions,targets=targets,residual=residual,dsti=ratio,capacities=capacities)
}

.rl_history<-function(c,p,o){
 panel<-build_account_month_panel(c$accounts,c$statements,c$events,observation_dates=unlist(c$observation_dates),obligations=c$obligations,payments=c$payments)$value$accounts
 labels<-build_behavioral_targets(panel,c$events,horizon_months=2,target_event='DEFAULT',competing_events='CLOSED',exit_treatment='competing',evaluation_cutoff='2025-01-02',coverage=c$coverage)$value
 args<-c$execution_inputs$cohort
 cohort<-do.call(validate_retail_cohort,c(list(application_targets=o$targets,account_panel=panel,behavioral_targets=labels),args))$value
 list(panel=panel,labels=labels,cohort=cohort)
}
.rl_workout<-function(c,p,risk){
 x<-c$execution_inputs
 episodes<-do.call(build_collections_episodes,x$collections_history)$value
 labels<-build_cure_targets(episodes,horizon_days=90,redefault_horizon_days=90,evaluation_cutoff='2024-08-31')$value
 workouts<-lapply(x$workout_claims,function(w){
  pv<-discount_recoveries(w$cashflows,w$discount_curve,w$fx_contract,default_date=w$default_date,evaluation_cutoff='2024-08-31',reporting_currency='EUR')
  w$discounted_recovery<-pv$value;w$loss<-workout_lgd(w$ead,pv,episode_status='CLOSED',default_date=w$default_date,evaluation_cutoff='2024-08-31',currency='EUR')$value;w
 })
 inputs<-x$collections_validation
 inputs$forecasts<-Map(function(f,w){f$predicted_cashflows[[1]]$amount<-(1-unlist(risk$lgd$mean_lgd)[1])*w$ead;f$predicted_cashflows[[1]]$occurred_at<-as.character(as.Date(w$default_date)+25);f},inputs$forecasts,workouts)
 validation<-do.call(collections_backtest,c(list(episodes=episodes),inputs))$value
 stopifnot(workouts[[1]]$loss$economic_lgd==0,abs(workouts[[2]]$loss$economic_lgd-c$second_default$expected_lgd)<1e-12,labels[[1]]$cure_at=='2024-05-20',labels[[1]]$redefault_at=='2024-06-20')
 list(episodes=episodes,cure_targets=labels,workouts=workouts,validation=validation)
}
.rl_vintage<-function(c,p,h){
 x<-c$execution_inputs;tr<-x$vintage_prior
 fit<-fit_discrete_hazard(tr$rows,matrix(numeric(),length(tr$rows),0),feature_order=character(),period_grid=0:8,link='logit',partial_period_policy='exclude_censored',target=tr$target,training=tr$training,model_id=paste0(p,'-vintage'),model_version='1')
 curve<-predict_event_curves(fit,list(list(path_id=p,start=0,stop=8,covariates=list(),stratum='all',covariate_time=0)),1:8,feature_order=character(),target=tr$target)$value$curves[[1]]
 table<-build_vintage_performance(c$accounts,h$panel,c$events,months_on_book=0:7,evaluation_cutoff='2025-01-02',coverage=c$coverage,default_event='DEFAULT',delinquency_states=character(),exit_events='CLOSED')$value
 cumulative<-1-unlist(curve$survival);predictions<-list()
 for(a in c$accounts)for(j in 1:8)predictions[[length(predictions)+1L]]<-list(account_id=a$account_id,months_on_book=j-1,model_id=paste0(p,'-vintage'),model_version='1',parameter_version='1',forecast_at=a$opened_at,known_at=a$known_at,cumulative_pd=cumulative[j],interval_pd=cumulative[j]-if(j==1)0 else cumulative[j-1])
 validation<-do.call(vintage_backtest,c(list(vintage_table=table,account_predictions=predictions),x$vintage))$value
 stopifnot(validation$metrics$cells[[4]]$cumulative$mean_outcome==1/3)
 list(table=table,curve=curve,validation=validation,model=fit$model)
}
.rl_roll<-function(c,p,h){
 x<-c$execution_inputs;states<-c('ACTIVE','DEFAULT','CURED','CLOSED')
 estimated<-estimate_retail_roll_rates(x$roll_prior,states=states,from_date='2022-01-31',to_date='2022-02-28',group_by=character(),weighting='count',missing_successor='error')$value
 P<-estimated$groups[[1]]$transition_matrix
 first<-P;first[[2]]<-as.list(c(0,1,0,0))
 projection<-project_retail_roll_rates(c(1,0,0,0),list(first,first),states=states,default_states='DEFAULT',closure_states='CLOSED',exposure_by_period=list(c(rep(600*c$scale,3),0),c(rep(600*c$scale,3),0)))$value
 forecasts<-lapply(x$roll_forecasts,function(r){r$transition_matrix<-P;r})
 args<-x$roll;args$states<-unlist(args$states)
 validation<-do.call(roll_rate_backtest,c(list(forecasts=forecasts),args))$value
 stopifnot(length(validation$diagnostics$episode_violations)==0,any(vapply(validation$diagnostics$endpoint_traces,function(r)r$redefault_transitions==1,TRUE)))
 list(estimate=estimated,projection=projection,validation=validation)
}
.rl_pool<-function(c,p,h,w){
 x<-c$execution_inputs;tr<-x$pool_prior
 fit<-fit_retail_risk_pools(tr$rows,tr$spec,target=tr$target,training=tr$training,model_id=paste0(p,'-pool'),model_version='1')
 args<-x$pool
 means<-fit$model$parameters$segments[[1]]$pools[[1]]$statistics$means
 args$predictions<-lapply(args$predictions,function(r){r$pd<-means$pd;r$lgd<-means$lgd;r$ead<-means$ead;r})
 assignment<-do.call(assign_retail_risk_pool,c(list(fitted_model=fit),args$assignment_inputs))$value
 # Independently rebuilt native workout cash and curve remain the source of labels.
 args$workouts[[1]]$recovery_cashflows<-w$workouts[[1]]$cashflows
 args$workouts[[1]]$discount_curve<-w$workouts[[1]]$discount_curve
 args$workouts[[1]]$fx_contract<-w$workouts[[1]]$fx_contract
 validation<-do.call(retail_pool_diagnostics,c(list(fitted_model=fit$model,pool_assignments=assignment),args))$value
 estimated<-estimate_pool_risk_parameters(assignment,args$cohort_outcomes,args$workouts,args$estimation_profile,evaluation_cutoff='2025-01-02',calibration_id=paste0(p,'-holdout-descriptive'),calibration_version='1')$value
 stopifnot(validation$metrics$pools[[1]]$pd_count_weighted$observed_rate==.5,validation$metrics$pools[[1]]$pd_exposure_weighted$observed_rate==1)
 list(model=fit$model,assignment=assignment,validation=validation,estimated=estimated)
}
.rl_risks<-function(c,p,h){
 z<-c$execution_inputs$risk;t<-z$behavioral_target;tr<-z$training
 fitted<-fit_penalized_glm(matrix(numeric(),8,0),unlist(z$prior_target),feature_order=character(),penalty='ridge',lambda_path=.1,selected_lambda=.1,scale=FALSE,tolerance=1e-12,target=t,training=tr,model_id=paste0(p,'-behavioral'),model_version='1')
 model<-fitted$model;model$behavioral_contract<-list(risk_unit='account',event_policy='first_default',product_ids=list(p),allowed_states=list('ACTIVE','CURED'),maximum_source_age_days=0)
 active<-Filter(function(r)r$as_of%in%c('2024-01-31','2024-02-29')&&r$at_risk,h$panel)
 predictions<-lapply(active,function(r)predict_behavioral_risk(r,model,t)$value)
 mode<-if(is.null(c$ccf))'direct_ead'else'ccf'
 ef<-fit_ead_model(z$prior_ead,matrix(numeric(),2,0),feature_order=character(),target_mode=mode,model_spec=z$ead_spec,target=z$ead_target,training=tr,model_id=paste0(p,'-ead'),model_version='1',currency='EUR')
 aid<-Filter(function(a)endsWith(a$account_id,'-main'),c$accounts)[[1]]$account_id
 drawn<-sum(vapply(Filter(function(r)r$account_id==aid&&r$date<='2024-02-15'&&r$known_at<='2024-02-15',c$transactions),function(r)(if(r$kind=='DRAW')1 else -1)*r$amount,0.));limit<-c$regime$original_limit
 ep<-predict_ead(ef,matrix(numeric(),1,0),feature_order=character(),expected_target=z$ead_target,reference_dates='2024-02-15',default_dates='2024-04-15',reference_drawn=if(mode=='ccf')drawn else NULL,reference_limit=if(mode=='ccf')limit else NULL,currency='EUR')$value
 observed<-if(mode=='ccf')realized_ccf(drawn,limit,600*c$scale,reference_date='2024-02-15',reference_known_at='2024-02-15',default_date='2024-04-15',default_known_at='2024-04-15',evaluation_cutoff='2024-08-31',currency='EUR')$value else NULL
 tr$workout_population<-'closed_defaulted_episodes'
 lf<-fit_fractional_lgd(matrix(numeric(),4,0),c(.2,.4,.6,.4),feature_order=character(),link='logit',covariance='HC0',target=z$lgd_target,training=tr,model_id=paste0(p,'-lgd'),model_version='1')
 lp<-predict_lgd(lf,matrix(numeric(),1,0),feature_order=character(),expected_target=z$lgd_target,default_condition='given_default')$value
 mt<-list(event='default',probability_measure='PHYSICAL',default_definition='first-default-before-exit',unit='facility',horizon_days=60,default_grade='D')
 forecasts<-Map(function(r,v)list(forecast_id=paste0(r$account_id,'@',r$as_of),case_id=paste0(r$account_id,'@',r$as_of),entity_id=r$account_id,cluster_id=r$customer_id,cohort=r$as_of,segment=p,model_id=paste0(p,'-behavioral'),model_version='1',forecast_at=r$as_of,known_at=r$known_at,pd=round(v$pd,8),target=mt,origin_grade='A',decision=NULL,override=FALSE,prediction_origin='OUT_OF_TIME'),active,predictions)
 events<-lapply(Filter(function(e)e$event_type%in%c('DEFAULT','CLOSED'),c$events),function(e)list(event_id=e$event_id,entity_id=e$account_id,event_at=e$occurred_at,known_at=e$known_at,event=if(e$event_type=='DEFAULT')'default'else'exit',default_definition=mt$default_definition))
 coverage<-lapply(c$coverage,function(r)list(entity_id=r$account_id,complete_through=r$observed_through,known_at=r$known_at))
 cohorts<-build_monitoring_cohorts(forecasts,events,coverage,target_contract=mt,evaluation_cutoff='2025-01-02')
 monitor<-monitor_rating_cohort(cohorts,group_by='cohort',reference_window=list(start='2024-01-01',end='2024-01-31'),minimum_count=5,uncertainty='none')$value
 list(model=model,ead_model=ef$model,lgd_model=lf$model,ead=ep,realized_ccf=observed,lgd=lp,predictions=predictions,monitor=monitor,cohorts=cohorts$value,reference_drawn=drawn,reference_limit=limit)
}
.rl_capacity<-function(c,p){
 lapply(c$applications,function(a){
 cid<-a$customer_id;sid<-paste0(cid,'-bureau');D<-'2024-01-01'
 bureau<-build_bureau_features(list(list(snapshot_id=sid,release_seq=1,customer_id=cid,provider_id='synthetic-bureau',observed_at='2023-12-30',reported_at='2023-12-31',known_at='2023-12-31',coverage_status='COMPLETE',currency='EUR')),list(list(snapshot_id=sid,snapshot_release_seq=1,tradeline_id=paste0(cid,'-old-loan'),opened_at='2021-01-01',closed_at=NULL,status='OPEN',balance=12*c$existing_debt_service,limit=12*c$existing_debt_service,max_dpd=0)),list(),list(mapping_id='synthetic-v1',version='1',known_at='2023-01-01',snapshots=structure(list(),names=character()),tradelines=structure(list(),names=character()),inquiries=structure(list(),names=character())),customer_id=cid,provider_id='synthetic-bureau',as_of=D,information_cutoff=D,lookback_days=30,max_age_days=30,currency='EUR')$value
 row<-list(evidence_id=paste0(cid,'-income'),release_seq=1,known_at='2023-12-31',customer_id=cid,component_id='income',component_type=if(c$kind=='OVERDRAFT')'NET_BUSINESS_INCOME'else'SALARY',observed_from='2023-12-01',observed_until='2023-12-30',amount=c$monthly_income,basis='NET',period='month',currency='EUR',evidence_reference='Separate synthetic net-income document; owner draw after business costs')
 policy<-list(policy_id=paste0(p,'-income'),version='1',known_at='2023-01-01',source_priority=list('DOCUMENT','TRANSACTION','DECLARED'),included_component_types=list(row$component_type),expected_component_ids=list('income'),coverage_status='COMPLETE',maximum_age_days=30,allow_stale=FALSE,relative_discrepancy_threshold=.05)
 declared<-row;declared$evidence_id<-paste0(cid,'-declared')
 income<-reconcile_income_evidence(list(declared),list(row),list(),policy,customer_id=cid,as_of=D,information_cutoff=D,currency='EUR',period='month',basis='NET')$value
 calendar<-list(monthly=12,annual=1,weekly=52)
 normal<-normalize_income_period(list(list(economic_id=paste0(cid,'-income'),amount=income$total_selected_income,frequency='monthly',basis='net',currency='EUR',kind='recurring')),target_period='monthly',target_basis='net',currency='EUR',payments_per_year=calendar,variable_income_factor=1)$value
 expenses<-estimate_recurring_expenses(list(list(economic_id=paste0(cid,'-living'),category='living',amount=c$monthly_expenses,frequency='monthly',currency='EUR',kind='essential')),target_period='monthly',currency='EUR',payments_per_year=calendar,household_profile=list(profile_id=paste0(cid,'-household'),required_categories=list('living')),benchmarks=list())$value
 due<-if(c$kind=='CARD')30 else if(c$kind%in%c('CHARGE','OVERDRAFT'))600 else 1000
 residual<-residual_income(normal$total_income,expenses$recurring_expenses,c$existing_debt_service,due,period='monthly',currency='EUR')$value
 dsti<-debt_service_to_income(c$existing_debt_service+due,normal$total_income,period='monthly',currency='EUR')$value
 profile<-list(schema_version='1.0',policy_id=paste0(p,'-capacity'),version='1',required_evidence=list('income_verified'),rules=list(list(rule_id='service',metric='dsti',operator='<=',threshold=.45),list(rule_id='residual',metric='residual',operator='>',threshold=0)))
 metrics<-list(dsti=dsti$ratio,residual=residual$residual)
 assessment<-assess_affordability(metrics,profile,list(income_verified=income$document_supported_current_income==normal$total_income))$value
 missing<-assess_affordability(metrics,profile,list(income_verified=FALSE))$value
 list(customer_id=cid,bureau=bureau,income=income,normalization=normal,expenses=expenses,residual=residual,dsti=dsti,assessment=assessment,missing_evidence=missing)
 })
}
.rl_replay<-function(c,p,o){
 aid<-Filter(function(a)endsWith(a$account_id,'-main'),c$accounts)[[1]]$account_id
 e0<-list(event_id=paste0(aid,'-opening-limit'),idempotency_key=paste0(aid,'-opening-limit'),occurred_at='2024-01-02',known_at='2024-01-02',sequence=0,payload=list(limit=c$regime$original_limit),revision_of=NULL)
 e1<-list(event_id=paste0(aid,'-revised-limit'),idempotency_key=paste0(aid,'-revised-limit'),occurred_at=c$regime$effective_from,known_at=c$regime$known_at,sequence=1,payload=list(limit=c$regime$revised_limit),revision_of=e0$event_id)
 e0$payload_hash<-content_fingerprint(e0$payload)$value;e1$payload_hash<-content_fingerprint(e1$payload)$value;events<-list(e0,e0,e1)
 content<-list(MODEL=o$model,CALIBRATION=o$calibrator,POLICY=o$policy,SOURCE=c$features)
 snapshots<-lapply(names(content),function(k)list(snapshot_id=paste0(p,'-',k),kind=k,content_hash=content_fingerprint(content[[k]])$value,known_at='2023-12-31',effective_from='2023-12-31',effective_until=NULL))
 execute<-function(){lapply(c('2024-02-16','2024-02-29'),function(cutoff){
  state<-NULL;seen<-character();applied<-list();limit<-NULL
  for(e in events){
   if(e$known_at>cutoff||e$occurred_at>cutoff)next
   event<-list(event_id=e$event_id,revision='1',occurred_at=e$occurred_at,known_at=e$known_at,dependencies=list(list(type='account',id=aid)),payload=e$payload)
   result<-apply_assessment_event(state,event,list(list(assessment_id=paste0(aid,'-assessment'),dependency_type='account',dependency_id=aid)),cutoff)$value;state<-result$state
   if(e$idempotency_key%in%seen){stopifnot(result$status=='DUPLICATE');next}
   seen<-c(seen,e$idempotency_key);applied[[length(applied)+1L]]<-e$event_id;limit<-e$payload$limit
  }
  risk<-predict_application_risk(o$snapshots[[2]],o$model,c$application_target,calibrator=o$calibrator)$value
  list(checkpoint_id=paste0(aid,'@',cutoff),as_of=cutoff,known_at=cutoff,event_log_hash=content_fingerprint(events)$value,snapshot_hashes=setNames(lapply(snapshots,function(s)s$content_hash),vapply(snapshots,function(s)s$snapshot_id,'')),applied_event_ids=applied,state=list(limit=limit,original_application_pd=risk$pd,assessment_event_state=state))
 })}
 original<-execute();replayed<-execute();replayed<-lapply(replayed,function(r){r$mode<-'REPLAY';r})
 contract<-list(contract_id=paste0(p,'-replay'),version='1',reference='Independent execution of original frozen model/calibrator and dated limit reducer',ordering='KNOWN_AT_SEQUENCE_EVENT_ID',idempotency='FIRST_IDENTICAL_DELIVERY',corrections='APPEND_REVISION',required_checkpoint_ids=lapply(original,function(r)r$checkpoint_id),absolute_tolerance=1e-10,relative_tolerance=0)
 args<-list(event_log=events,original_checkpoints=original,replayed_checkpoints=replayed,snapshot_manifest=snapshots,replay_contract=contract)
 validation<-do.call(replay_consistency,args)$value;stopifnot(validation$decision$status=='PASS')
 list(validation=validation,input=args,known_limits=lapply(original,function(r)r$state$limit))
}
.rl_governance<-function(c,p,o){
 model<-o$model;D<-'2024-01-01'
 registry<-register_rating_model_version(NULL,list(model=model,dependencies=structure(list(),names=character()),configuration=list(population=paste0(p,'-new-funded'))),list(model_id=model$model_id,model_version='1',registered_at='2023-12-31'))$value
 mh<-registry$entries[[1]]$model_hash;results<-list()
 for(i in seq_along(c$applications)){
  app<-c$applications[[i]];snap<-o$snapshots[[i]];risk<-o$risks[[i]];sid<-app$customer_id;aid<-app$application_id;subject<-list(subject_id=sid,subject_type='customer')
  subjects<-build_rating_subjects(list(customer=list(list(subject_id=sid,version='1',effective_from=D,effective_to=NULL,known_at=D,attributes=list(product=p)))),list(),D,D)$value
  rs<-c(subject,list(as_of=D,known_at=D,attributes=list(product=list(value=p,observed_at=D,known_at=D),recent_arrears=list(value=0,observed_at='2023-12-31',known_at='2023-12-31'))))
  route<-route_rating_profile(rs,list(list(rule_id=p,profile_id=paste0(p,'-application'),profile_version='1',priority=1,effective_from=D,effective_to=NULL,known_at='2023-12-31',conditions=list(list(field='product',operator='eq',value=p)))),D,D)$value
  applicability<-assess_rating_applicability(rs,list(contract_id=paste0(p,'-support'),version='1',required_features=list('product','recent_arrears'),allowed_values=list(product=list(p)),support_bounds=list(recent_arrears=list(0,1)),maximum_age_days=list(recent_arrears=31)),D,D)$value
  assignments<-list(list(assignment_id=paste0(p,'-champion'),profile_id=route$profile_id,purpose='application',model_id=model$model_id,model_version='1',model_hash=mh,effective_from=D,effective_to='2026-01-01',known_at='2023-12-31'))
  selected<-select_effective_model(registry,assignments,route$profile_id,'application',D,D)$value
  request<-list(row_id=aid,known_at=D,model_id=model$model_id,model_version='1',model_hash=mh,recipe=list(method='application_risk',version='1.0',parameters=list(target=c$application_target,calibrator=o$calibrator)),input=list(snapshot=snap))
  batch<-rate_subject_batch(list(request),registry,paste0(p,'-',aid),D)$value;stopifnot(batch$rows[[1]]$status=='OK')
  binding<-list(profile_id=route$profile_id,profile_version='1',model_id=model$model_id,model_version='1',model_hash=mh,input_hash=batch$rows[[1]]$input_hash)
  assessment<-compose_risk_assessment(paste0(aid,'-assessment'),subject,list(c(list(component_id=paste0(aid,'-pd'),quantity='PD',value=risk$pd),subject,list(variant='application_given_funding',definition=c$application_target))),binding,D,D,D)$value
  replay<-replay_assessment_history(list(request),registry,batch$rows,D)$value
  results[[i]]<-list(subjects=subjects,route=route,applicability=applicability,selected=selected,assessment=assessment,batch=batch,replay=replay)
 }
 list(registry=registry,rows=results)
}
.rl_phases<-function(c,p){
 aid<-Filter(function(a)endsWith(a$account_id,'-main'),c$accounts)[[1]]$account_id
 obs<-Filter(function(r)r$account_id==aid,c$obligations);payments<-Filter(function(r)r$account_id==aid,c$payments)
 arrears<-compute_days_past_due(obs,payments,as_of='2024-03-15',known_at='2024-03-15',currency='EUR')$value
 state<-assign_delinquency_state(arrears$dpd,arrears$past_due_amount,600*c$scale,list(closed=FALSE,confirmed_default=FALSE),c$state_profile)$value
 payment<-build_payment_history_features(obs,payments,list(status='COMPLETE',known_at='2024-03-15',observed_from='2024-02-01',complete_through='2024-03-15'),account_id=aid,as_of='2024-03-15',information_cutoff='2024-03-15',lookback_days=30,allocation_policy='fifo',currency='EUR',grace_days=0)$value
 a<-c$restructuring;a$discount_factors<-unlist(a$discount_factors)
 for(n in c('baseline','candidate'))for(k in c('payments','period_interest_rates','new_fees','servicing_costs','receipt_fractions'))a[[n]][[k]]<-unlist(a[[n]][[k]])
 a$affordability_paths<-lapply(a$affordability_paths,function(x){x$available_cash<-unlist(x$available_cash);x})
 arrangement<-do.call(simulate_repayment_arrangement,a)$value
 ah<-c$action_history;ah$required_features<-unlist(ah$required_features);action<-do.call(build_collections_action_history,ah)$value
 stopifnot(!arrangement$contract_changed,!arrangement$default_status_changed,!action$rows[[1]]$accepted,!action$rows[[1]]$performed,arrears$dpd==14,state$state=='ARREARS')
 revolving<-do.call(build_revolving_features,c$revolving_history)$value
 transactions<-if(c$kind=='OVERDRAFT')do.call(build_transaction_features,c$transaction_history)$value else NULL
 household<-NULL
 if(c$kind=='HELOC'){
  joint<-do.call(assemble_joint_application,c$household_joint)$value
  graph<-do.call(build_customer_household_graph,c$household_graph)
  aggregate<-do.call(aggregate_household_obligations,c(list(household_graph=graph),c$household_obligations))$value
  cltv<-do.call(combined_loan_to_value,c$cltv)$value
  stopifnot(joint$monthly_income==5000,joint$monthly_debt_service==1000,aggregate$households[[1]]$total_budget_debt_service==1000)
  av<-c$affordability_validation
  av$cases<-lapply(av$cases,function(r){r$assessment<-stress_affordability(r$baseline,r$scenario)$value;r})
  validation<-do.call(affordability_stress_backtest,av)$value;stopifnot(validation$decision$status=='PASS')
  household<-list(joint=joint,graph=graph$value,aggregate=aggregate,combined_ltv=cltv,affordability_validation=validation,affordability_inputs=av)
 }
 list(delinquency=arrears,delinquency_state=state,payment_history=payment,arrangement=arrangement,action_history=action,household=household,revolving_features=revolving,transaction_features=transactions)
}
.rl_verify_ledgers<-function(c){
 ids<-vapply(c$accounts,function(a)a$account_id,'');booked<-vapply(c$bookings,function(a)a$facility_id,'');if(!setequal(ids,booked))stop('Journey booking/account identities differ')
 for(row in c$statements){
  rows<-Filter(function(r)r$account_id==row$account_id&&r$date<=row$observed_at&&r$known_at<=row$known_at,c$transactions)
  balance<-sum(vapply(rows,function(r)(if(r$kind=='DRAW')1 else -1)*r$amount,0.))
  if(abs(balance-row$balance)>1e-10)stop('Statement does not reconcile principal ledger')
 }
 for(row in c$execution_inputs$vintage$account_outcomes){
  events<-Filter(function(e)e$account_id==row$account_id&&e$event_type%in%c('DEFAULT','CLOSED'),c$events)
  if(length(events))events<-events[order(vapply(events,function(e)e$occurred_at,''))]
  first<-if(length(events))events[[1]]else NULL;hit<-!is.null(first)&&first$occurred_at<=row$period_end
  if(row$cumulative_default!=as.integer(hit&&first$event_type=='DEFAULT')||row$exited!=(hit&&first$event_type=='CLOSED'))stop('Vintage labels must derive from same first event ledger')
 }
 for(w in c$execution_inputs$workout_claims){
  rows<-Filter(function(r)endsWith(r$account_id,'-main')&&r$kind%in%c('REPAYMENT','RECOVERY')&&r$date>=w$default_date&&r$date==w$cashflows[[1]]$cashflow_date,c$transactions)
  expected<-sum(vapply(rows,function(r)r$amount,0.));actual<-sum(vapply(Filter(function(f)f$kind=='RECOVERY',w$cashflows),function(f)f$amount,0.))
  if(abs(expected-actual)>1e-10)stop('Recovery label is not the actual episode receipt')
 }
 if(c$new_draw_contract$known_at>'2024-06-01'||c$new_draw_contract$effective_from!='2024-06-01')stop('Post-cure draw requires prior known historical amendment')
}
run_revolving_lifecycle<-function(data=NULL,verify_reference=TRUE,product_ids=NULL,return_artifacts=FALSE,accounts_path=NULL,releases_path=NULL,input_helper_path=NULL){
 resource<-function(n)system.file(paste0('extdata/',n),package='creditratingsystem')
 if(is.null(data))data<-jsonlite::fromJSON(resource('revolving_lifecycle.json'),simplifyVector=FALSE)
 if(is.null(accounts_path))accounts_path<-resource('revolving_accounts.csv');if(is.null(releases_path))releases_path<-resource('revolving_releases.csv')
 if(is.null(input_helper_path))input_helper_path<-system.file('examples/input_lineage.R',package='creditratingsystem')
 helper<-new.env(parent=environment());sys.source(input_helper_path,envir=helper)
 source<-list(original=helper$prepare_input_snapshot(accounts_path,releases_path,data$lineage,list(known_at='2024-02-15',revision='0')),revised=helper$prepare_input_snapshot(accounts_path,releases_path,data$lineage,list(known_at='2024-02-29',revision='1')))
 if(any(vapply(source,function(s)s$audit$decision$status!='PASS',TRUE)))stop('Historical local source ledger failed')
 output<-list();artifacts<-list()
 for(product in names(data$cases)){
  if(!is.null(product_ids)&&!product%in%product_ids)next
  c<-data$cases[[product]];.rl_verify_ledgers(c)
  o<-.rl_origin(c,product);h<-.rl_history(c,product,o);risk<-.rl_risks(c,product,h);w<-.rl_workout(c,product,risk);v<-.rl_vintage(c,product,h);r<-.rl_roll(c,product,h);pool<-.rl_pool(c,product,h,w);replay<-.rl_replay(c,product,o);g<-.rl_governance(c,product,o);ph<-.rl_phases(c,product);supplement<-.rl_diagnostics(c,product,h,risk,v,pool,file.path(dirname(input_helper_path),'retail_collections_review.R'))
  for(x in c$execution_inputs$roll$account_panel){actual<-Filter(function(y)y$account_id==x$account_id&&y$as_of==x$as_of,h$panel)[[1]];if(actual$state!=x$state)stop('Roll witness state differs from actual account panel')}
  aid<-Filter(function(a)endsWith(a$account_id,'-main'),c$accounts)[[1]]$account_id
  sr<-Filter(function(s)s$record_id==aid,source$original$rows)[[1]]
  if(sr$credit_limit!=risk$reference_limit)stop('EAD forecast used a future limit revision')
  dictionary<-setNames(lapply(c$application_policy$rules,function(r)tolower(gsub('_',' ',r$reason_code))),vapply(c$application_policy$rules,function(r)r$reason_code,''))
  reasons<-lapply(o$decisions,function(d)decompose_policy_decision(d,dictionary)$value)
  validation<-list(retail_cohort=h$cohort,vintage=v$validation,roll_rate=r$validation,collections=w$validation,retail_pool=pool$validation,replay=replay$validation)
  affordability_inputs<-NULL
  if(!is.null(ph$household)){
   w$lgd_validation_input<-.rl_heloc_lgd(c,product,risk,w);validation$lgd<-do.call(lgd_backtest,w$lgd_validation_input)$value
   validation$affordability<-ph$household$affordability_validation;ph$household$affordability_validation<-NULL
   affordability_inputs<-ph$household$affordability_inputs;ph$household$affordability_inputs<-NULL
   ph$household$affordability_metrics<-validation$affordability$metrics;ph$household$stress_projection<-affordability_inputs$cases[[1]]$assessment
  }
  object<-function(x)if(is.null(x))structure(list(),names=character())else x
  output[[product]]<-list(account_ids=lapply(c$accounts,function(a)a$account_id),application_ids=lapply(c$applications,function(a)a$application_id),application_pd=lapply(o$risks,function(x)round(x$pd,8)),decision_actions=lapply(o$decisions,function(d)d$action),application_targets=o$targets,capacity=o$capacities,panel=h$panel,behavioral_targets=h$labels,behavioral_pd=lapply(risk$predictions,function(x)round(x$pd,8)),ead_forecast=risk$ead$conditional_ead,ead_reference=list(drawn=risk$reference_drawn,limit=risk$reference_limit),observed_ccf=risk$realized_ccf,lgd_forecast=risk$lgd$mean_lgd,phase_details=ph,episodes=w$episodes,cure_targets=w$cure_targets,workout_losses=lapply(w$workouts,function(x)x$loss),vintage_table=v$table,vintage_curve=v$curve,roll_matrix=r$estimate$groups[[1]]$transition_matrix,roll_projection=r$projection,monitor=risk$monitor,validation_metrics=lapply(validation,function(z)object(z$metrics)),validation_coverage=lapply(validation,function(z)object(z$coverage)),cohort_contract_status=h$cohort$decision$status,replay_status=replay$validation$decision$status,known_limits=replay$known_limits,governance_replayed=lapply(g$rows,function(x)x$replay$status),phase_status=list(ORIGINATION='APPROVED_AND_FUNDED',PERFORMING='ACTIVE_ZERO_RETAINED',REVIEW_EARLY_WARNING='DATED_LIMIT_AND_RISK_REVIEW',DELINQUENCY='OBSERVED_ARREARS',RESTRUCTURING='OFFERED_NOT_ACCEPTED',DEFAULT_WORKOUT='TWO_DISTINCT_DEFAULT_CLAIMS',EXIT='ACTUAL_CLOSURE',PORTFOLIO_MONITORING='SIX_LINKED_VALIDATORS'))
  output[[product]]$linked_diagnostics<-supplement[c('lifetime_loss','path_loss','grade_calibration','grade_assignment','default_count','loss_assumption')]
  output[[product]]$linked_diagnostics$collections<-supplement$collections[setdiff(names(supplement$collections),'raw_artifacts')]
  output[[product]]$linked_diagnostics$survival_metrics<-supplement$survival_validation$metrics
  output[[product]]$linked_diagnostics$report_decision<-supplement$validation_report$decision
  artifacts[[product]]<-list(origin=o,behavioral_and_loss_models=risk,governance=g,validation=validation,replay=replay,workout=w,pools=pool,vintage=v,roll=r,decision_reasons=reasons,phase_details=ph,affordability_inputs=affordability_inputs,linked_diagnostics=supplement)
  if(verify_reference){stopifnot(h$cohort$decision$status=='PASS',replay$validation$decision$status=='PASS',all(vapply(g$rows,function(x)x$replay$status=='REPRODUCED',TRUE)),length(h$panel)==24,isTRUE(all.equal(lapply(o$targets,function(x)x$target),c$expected_application_targets,check.attributes=FALSE)),abs(w$workouts[[2]]$loss$economic_lgd-c$second_default$expected_lgd)<1e-12)}
 }
 result<-list(data_origin='SYNTHETIC',journey='J_REVOLVING_LIFECYCLE',products=output,source_lineage_status=lapply(source,function(v)v$audit$decision$status),prediction_reporting_decimal_places=8,raw_artifacts_available=TRUE,interpretation='Tiny synthetic holdout evidence, not bank acceptance. Cured first-default accounts remain outside the original risk set. New draw is a separately observed historical amendment. Forecast receipt timing is an explicit25-day assumption; proposals never alter contractual cash or defaults.')
 if(return_artifacts)result$raw_artifacts<-list(products=artifacts,source=source)
 result
}
.rl_diagnostics<-function(c,p,h,risk,vintage,pool,helper_path){
 helper<-new.env(parent=environment());sys.source(helper_path,envir=helper)
 chain<-c$collections_chain;aid<-chain$claim_binding$account_id
 actual<-Filter(function(e)e$account_id==aid&&e$event_type=='DEFAULT',c$events)[[1]]
 if(actual$occurred_at!=chain$open_workout$episode$default_date||chain$valuation_date>='2024-05-10')stop('Open recovery must bind actual first default before its known full repayment')
 collections<-helper$run_retail_collections_review(chain,verify_reference=FALSE,return_artifacts=TRUE)
 stopifnot(collections$cure_targets[[1]]$cure_at=='2024-05-20',collections$cure_targets[[1]]$redefault_at=='2024-06-20')
 residual<-600*c$scale;expected<-.25*(7/12*residual-5*c$scale)+.75*.2*residual-2*c$scale
 stopifnot(abs(collections$action_value$expected_net_recovery-expected)<1e-8)
 survival<-unlist(vintage$curve$survival);marginal<-diff(c(0,1-survival));lgd<-unlist(risk$lgd$mean_lgd)[1];conditional<-rep(lgd*c$regime$original_limit,8)
 loss<-lifetime_expected_loss(marginal,conditional,rep(1,8),(1:8)/12,currency='EUR',probability_measure='PHYSICAL')$value
 indicators<-matrix(0,9,8);for(i in 1:8)indicators[i,i]<-1
 paths<-expected_loss_from_paths(indicators,matrix(lgd,9,8),matrix(c$regime$original_limit,9,8),c(marginal,tail(survival,1)),rep(1,8),scenario_ids=paste0(p,'-first-event-bin-',0:8),time_grid=(1:8)/12,currency='EUR',probability_measure='PHYSICAL')$value
 stopifnot(abs(loss$lifetime_expected_loss-paths$expected_loss)<1e-8)
 grade_rows<-c$execution_inputs$pool_prior$rows
 counts<-vapply(c(TRUE,FALSE),function(low)sum(vapply(grade_rows,function(r)(r$score<50)==low,TRUE)),0.)
 defaults<-vapply(c(TRUE,FALSE),function(low)sum(vapply(grade_rows,function(r)r$defaulted&&(r$score<50)==low,TRUE)),0.)
 grade<-calibrate_grade_pd(counts,defaults,grades=c('LOW_UTILIZATION','HIGH_UTILIZATION'))$value
 active<-Filter(function(s)s$as_of%in%c('2024-01-31','2024-02-29')&&s$at_risk,h$panel);grade_assignment<-lapply(active,function(r){g<-grade$grades[[1+as.integer(r$features$utilization>=.5)]];list(account_id=r$account_id,as_of=r$as_of,grade=g$grade,pd=g$pd)});obs<-list();forecasts<-list();surv<-list()
 for(i in seq_along(active)){
  s<-active[[i]];pred<-risk$predictions[[i]];label<-Filter(function(l)l$account_id==s$account_id&&l$as_of==s$as_of,h$labels)[[1]];oid<-paste0(s$account_id,'@',s$as_of)
  events<-Filter(function(e)e$account_id==s$account_id&&s$as_of<e$occurred_at&&e$occurred_at<=label$horizon_end&&e$event_type%in%c('DEFAULT','CLOSED'),c$events);event<-if(length(events))events[[1]]else NULL
  obs[[i]]<-list(observation_id=oid,facility_id=s$account_id,as_of=s$as_of,performance_start=s$as_of,outcome_end=label$horizon_end,eligible=label$eligible,prior_default=FALSE,target=label$label,outcome_known_at='2025-01-02',default_definition='first-default-before-exit',cohort=s$as_of)
  forecasts[[i]]<-list(observation_id=oid,pd=round(pred$pd,8),forecast_at=s$as_of,known_at=s$as_of,default_definition='first-default-before-exit',horizon_days=60,unit='facility',probability_measure='PHYSICAL',model_id=paste0(p,'-behavioral'),model_version='1')
  hit<-!is.null(event)&&event$event_type=='DEFAULT';duration<-if(hit)as.numeric(as.Date(event$occurred_at)-as.Date(s$as_of))else 61
  surv[[i]]<-list(prediction_id=oid,entity_id=s$account_id,cluster_id=s$customer_id,segment=p,as_of=s$as_of,forecast_known_at=s$as_of,horizon_end=label$horizon_end,model_id=paste0(p,'-vintage'),model_version='1',parameter_version='1',outcome_known_at='2025-01-02',observed_time=duration,event=hit,terminal_exit_at=if(!is.null(event)&&!hit)event$occurred_at else NULL)
 }
 count<-default_count_backtest(obs,forecasts,'cohort',unit='facility',horizon_days=60,default_definition='first-default-before-exit',redefault_policy='exclude_previous_default',evaluation_cutoff='2025-01-02')$value
 target<-list(event='default',horizon=60,time_unit='days',probability_measure='PHYSICAL',conditioning='performing');d<-list(model_id=paste0(p,'-vintage'),model_version='1',parameter_version='1',data_fingerprint=paste0(p,'-actual-dated-ledger'),population_id=paste0(p,'-funded'),split_id=paste0(p,'-2024-holdout'),prediction_origin='HOLDOUT',evaluation_cutoff='2025-01-02',target=target,days_per_time_unit=1)
 sv<-survival_backtest(matrix(rep(1-survival[1:2],each=length(surv)),nrow=length(surv)),surv,horizons=c(29,60),censoring=list(method='complete_followup',reference='Complete first-default-before-terminal-exit target; observed terminal closure permanently ends facility target, not issuer followup'),design=d,uncertainty='none')$value
 manifests<-list(evaluation_started_at='2025-01-02',model_manifest=list(prior_vintage_model=content_fingerprint(vintage$model)$value),data_manifest=list(actual_account_events=content_fingerprint(c$events)$value),split_manifest=list(heldout_customers=content_fingerprint(as.list(sort(vapply(c$accounts,function(a)a$customer_id,''))))$value))
 policy<-list(policy_id=paste0(p,'-illustrative-validation'),version='1',reference='Tiny fixture arithmetic threshold, not bank acceptance',declared_at='2023-12-01',required_result_ids=list('survival'),criteria=list(list(criterion_id='brier',result_id='survival',path=list('metrics','integrated_brier'),operator='LE',threshold=.3,rationale='Predeclared illustrative fixture criterion')))
 keys<-c('model_manifest','data_manifest','split_manifest');entries<-list(list(result_id='survival',result=sv,reference='Same five actual overlapping facility windows; terminal first-exit definition',data_origin='SYNTHETIC',manifest_hashes=setNames(lapply(keys,function(k)content_fingerprint(manifests[[k]])$value),keys)))
 report<-validation_report(entries,acceptance_policy=policy,manifests=manifests)$value
 list(collections=collections,lifetime_loss=loss,path_loss=paths,grade_calibration=grade,grade_assignment=grade_assignment,default_count=count,survival_validation=sv,validation_report=report,report_inputs=list(results=entries,acceptance_policy=policy,manifests=manifests),loss_assumption='Full original authorized ceiling conditional exposure (charge authorization is not an undrawn commitment), LGD prior mean0.4, no future contract amendments and zero discount; first-event calendar buckets, not multiplying cumulative PD at each date')
}

.rl_heloc_lgd<-function(c,product,risk,workout){
 account<-Filter(function(a)grepl('-main$',a$account_id),c$accounts)[[1]]
 rows<-lapply(seq_along(workout$workouts),function(i){w<-workout$workouts[[i]];list(prediction_id=w$episode_id,entity_id=account$account_id,cluster_id=paste0(account$customer_id,'-household'),segment=product,as_of=w$default_date,forecast_known_at=w$default_date,horizon_end=as.character(as.Date(w$default_date)+60),model_id=paste0(product,'-lgd'),model_version='1',parameter_version='1',default_date=w$default_date,outcome_known_at='2024-08-31',workout_closed_at=if(i==1)'2024-05-20'else'2024-08-01',workout_status='CLOSED',predicted_lgd=unlist(risk$lgd$mean_lgd)[1],realized_lgd=w$loss$economic_lgd,ead_at_default=w$ead)})
 target<-list(event='default',horizon=60,time_unit='days',probability_measure='PHYSICAL',conditioning='default')
 design<-list(model_id=paste0(product,'-lgd'),model_version='1',parameter_version='1',data_fingerprint=paste0(product,'-actual-dated-ledger'),population_id=paste0(product,'-funded'),split_id=paste0(product,'-2024-holdout'),prediction_origin='HOLDOUT',evaluation_cutoff='2025-01-02',target=target)
 list(records=rows,design=design,workout_policy=list(method='closed_only',reference='Two actual same-borrower closed HELOC claims; prior frozen LGD, no independent-episode inference'),uncertainty='none')
}
