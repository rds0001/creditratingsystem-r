# Prior-only behavioral development with whole-customer overlapping-window validation.
if(!exists('fit_penalized_glm',mode='function'))library(creditratingsystem)
.bd_panel<-function(c)build_account_month_panel(c$accounts,c$statements,c$events,observation_dates=unlist(c$observation_dates),obligations=c$obligations,payments=c$payments)$value$accounts
.bd_labels<-function(panel,c,cutoff)build_behavioral_targets(panel,c$events,horizon_months=2,target_event='DEFAULT',competing_events='CLOSED',exit_treatment='competing',evaluation_cutoff=cutoff,coverage=c$coverage)$value
run_behavioral_development<-function(data=NULL,verify_reference=TRUE,measured_fit=identical(Sys.getenv('CREDITRATINGSYSTEM_EXTENDED_TESTS'),'true'),return_artifacts=FALSE){
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/behavioral_development.json',package='creditratingsystem'),simplifyVector=FALSE)
 d<-data;tr<-d$train;hold<-d$holdout;target<-d$target;groups<-list(sort(unique(vapply(tr$accounts,function(r)r$customer_id,''))),sort(unique(vapply(d$calibration,function(r)r$customer_id,''))),sort(unique(vapply(hold$accounts,function(r)r$customer_id,''))))
 if(length(intersect(groups[[1]],groups[[2]]))||length(intersect(groups[[1]],groups[[3]]))||length(intersect(groups[[2]],groups[[3]])))stop('Customer groups must be disjoint across training, calibration and holdout')
 if(!(d$training$known_at<min(vapply(d$calibration,function(r)r$index_date,''))&&d$pool_training$known_at<min(unlist(hold$observation_dates))))stop('Training and calibration must be known before later forecasts')
 trainpanel<-.bd_panel(tr);trainjan<-Filter(function(r)r$as_of=='2020-01-31',trainpanel);labels<-.bd_labels(trainjan,tr,d$training$known_at)
 if(!all(vapply(labels,function(r)r$calendar_mature&&r$eligible,TRUE)))stop('Training needs fully mature eligible outcomes')
 y<-vapply(labels,function(r)r$label,0)
 anchor<-fit_penalized_glm(matrix(numeric(),length(y),0),y,feature_order=character(),penalty='ridge',lambda_path=c(.1),selected_lambda=.1,scale=FALSE,tolerance=1e-12,target=target,training=d$training,model_id='behavioral-anchor',model_version='1')
 model<-anchor$model;measured<-NULL
 if(measured_fit){
  utilization<-vapply(trainjan,function(r)r$features$utilization,0)
  fitted<-fit_penalized_glm(matrix(utilization,ncol=1),y,feature_order='utilization',penalty='ridge',lambda_path=c(.1),selected_lambda=.1,scale=FALSE,tolerance=1e-12,target=target,training=d$training,model_id='behavioral-utilization',model_version='1')
  model<-fitted$model;measured<-list(coefficients=model$parameters$coefficients,training_utilization=as.list(utilization),training_labels=as.list(y))
 }
 model$behavioral_contract<-list(risk_unit='account',event_policy='first_default',product_ids=list('card'),allowed_states=list('ACTIVE','DEFAULT'),maximum_source_age_days=0)
 intervals<-list()
 for(row in trainjan){
  aid<-row$account_id;ev<-Filter(function(e)e$account_id==aid,tr$events);event<-if(length(ev))ev[[1]]$occurred_at else NULL;early<-!is.null(event)&&event<='2020-02-29'
  intervals[[length(intervals)+1]]<-list(entity_id=aid,start=0,stop=29,event=as.integer(early),period=0,interval_end_type='scheduled')
  if(!early)intervals[[length(intervals)+1]]<-list(entity_id=aid,start=29,stop=60,event=as.integer(!is.null(event)&&event<='2020-03-31'),period=1,interval_end_type='scheduled')
 }
 hazard<-fit_discrete_hazard(intervals,matrix(numeric(),length(intervals),0),feature_order=character(),period_grid=c(0,29,60),link='logit',partial_period_policy='exclude_censored',target=target,training=d$training,model_id='behavioral-discrete',model_version='1')
 curve<-predict_event_curves(hazard,list(list(path_id='account',start=0,stop=60,covariates=list(),covariate_time=0,stratum='all')),c(29,60),feature_order=character(),target=target)$value$curves[[1]]
 spec<-d$pooling_spec;spec$stratum_fields<-unlist(spec$stratum_fields);spec$segments[[1]]$cutpoints<-unlist(spec$segments[[1]]$cutpoints)
 pool<-fit_retail_risk_pools(d$calibration,spec,target=d$pool_target,training=d$pool_training,model_id='behavioral-pools',model_version='1')
 fields<-c('observation_id','index_date','feature_observed_at','feature_known_at','score',names(d$strata));calobs<-lapply(d$calibration,function(r)r[intersect(names(r),fields)])
 calassign<-assign_retail_risk_pool(calobs,pool,as_of=d$pool_training$known_at,information_cutoff=d$pool_training$known_at,expected_target=d$pool_target,uncovered_policy='error')
 outcomes<-lapply(d$calibration,function(r)list(observation_id=r$observation_id,default_unit_id=r$default_unit_id,index_date=r$index_date,label_window_end=r$label_window_end,label_known_at=r$label_known_at,outcome_status='MATURE',reference_exposure=100,reference_known_at=r$index_date,defaulted=r$defaulted,default_date=r$default_date))
 parameters<-estimate_pool_risk_parameters(calassign,outcomes,list(),d$estimation_profile,evaluation_cutoff=d$pool_training$known_at,calibration_id='behavioral-prior-calibration',calibration_version='1')$value
 grades<-c('A','B','C');counts<-vapply(grades,function(g)sum(vapply(d$calibration,function(r)r$grade==g,TRUE)),0);defaults<-vapply(grades,function(g)sum(vapply(d$calibration,function(r)r$grade==g&&r$defaulted,TRUE)),0)
 grade<-calibrate_grade_pd(counts,defaults,grades=grades)$value
 panel<-.bd_panel(hold);active<-Filter(function(r)r$as_of%in%c('2024-01-31','2024-02-29')&&r$at_risk,panel);predictions<-list();assignments<-list();history<-list()
 for(snap in active){
  predictions[[length(predictions)+1]]<-predict_behavioral_risk(snap,model,target)$value
  o<-c(list(observation_id=paste0(snap$account_id,'-',snap$as_of),index_date=snap$as_of,feature_observed_at=snap$as_of,feature_known_at=snap$as_of,score=100*snap$features$utilization),d$strata)
  assignments[[length(assignments)+1]]<-assign_retail_risk_pool(list(o),pool,as_of=snap$as_of,information_cutoff=snap$known_at,expected_target=d$pool_target,uncovered_policy='error')$value
 }
 for(snap in Filter(function(r)r$as_of=='2024-01-31',active)){
  aid<-snap$account_id;obs<-Filter(function(r)r$account_id==aid,hold$obligations);pays<-Filter(function(r)r$account_id==aid,hold$payments)
  dpd<-compute_days_past_due(obs,pays,as_of=snap$as_of,known_at=snap$known_at,currency='EUR')$value
  state<-assign_delinquency_state(dpd$dpd,dpd$past_due_amount,snap$features$balance,list(utp=FALSE),d$state_profile)$value
  features<-build_payment_history_features(obs,pays,list(status='COMPLETE',known_at=snap$as_of,observed_from='2024-01-01',complete_through=snap$as_of),account_id=aid,as_of=snap$as_of,information_cutoff=snap$known_at,lookback_days=30,allocation_policy='fifo',currency='EUR',grace_days=0)$value
  history[[length(history)+1]]<-list(account_id=aid,dpd=dpd,state=state,features=features)
 }
 poolpd<-setNames(lapply(parameters$pool_risk_table,function(r)r$pd$estimate),vapply(parameters$pool_risk_table,function(r)r$pool_id,''));gradepd<-setNames(lapply(grade$grades,function(r)r$pd),vapply(grade$grades,function(r)r$grade,''))
 risk_outputs<-lapply(seq_along(active),function(i){s<-active[[i]];g<-if(100*s$features$utilization<25)'A'else if(100*s$features$utilization<60)'B'else'C';list(account_id=s$account_id,as_of=s$as_of,model_pd=round(predictions[[i]]$pd,8),prior_pool_pd=poolpd[[assignments[[i]]$assignments[[1]]$pool_id]],utilization_grade=g,prior_grade_pd=gradepd[[g]],calibration_known_at=d$pool_training$known_at)})
 heldlabels<-.bd_labels(active,hold,'2024-05-01');earlylabels<-.bd_labels(active,hold,'2024-02-20');observations<-list();forecasts<-list();survrows<-list()
 for(i in seq_along(active)){
  snap<-active[[i]];label<-heldlabels[[i]];pred<-predictions[[i]];aid<-snap$account_id;oid<-paste0(aid,'-',snap$as_of)
  observations[[i]]<-list(observation_id=oid,facility_id=aid,as_of=snap$as_of,performance_start=snap$as_of,outcome_end=label$horizon_end,eligible=label$eligible,prior_default=FALSE,target=label$label,outcome_known_at='2024-05-01',default_definition=d$pool_target$default_definition,cohort=snap$as_of)
  forecasts[[i]]<-list(observation_id=oid,pd=round(pred$pd,8),forecast_at=snap$as_of,known_at=snap$as_of,default_definition=d$pool_target$default_definition,horizon_days=60,unit='facility',probability_measure='PHYSICAL',model_id=model$model_id,model_version='1')
  event<-Filter(function(e)e$account_id==aid&&snap$as_of<e$occurred_at&&e$occurred_at<=label$horizon_end,hold$events)
  survrows[[i]]<-list(prediction_id=oid,entity_id=aid,cluster_id=snap$customer_id,segment='card',as_of=snap$as_of,forecast_known_at=snap$as_of,horizon_end=label$horizon_end,model_id='behavioral-discrete',model_version='1',parameter_version='1',outcome_known_at='2024-05-01',observed_time=if(length(event))as.numeric(as.Date(event[[1]]$occurred_at)-as.Date(snap$as_of))else 61,event=length(event)>0)
 }
 count<-default_count_backtest(observations,forecasts,'cohort',unit='facility',horizon_days=60,default_definition=d$pool_target$default_definition,redefault_policy='exclude_previous_default',evaluation_cutoff='2024-05-01')$value
 cids<-groups[[3]];draws<-list(cids,rev(cids),rep(cids[1],10),rep(tail(cids,1),10))
 design<-list(model_id='behavioral-discrete',model_version='1',parameter_version='1',data_fingerprint='actual-two-overlapping-holdout-months',population_id='active-card-account-months',split_id='holdout-customers-2024',prediction_origin='HOLDOUT',evaluation_cutoff='2024-05-01',days_per_time_unit=1,target=target)
 cumulative<-round(1-unlist(curve$survival),12)
 survival<-survival_backtest(matrix(rep(cumulative,each=length(survrows)),nrow=length(survrows)),survrows,horizons=c(29,60),censoring=list(method='complete_followup',reference='Separate actual complete target windows; default remains absorbing'),design=design,uncertainty='cluster_bootstrap',cluster_resamples=draws)
 rolls<-estimate_retail_roll_rates(trainpanel,states=c('ACTIVE','DEFAULT'),from_date='2020-02-29',to_date='2020-03-31',group_by=character(),weighting='count',missing_successor='error')$value
 transition<-do.call(rbind,lapply(rolls$groups[[1]]$transition_matrix,unlist));projection<-project_retail_roll_rates(c(1,0),list(transition,transition),states=c('ACTIVE','DEFAULT'),default_states='DEFAULT',closure_states=character(),exposure_by_period=matrix(c(100,100,80,80),2,byrow=TRUE))$value
 vintage<-build_vintage_performance(hold$accounts,panel,hold$events,months_on_book=c(0,1,2,5),evaluation_cutoff='2024-05-01',coverage=hold$coverage,default_event='DEFAULT',delinquency_states=character(),exit_events='CLOSED')$value
 w<-d$open_workout;event<-Filter(function(e)e$account_id=='hold0',hold$events)[[1]];default_snapshot<-Filter(function(s)s$account_id=='hold0'&&s$as_of=='2024-02-29',panel)[[1]]
 if(w$episode$episode_id!='hold0-default'||w$episode$default_date!=event$occurred_at||default_snapshot$at_risk||!default_snapshot$ever_default)stop('Recovery must bind the same observed defaulted account and date')
 recovery<-estimate_open_workout_lgd(list(w$episode),w$cashflows,d$residual_model,matrix(numeric(),1,0),list(w$valuation),w$future_costs,feature_order=character(),expected_target=d$recovery_target,stage_spec=d$stage_spec,as_of='2024-03-29',currency='EUR',extrapolation='parametric')$value
 manifests<-list(evaluation_started_at='2024-05-01',model_manifest=list(discrete_training=content_fingerprint(list(rows=intervals,target=target,training=d$training))$value),data_manifest=list(holdout=content_fingerprint(hold)$value),split_manifest=list(customer_groups=content_fingerprint(lapply(groups,as.list))$value))
 policy<-list(policy_id='synthetic-behavioral-review',version='1',reference='Illustrative predeclared criterion; not bank acceptance',declared_at='2023-12-01',required_result_ids=list('survival'),criteria=list(list(criterion_id='brier',result_id='survival',path=list('metrics','integrated_brier'),operator='LE',threshold=.3,rationale='Illustrative test threshold only')))
 bindings<-lapply(manifests[c('model_manifest','data_manifest','split_manifest')],function(m)content_fingerprint(m)$value)
 report<-validation_report(list(list(result_id='survival',result=survival,reference='Actual grouped holdout Brier calculation',data_origin='SYNTHETIC',manifest_hashes=bindings)),acceptance_policy=policy,manifests=manifests)$value
 out<-list(data_origin=d$data_origin,split_groups=lapply(groups,as.list),training_labels=labels,anchor_pd=as.list(round(unlist(predict_pd(anchor,matrix(numeric(),1,0),feature_order=character(),expected_target=target)$value),8)),measured_fit=measured,discrete_curve=curve,pool_parameters=lapply(parameters$pool_risk_table,function(r)c(r[c('pool_id','mature_default_units','defaults','parameter_status','missing_default_workouts')],list(pd=r$pd[c('estimate','numerator','denominator','count')]))),grade_calibration=grade,risk_outputs=risk_outputs,predictions=lapply(predictions,function(p){p$pd<-round(p$pd,8);p$unadjusted_pd<-round(p$unadjusted_pd,8);p}),prediction_reporting_decimal_places=8,pool_assignments=assignments,payment_history=history,heldout_labels=heldlabels,immature_labels=earlylabels,default_count=count,survival_validation=survival$value,prior_roll_rates=rolls,roll_projection=projection,vintage=vintage,default_recovery=recovery,validation_report=report[c('decision','findings','metrics','acceptance_scope','manifests')],raw_artifacts_available=TRUE)
 if(verify_reference){stopifnot(max(abs(unlist(curve$survival)-c(.8,.6)))<1e-8,count$overall$observed_count==18,count$overall$observed_defaults==8,max(abs(vapply(survival$value$metrics$by_horizon,function(r)r$brier,0)-c(3.12/18,4.48/18)))<1e-10,all(transition==matrix(c(.75,.25,0,1),2,byrow=TRUE)),all(unlist(projection$periods[[2]]$state_probabilities)==c(.5625,.4375)),!any(vapply(earlylabels,function(r)r$calendar_mature,TRUE)),!any(vapply(active,function(r)r$ever_default,TRUE)),report$decision$status=='PASS')};if(return_artifacts)out$raw_artifacts<-list(behavioral_predictions=predictions,anchor_prediction=predict_pd(anchor,matrix(numeric(),1,0),feature_order=character(),expected_target=target)$value,validation_report=report,survival_validation=survival$value,pool_parameters=parameters,model=model,hazard_model=hazard$model,pool_model=pool$model,report_results=list(list(result_id='survival',result=survival$value,reference='Actual grouped holdout Brier calculation',data_origin='SYNTHETIC',manifest_hashes=bindings)),report_policy=policy);out
}
