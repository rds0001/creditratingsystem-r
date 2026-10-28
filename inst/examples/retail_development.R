# Chronological applicant development; pseudo outcomes never become test truth.
rd_fingerprint<-function(x)content_fingerprint(x)$value
rd_index<-function(rows,key)setNames(rows,vapply(rows,function(r)r[[key]],character(1)))
rd_matrix<-function(rows,features)unname(do.call(rbind,lapply(rows,function(r)vapply(features,function(f)as.numeric(r$snapshot$features[[f]]),numeric(1)))))
rd_outcomes<-function(rows)lapply(rows,function(r)r$outcome)
rd_selected<-function(rows)vapply(rows,function(r)r$selected,numeric(1))
prepare_retail_development<-function(data){
 snapshots<-build_application_snapshot(data$applications,data$features,required_features=c('x','instrument','cashflow_ratio'))$value
 labels<-build_application_targets(data$applications,data$decisions,data$bookings,data$defaults,data$exits,30,data$evaluation_cutoff)$value
 designs<-rd_index(data$cohort_design,'application_id');apps<-rd_index(data$applications,'application_id');decisions<-rd_index(data$decisions,'application_id');bookings<-rd_index(data$bookings,'application_id')
 sa<-sd<-sb<-so<-panel<-list()
 for(label in labels){
  key<-label$application_id;app<-apps[[key]];decision<-decisions[[key]];design<-designs[[key]]
  sa[[length(sa)+1L]]<-list(application_id=key,entity_id=app$customer_id,submitted_at=app$application_at)
  sd[[length(sd)+1L]]<-list(application_id=key,as_of=decision$decided_at,known_at=decision$known_at,decision=if(decision$status=='approved')'offered'else'rejected',policy_version='synthetic-fixed-policy')
  if(label$funded){
   booking<-bookings[[key]];sb[[length(sb)+1L]]<-list(application_id=key,as_of=booking$first_draw_at,known_at=booking$first_draw_known_at,taken_up=TRUE,funded=TRUE)
   selected<-label$label_observed&&label$mature
   so[[length(so)+1L]]<-list(application_id=key,as_of=if(selected)label$outcome_end else data$evaluation_cutoff,known_at=if(selected)paste0(design$vintage,'-02-03')else data$evaluation_cutoff,outcome_scope='own_contract',status=if(selected)'observed'else'immature',outcome=if(selected)label$target else NULL,target=data$target)
  }
  panel[[length(panel)+1L]]<-list(observation_id=key,entity_id=app$customer_id,group_id=design$group_id,observed_at=app$application_at,outcome_end=design$planned_window_end)
 }
 selection<-build_selection_dataset(sa,sd,sb,so,as_of=data$evaluation_cutoff,target_population='All dated applicants; unknown own-loan potential outcomes remain missing',target=data$target)$value
 split<-do.call(make_grouped_temporal_splits,c(list(panel=panel),data$split_contract))$value
 roles<-setNames(vapply(split$assignments,function(r)r$split,character(1)),vapply(split$assignments,function(r)r$observation_id,character(1)));snap<-rd_index(snapshots,'application_id');sel<-rd_index(selection$rows,'application_id')
 rows<-lapply(data$applications,function(app){key<-app$application_id;s<-sel[[key]];list(application_id=key,entity_id=app$customer_id,as_of=app$application_at,features_known_at=app$information_cutoff,role=unname(roles[[key]]),snapshot=snap[[key]],design=designs[[key]],selected=as.numeric(s$observed),outcome=s$outcome)})
 list(snapshots=snapshots,labels=labels,selection=selection,splits=split,rows=rows)
}
rd_pool_analysis<-function(data,train_obs,cal_obs,model){
 T<-list(event='default',horizon=30,time_unit='days',probability_measure='PHYSICAL',conditioning='not_defaulted_at_index',default_unit='facility',default_definition='DECLARED_FIRST_DEFAULT',currency='EUR')
 strata<-list(product='RETAIL_UNSECURED_INSTALLMENT',borrower_type='person',facility_type='installment',delinquency_state='performing');event_dates<-setNames(lapply(data$defaults,function(e)e$default_at),vapply(data$defaults,function(e)e$facility_id,character(1)))
 features<-unlist(model$model$feature_order)
 score<-function(rows)unlist(predict_pd(model,rd_matrix(rows,features),feature_order=features,expected_target=data$target)$value)
 assignment_row<-function(r,p)c(list(observation_id=r$application_id,index_date=paste0(r$design$vintage,'-01-03'),feature_observed_at=r$as_of,feature_known_at=r$as_of,score=p),strata)
 values<-score(train_obs);observations<-lapply(seq_along(train_obs),function(i){row<-train_obs[[i]];year<-row$design$vintage;key<-row$application_id;c(assignment_row(row,values[i]),list(default_unit_id=paste0('facility-',key),role='TRAIN',label_window_end=row$design$planned_window_end,label_known_at=paste0(year,'-02-03'),defaulted=as.logical(row$outcome),default_date=event_dates[[paste0('facility-',key)]],lgd_status=if(row$outcome)'OPEN'else'NOT_DEFAULTED',lgd=NULL,ead=NULL,lgd_known_at=NULL,ead_known_at=NULL))})
 spec<-list(spec_id='application-funded-pools',version='1',known_at='2015-12-31',stratum_fields=as.list(names(strata)),score_field='score',segments=list(list(segment_id='funded-installment',values=strata,cutpoints=list(.5),score_min=0,score_max=1,monotonicity=list(pd='increasing',lgd='none',ead='none'))),minimum_cases=2,minimum_defaults=0,minimum_lgd_count=0,minimum_ead_count=0,maximum_bin_mean_spread=list(pd=NULL,lgd=NULL,ead=NULL))
 training<-data$training;training$role<-'TRAIN';pool_model<-fit_retail_risk_pools(observations,spec,target=T,training=training,model_id='funded-pools',model_version='1')
 values<-score(cal_obs);cal_rows<-lapply(seq_along(cal_obs),function(i)assignment_row(cal_obs[[i]],values[i]))
 assigned<-assign_retail_risk_pool(cal_rows,pool_model,as_of='2020-01-03',information_cutoff='2020-01-03',expected_target=T,uncovered_policy='error')
 outcomes<-lapply(cal_obs,function(row)list(observation_id=row$application_id,default_unit_id=paste0('facility-',row$application_id),index_date='2020-01-03',label_window_end=row$design$planned_window_end,label_known_at='2020-02-03',outcome_status='MATURE',reference_exposure=100,reference_known_at='2020-01-03',defaulted=as.logical(row$outcome),default_date=event_dates[[paste0('facility-',row$application_id)]]))
 profile<-list(profile_id='heldout-funded',version='1',known_at='2019-12-31',pd_weighting='reference_exposure',lgd_weighting='ead',ead_weighting='episode',ccf_weighting='undrawn',confidence=.95,independence_assumption='independent_default_units',minimum_pool_cases=2)
 parameters<-estimate_pool_risk_parameters(assigned,outcomes,list(),profile,evaluation_cutoff='2020-02-03',calibration_id='pool-calibration',calibration_version='1')$value
 list(training_inputs=observations,model=pool_model$model,assignments=assigned$value,calibration_parameters=parameters)
}
rd_evaluate_holdout<-function(data,prepared,test_obs,predictions,models,calibrators){
 base<-function(name)models[[if(name=='calibrated')'scorecard'else if(name=='thin_calibrated')'firth'else name]]
 training_entities<-lapply(Filter(function(r)r$role%in%c('train','calibration'),prepared$rows),function(r)r$entity_id)
 candidates<-lapply(names(predictions),function(m)list(model_id=m,model_hash=rd_fingerprint(list(model=base(m),calibrator=calibrators[[m]])),training_known_at='2020-12-31',training_entities=training_entities))
 evaluation_rows<-lapply(test_obs,function(r)list(application_id=r$application_id,entity_id=r$entity_id,as_of=r$as_of,known_at='2022-02-03',label_origin='OBSERVED_OWN_CONTRACT',outcome=r$outcome,target=data$target,funded=TRUE,mature=TRUE))
 pseudo<-evaluation_rows[[1]];pseudo$application_id<-pseudo$entity_id<-'diagnostic-pseudo';pseudo$label_origin<-'PSEUDO';pseudo$outcome<-1
 evaluation<-prepare_reject_evaluation(candidates,c(evaluation_rows,list(pseudo)),as_of=data$evaluation_cutoff,target=data$target)$value
 ct<-list(event='default',probability_measure='PHYSICAL',default_definition='DECLARED_FIRST_DEFAULT',unit='facility',horizon_days=30,default_grade='D');entries<-list();defaults<-rd_index(data$defaults,'facility_id')
 for(name in names(predictions)){
  forecasts<-events<-coverage<-list();values<-unlist(predictions[[name]])
  for(i in seq_along(test_obs)){
   row<-test_obs[[i]];key<-row$application_id;facility<-paste0('facility-',key)
   forecasts[[i]]<-list(forecast_id=paste0(name,'-',key),case_id=key,entity_id=facility,cluster_id=row$entity_id,cohort='2022-01',segment='funded-installment',model_id=name,model_version='1',forecast_at='2022-01-03',known_at='2022-01-03',original_prediction_at=row$as_of,pd=values[i],target=ct,prediction_origin='HOLDOUT')
   e<-defaults[[facility]];if(!is.null(e))events[[length(events)+1L]]<-list(event_id=e$event_id,entity_id=facility,event='default',event_at=e$default_at,known_at=e$known_at,default_definition='DECLARED_FIRST_DEFAULT')
   coverage[[i]]<-list(entity_id=facility,complete_through=row$design$planned_window_end,known_at='2022-02-03')
  }
  cohort<-build_monitoring_cohorts(forecasts,events,coverage,target_contract=ct,evaluation_cutoff=data$evaluation_cutoff)$value
  entries[[name]]<-list(cohort=cohort,model_version='1',parameter_version='1',model_fingerprint=rd_fingerprint(list(model=base(name),calibrator=calibrators[[name]])),training_known_at='2020-12-31')
 }
 design<-list(model_id='scorecard',model_version='1',parameter_version='1',data_fingerprint=rd_fingerprint(evaluation_rows),population_id='observed-funded-test-only',split_id='untouched-2022',prediction_origin='HOLDOUT',evaluation_cutoff=data$evaluation_cutoff,target=list(event='default',horizon=30,time_unit='days',probability_measure='PHYSICAL',conditioning='performing'))
 benchmark<-benchmark_predictions(entries,reference_model_id='scorecard',metrics=c('brier','auc','log_loss'),matching_policy='strict',multiple_comparisons='none',design=design,uncertainty='none')$value
 manifests<-list(evaluation_started_at='2022-01-01',model_manifest=lapply(entries,function(v)v$model_fingerprint),data_manifest=list(test=rd_fingerprint(evaluation_rows)),split_manifest=list(assignments=rd_fingerprint(prepared$splits)))
 binding<-lapply(manifests[c('model_manifest','data_manifest','split_manifest')],rd_fingerprint)
 report<-validation_report(list(list(result_id='paired-holdout',result=benchmark,reference='Actual independently held-out synthetic funded outcomes; not rejects',data_origin='SYNTHETIC',manifest_hashes=binding)),acceptance_policy=data$predeclared_acceptance,manifests=manifests)$value
 list(evaluation_roles=evaluation,cohorts=entries,benchmark=benchmark,report=report)
}
rd_thin_analysis<-function(data,train_obs,cal_obs,test_obs){
 X<-function(rows){x<-rd_matrix(rows,'cashflow_ratio');colnames(x)<-'cashflow_ratio';x}
 firth<-fit_firth_logit(X(train_obs),unlist(rd_outcomes(train_obs)),feature_order='cashflow_ratio',target=data$target,training=data$training,model_id='firth',model_version='1')
 model<-firth$model;model$application_contract<-list(product_ids=list('RETAIL_UNSECURED_INSTALLMENT'),population_ids=list('synthetic_all_applicants'),performance_origin='first_draw_at',term_stage='requested',term_features=structure(list(),names=character()))
 cal_pd<-predict_pd(model,X(cal_obs),feature_order='cashflow_ratio',expected_target=data$target)$value
 groups<-vapply(cal_obs,function(r)r$snapshot$features$thin_file_stratum,character(1))
 hierarchical<-fit_hierarchical_calibration(unlist(cal_pd),unlist(rd_outcomes(cal_obs)),groups,metadata=data$calibration_metadata)$value
 test_pd<-predict_pd(model,X(test_obs),feature_order='cashflow_ratio',expected_target=data$target)$value
 test_cal<-apply_calibrator(hierarchical,unlist(test_pd),target=data$target,base_model_version='1',groups=vapply(test_obs,function(r)r$snapshot$features$thin_file_stratum,character(1)))$value
 cases<-list()
 for(name in names(data$thin_file_cases)){
  c<-data$thin_file_cases[[name]];bureau<-do.call(build_bureau_features,c$bureau)
  evidence<-build_thin_file_features(bureau,c$evidence,c$eligibility_profile,customer_id=c$customer_id,as_of='2022-02-28',information_cutoff='2022-02-28')$value
  transactions<-do.call(build_transaction_features,c$transactions)$value;income<-do.call(reconcile_income_evidence,c$income)$value;ratio<-income$total_selected_income/c$monthly_expense_budget
  app<-list(application_id=c$customer_id,customer_id=c$customer_id,product_id='RETAIL_UNSECURED_INSTALLMENT',population_id='synthetic_all_applicants',application_at='2022-03-01',information_cutoff='2022-03-01',requested_terms=list(amount=1000,currency='EUR',term_months=12))
  snapshot<-build_application_snapshot(list(app),list(list(application_id=c$customer_id,feature='cashflow_ratio',value=ratio,observed_at='2022-02-28',known_at='2022-02-28',source_id='reconciled-dated-income-budget')),required_features='cashflow_ratio')$value[[1]]
  support_snapshot<-list(subject_id=c$customer_id,subject_type='customer',as_of='2022-03-01',known_at='2022-03-01',attributes=list(cashflow_ratio=list(value=ratio,observed_at='2022-02-28',known_at='2022-02-28')))
  support<-assess_rating_applicability(support_snapshot,list(contract_id='firth-support',version='1',required_features=list('cashflow_ratio'),support_bounds=list(cashflow_ratio=list(.5,1.5)),allowed_values=structure(list(),names=character()),maximum_age_days=list(cashflow_ratio=30)),'2022-03-01','2022-03-01')$value
  risk<-calibrated<-NULL
  if(support$status=='APPLICABLE'){
   risk<-predict_application_risk(snapshot,model,data$target)$value
   calibrated<-apply_calibrator(hierarchical,risk$pd,target=data$target,base_model_version='1',groups=list(evidence$thin_file_stratum))$value
  }
  cases[[name]]<-list(bureau=bureau$value,evidence=evidence,transactions=transactions,income=income,cashflow_ratio=ratio,snapshot=snapshot,applicability=support,raw_application_risk=risk,calibrated_pd=if(is.null(calibrated))NULL else as.list(calibrated))
 }
 unknown<-apply_calibrator(hierarchical,.5,target=data$target,base_model_version='1',groups=list('NEW_GROUP'))$value
 list(calibration_inputs=list(p=as.list(cal_pd),w=as.list(rep(1,length(cal_obs))),y=rd_outcomes(cal_obs),groups=as.list(groups)),model=model,hierarchical_calibrator=hierarchical,cases=cases,test_raw=as.list(test_pd),test_calibrated=as.list(test_cal),unknown_group_reference=as.list(unknown))
}
run_retail_development<-function(data=NULL,verify_reference=TRUE,full_profiles=FALSE,include_holdout=TRUE){
 if(is.null(data)){path<-system.file('extdata','retail_development.json',package='creditratingsystem');if(!nzchar(path))path<-'creditratingsystem-r/inst/extdata/retail_development.json';data<-jsonlite::fromJSON(path,simplifyVector=FALSE)}
 prepared<-prepare_retail_development(data);rows<-prepared$rows;train<-Filter(function(r)r$role=='train',rows);cal<-Filter(function(r)r$role=='calibration',rows);test<-Filter(function(r)r$role=='test',rows)
 observed<-function(rr)Filter(function(r)r$selected==1,rr);train_obs<-observed(train);cal_obs<-observed(cal);test_obs<-observed(test)
 X<-rd_matrix;Y<-rd_outcomes;target<-data$target;training<-data$training
 stepwise<-fit_stepwise(X(train_obs,c('x','instrument')),unlist(Y(train_obs)),feature_order=c('x','instrument'),candidate_groups=list(signal=list('x'),selection_instrument=list('instrument')),direction='forward',criterion='AIC',estimator='GLM',training=training)$value
 chosen<-unlist(stepwise$selected_model$feature_order)
 fit_spec<-list(neutral_profile=as.list(rep(0,length(chosen))),feature_kinds=as.list(rep('expert',length(chosen))))
 scorecard<-fit_scorecard(X(train_obs,chosen),unlist(Y(train_obs)),feature_order=chosen,fit_spec=fit_spec,model_id='scorecard',target=target,training=training,model_version='1')
 gam<-fit_gam_pd(X(train_obs,'x'),unlist(Y(train_obs)),feature_order='x',linear_features=character(),smooth_specs=list(list(feature='x',boundary_knots=list(-1,1),interior_knots=list(0),degree=1,smoothing=.1,monotonicity='increasing',extrapolation='error')),variant='monotone',model_id='gam',target=target,training=training,model_version='1')
 cal_raw<-predict_pd(scorecard,X(cal_obs,chosen),feature_order=chosen,expected_target=target)$value
 calibrator<-calibrate_logistic(cal_raw,unlist(Y(cal_obs)),metadata=data$calibration_metadata)$value
 test_raw<-predict_pd(scorecard,X(test_obs,chosen),feature_order=chosen,expected_target=target)$value
 test_calibrated<-apply_calibrator(calibrator,test_raw,target=target,base_model_version='1')$value
 test_gam<-predict_pd(gam,X(test_obs,'x'),feature_order='x',expected_target=target)$value
 metadata<-lapply(train,function(r)list(entity_id=r$entity_id,as_of=r$as_of,features_known_at=r$features_known_at,label_known_at=paste0(r$design$vintage,'-02-03')))
 warm<-which(vapply(train,function(r)r$design$vintage==2016,logical(1)))-1L;forward<-which(vapply(train,function(r)r$design$vintage==2018,logical(1)))-1L;folds<-list(list(train=as.list(warm),validation=as.list(forward)))
 selection_target<-modifyList(target,list(event='funding',conditioning='eligible_applicants'))
 propensity<-fit_selection_propensity(X(train,'instrument'),rd_selected(train),feature_order='instrument',mechanism='funding',folds=folds,row_metadata=metadata,target=selection_target,training=training,model_id='propensity',model_version='1')
 fold_training<-modifyList(training,list(known_at='2016-02-03',data_fingerprint=rd_fingerprint(list(X=lapply(warm+1L,function(i)list(train[[i]]$snapshot$features$instrument)),selected=as.list(rd_selected(train[warm+1L]))))))
 fold_model<-fit_binary_glm(X(train[warm+1L],'instrument'),rd_selected(train[warm+1L]),feature_order='instrument',link='logit',fit_intercept=TRUE,target=selection_target,training=fold_training,model_id='propensity',model_version='1')$model
 stopifnot(rd_fingerprint(fold_model)==propensity$diagnostics$folds[[1]]$model_hash)
 p_all<-unlist(predict_pd(propensity,X(train,'instrument'),feature_order='instrument',expected_target=selection_target)$value)
 oof_p<-vapply(seq_along(train),function(i){v<-propensity$diagnostics$out_of_fold_propensity[[i]];if(is.null(v))p_all[i]else v},numeric(1))
 warm_obs<-Filter(function(r)r$design$vintage==2016,train_obs)
 warm_model<-fit_scorecard(X(warm_obs,chosen),unlist(Y(warm_obs)),feature_order=chosen,fit_spec=fit_spec,target=target,training=modifyList(training,list(known_at='2016-12-31')),model_id='warmup-scorecard',model_version='1')
 oof_m<-predict_pd(warm_model,X(train,chosen),feature_order=chosen,expected_target=target)$value;weights<-as.numeric((seq_along(train)-1L)%in%forward)
 ipw<-selection_ipw_weights(rd_selected(train),oof_p,target_weights=weights)$value
 aipw<-estimate_population_risk_aipw(Y(train),rd_selected(train),oof_m,oof_p,fold_lineage=list(rows=metadata,folds=folds,nuisance_prediction_origin='OUT_OF_FOLD'),target_weights=weights)$value
 fuzzy<-fuzzy_reject_augmentation(Y(train),rd_selected(train),oof_m,application_ids=vapply(train,function(r)r$application_id,character(1)))$value
 counts<-bads<-rejects<-numeric()
 for(x in c(-1,1)){band<-Filter(function(r)r$design$x==x,train);counts<-c(counts,sum(rd_selected(band)));bads<-c(bads,sum(unlist(Y(band))));rejects<-c(rejects,length(band)-tail(counts,1))}
 parcel<-parcel_rejected_applications(c('-1','1'),bads,counts,rejects,log_odds_shift=log(2))$value
 joint<-fit_joint_selection_model(X(train,'x'),X(train,'instrument'),rd_selected(train),Y(train),outcome_features='x',selection_features='instrument',model_id='joint',rho_grid=if(full_profiles)c(-.3,0,.3)else 0,selected_rho=0,target=target,training=training,model_version='1')
 bounds<-reject_inference_bounds(Y(train),rd_selected(train),target_weights=weights)$value
 sensitivity<-reject_inference_sensitivity(oof_m,rd_selected(train),Y(train),log_odds_shifts=c(-log(2),0,log(2)),target_weights=weights,threshold=.55)$value
 overlap_code<-tryCatch({selection_ipw_weights(c(1,0),c(1,0));NULL},error=function(e)e$code)
 models<-list(scorecard=scorecard$model,gam=gam$model,propensity=propensity$model,warmup=warm_model$model,joint=joint$model)
 thin<-rd_thin_analysis(data,train_obs,cal_obs,test_obs);thin$population_bounds<-list()
 for(group in c('BUREAU_NO_HIT','BUREAU_STALE')){group_rows<-Filter(function(r)r$snapshot$features$thin_file_stratum==group,train);thin$population_bounds[[group]]<-reject_inference_bounds(Y(group_rows),rd_selected(group_rows))$value}
 models$firth<-thin$model;pools<-rd_pool_analysis(data,train_obs,cal_obs,scorecard)
 holdout<-if(!include_holdout)NULL else rd_evaluate_holdout(data,prepared,test_obs,list(scorecard=as.list(test_raw),calibrated=as.list(test_calibrated),gam=as.list(test_gam),firth=thin$test_raw,thin_calibrated=thin$test_calibrated),models,list(calibrated=calibrator,thin_calibrated=thin$hierarchical_calibrator))
 output<-list(fit_inputs=list(logistic=list(values=as.list(cal_raw),weights=as.list(rep(1,length(cal_obs))),outcomes=Y(cal_obs)),propensity_fold_model=fold_model),thin_file=thin,pools=pools,holdout=holdout,data_origin=data$data_origin,version=data$version,product=data$product,prepared=prepared,stepwise=stepwise,models=models,logistic_calibrator=calibrator,test_predictions=list(scorecard=as.list(test_raw),calibrated=as.list(test_calibrated),gam=as.list(test_gam)),selection_analysis=list(ipw=ipw,aipw=aipw,fuzzy=fuzzy,parcel=parcel,joint_diagnostics=joint$diagnostics,bounds=bounds,sensitivity=sensitivity,overlap_failure=overlap_code))
 if(verify_reference){stopifnot(identical(chosen,'x'),max(abs(unlist(test_raw)-vapply(test_obs,function(r)if(r$design$x==-1).25 else .75,numeric(1))))<1e-7,abs(aipw$population_pd-.5)<1e-8,abs(ipw$effective_sample_size-38.4)<1e-7,abs(bounds$lower-.3125)<1e-12,abs(bounds$upper-.6875)<1e-12,identical(overlap_code,'POSITIVITY_FAILURE'))}
 output
}
