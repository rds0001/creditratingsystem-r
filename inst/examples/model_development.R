# Dated PD and workout development on one borrower/facility population.
md_upstream<-function(){
 path<-system.file('examples','retail_development.R',package='creditratingsystem');if(!nzchar(path))path<-'creditratingsystem-r/examples/retail_development.R';source(path)
}
md_canonical<-function(data,retail,prepared){
 applications<-rd_index(retail$applications,'application_id');booked<-rd_index(retail$bookings,'application_id');rows<-Filter(function(r)r$selected==1,prepared$rows)
 tables<-list(obligors=lapply(prepared$rows,function(r)list(entity_id=r$entity_id)),observations=lapply(prepared$rows,function(r)list(entity_id=r$entity_id,observed_at=r$as_of,available_at=r$features_known_at)),facilities=lapply(names(booked),function(key)list(facility_id=booked[[key]]$facility_id,entity_id=applications[[key]]$customer_id,ead=100)),defaults=lapply(data$workouts,function(r)list(episode_id=r$episode_id,entity_id=r$entity_id,default_at=r$default_date,closed_at=r$resolution_date)),recoveries=lapply(data$cashflows,function(f)list(cashflow_id=f$cashflow_id,episode_id=f$episode_id,cashflow_at=f$cashflow_date,amount=f$amount,kind=if(f$kind=='RECOVERY')'recovery'else'cost')))
 validation<-validate_credit_dataset(tables)$value;if(!validation$valid)stop('Canonical credit tables failed validation')
 ratings<-lapply(rows,function(r)list(observation_id=r$application_id,entity_id=r$entity_id,as_of=r$as_of,performance_start=booked[[r$application_id]]$first_draw_at,followup_end=booked[[r$application_id]]$followup_end,funded=TRUE))
 defaults<-lapply(data$workouts,function(r)list(event_id=r$episode_id,entity_id=r$entity_id,default_at=r$default_date,known_at=r$default_known_at))
 windows<-build_observation_windows(ratings,defaults,list(),30,data$evaluation_cutoff)$value
 stopifnot(identical(vapply(windows,function(w)as.numeric(w$target),numeric(1)),unlist(rd_outcomes(rows),use.names=FALSE)))
 list(tables=tables,validation=validation,windows=windows)
}
md_realized_loss<-function(data,row,cutoff){
 if(is.null(row$resolution_known_at)||row$resolution_known_at>cutoff)return(NULL)
 flows<-Filter(function(f)f$episode_id==row$episode_id&&f$known_at<=cutoff&&f$cashflow_date<=row$resolution_date,data$cashflows)
 net<-sum(vapply(flows,function(f)if(f$kind=='RECOVERY')f$amount else-f$amount,numeric(1)))
 value<-(row$ead_at_default-net-row$cure_value)/row$ead_at_default
 if(!is.null(row$realized_lgd)&&abs(value-row$realized_lgd)>1e-10)stop('Closed loss does not reconcile to cash and explicit cure asset value')
 value
}
md_loss_development<-function(data){
 train<-Filter(function(r)r$vintage%in%c(2016,2018)&&!is.null(r$resolution_known_at)&&r$resolution_known_at<=data$loss_training$known_at,data$workouts)
 y<-vapply(train,function(r)md_realized_loss(data,r,data$loss_training$known_at),numeric(1));x<-matrix(numeric(),length(train),0)
 fractional<-fit_fractional_lgd(x,y,feature_order=character(),target=data$loss_target,training=data$loss_training,model_version='1',link='logit',covariance='HC0',model_id='fractional-lgd')
 mixture<-fit_zero_one_inflated_lgd(x,y,feature_order=character(),target=data$loss_target,training=data$loss_training,model_version='1',component_spec=list(boundary_origin='structural',gate_link='logit'),model_id='mixture-lgd')
 cutoff<-data$residual_training_cutoff;landmarks<-Filter(function(r)r$vintage%in%c(2016,2018),data$workouts)
 landmarks<-lapply(landmarks,function(r){r$observation_end<-min(r$observation_end,cutoff);r})
 ids<-vapply(landmarks,function(r)r$episode_id,character(1));flows<-Filter(function(f)f$episode_id%in%ids,data$cashflows)
 training<-modifyList(data$loss_training,list(known_at=cutoff,split_id='residual-landmarks-before-2018-02-15',workout_population='open_and_closed_defaulted_landmarks'))
 hurdle<-fit_recovery_hurdle(landmarks,flows,matrix(numeric(),length(landmarks),0),feature_order=character(),stage_spec=data$stage_spec,target=data$residual_target,training=training,model_id='residual-hurdle',model_version='1',currency='EUR')
 test<-Filter(function(r)r$vintage==2022,data$workouts);predictions<-lapply(list(fractional=fractional,mixture=mixture),function(model)predict_lgd(model,matrix(numeric(),length(test),0),feature_order=character(),expected_target=data$loss_target,default_condition='given_default')$value)
 opened<-Filter(function(r)is.null(r$resolution_date),test);stopifnot(length(opened)==1);reference<-opened[[1]];past<-Filter(function(f)f$episode_id==reference$episode_id,data$cashflows)
 dates<-c('2022-01-15','2022-03-01','2022-04-01');valuation<-list(episode_id=reference$episode_id,discount_curve=list(anchor_date=reference$default_date,known_at='2022-02-01',discount_basis='supplied economic reference curve',points=lapply(seq_along(dates),function(i)list(cashflow_date=dates[i],discount_factor=c(1,.9,.8)[i]))),fx_contract=list(reporting_currency='EUR',known_at='2022-02-01',conversion_basis='cashflow_date',rates=lapply(dates,function(d)list(currency='EUR',date=d,rate=1))),future_resolution_dates=as.list(dates[-1]),resolution_time_convention='RIGHT_ENDPOINT',tail_discount_factor=.7,tail_assumption_reference='User flat discounted terminal payout beyond grid; not empirically identified',unmodeled_future_costs_assumption='NONE_BEYOND_SUPPLIED')
 value<-estimate_open_workout_lgd(opened,past,hurdle,matrix(numeric(),1,0),list(valuation),list(),feature_order=character(),expected_target=data$residual_target,stage_spec=data$stage_spec,as_of='2022-02-01',currency='EUR',extrapolation='parametric')$value
 no_tail<-valuation;no_tail['tail_discount_factor']<-list(NULL);no_tail['tail_assumption_reference']<-list(NULL)
 unresolved<-estimate_open_workout_lgd(opened,past,hurdle,matrix(numeric(),1,0),list(no_tail),list(),feature_order=character(),expected_target=data$residual_target,stage_spec=data$stage_spec,as_of='2022-02-01',currency='EUR',extrapolation='parametric')$value
 list(models=list(fractional=fractional$model,mixture=mixture$model,hurdle=hurdle$model),training_rows=train,hurdle_training_rows=landmarks,hurdle_training_cashflows=flows,test_rows=test,predictions=predictions,open_valuation=value,open_without_tail=unresolved,valuation_contract=valuation,open_inputs=list(episodes=opened,cashflows=past,as_of='2022-02-01'))
}
md_evaluate_losses<-function(data,loss){
 records<-lapply(seq_along(loss$test_rows),function(i){row<-loss$test_rows[[i]];opened<-is.null(row$resolution_date);list(prediction_id=row$episode_id,entity_id=row$entity_id,cluster_id=row$entity_id,segment='installment',as_of=row$default_date,forecast_known_at=row$default_date,horizon_end=as.character(as.Date(row$default_date)+180),model_id='mixture-lgd',model_version='1',parameter_version='1',default_date=row$default_date,outcome_known_at=if(opened)data$evaluation_cutoff else row$resolution_known_at,workout_closed_at=row$resolution_date,workout_status=if(opened)'OPEN'else'CLOSED',predicted_lgd=loss$predictions$mixture$mean_lgd[[i]],realized_lgd=md_realized_loss(data,row,data$evaluation_cutoff),ead_at_default=row$ead_at_default)})
 design<-list(model_id='mixture-lgd',model_version='1',parameter_version='1',data_fingerprint=rd_fingerprint(records),population_id='same-facility-default-workouts',split_id='untouched-2022-workouts',prediction_origin='HOLDOUT',evaluation_cutoff=data$evaluation_cutoff,currency='EUR',target=list(event='default',horizon=180,time_unit='days',probability_measure='PHYSICAL',conditioning='default'))
 result<-lgd_backtest(records,design=design,workout_policy=list(method='closed_only',reference='Actual closed ledger outcomes only; open terminal payout estimate is not truth'),uncertainty='none')$value
 list(records=records,design=design,result=result)
}
run_model_development<-function(data=NULL,retail_data=NULL,verify_reference=TRUE){
 load_data<-function(name){p<-system.file('extdata',name,package='creditratingsystem');if(!nzchar(p))p<-file.path('creditratingsystem-r/inst/extdata',name);jsonlite::fromJSON(p,simplifyVector=FALSE)}
 if(is.null(data))data<-load_data('model_development.json');if(is.null(retail_data))retail_data<-load_data(data$upstream_dataset)
 md_upstream();upstream<-run_retail_development(retail_data,verify_reference=verify_reference,include_holdout=FALSE);prepared<-upstream$prepared
 roles<-setNames(lapply(c('train','calibration','test'),function(role)Filter(function(r)r$role==role&&r$selected==1,prepared$rows)),c('train','calibration','test'))
 relational<-md_canonical(data,retail_data,prepared);target<-retail_data$target;training<-retail_data$training
 features<-function(rows)lapply(rows,function(r)list(x=r$snapshot$features$x))
 pre<-fit_preprocessor(features(roles$train),list(list(name='x',type='numeric',imputation='error',scaling='standard',missing_indicator=FALSE)),target=target,training=training,model_id='pd-preprocessor',model_version='1')
 transformed<-lapply(roles,function(rows){value<-transform_features(features(rows),pre)$value;matrix<-value$design_matrix;value$design_matrix<-lapply(seq_len(nrow(matrix)),function(i)as.list(unname(matrix[i,])));value})
 x<-lapply(transformed,function(value)do.call(rbind,lapply(value$design_matrix,unlist,use.names=FALSE)));y<-lapply(roles,function(rows)unlist(rd_outcomes(rows),use.names=FALSE))
 glm<-fit_binary_glm(x$train,y$train,feature_order='x',target=target,training=training,model_version='1',link='logit',fit_intercept=TRUE,model_id='glm')
 ridge<-fit_penalized_glm(x$train,y$train,feature_order='x',target=target,training=training,model_version='1',penalty='ridge',lambda_path=c(1,.1,0),selected_lambda=.1,scale=FALSE,model_id='ridge')
 firth<-fit_firth_logit(x$train,y$train,feature_order='x',target=target,training=training,model_version='1',model_id='firth-x')
 models<-list(glm=glm$model,ridge=ridge$model,'firth-x'=firth$model)
 pred<-lapply(models,function(model)as.list(unlist(predict_pd(model,x$test,feature_order='x',expected_target=target)$value)))
 cal<-unlist(predict_pd(glm,x$calibration,feature_order='x',expected_target=target)$value)
 isotonic<-calibrate_isotonic(cal,y$calibration,metadata=retail_data$calibration_metadata)$value
 counts<-vapply(c(-1,1),function(value)sum(vapply(roles$calibration,function(r)(r$snapshot$features$x<0)==(value==-1),logical(1))),numeric(1))
 bads<-vapply(c(-1,1),function(value)sum(vapply(Filter(function(r)(r$snapshot$features$x<0)==(value==-1),roles$calibration),function(r)r$outcome,numeric(1))),numeric(1))
 grade<-calibrate_grade_pd(counts,bads,grades=c('LOW','HIGH'),prior_alpha=0,prior_beta=0,monotonic=TRUE)$value
 pred$isotonic<-as.list(apply_calibrator(isotonic,unlist(pred$glm),target=target,base_model_version='1')$value);models$isotonic<-glm$model
 models$scorecard<-upstream$models$scorecard;pred$scorecard<-upstream$test_predictions$scorecard
 pred$grade<-lapply(roles$test,function(row)grade$grades[[if(row$snapshot$features$x<0)1L else 2L]]$pd)
 models$grade<-list(artifact_type='FrozenGradePDMapping',model_id='grade',model_version='1',method_id='calibration.calibrate_grade_pd',target=target,cutpoint=0,feature='x',lower_grade='LOW',upper_grade='HIGH',known_at=retail_data$calibration_metadata$calibration$known_at,calibration=grade)
 candidate_models<-setNames(lapply(names(models),function(name)list(estimator=models[[name]],preprocessor=if(name%in%c('scorecard','grade'))NULL else pre$model)),names(models))
 holdout<-rd_evaluate_holdout(retail_data,prepared,roles$test,pred,candidate_models,list(isotonic=isotonic))
 pd<-unlist(pred$glm);ids<-vapply(roles$test,function(r)r$application_id,character(1))
 design<-list(forecast_fingerprint=rd_fingerprint(as.list(pd)),prediction_origin='HOLDOUT',forecast_known_at='2021-12-31',evaluation_at=data$evaluation_cutoff,split_id='untouched-2022',target=target,observation_ids=as.list(ids))
 spiegelhalter<-spiegelhalter_test(pd,y$test,design=design,alternative='two-sided',independence_assumption='independent_fixed_bernoulli')
 null<-data$dependent_null;innovations<-list(draw_assumption='independent_standard_normal_rows',observation_ids=as.list(ids),factor_keys=list(list(cluster_id='shared-macro',time_id='2022')),factor_normals=null$factor_normals,idiosyncratic_normals=null$idiosyncratic_normals)
 dependent<-dependent_pd_backtest(pd,y$test,as.list(rep('shared-macro',length(pd))),as.list(rep('2022',length(pd))),design=design,null_model=list(family='gaussian_factor',asset_correlation=null$asset_correlation,factor_correlation=list(list(1)),parameter_uncertainty='conditional_fixed'),innovations=innovations,alternative='greater')
 loss<-md_loss_development(data);loss_validation<-md_evaluate_losses(data,loss)
 policy<-modifyList(data$predeclared_policy,list(required_result_ids=list('pd-heldout','lgd-heldout'),criteria=list(list(criterion_id='pd-brier',result_id='pd-heldout',path=list('metrics','by_model','glm','brier'),operator='LE',threshold=.1875001,rationale='Independent matched-cell Brier3/16'),list(criterion_id='closed-lgd-bias',result_id='lgd-heldout',path=list('metrics','overall','count_weighted','bias'),operator='BETWEEN',threshold=list(-.1,.1),rationale='Predeclared closed-only synthetic calibration check; excludes open outcome'))))
 manifests<-list(evaluation_started_at='2022-01-01',model_manifest=list(pd=rd_fingerprint(candidate_models),lgd=rd_fingerprint(loss$models)),data_manifest=list(applicants=rd_fingerprint(retail_data),workouts=rd_fingerprint(data)),split_manifest=list(assignments=rd_fingerprint(prepared$splits)))
 bindings<-lapply(manifests[c('model_manifest','data_manifest','split_manifest')],rd_fingerprint)
 results<-list(list(result_id='pd-heldout',result=holdout$benchmark,reference='Observed dated synthetic holdout ledger',data_origin='SYNTHETIC',manifest_hashes=bindings),list(result_id='lgd-heldout',result=loss_validation$result,reference='Observed dated synthetic holdout ledger',data_origin='SYNTHETIC',manifest_hashes=bindings))
 report<-validation_report(results,acceptance_policy=policy,manifests=manifests)$value
 upstream_summary<-list(component_id='RETAIL_APPLICATION_DEVELOPMENT',input_fingerprint=rd_fingerprint(retail_data),prepared=prepared,stepwise=upstream$stepwise,logistic_calibrator=upstream$logistic_calibrator,logistic_calibration_inputs=upstream$fit_inputs$logistic,selected_application_model=upstream$models$scorecard)
 output<-list(data_origin='SYNTHETIC',version=data$version,upstream=upstream_summary,relational=relational,preprocessor=pre$model,transformed=transformed,pd_models=models,candidate_models=candidate_models,isotonic_calibrator=isotonic,isotonic_inputs=list(values=as.list(cal),weights=as.list(rep(1,length(cal))),outcomes=as.list(y$calibration)),grade_calibration=grade,holdout=holdout,spiegelhalter=list(value=spiegelhalter$value,diagnostics=spiegelhalter$diagnostics),dependent=list(value=dependent$value,diagnostics=dependent$diagnostics,innovations=innovations),loss=loss,loss_validation=loss_validation,report=report)
 if(verify_reference)stopifnot(abs(holdout$benchmark$metrics$by_model$glm$brier-3/16)<1e-10,abs(loss$predictions$mixture$mean_lgd[[1]]-.5)<1e-7,is.null(loss$open_without_tail$episodes[[1]]$expected_final_lgd))
 output
}
