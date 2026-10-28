# Paired out-of-time comparison with explicit local role changes and pinned replay.
run_champion_challenger <- function(data=NULL,verify_reference=TRUE) {
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/champion_challenger.json',package='creditratingsystem'),simplifyVector=FALSE)
 registry<-NULL
 for(model in data$models)registry<-register_rating_model_version(registry,list(model=model,dependencies=list(source_reference='explicit analytic model'),configuration=list(purpose='paired out-of-time review')),list(model_id=model$model_id,model_version=model$model_version,registered_at='2024-01-01'))$value
 event<-function(version,identity,role,slot,effective){
  entry<-Filter(function(e)e$model_version==version,registry$entries)[[1]]
  list(event_id=identity,profile_id='paired-example',purpose='monitoring',role=role,slot_id=slot,model_id=entry$model_id,model_version=version,model_hash=entry$model_hash,effective_from=effective,known_at='2024-12-01',evaluation_reference='Explicit prior local assignment; current test outcomes unavailable',reason='Synthetic governance history',required_feature_order=list('x','z'))
 }
 history<-assign_model_role(NULL,registry,event('1','original','champion','champion','2024-12-01'))$value
 history<-assign_model_role(history,registry,event('2','shadow','challenger','candidate','2024-12-01'))$value
 history<-assign_model_role(history,registry,event('2','prior-promotion','champion','champion','2025-01-01'))$value
 request<-list(row_id='historical',known_at='2024-12-15',model_id=data$models[[1]]$model_id,model_version='1',model_hash=registry$entries[[1]]$model_hash,recipe=list(method='pd_model',version='1.0',parameters=list(expected_target=data$target)),input=list(X=list(list(1,1)),feature_order=list('x','z')))
 original<-rate_subject_batch(list(request),registry,'historic','2024-12-15')$value$rows
 cohorts<-panels<-predictions<-forecast_inputs<-list()
 for(model in data$models){
  version<-model$model_version;X<-do.call(rbind,lapply(data$cases,function(c)unlist(c$X)))
  p<-predict_pd(model,X,feature_order=c('x','z'),expected_target=data$target)$value;predictions[[version]]<-as.list(p)
  forecasts<-lapply(seq_along(data$cases),function(i){c<-data$cases[[i]];list(forecast_id=paste0(version,'-',c$case_id),case_id=c$case_id,entity_id=c$entity_id,cluster_id=c$cluster_id,cohort=c$cohort,segment='paired-example',model_id=model$model_id,model_version=version,forecast_at=c$forecast_at,known_at=c$known_at,pd=p[[i]],target=data$target,origin_grade='A',decision=NULL,override=FALSE,prediction_origin='OUT_OF_TIME')})
  forecast_inputs[[version]]<-forecasts
  cohorts[[version]]<-build_monitoring_cohorts(forecasts,data$events,data$coverage,target_contract=data$target,evaluation_cutoff=data$evaluation_cutoff)$value
  panels[[version]]<-monitor_rating_cohort(cohorts[[version]],group_by='cohort',reference_window=list(start='2024-01-01',end='2024-12-31'),minimum_count=4,uncertainty='independent_cluster_bootstrap',cluster_resamples=data$cluster_resamples)$value
 }
 comparison<-compare_challenger_cohorts(cohorts[['1']],cohorts[['2']],cluster_resamples=data$cluster_resamples,selection_protocol=data$selection_protocol)$value
 fairness<-diagnose_fairness_slices(cohorts[['2']],group_labels=lapply(data$cases,function(c)list(case_id=c$case_id,group=c$group)),reference_group='A',use_basis='Supplied synthetic groups for demonstration, no inferred protected traits',minimum_count=8,risk_threshold=.85,cluster_resamples=data$cluster_resamples)$value
 current<-lapply(data$cases[1:4],function(c)setNames(c$X,c('x','z')))
 drift<-detect_credit_feature_drift(data$reference_features,current,feature_order=c('x','z'),method='ks',permutations=do.call(rbind,lapply(data$drift_permutations,unlist)),multiple_testing='holm',dependence_assumption='exchangeable_rows')$value
 ordered<-panels[['2']][order(vapply(panels[['2']],function(r)r$group$cohort,character(1)))]
 times<-c('2025-02-15','2025-03-15','2025-04-15')
 residuals<-lapply(seq_along(ordered),function(i)list(time=times[[i]],value=ordered[[i]]$metrics$calibration_error,mature=TRUE))
 sequential<-detect_pd_calibration_drift(residuals,method='ewma',parameters=list(smoothing=.5),null_paths=do.call(rbind,lapply(data$null_paths,unlist)),null_spec=list(model_id=data$models[[2]]$model_id,model_version='2',calibration_id='prior-Bernoulli-reference',residual_scale='observed_minus_expected_rate',dependence='independent',probability_measure='PHYSICAL',calibrated_before='2024-12-01',validation_paths_independent=TRUE),alpha=.05,validation_null_paths=do.call(rbind,lapply(data$validation_null_paths,unlist)))$value
 metric_panel<-lapply(ordered,function(r)list(series_id='candidate',metric='absolute_calibration_gap',period=r$group$cohort,status='OK',value=abs(r$metrics$calibration_error)))
 findings<-classify_monitoring_findings(metric_panel,data$monitor_policy)$value
 model<-data$models[[2]];coef<-unlist(model$parameters$coefficients)
 contract<-list(model_id=model$model_id,model_version='2',feature_order=list('x','z'),output_scale='probability',deterministic_frozen=TRUE,target=data$target)
 linear_contract<-c(contract,list(coefficients=as.list(coef[-1]),intercept=coef[[1]],link='logit',transformed_feature_lineage=list(x=list('x'),z=list('z'))))
 linear<-explain_linear_credit_prediction(linear_contract,matrix(c(1,1),nrow=1),c(0,0),feature_groups=list(x='x',z='z'))$value
 predictor<-function(x)predict_pd(model,x,feature_order=c('x','z'),expected_target=data$target)$value
 shapley<-estimate_credit_shapley_values(predictor,c(1,1),list(values=list(list(0,0)),weights=list(1)),contract,feature_groups=list(x='x',z='z'),masking_contract=list(masking='INTERVENTIONAL_EMPIRICAL',reference_id='fixed-zero',reference_version='1'))$value
 rolled<-rollback_model_assignment(history,registry,'original',list(event_id='explicit-rollback',effective_from='2025-04-20',known_at='2025-04-16',reason='Explicit reviewer action after worse Brier and calibration; AUC alone insufficient',evaluation_reference='held-out-paired-comparison',required_feature_order=list('x','z')))$value
 days<-list(historical='2024-12-15',promoted='2025-02-01',rolled_back='2025-04-21')
 selections<-lapply(days,function(day)select_effective_model(registry,rolled,'paired-example','monitoring',day,day)$value)
 replay<-replay_assessment_history(list(request),registry,original,'2025-04-21')$value
 if(verify_reference){
  near<-function(a,b)max(abs(unlist(a)-unlist(b)))<1e-12
  stopifnot(near(comparison$champion_metrics$auc,.75),comparison$challenger_metrics$auc==1,near(comparison$champion_metrics$brier,.1875),near(comparison$challenger_metrics$brier,.325),near(comparison$differences$calibration_error,-.225),near(sequential$statistic_path,c(.175,.2625,.30625)),findings$status=='WARNING',replay$status=='REPRODUCED',identical(vapply(selections,function(s)s$selected$model_version,character(1),USE.NAMES=FALSE),c('1','2','1')),near(shapley$values,c(0,.9-6/7)))
 }
 list(forecast_inputs=forecast_inputs,predictions=predictions,cohorts=cohorts,panels=panels,comparison=comparison,fairness=fairness,feature_drift=drift,sequential=sequential,findings=findings,linear_explanation=linear,shapley=shapley,role_history=rolled,selections=selections,historical_request=request,historical_rows=original,replay=replay,interpretation='Synthetic prior-specified frozen models and untouched paired outcomes. Descriptive monitoring with explicit human review and local role events; no automatic deployment or empirical performance claim.')
}
