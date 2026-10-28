# Native corporate lifecycle: observed cohorts, conditional workouts, local replay.
if(!exists('fit_binary_glm'))library(creditratingsystem)
run_wholesale_corporate_lifecycle <- function(data=NULL,verify_reference=TRUE,artifact_paths=NULL) {
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/wholesale_corporate_lifecycle.json',package='creditratingsystem'),simplifyVector=FALSE)
 target<-data$target;tr<-data$development
 model<-fit_binary_glm(do.call(rbind,lapply(tr$X,unlist)),unlist(tr$y),weights=unlist(tr$weights),feature_order=unlist(tr$feature_order),link='logit',fit_intercept=TRUE,target=target,training=tr$training,model_id='corporate-lifecycle',model_version='1')
 prediction<-predict_pd(model,matrix(2,ncol=1),feature_order=unlist(tr$feature_order),expected_target=target)
 policy<-compile_credit_decision_policy(data$decision_policy);timing<-list(as_of='2024-01-01',known_at='2024-01-01')
 decision<-apply_credit_decision_policy(policy,risk=c(timing,list(pd=prediction$value[1],target=target,freshness='CURRENT')),affordability=c(timing,list(status=if(data$credit_gate_inputs$available_cashflow>=data$credit_gate_inputs$debt_service_due)'AFFORDABLE'else'NOT_AFFORDABLE')),eligibility=c(timing,list(status=if(data$credit_gate_inputs$legal_eligibility)'ELIGIBLE'else'INELIGIBLE')),operations=c(timing,list(status=data$credit_gate_inputs$operations_status)),as_of=timing$as_of,known_at=timing$known_at)$value
 governance<-.wholesale_corporate_governance(data,model)
 monitor<-do.call(build_monitoring_cohorts,data$monitoring)
 summary<-monitor_rating_cohort(monitor,group_by=c('cohort'),reference_window=list(start='2024-01-01',end='2024-12-31'),minimum_count=10,uncertainty='iid_homogeneous_binomial')$value
 args<-data$feature_drift;args$feature_order<-unlist(args$feature_order);args$permutations<-do.call(rbind,lapply(args$permutations,unlist));feature_drift<-do.call(detect_credit_feature_drift,args)$value
 args<-data$pd_drift;args$residuals<-list();drift_sources<-list()
 for(observed in data$drift_observed_cohorts){
  cohort<-do.call(build_monitoring_cohorts,observed);rows<-Filter(function(x)isTRUE(x$eligible),cohort$value$rows)
  residual<-mean(vapply(rows,function(x)x$target-x$pd,numeric(1)))
  args$residuals[[length(args$residuals)+1]]<-list(time=observed$evaluation_cutoff,value=residual,mature=TRUE)
  drift_sources[[length(drift_sources)+1]]<-list(evaluation_cutoff=observed$evaluation_cutoff,eligible_count=length(rows),defaults=sum(vapply(rows,function(x)x$target,numeric(1))),mean_pd=mean(vapply(rows,function(x)x$pd,numeric(1))),residual=residual)
 }
 args$null_paths<-do.call(rbind,lapply(args$null_paths,unlist));pd_drift<-do.call(detect_pd_calibration_drift,args)$value
 windows<-do.call(build_observation_windows,data$observation_windows)$value
 sm<-data$survival_metadata
 multi<-estimate_multistate_transitions(data$multistate_history,states=c('performing','default','cured'),allowed_transitions=list(c('performing','default'),c('default','cured'),c('cured','default')),initial_state='performing',target=sm$target,training=sm$training,model_id='corporate-multistate',model_version='1')
 multi_curves<-predict_event_curves(multi,list(),c(0,1,2,3),feature_order=character(),target=sm$target,event_states=c('default'))$value
 competing<-fit_cause_specific_hazards(data$cause_panel,matrix(numeric(),nrow=length(data$cause_panel),ncol=0),data$cause_events,feature_order=character(),causes=c('default','exit'),ties='breslow',target=sm$target,training=sm$training,model_id='corporate-competing',model_version='1')
 competing_curves<-predict_event_curves(competing,list(list(path_id='corporate-exit',start=0,stop=2,covariates=list(),stratum='all',covariate_time=0)),c(0,1,2),feature_order=character(),target=sm$target)$value
 treatment<-do.call(evaluate_treatment_candidates,data$treatment)$value
 args<-data$exposure;args$annual_interest_rates<-unlist(args$annual_interest_rates);args$period_end_dates<-unlist(args$period_end_dates);exposure<-do.call(exposure_schedule,args)$value
 episodes<-do.call(build_recovery_episodes,data$recovery_episodes)$value
 recovery<-do.call(discount_recoveries,data$recovery_value)
 closed<-workout_lgd(80,recovery,episode_status='CLOSED',default_date='2024-05-01',evaluation_cutoff='2024-09-03',currency='EUR')$value
 incomplete<-workout_lgd(80,recovery,episode_status='OPEN',default_date='2024-05-01',evaluation_cutoff='2024-09-03',currency='EUR')$value
 args<-data$workout_fit;args$X_train<-matrix(numeric(),nrow=length(args$episodes),ncol=0);args$feature_order<-character();workout_model<-do.call(fit_recovery_hurdle,args)
 args<-data$open_workout;args$X<-matrix(numeric(),nrow=length(args$episodes),ncol=0);args$feature_order<-character();args$fitted_recovery_model<-workout_model;open_workout<-do.call(estimate_open_workout_lgd,args)$value
 frozen_workout_replay<-list()
 for(language in c('python','r')){
  path<-if(is.null(artifact_paths))system.file(paste0('extdata/wholesale_corporate_workout_',language,'.json'),package='creditratingsystem')else artifact_paths[[language]]
  frozen<-load_fitted_model(path);args$fitted_recovery_model<-frozen
  frozen_workout_replay[[language]]<-list(model_hash=frozen$diagnostics$sha256,forecast=do.call(estimate_open_workout_lgd,args)$value)
 }
 args<-data$event_loss;args$event_names<-unlist(args$event_names);loss<-do.call(event_conditioned_loss,args)$value
 graph<-build_entity_graph(lapply(data$positions,function(x)list(entity_id=x$entity_id)),list(),data$graph_policy,as_of='2024-01-01',known_at='2024-01-01')
 portfolio<-prepare_credit_portfolio(data$positions,graph,data$default_link_policy,data$portfolio_contract)$value
 concentration<-credit_concentration_measures(data$positions,weight_field='ead',group_fields=c('sector'),weight_unit='EUR')$value
 panel<-lapply(windows,function(row){row$prior_default<-row$label_status=='prior_default';row$default_definition<-'first_default';row$outcome_known_at<-'2025-01-01';row$grade<-'B';row})
 forecasts<-lapply(panel,function(row)list(observation_id=row$observation_id,pd=.1,forecast_at='2024-01-01',known_at='2024-01-01',default_definition='first_default',horizon_days=365,unit='obligor',probability_measure='PHYSICAL',model_id='corporate-lifecycle',model_version='1'))
 counts<-default_count_backtest(panel,forecasts,c('grade'),unit='obligor',horizon_days=365,default_definition='first_default',redefault_policy='exclude_previous_default',evaluation_cutoff='2025-01-02')$value
 lgdtest<-lgd_backtest(data$loss_records,design=data$loss_backtest_design,workout_policy=list(method='closed_only',reference='Actual closed corporate settlements; open episodes excluded'),uncertainty='none')$value
 eadtest<-ead_backtest(data$loss_records,design=data$loss_backtest_design,weighting_policy=list(method='equal',reference='Each defaulted facility counted once'),uncertainty='none')$value
 if(verify_reference){
  stopifnot(decision$action=='APPROVE',summary[[1]]$metrics$n==20,abs(summary[[1]]$metrics$brier-.09)<1e-12,feature_drift[[1]]$statistic==3,max(abs(unlist(pd_drift$statistic_path)-c(.05,.125,.2125)))<1e-12,sum(vapply(windows,function(w)w$target,numeric(1)))==2)
  curve<-multi_curves$curves[[1]];stopifnot(max(abs(unlist(curve$first_hit_probability)-c(0,.25,.25,.25)))<1e-12)
  occupancy<-if(is.matrix(curve$state_occupancy))curve$state_occupancy else do.call(rbind,lapply(curve$state_occupancy,unlist));stopifnot(max(abs(occupancy[,2]-c(0,.25,0,.25)))<1e-12)
  expected<-1-exp(-3/8);stopifnot(abs(unlist(competing_curves$curves[[1]]$cumulative_incidence$default)[2]-expected/3)<1e-12,abs(unlist(competing_curves$curves[[1]]$cumulative_incidence$exit)[2]-expected*2/3)<1e-12)
  stopifnot(exposure$schedule[[1]]$conditional_ead==80,recovery$value$discounted_net_recoveries==56,closed$economic_lgd==.3,is.null(incomplete$economic_lgd),abs(open_workout$episodes[[1]]$expected_final_lgd-(1-(10+80*(1+exp(.125))/3)/100))<1e-7,abs(loss$expected_loss-3.4)<1e-12,abs(concentration$dimensions[[1]]$hhi-41/81)<1e-12,counts$overall$observed_count==20,counts$overall$observed_defaults==2,abs(lgdtest$metrics$overall$ead_weighted$bias-1/30)<1e-12,abs(eadtest$metrics$ead$overall$selected_weighting$bias+5)<1e-12)
 }
 list(product=data$product,data_origin='SYNTHETIC',target=target,credit_gate_inputs=data$credit_gate_inputs,drift_sources=drift_sources,origination_decision=decision$action,governance=governance,cohort_summary=summary,feature_drift=feature_drift,pd_drift=pd_drift,observation_windows=windows,multistate=multi_curves,competing_exit=competing_curves,treatment=treatment,exposure=exposure,recovery_episodes=episodes,discounted_recoveries=recovery$value,closed_workout=closed,incomplete_workout=incomplete,estimated_open_workout=open_workout,frozen_workout_replay=frozen_workout_replay,event_loss=loss,portfolio_position_count=length(portfolio$positions),concentration=concentration,default_backtest=counts,lgd_backtest=lgdtest,ead_backtest=eadtest)
}

.wholesale_corporate_governance <- function(data,model,return_audit_artifacts=FALSE){
 target<-data$target;sid<-data$subject_id;object<-setNames(list(),character())
 rule<-list(rule_id='corporate',profile_id='corporate',profile_version='1',priority=1,effective_from='2023-01-01',effective_to=NULL,known_at='2023-01-01',conditions=list(list(field='financial_statements',operator='eq',value=TRUE)))
 contract<-list(contract_id='corporate',version='1',required_features=list('financial_statements'),allowed_values=list(financial_statements=list(TRUE)),support_bounds=object,maximum_age_days=list(financial_statements=90))
 ref<-function(source,identity,...){setNames(list(list(source=source,id=identity,path=list(...))),'$ref')}
 recipe<-list(method='assessment_pipeline',version='2.0',parameters=list(routing_rules=list(rule),applicability_contract=contract,steps=list(list(step_id='pd',method_id='classification.predict_pd',arguments=list(fitted_model=ref('model','primary'),X=ref('input','features','value'),feature_order=data$development$feature_order,expected_target=target),argument_types=object,output_contract=list(shape='vector',unit='probability',target=target))),components=list(list(component_id='pd',quantity='PD',value=ref('step','pd','value',0),subject_id=sid,subject_type='obligor',variant='standalone',definition=target)),effects=list(),transitions=list(list(from_state='DRAFT',to_state='REVIEWED',event_type='review',required_gates=list('checked')),list(from_state='REVIEWED',to_state='FINAL',event_type='finalize',required_gates=list('approved')),list(from_state='FINAL',to_state='SUPERSEDED',event_type='exit',required_gates=list('settled'))),event_dependencies=list(list(assessment_id='corporate-assessment',dependency_type='subject',dependency_id=sid)),decision=NULL))
 registry<-register_rating_model_version(NULL,list(model=model$model,dependencies=object,configuration=list(assessment_recipe=recipe)),list(model_id='corporate-lifecycle',model_version='1',registered_at='2023-12-01'))$value
 assignment<-list(assignment_id='corporate-champion-1',profile_id='corporate',purpose='PD',model_id='corporate-lifecycle',model_version='1',model_hash=registry$entries[[1]]$model_hash,effective_from='2024-01-01',effective_to=NULL,known_at='2023-12-01')
 selection<-select_effective_model(registry,list(assignment),'corporate','PD','2024-01-01','2024-01-01')$value
 snapshot<-list(subject_id=sid,subject_type='obligor',as_of='2024-01-01',known_at='2024-01-01',attributes=list(financial_statements=list(value=TRUE,observed_at='2023-12-31',known_at='2024-01-01')))
 subject<-list(subject_id=sid,version='1',attributes=snapshot$attributes,effective_from='2024-01-01',effective_to=NULL,known_at='2024-01-01')
 subjects<-build_rating_subjects(list(obligor=list(subject)),list(),'2024-01-01','2024-01-01')$value
 request<-list(row_id='origination',known_at='2024-01-02',model_id='corporate-lifecycle',model_version='1',model_hash=assignment$model_hash,recipe=recipe,input=list(snapshot=snapshot,datasets=list(features=list(value=list(list(2)),as_of='2024-01-01',known_at='2024-01-01')),assessment_id='corporate-assessment',assessment_known_at='2024-01-01',calculated_at='2024-01-02',lifecycle_events=list(),events=list()))
 original<-rate_subject_batch(list(request),registry,'corporate-origination','2024-01-02')$value$rows[[1]]
 if(original$status!='OK')stop(paste('Corporate pipeline:',original$error_code,original$error_message))
 assessment<-original$value$assessment
 fp<-list(maximum_age_days=90,maximum_information_age_days=90,trigger_types=list('financial_deterioration'))
 freshness<-check_assessment_freshness(assessment,fp,list(),'2024-03-01','2024-03-01')$value
 trigger<-list(event_id='review-trigger',subject_id=sid,type='financial_deterioration',occurred_at='2024-04-01',known_at='2024-04-02')
 stale<-check_assessment_freshness(assessment,fp,list(trigger),'2024-04-02','2024-04-02')$value
 history<-request;history$row_id<-'closed-history';history$known_at<-'2024-09-03'
 history$input$lifecycle_events<-list(list(event_id='review',type='review',occurred_at='2024-01-03',known_at='2024-01-03',reason='Financial statements checked',gates=list(checked=TRUE)),list(event_id='approval',type='finalize',occurred_at='2024-01-04',known_at='2024-01-04',reason='Local approval',gates=list(approved=TRUE)),list(event_id='exit',type='exit',occurred_at='2024-09-03',known_at='2024-09-03',reason='Settled corporate claim',gates=list(settled=TRUE)))
 event<-list(event_id='credit-review',revision='1',occurred_at='2024-04-01',known_at='2024-04-02',dependencies=list(list(type='subject',id=sid)),payload=list(review_required=TRUE))
 default_event<-list(event_id='credit-default',revision='1',occurred_at='2024-05-01',known_at='2024-05-02',dependencies=list(list(type='subject',id=sid)),payload=list(default_observed=TRUE))
 delinquency<-list(event_id='credit-delinquency',revision='1',occurred_at='2024-04-15',known_at='2024-04-16',dependencies=list(list(type='subject',id=sid)),payload=list(due_date='2024-03-15',scheduled_amount=8,received_amount=0,days_past_due=31))
 proposal<-list(event_id='restructuring-proposal',revision='1',occurred_at='2024-04-20',known_at='2024-04-20',dependencies=list(list(type='subject',id=sid)),payload=list(treatment_id='extend',proposal_only=TRUE,contract_changed=FALSE))
 history$input$events<-list(event,event,delinquency,proposal,default_event)
 row<-rate_subject_batch(list(history),registry,'corporate-exit','2024-09-03')$value$rows[[1]]
 if(row$status!='OK')stop(paste('Corporate lifecycle:',row$error_code,row$error_message))
 replay<-replay_assessment_history(list(request,history),registry,list(original,row),'2025-01-02')$value
 tr<-data$development;later_training<-tr$training;later_training$known_at<-'2025-01-01';later_training$split_id<-'train_2023';later_training$data_fingerprint<-'later-observed-corporate-cohort'
 later_model<-fit_binary_glm(do.call(rbind,lapply(tr$X,unlist)),unlist(tr$y),weights=c(8,2,6,4),feature_order=unlist(tr$feature_order),link='logit',fit_intercept=TRUE,target=target,training=later_training,model_id='corporate-lifecycle',model_version='2')
 later_registry<-register_rating_model_version(registry,list(model=later_model$model,dependencies=object,configuration=list(assessment_recipe=recipe)),list(model_id='corporate-lifecycle',model_version='2',registered_at='2025-02-01'))$value
 later_replay<-replay_assessment_history(list(request,history),later_registry,list(original,row),'2025-03-01')$value
 stopifnot(later_replay$status=='REPRODUCED')
 tampered<-row;tampered$event_state_hash<-'changed';rejected<-replay_assessment_history(list(history),registry,list(tampered),'2025-01-02')$value
 statuses<-vapply(row$value$event_trace,function(x)x$status,character(1))
 stopifnot(replay$status=='REPRODUCED',rejected$status=='NOT_REPRODUCIBLE',row$value$assessment$state=='SUPERSEDED',identical(statuses,c('APPLIED','DUPLICATE','APPLIED','APPLIED','APPLIED')),freshness$status=='CURRENT',stale$status=='STALE')
 report<-list(subject_count=length(subjects$subjects),selection=selection$status,initial_state=assessment$state,final_state=row$value$assessment$state,current=freshness$status,review_required=stale$status,event_statuses=as.list(statuses),replay=replay$status,replay_after_new_champion=later_replay$status,tampered_replay=rejected$status,origination_pd=assessment$components[[1]]$value,historical_pd=row$value$assessment$components[[1]]$value)
 if(return_audit_artifacts)list(report=report,registry=registry,requests=list(request,history),results=list(original,row))else report
}
