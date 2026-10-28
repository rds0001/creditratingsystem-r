# Joint mortgage household and actual funded-account validation, native R.
if(!exists('validate_retail_cohort'))library(creditratingsystem)
run_mortgage_household_review<-function(data=NULL,verify_reference=TRUE,return_artifacts=FALSE){
 load<-function(name)jsonlite::fromJSON(system.file(paste0('extdata/',name,'.json'),package='creditratingsystem'),simplifyVector=FALSE)
 fp<-function(value)content_fingerprint(value)$value
 if(is.null(data))data<-load('mortgage_household_review')
 life<-load('mortgage_lifecycle')
 sys.source(system.file('examples/input_lineage.R',package='creditratingsystem'),envir=environment())
 lineage<-prepare_input_snapshot(system.file('extdata/mortgage_applications.csv',package='creditratingsystem'),system.file('extdata/mortgage_releases.csv',package='creditratingsystem'),data$lineage_config,data$lineage_config$cutoffs[[1]])
 stopifnot(lineage$audit$decision$status=='PASS')
 for(row in lineage$rows){ix<-which(vapply(life$features,function(f)f$application_id==row$entity_id,logical(1)));stopifnot(life$features[[ix]]$value==row$x);life$features[[ix]]$value<-row$x}
 sys.source(system.file('examples/retail_slice.R',package='creditratingsystem'),envir=environment())
 retail<-run_retail_slice(data=life,return_artifacts=TRUE)
 sys.source(system.file('examples/mortgage_lifecycle.R',package='creditratingsystem'),envir=environment())
 lifecycle<-run_mortgage_lifecycle(data=life)
 stopifnot(identical(lifecycle$application_and_monitoring,retail$summary))
 D<-'2024-02-01';subject<-list(subject_id='customer_A',subject_type='customer')
 subjects<-build_rating_subjects(list(customer=list(list(subject_id='customer_A',version='1',effective_from=D,effective_to=NULL,known_at=D,attributes=list(product_id='residential_mortgage')))),list(),D,D)$value
 snapshot<-life$route_snapshot;snapshot$attributes$recent_arrears<-list(value=retail$requests[[1]]$input$snapshot$features$recent_arrears,observed_at=D,known_at=D)
 contract<-list(contract_id='mortgage-support',version='1',required_features=list('product_id','recent_arrears'),allowed_values=list(product_id=list('residential_mortgage')),support_bounds=list(recent_arrears=list(0,1)),maximum_age_days=list(recent_arrears=31))
 applicability<-assess_rating_applicability(snapshot,contract,D,D)$value;bad<-snapshot;bad$attributes$product_id$value<-'unsupported_product';negative<-assess_rating_applicability(bad,contract,D,D)$value
 row<-retail$batch$rows[[1]];binding<-list(profile_id='residential-mortgage',profile_version='1',model_id=retail$model$model_id,model_version='1',model_hash=retail$requests[[1]]$model_hash,input_hash=row$input_hash)
 assessment<-compose_risk_assessment('mortgage-A',subject,list(c(list(component_id='application-PD',quantity='PD',value=row$value$pd),subject,list(variant='application_given_funding',definition=life$target))),binding,D,D,D)$value
 explanation<-decompose_policy_decision(retail$decision_artifacts[[1]],list(ELIGIBILITY='Declared eligibility failed',AFFORDABILITY='Verified household capacity failed',RISK_HIGH='PD exceeds risk threshold',RISK_LOW='PD within approval threshold'))$value
 governance<-list(subjects=subjects,applicability=applicability,negative_applicability=negative,assessment=assessment,policy_explanation=explanation)
 graph<-do.call(build_customer_household_graph,data$household$graph)
 household<-do.call(aggregate_household_obligations,c(data$household$household,list(household_graph=graph)))$value
 joint<-do.call(assemble_joint_application,data$joint_application)$value;collateral<-do.call(combined_loan_to_value,data$collateral)$value
 a<-data$affordability;stress<-stress_affordability(a$baseline,a$scenario)$value
 bindings<-setNames(lapply(a$economic_evidence,function(r)r$evidence_id),vapply(a$economic_evidence,function(r)r$economic_id,character(1)))
 groups<-c('incomes','expenses','contracts','collateral');expected<-setNames(lapply(groups,function(k)lapply(a$baseline[[k]],function(x)x$economic_id)),groups)
 case<-list(prediction_id='joint-mortgage-budget',entity_id='customer_A',cluster_id='household_A',segment='residential_mortgage',as_of='2024-02-01',forecast_known_at='2024-02-01',horizon_end='2024-03-01',model_id='contract-budget',model_version='1',parameter_version='1',household_id='household_A',baseline=a$baseline,scenario=a$scenario,assessment=stress,evidence_bindings=bindings,expected_economic_ids=expected,scenario_known_at='2024-02-01',profile_known_at='2024-02-01',profile_version='1')
 design<-list(model_id='contract-budget',model_version='1',parameter_version='1',data_fingerprint=fp(a),population_id='joint-household',split_id='mortgage-holdout',prediction_origin='HOLDOUT',evaluation_cutoff='2025-03-01',currency='EUR',target=list(event='default',horizon=1,time_unit='months',probability_measure='PHYSICAL',conditioning='household_budget'))
 affordability_inputs<-list(cases=list(case),economic_evidence=a$economic_evidence,allocations=a$allocations,design=design,payment_outcomes=list())
 affordability<-do.call(affordability_stress_backtest,affordability_inputs)$value
 profile<-list(profile_id='mortgage-first-default',version='1',reference='Original actual mortgage target and account histories',known_at='2023-12-31',default_definition='DECLARED_FIRST_DEFAULT',default_group_rule='facility-only',default_unit='facility',first_default=TRUE,exit_treatment='censor',default_event='DEFAULT',chargeoff_event='CHARGEOFF',cure_event='CURED',competing_events=list('CLOSED'),risk_set_by_product=list(residential_mortgage=list(active_states=list('ACTIVE'),include_zero_balance=TRUE)),split_isolation_keys=list('customer_id','household_id'),application_horizon_days=365,behavioral_horizon_months=12)
 splits<-lapply(life$applications,function(r)list(customer_id=r$customer_id,household_id=paste0('household_',r$application_id),role='test'))
 sources<-lapply(retail$requests,function(r)list(source_id=paste0('application-',r$row_id),prediction_key=paste0('application:',r$row_id),source_hash=fp(r$input$snapshot),kind='DOCUMENT',observed_at='2024-02-01',available_at='2024-02-01'))
 for(s in retail$account_panel)sources[[length(sources)+1L]]<-list(source_id=paste0('account-',s$as_of),prediction_key=paste0('account:',s$account_id,'@',s$as_of),source_hash=fp(s),kind='STATEMENT',observed_at=s$as_of,available_at=s$known_at)
 cohort_design<-design;cohort_design$model_id<-retail$behavioral_model$model_id;cohort_design$target<-life$behavioral$target;cohort_design$target$event<-'default'
 cohort_inputs<-list(application_targets=retail$application_targets,account_panel=retail$account_panel,behavioral_targets=retail$behavioral_targets,target_profile=profile,split_memberships=splits,source_lineage=sources,design=cohort_design)
 cohort<-do.call(validate_retail_cohort,cohort_inputs)$value
 pool<-data$pool;fit<-do.call(fit_retail_risk_pools,pool$fit);assigned<-do.call(assign_retail_risk_pool,c(list(fitted_model=fit),pool$assignment_inputs))
 arg<-pool$lgd_fit;arg$X_train<-matrix(numeric(),nrow=length(arg$X_train),ncol=0);arg$lgd_targets<-unlist(arg$lgd_targets);arg$feature_order<-character()
 lgdfit<-do.call(fit_fractional_lgd,arg);severity<-unlist(predict_lgd(lgdfit,matrix(numeric(),nrow=1,ncol=0),feature_order=character(),expected_target=pool$lgd_fit$target,default_condition='given_default')$value$mean_lgd)[1]
 pid<-assigned$value$assignments[[1]]$pool_id;statistics<-NULL
 for(s in fit$model$parameters$segments)for(p in s$pools)if(p$pool_id==pid)statistics<-p$statistics
 pd<-statistics$means$pd;d<-pool$design;outcome<-pool$cohort[[1]]
 forecast<-list(prediction_id='mortgage-at-funding',observation_id='facility_A',entity_id='facility_A',cluster_id='household_A',segment='residential_mortgage',as_of=outcome$index_date,forecast_known_at=outcome$index_date,horizon_end=outcome$label_window_end,model_id=d$model_id,model_version='1',parameter_version='1',pd=pd,lgd=severity,ead=pool$forecast_ead,reference_exposure=100000,excluded_from_training=TRUE,excluded_from_calibration=TRUE)
 pool_inputs<-list(fitted_model=fit$model,pool_assignments=assigned$value,assignment_inputs=pool$assignment_inputs,predictions=list(forecast),cohort_outcomes=pool$cohort,workouts=pool$workouts,links=pool$links,estimation_profile=pool$estimation_profile,design=d)
 diagnostics<-do.call(retail_pool_diagnostics,pool_inputs)$value
 discounted<-do.call(discount_recoveries,life$realized_recovery)
 closed<-workout_lgd(life$default_ead,discounted,episode_status='CLOSED',default_date=life$realized_recovery$default_date,evaluation_cutoff=life$realized_recovery$evaluation_cutoff,currency='EUR')$value
 loss_design<-d;loss_design$target$conditioning<-'default'
 record<-c(forecast,list(predicted_lgd=severity,predicted_ead=pool$forecast_ead,realized_lgd=closed$economic_lgd,realized_ead=95000,realized_ead_date='2024-09-01',default_date='2024-09-01',default_known_at='2024-09-02',defaulted=TRUE,workout_status='CLOSED',workout_closed_at='2025-09-15',outcome_known_at='2025-09-30',ead_at_default=95000,currency='EUR',drawn_at_reference=100000,limit_at_reference=100000))
 lgd_inputs<-list(records=list(record),design=loss_design,workout_policy=list(method='closed_only',reference='Observed property sale and closed actual mortgage workout'),uncertainty='none')
 lgd<-do.call(lgd_backtest,lgd_inputs)$value
 ead<-ead_backtest(list(record),design=loss_design,weighting_policy=list(method='equal',reference='One actual observed defaulted facility'),uncertainty='none')$value
 phases<-c('APPLICATION','FUNDING','DEFAULT','SETTLEMENT');dates<-c('2024-02-01','2024-02-03','2024-09-02','2025-09-30');payloads<-list(retail$requests,life$booked,life$defaults,life$realized_recovery)
 log<-lapply(seq_along(phases),function(i){payload<-list(records=payloads[[i]]);list(event_id=phases[i],idempotency_key=phases[i],occurred_at=dates[i],known_at=dates[i],sequence=i-1L,payload=payload,payload_hash=fp(payload),revision_of=NULL)})
 kinds<-c('MODEL','CALIBRATION','POLICY','SOURCE');values<-list(retail$model,retail$calibrator,retail$policy,life$features);known<-c('2023-12-31','2023-12-31','2023-12-31','2024-02-01')
 snapshots<-lapply(seq_along(kinds),function(i)list(snapshot_id=kinds[i],kind=kinds[i],content_hash=fp(values[[i]]),known_at=known[i],effective_from=known[i],effective_until=NULL))
 base<-list(checkpoint_id='final',as_of='2025-09-30',known_at='2025-09-30',event_log_hash=fp(log),snapshot_hashes=setNames(lapply(snapshots,function(s)s$content_hash),kinds),applied_event_ids=as.list(phases))
 event_state<-function(){
  state<-NULL
  for(event in log){change<-list(event_id=event$event_id,revision='1',occurred_at=event$occurred_at,known_at=event$known_at,dependencies=list(list(type='facility',id='facility_A')),payload=event$payload);state<-apply_assessment_event(state,change,list(list(assessment_id='mortgage-A',dependency_type='facility',dependency_id='facility_A')),'2025-09-30')$value$state}
  state
 }
 original<-c(base,list(state=list(ratings=retail$batch$rows,dependency_state=event_state())))
 rerated<-rate_subject_batch(retail$requests,retail$registry,'synthetic-applications-2024','2024-02-01')$value
 replayed<-c(base,list(state=list(ratings=rerated$rows,dependency_state=event_state()),mode='REPLAY'))
 replay_inputs<-list(event_log=log,original_checkpoints=list(original),replayed_checkpoints=list(replayed),snapshot_manifest=snapshots,replay_contract=list(contract_id='mortgage-historical-execution',version='1',reference='Actual rerating of original requests against retained model and calibrator',ordering='KNOWN_AT_SEQUENCE_EVENT_ID',idempotency='FIRST_IDENTICAL_DELIVERY',corrections='APPEND_REVISION',required_checkpoint_ids=list('final'),absolute_tolerance=1e-12,relative_tolerance=0))
 replay<-do.call(replay_consistency,replay_inputs)$value
 raw<-list(joint_application=joint,collateral=collateral,lineage=lineage,governance=governance,lifecycle=lifecycle,retail=retail,household=household,stress=stress,affordability_inputs=affordability_inputs,affordability=affordability,cohort_inputs=cohort_inputs,cohort=cohort,pool_inputs=pool_inputs,pool=diagnostics,lgd_model=lgdfit$model,closed_workout=closed,lgd_inputs=lgd_inputs,lgd=lgd,ead=ead,replay_inputs=replay_inputs,replay=replay)
 if(verify_reference){
  o<-data$oracles;p<-stress$periods[[1]];stopifnot(abs(p$net_income-o$stressed_income)<1e-8,abs(p$expenses-o$stressed_expenses)<1e-8,abs(p$debt_service-o$debt_service)<1e-8,abs(p$residual-o$residual)<1e-8)
  stopifnot(household$households[[1]]$total_budget_debt_service==900,cohort$decision$status=='PASS',affordability$decision$status!='BLOCKED',abs(diagnostics$metrics$overall_count$brier-o$pool_brier)<1e-12,abs(closed$economic_lgd-o$lgd)<1e-12,abs(ead$metrics$ead$overall$selected_weighting$bias-o$ead_bias)<1e-8,replay$decision$status=='PASS')
 }
 if(return_artifacts)return(raw)
 p<-raw$stress$periods[[1]];fields<-c('net_income','expenses','debt_service','residual')
 list(data_origin='SYNTHETIC',product='residential_mortgage',facility_id='facility_A',
  joint_application=raw$joint_application,collateral=raw$collateral,lineage=raw$lineage,governance=list(applicability=raw$governance$applicability,negative_applicability=raw$governance$negative_applicability,policy_action=raw$governance$policy_explanation$action),application=raw$retail$summary,household=raw$household,lifecycle=raw$lifecycle,
  stressed_budget=setNames(lapply(fields,function(k)round(p[[k]],2)),fields),fixed_rate_reset=lapply(p$contracts,function(c)c$reset),
  affordability_status=raw$affordability$decision$status,missing_observed_payment_periods=raw$affordability$missing_payment_periods,
  cohort_status=raw$cohort$decision$status,cohort_metrics=raw$cohort$metrics,pool_coverage=raw$pool$coverage,pool_brier=raw$pool$metrics$overall_count$brier,
  closed_workout=raw$closed_workout,lgd_bias=round(raw$lgd$metrics$overall$ead_weighted$bias,8),ead_bias=raw$ead$metrics$ead$overall$selected_weighting$bias,
  replay_status=raw$replay$decision$status,presentation_precision='Monetary budget2decimals; LGD error8decimals; raw artifacts retained through return_artifacts',
  scope='One actual synthetic funded mortgage; missing payments and nonfunded application outcomes remain unobserved')
}
