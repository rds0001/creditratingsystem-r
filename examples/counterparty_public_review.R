# Same actual counterparty claim; explicit absence of external benchmarks.
if(!exists('benchmark_ratings'))library(creditratingsystem)
run_counterparty_public_review<-function(data=NULL,verify_reference=TRUE,return_artifacts=FALSE,accounts_path=NULL,releases_path=NULL,helper_path=NULL){
 load_data<-function(n)jsonlite::fromJSON(system.file(paste0('extdata/',n,'.json'),package='creditratingsystem'),simplifyVector=FALSE)
 if(is.null(data))data<-load_data('counterparty_public_review')
 if(is.null(helper_path))helper_path<-system.file('examples',package='creditratingsystem')
 for(n in c('input_lineage','structured_governance','structured_lifecycle','structured_workout','structured_replay_review','counterparty_credit_products'))sys.source(file.path(helper_path,paste0(n,'.R')),envir=environment())
 p<-data$product;sid<-data$subject_id
 if(is.null(accounts_path))accounts_path<-system.file('extdata/counterparty_positions.csv',package='creditratingsystem')
 if(is.null(releases_path))releases_path<-system.file('extdata/counterparty_releases.csv',package='creditratingsystem')
 lineage<-prepare_input_snapshot(accounts_path,releases_path,data$lineage_config,data$lineage_config$cutoffs[[1]])
 gd<-load_data('structured_governance');ld<-load_data('structured_lifecycle');wd<-load_data('structured_workout');gd$variants<-gd$variants[p];ld$variants<-ld$variants[p];wd$variants<-wd$variants[p]
 c<-gd$variants[[p]];source<-lineage$rows[[1]]
 stopifnot(source$entity_id==sid,sid==c$subject_id,sid==ld$variants[[p]]$subject_id,sid==wd$variants[[p]]$subject_id,source$balance==wd$variants[[p]]$ead,source$x==0)
 c$snapshot$attributes$x$value<-source$x;c$source_recipe_reference<-list(kind='ACTUAL_2024_CLAIM',source_snapshot_hash=lineage$selected_snapshot_hash,subject_id=sid,contract_method_reference='counterparty_credit_products:collateralized; separate economic scenario, not the 2024 realized ledger');gd$variants[[p]]<-c
 g<-run_structured_governance(gd)$variants[[p]];ld$variants[[p]]$governance_assessment_hash<-g$assessment$assessment_hash;life<-run_structured_lifecycle(ld)$variants[[p]];workout<-run_structured_workout(wd)$variants[[p]];replay<-run_structured_replay_review(gd)$products[[p]]
 economic<-run_counterparty_credit_products()$variants$collateralized
 ci<-c$cohort_input;f<-ci$predictions[[1]];f$forecast_id<-paste0(sid,'-actual');f$case_id<-sid;f$entity_id<-sid;f$cluster_id<-sid;f$forecast_at<-'2024-01-15';f$known_at<-'2024-01-15';f$pd<-g$performing_assessment$components[[1]]$value
 ci$predictions<-list(f);ci$outcome_events<-list(list(event_id=paste0(sid,'-default'),entity_id=sid,event_at=wd$variants[[p]]$default_date,known_at='2024-02-02',event='default',default_definition=ci$target_contract$default_definition));ci$coverage<-list(list(entity_id=sid,complete_through='2024-05-01',known_at='2024-05-02'));ci$evaluation_cutoff<-'2024-05-02'
 cohort<-do.call(build_monitoring_cohorts,ci)$value;monitor<-monitor_rating_cohort(cohort,group_by='cohort',reference_window=list(start='2024-01-01',end='2024-01-31'),minimum_count=1,uncertainty='none')$value
 pd<-f$pd;grade<-if(pd<.25)'LOW'else if(pd<.75)'MID'else'HIGH';data$benchmark$internal_ratings[[1]]$grade<-grade
 for(k in c('entity','final','unsupported_counterfactual'))data$support$records[[1]]$stage_predictions[[k]]<-pd
 benchmark<-do.call(benchmark_ratings,data$benchmark)$value;support<-do.call(support_transfer_backtest,data$support)$value
 reasons<-setNames(lapply(g$decision$rule_trace,function(r)paste0('Recorded policy rule ',r$rule_id)),vapply(g$decision$rule_trace,function(r)r$reason_code,''));explanation<-decompose_policy_decision(g$decision,reasons)$value
 if(verify_reference)stopifnot(lineage$audit$decision$status=='PASS',benchmark$applicability=='NOT_APPLICABLE',monitor[[1]]$metrics$observed_rate==1,monitor[[1]]$metrics$brier==.25,support$metrics$by_stage$entity$brier==.25,support$metrics$by_stage$transfer$applicability=='NOT_APPLICABLE',workout$closed_workout$economic_loss==48,replay$audit$decision$status=='PASS')
 out<-list(data_origin='SYNTHETIC',product=p,subject_id=sid,source_lineage_status=lineage$audit$decision$status,opening_balance=source$balance,opening_pd=pd,decision=g$decision$action,policy_explanation=explanation,benchmark=benchmark,support_transfer=support,actual_monitoring=monitor,workout=workout,replay_status=replay$audit$decision$status,negative_replay_status=replay$negative_audit$decision$status,historical_replay_status=replay$historical_replay$status,lifecycle=life,contract_reference=list(closeout_exposure=economic$closeout$closeouts[[1]]$closeout_exposure,expected_loss=economic$loss$expected_loss,scope='Separate signed-netting/CSA scenario; not asserted to be observed 2024 claim history'),phase_status=list(ORIGINATION='APPROVED',PERFORMING='SAME_CLAIM_MONITORED',REVIEW_EARLY_WARNING='SOURCE_CORRECTION_AND_DELAY_TRIGGER',DELINQUENCY='OBSERVED_PAYMENT_DELAY',RESTRUCTURING='UNACCEPTED_PROPOSAL',DEFAULT_WORKOUT='FEBRUARY1_DEFAULT',EXIT='MAY1_SETTLEMENT',PORTFOLIO_MONITORING='SINGLE_CLAIM_DIAGNOSTIC_NOT_POPULATION_VALIDATION'),interpretation=data$interpretation,raw_artifacts_available=TRUE)
 if(return_artifacts)out$raw_artifacts<-list(lineage=lineage,governance=g,cohort=cohort,cohort_inputs=ci,lifecycle=life,workout=workout,replay=replay,benchmark_inputs=data$benchmark,support_inputs=data$support,economic_reference=economic)
 out
}
