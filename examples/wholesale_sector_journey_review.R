# Actual source-to-claim bindings, independent historical replay and absent benchmarks.
.sector_journey_replay <- function(audit,source){
 registry<-audit$registry;request<-audit$requests[[2]];cutoff<-request$known_at;sid<-request$input$snapshot$subject_id;seen<-list();log<-list()
 for(event in request$input$events){
  if(is.null(seen[[event$event_id]]))seen[[event$event_id]]<-length(seen)
  log[[length(log)+1L]]<-list(event_id=event$event_id,idempotency_key=event$event_id,occurred_at=event$occurred_at,known_at=event$known_at,sequence=seen[[event$event_id]],payload=event$payload,payload_hash=content_fingerprint(event$payload)$value,revision_of=NULL)
 }
 ev<-request$input$events;ord<-order(vapply(ev,function(e)e$known_at,character(1)),vapply(ev,function(e)seen[[e$event_id]],numeric(1)),vapply(ev,function(e)e$event_id,character(1)),method='radix');request$input$events<-ev[ord]
 original<-rate_subject_batch(list(request),registry,paste0(sid,'-ordered-original'),cutoff)$value$rows[[1]];replayed<-rate_subject_batch(list(request),registry,paste0(sid,'-ordered-replay'),cutoff)$value$rows[[1]];stopifnot(original$status=='OK',replayed$status=='OK')
 entry<-registry$entries[[1]];bundle<-entry$bundle;known<-entry$registered_at;snapshots<-list();contents<-list(MODEL=bundle$model,CALIBRATION=bundle$dependencies$artifacts$calibrator$content,POLICY=bundle$dependencies$artifacts$credit_policy$content,SOURCE=source)
 for(kind in names(contents))snapshots[[length(snapshots)+1L]]<-list(snapshot_id=kind,kind=kind,content_hash=content_fingerprint(contents[[kind]])$value,known_at=known,effective_from=known,effective_until=NULL)
 hashes<-setNames(lapply(snapshots,function(s)s$content_hash),vapply(snapshots,function(s)s$snapshot_id,character(1)))
 base<-list(checkpoint_id=paste0(sid,'-closed'),as_of=cutoff,known_at=cutoff,event_log_hash=content_fingerprint(log)$value,snapshot_hashes=hashes)
 checkpoint<-function(row)c(base,list(state=row$value,applied_event_ids=lapply(Filter(function(e)e$status=='APPLIED',row$value$event_trace),function(e)e$event_id)))
 contract<-list(contract_id=paste0(sid,'-exact-replay'),version='1',reference='Two actual executions of the frozen historical registry after stable knowledge-ordering; identical duplicate deliveries retained',ordering='KNOWN_AT_SEQUENCE_EVENT_ID',idempotency='FIRST_IDENTICAL_DELIVERY',corrections='APPEND_REVISION',required_checkpoint_ids=list(base$checkpoint_id),absolute_tolerance=1e-12,relative_tolerance=0)
 args<-list(event_log=log,original_checkpoints=list(checkpoint(original)),replayed_checkpoints=list(c(checkpoint(replayed),list(mode='REPLAY'))),snapshot_manifest=snapshots,replay_contract=contract)
 result<-do.call(replay_consistency,args)$value;changed<-args;changed$replayed_checkpoints[[1]]$state$assessment$components[[1]]$value<-changed$replayed_checkpoints[[1]]$state$assessment$components[[1]]$value+.01;negative<-do.call(replay_consistency,changed)$value
 stopifnot(result$decision$status=='PASS',negative$decision$status=='BLOCKED');list(result=result,negative=negative,inputs=args)
}
.sector_prediction_review <- function(case,model,observed){
 source<-observed$monitoring;pd<-predict_pd(model,do.call(rbind,lapply(case$features,unlist)),feature_order=unlist(case$feature_order),expected_target=case$target)$value[[1]];entries<-list();known<-model$value$training$known_at;mh<-content_fingerprint(model$value)$value
 for(identity in c('raw','calibrated','reviewed')){
  value<-if(identity=='raw')pd else case$reference_pd;args<-source
  args$predictions<-lapply(args$predictions,function(row){row$model_id<-identity;row$forecast_id<-paste0(row$case_id,'-',identity);row$pd<-value;row$prediction_origin<-'EXTERNAL';row})
  cohort<-do.call(build_monitoring_cohorts,args)$value
  entries[[identity]]<-list(cohort=cohort,model_version='1',parameter_version='1',model_fingerprint=if(identity=='raw')mh else content_fingerprint(list(model=mh,calibrator=case$calibrator))$value,training_known_at=known)
 }
 target<-case$target;target$horizon<-source$target_contract$horizon_days;target$time_unit<-'days';target$conditioning<-'performing'
 design<-list(model_id='raw',model_version='1',parameter_version='1',data_fingerprint=content_fingerprint(source)$value,population_id=paste0(case$case_id,'-actual-observed-cohort'),split_id='out_of_time_2024',prediction_origin='EXTERNAL',evaluation_cutoff=source$evaluation_cutoff,target=target)
 benchmark<-benchmark_predictions(entries[c('raw','calibrated')],reference_model_id='raw',metrics='brier',matching_policy='strict',multiple_comparisons='none',design=design,uncertainty='none')$value
 override_design<-design;override_design$model_id<-'calibrated'
 override<-override_monitoring(entries$calibrated,entries$reviewed,list(),policy=list(approved_authorities=list('credit_committee'),reference='No actual PD override recorded in this servicing cohort; unchanged review is explicitly audited',concentration_threshold=.6),design=override_design,metrics='brier',uncertainty='none')$value
 stopifnot(override$metrics$overridden_cases==0)
 list(benchmark=benchmark,override=override,source_model_hash=mh,raw_pd=pd,calibrated_pd=case$reference_pd,interpretation='Raw and calibrated frozen prediction pipelines compared on identical genuinely observed cases; no override invented')
}
run_wholesale_sector_journey_review <- function(data=NULL,verify_reference=TRUE,accounts_path=NULL,releases_path=NULL,artifact_paths=NULL,return_artifacts=FALSE,engine_paths=NULL){
 resource<-function(name)system.file('extdata',name,package='creditratingsystem')
 if(is.null(data))data<-jsonlite::fromJSON(resource('wholesale_sector_journey_review.json'),simplifyVector=FALSE)
 helper<-function(name){path<-if(is.null(engine_paths))system.file('examples',paste0(name,'.R'),package='creditratingsystem')else engine_paths[[name]];env<-new.env(parent=environment());sys.source(path,envir=env);env}
 le<-helper('input_lineage');ge<-helper('wholesale_sector_governance')
 lineage<-le$prepare_input_snapshot(if(is.null(accounts_path))resource('wholesale_sector_positions.csv')else accounts_path,if(is.null(releases_path))resource('wholesale_sector_releases.csv')else releases_path,data$lineage_config,data$lineage_config$cutoffs[[1]])
 rows<-setNames(lineage$rows,vapply(lineage$rows,function(x)x$entity_id,character(1)))
 for(i in seq_along(data$governance$cases)){
  case<-data$governance$cases[[i]];claim<-Filter(function(x)x$case_id==case$case_id,data$claims)[[1]];source<-rows[[case$case_id]]
  if(source$product_id!=case$product||source$balance!=claim$source_balance||source$x!=case$features[[1]][[1]])stop('Source must bind the actual claim balance, product and frozen financial design coordinate')
  data$governance$cases[[i]]$features<-list(list(source$x))
 }
 governance<-ge$run_wholesale_sector_governance(data$governance,artifact_paths=artifact_paths,return_audit_artifacts=TRUE);out<-list();artifacts<-list()
 for(i in seq_along(data$governance$cases)){
  case<-data$governance$cases[[i]];summary<-governance$report$cases[[i]];audit<-governance$audits[[i]];adapter<-case$validation_event_adapter
  if(!identical(adapter$source_target,case$target)||!identical(adapter$validator_target,modifyList(case$target,list(event='default')))||adapter$population_id!=case$case_id||adapter$evidence$event_at!=case$observed$default_at||(case$product=='SOVEREIGN_DEBT'&&!(adapter$evidence$available_unrestricted_cashflow<adapter$evidence$debt_service_due)))stop('Validation adapter must preserve target/population and bind observed insufficient payment capacity')
  sid<-case$case_id;claim<-Filter(function(x)x$case_id==sid,data$claims)[[1]];pd<-summary$origination_pd;decision<-audit$results[[1]]$value$decision
  reasons<-setNames(lapply(decision$rule_trace,function(r)paste0('Declared policy rule ',r$rule_id)),vapply(decision$rule_trace,function(r)r$reason_code,character(1)));explanation<-decompose_policy_decision(decision,reasons)$value
  benchmark<-data$benchmark_template;support<-data$support_template;horizon_end<-'2025-03-01';evaluation<-'2025-03-02';model_id<-audit$registry$entries[[1]]$model_id
  design<-list(model_id=model_id,model_version='1',parameter_version='1',data_fingerprint=content_fingerprint(case$observed)$value,population_id=sid,split_id='out_of_time_2024',prediction_origin='EXTERNAL',evaluation_cutoff=evaluation,target=adapter$validator_target,currency='EUR')
  common<-list(prediction_id=paste0(sid,'-origin'),entity_id=sid,cluster_id=sid,segment=case$product,as_of='2024-03-01',forecast_known_at='2024-03-01',horizon_end=horizon_end,model_id=model_id,model_version='1',parameter_version='1')
  internal<-benchmark$internal_ratings[[1]];for(k in names(common))internal[[k]]<-common[[k]];internal$rating_unit_id<-sid;internal$grade<-if(pd<.25)'LOW'else if(pd<.75)'MID'else'HIGH';benchmark$internal_ratings<-list(internal);benchmark$design<-design
  record<-support$records[[1]];for(k in names(common))record[[k]]<-common[[k]];record$stage_predictions<-list(entity=pd,transfer=0,joint=0,final=pd,unsupported_counterfactual=pd)
  event<-Filter(function(e)e$event_id=='credit-default',audit$requests[[2]]$input$events)[[1]];outcome<-list(value=1,known_at=evaluation,complete_through=horizon_end,event_ids=list(paste0(sid,':',event$event_id)),event_times=list(event$occurred_at));record$stage_outcomes<-list(entity=outcome,final=outcome);support$records<-list(record);support$design<-design
  for(name in names(support$stage_contract)){support$stage_contract[[name]]$reference<-'Actual standalone claim ledger: no executed support or observed transfer restriction. Unobserved transfer/intersection/counterfactual labels remain unavailable.';support$stage_contract[[name]]$event_definition<-if(name%in%c('entity','final'))case$target$event else paste0('unobserved-',name)}
  b<-do.call(benchmark_ratings,benchmark)$value;s<-do.call(support_transfer_backtest,support)$value;replay<-.sector_journey_replay(audit,rows[[sid]]);prediction_review<-NULL
  if(case$product=='PUBLIC_SERVICE_FINANCE'){name<-case$model_resource;model<-load_fitted_model(if(is.null(artifact_paths))resource(name)else artifact_paths[[name]]);prediction_review<-.sector_prediction_review(case,model,claim$observed_contract)}
  if(verify_reference)stopifnot(lineage$audit$decision$status=='PASS',b$applicability=='NOT_APPLICABLE',abs(s$metrics$by_stage$entity$brier-(1-pd)^2)<1e-12,s$metrics$by_stage$transfer$applicability=='NOT_APPLICABLE')
  out[[length(out)+1L]]<-list(product=case$product,case_id=sid,claim_id=claim$claim_id,source_balance=rows[[sid]]$balance,validation_event_adapter=adapter,original_target=case$target,benchmark_event_definition=case$target$event,source_feature=rows[[sid]]$x,source_snapshot_hash=lineage$selected_snapshot_hash,governance=summary,policy_explanation=explanation,rating_benchmark=b,support_transfer=s,replay=replay$result,negative_replay=replay$negative,prediction_review=prediction_review,scope='Actual single-claim stage diagnostic; no external rating or causal support label fabricated')
  artifacts[[length(artifacts)+1L]]<-list(case_id=sid,governance=audit,benchmark_inputs=benchmark,support_inputs=support,replay_inputs=replay$inputs)
 }
 report<-list(data_origin='SYNTHETIC',lineage=lineage$audit,archive_sha256=lineage$archive_sha256,cases=out)
 if(return_artifacts)list(report=report,artifacts=artifacts,lineage_inputs=lineage$audit_inputs)else report
}
