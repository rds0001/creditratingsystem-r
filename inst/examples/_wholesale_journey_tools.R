# Actual source-to-claim bindings, independent historical replay and absent benchmarks.
.sector_journey_replay <- function(audit,source,policy=NULL){
 registry<-audit$registry;request<-audit$requests[[2]];cutoff<-request$known_at;sid<-request$input$snapshot$subject_id;seen<-list();log<-list()
 for(event in request$input$events){
  if(is.null(seen[[event$event_id]]))seen[[event$event_id]]<-length(seen)
  log[[length(log)+1L]]<-list(event_id=event$event_id,idempotency_key=event$event_id,occurred_at=event$occurred_at,known_at=event$known_at,sequence=seen[[event$event_id]],payload=event$payload,payload_hash=content_fingerprint(event$payload)$value,revision_of=NULL)
 }
 ev<-request$input$events;ord<-order(vapply(ev,function(e)e$known_at,character(1)),vapply(ev,function(e)seen[[e$event_id]],numeric(1)),vapply(ev,function(e)e$event_id,character(1)),method='radix');request$input$events<-ev[ord]
 original<-rate_subject_batch(list(request),registry,paste0(sid,'-ordered-original'),cutoff)$value$rows[[1]];replayed<-rate_subject_batch(list(request),registry,paste0(sid,'-ordered-replay'),cutoff)$value$rows[[1]];stopifnot(original$status=='OK',replayed$status=='OK')
 entry<-registry$entries[[1]];bundle<-entry$bundle;known<-entry$registered_at;snapshots<-list();artifacts<-bundle$dependencies$artifacts;calibration<-artifacts$calibrator$content
 if(is.null(calibration))calibration<-list(applied=FALSE,reference='This frozen recipe has no separate PD calibration step')
 if(!is.null(artifacts$credit_policy$content))policy<-artifacts$credit_policy$content
 if(is.null(policy))stop('Actual decision policy required for replay snapshot')
 contents<-list(MODEL=bundle$model,CALIBRATION=calibration,POLICY=policy,SOURCE=source)
 for(kind in names(contents)){available<-if(kind=='SOURCE'&&!is.null(source$known_at))source$known_at else known;snapshots[[length(snapshots)+1L]]<-list(snapshot_id=kind,kind=kind,content_hash=content_fingerprint(contents[[kind]])$value,known_at=available,effective_from=available,effective_until=NULL)}
 hashes<-setNames(lapply(snapshots,function(s)s$content_hash),vapply(snapshots,function(s)s$snapshot_id,character(1)))
 base<-list(checkpoint_id=paste0(sid,'-closed'),as_of=cutoff,known_at=cutoff,event_log_hash=content_fingerprint(log)$value,snapshot_hashes=hashes)
 checkpoint<-function(row)c(base,list(state=row$value,applied_event_ids=lapply(Filter(function(e)e$status=='APPLIED',row$value$event_trace),function(e)e$event_id)))
 contract<-list(contract_id=paste0(sid,'-exact-replay'),version='1',reference='Two actual executions of the frozen historical registry after stable knowledge-ordering; identical duplicate deliveries retained',ordering='KNOWN_AT_SEQUENCE_EVENT_ID',idempotency='FIRST_IDENTICAL_DELIVERY',corrections='APPEND_REVISION',required_checkpoint_ids=list(base$checkpoint_id),absolute_tolerance=1e-12,relative_tolerance=0)
 args<-list(event_log=log,original_checkpoints=list(checkpoint(original)),replayed_checkpoints=list(c(checkpoint(replayed),list(mode='REPLAY'))),snapshot_manifest=snapshots,replay_contract=contract)
 result<-do.call(replay_consistency,args)$value;changed<-args;changed$replayed_checkpoints[[1]]$state$assessment$components[[1]]$value<-changed$replayed_checkpoints[[1]]$state$assessment$components[[1]]$value+.01;negative<-do.call(replay_consistency,changed)$value
 stopifnot(result$decision$status=='PASS',negative$decision$status=='BLOCKED');list(result=result,negative=negative,inputs=args)
}
.sector_prediction_review <- function(case,model,observed,reference_id='calibrated'){
 source<-observed$monitoring;pd<-predict_pd(model,do.call(rbind,lapply(case$features,unlist)),feature_order=unlist(case$feature_order),expected_target=case$target)$value[[1]];entries<-list();known<-model$value$training$known_at;mh<-content_fingerprint(model$value)$value
 for(identity in c('raw',reference_id,'reviewed')){
  value<-if(identity=='raw')pd else case$reference_pd;args<-source
  args$predictions<-lapply(args$predictions,function(row){row$model_id<-identity;row$forecast_id<-paste0(row$case_id,'-',identity);row$pd<-value;row$prediction_origin<-'EXTERNAL';row})
  cohort<-do.call(build_monitoring_cohorts,args)$value
  entries[[identity]]<-list(cohort=cohort,model_version='1',parameter_version='1',model_fingerprint=if(identity=='raw')mh else content_fingerprint(list(model=mh,calibrator=case$calibrator))$value,training_known_at=known)
 }
 target<-case$target;target$horizon<-source$target_contract$horizon_days;target$time_unit<-'days';target$conditioning<-'performing'
 design<-list(model_id='raw',model_version='1',parameter_version='1',data_fingerprint=content_fingerprint(source)$value,population_id=paste0(case$case_id,'-actual-observed-cohort'),split_id='out_of_time_2024',prediction_origin='EXTERNAL',evaluation_cutoff=source$evaluation_cutoff,target=target)
 benchmark<-benchmark_predictions(entries[c('raw',reference_id)],reference_model_id='raw',metrics='brier',matching_policy='strict',multiple_comparisons='none',design=design,uncertainty='none')$value
 override_design<-design;override_design$model_id<-reference_id
 override<-override_monitoring(entries[[reference_id]],entries$reviewed,list(),policy=list(approved_authorities=list('credit_committee'),reference='No actual PD override recorded in this servicing cohort; unchanged review is explicitly audited',concentration_threshold=.6),design=override_design,metrics='brier',uncertainty='none')$value
 stopifnot(override$metrics$overridden_cases==0)
 list(benchmark=benchmark,override=override,source_model_hash=mh,raw_pd=pd,reference_pd=case$reference_pd,reference_pipeline=reference_id,original_model_target=case$target,interpretation='Frozen raw and declared reference prediction pipelines compared on identical genuinely observed cases; no override invented')
}
