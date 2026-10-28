# Actual historical recomputation after source corrections and model replacement.
review_structured_replay_case<-function(product,case){
 origin<-'2024-01-01';revised<-'2024-01-16';model<-case$model;subject<-list(subject_id=case$subject_id,subject_type=case$subject_type)
 bundle<-list(model=model,dependencies=setNames(list(),character()),configuration=list(source_recipe_reference=case$source_recipe_reference))
 registry<-register_rating_model_version(NULL,bundle,list(model_id=model$model_id,model_version='v1',registered_at='2023-12-31'))$value
 mh<-registry$entries[[1]]$model_hash;policy<-compile_credit_decision_policy(case$policy)$value
 request<-function(x,known)list(row_id=paste0(product,'-',known),known_at=known,model_id=model$model_id,model_version='v1',model_hash=mh,recipe=list(method='binary_glm',version='1.0',parameters=list(expected_target=model$target)),input=list(X=list(list(x)),feature_order=list('x')))
 requests<-list(request(0,origin),request(log(3),revised))
 events<-list(list(event_id=paste0(product,'-source'),revision='1',occurred_at=origin,known_at=origin,dependencies=list(list(type=subject$subject_type,id=subject$subject_id)),payload=list(source_input=requests[[1]]$input)),list(event_id=paste0(product,'-source'),revision='2',corrects_revision='1',occurred_at=origin,known_at=revised,dependencies=list(list(type=subject$subject_type,id=subject$subject_id)),payload=list(source_input=requests[[2]]$input)))
 deliveries<-list(events[[1]],events[[1]],events[[2]])
 validation_log<-lapply(deliveries,function(event){eid<-paste0(event$event_id,':',event$revision);list(event_id=eid,idempotency_key=eid,occurred_at=event$occurred_at,known_at=event$known_at,sequence=as.numeric(event$revision),payload=event$payload,payload_hash=content_fingerprint(event$payload)$value,revision_of=if(event$revision=='1')NULL else paste0(event$event_id,':1'))})
 snapshots<-list()
 snapshot<-function(identity,kind,content,known){row<-list(snapshot_id=paste0(product,'-',identity),kind=kind,content_hash=content_fingerprint(content)$value,known_at=known,effective_from=origin,effective_until=NULL);snapshots[[length(snapshots)+1L]]<<-row;row}
 common<-list(snapshot('model','MODEL',bundle,'2023-12-31'),snapshot('calibration','CALIBRATION',list(method='NO_POST_CALIBRATION',version='1',target=model$target),'2023-12-31'),snapshot('policy','POLICY',policy,'2023-12-31'))
 sources<-lapply(seq_along(requests),function(i)snapshot(paste0('source-',i-1L),'SOURCE',requests[[i]]$input,requests[[i]]$known_at))
 log_hash<-content_fingerprint(validation_log)$value
 dependencies<-list(list(assessment_id=paste0(product,'-source-review'),dependency_type=subject$subject_type,dependency_id=subject$subject_id))
 execute<-function(index,active_registry,mode){
  req<-requests[[index]];known<-req$known_at;row<-rate_subject_batch(list(req),active_registry,paste0(product,'-single'),known)$value$rows[[1]]
  stopifnot(row$status=='OK');pd<-row$value[[1]]
  assessment<-compose_risk_assessment(paste0(product,'-source-review'),subject,list(c(list(component_id='pd',quantity='PD',value=pd),subject,list(variant='corrected_historical_input',definition=model$target))),list(profile_id=product,profile_version='1',model_id=model$model_id,model_version='v1',model_hash=mh,input_hash=row$input_hash),origin,known,known)$value
  timing<-list(as_of=origin,known_at=known);approval<-case$policy$approval_contract
  decision<-apply_credit_decision_policy(policy,risk=c(timing,list(pd=pd,target=model$target,freshness='CURRENT')),affordability=c(timing,list(status='NOT_APPLICABLE',domain=approval$domain,applicability_reference=approval$affordability$reference,valid_until='2024-02-01')),eligibility=c(timing,list(status='ELIGIBLE')),operations=c(timing,list(status='CLEAR',legal_status='VERIFIED')),as_of=origin,known_at=known)$value
  state<-NULL;applied<-list();statuses<-list()
  for(event in deliveries){result<-apply_assessment_event(state,event,dependencies,known)$value;state<-result$state;statuses[[length(statuses)+1L]]<-result$status;if(result$status=='APPLIED')applied[[length(applied)+1L]]<-paste0(event$event_id,':',event$revision)}
  used<-c(common,list(sources[[index]]));hashes<-setNames(lapply(used,function(s)s$content_hash),vapply(used,function(s)s$snapshot_id,character(1)))
  checkpoint<-list(checkpoint_id=paste0(product,'-',index-1L),as_of=origin,known_at=known,snapshot_hashes=hashes,applied_event_ids=applied,event_log_hash=log_hash,state=list(assessment=assessment,decision=decision,event_state=state,event_statuses=statuses),mode=mode)
  list(checkpoint=checkpoint,row=row)
 }
 original<-lapply(1:2,function(i)execute(i,registry,'ORIGINAL'))
 newer<-bundle;newer$model$model_version<-'v2';newer$model$parameters$coefficients[[1]]<-10
 later_registry<-register_rating_model_version(registry,newer,list(model_id=model$model_id,model_version='v2',registered_at='2024-02-01'))$value
 replayed<-lapply(1:2,function(i)execute(i,later_registry,'REPLAY'))
 originals<-lapply(original,function(x)x$checkpoint);replays<-lapply(replayed,function(x)x$checkpoint)
 contract<-list(contract_id='actual-structured-replay',version='1',reference='same-pinned-model-policy-source-and-no-post-calibration',ordering='KNOWN_AT_SEQUENCE_EVENT_ID',idempotency='FIRST_IDENTICAL_DELIVERY',corrections='APPEND_REVISION',required_checkpoint_ids=lapply(originals,function(x)x$checkpoint_id),absolute_tolerance=0,relative_tolerance=0)
 audit<-replay_consistency(validation_log,originals,replays,snapshot_manifest=snapshots,replay_contract=contract)$value
 historical<-replay_assessment_history(requests,later_registry,lapply(original,function(x)x$row),'2024-03-01')$value
 bad<-requests[[1]];bad$row_id<-paste0('invalid-',product);bad$model_hash<-paste(rep('0',64),collapse='')
 batch<-rate_subject_batch(c(requests,list(bad)),registry,paste0(product,'-batch'),revised)$value
 permuted<-rate_subject_batch(c(list(bad),rev(requests)),registry,paste0(product,'-batch'),revised)$value
 early<-rate_subject_batch(list(requests[[2]]),registry,paste0(product,'-early'),origin)$value
 mutated<-replays[[1]];mutated$state$assessment$components[[1]]$value<-.9
 negative<-replay_consistency(validation_log,originals,list(mutated,replays[[2]]),snapshot_manifest=snapshots,replay_contract=contract)$value
 list(original_checkpoints=originals,actual_replay_checkpoints=replays,audit=audit,negative_audit=negative,historical_replay=historical,batch=batch,batch_permutation_exact=identical(batch,permuted),early_revision=early,event_log=validation_log,snapshots=snapshots,interpretation='The January estimate is restated using a later-known source correction, not overwritten. No post-calibration is applied; its explicit versioned identity contract is retained. Model replacement does not change pinned historical execution.')
}
run_structured_replay_review<-function(data=NULL,verify_reference=TRUE){
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/structured_governance.json',package='creditratingsystem'),simplifyVector=FALSE)
 out<-setNames(lapply(names(data$variants),function(product)review_structured_replay_case(product,data$variants[[product]])),names(data$variants))
 if(verify_reference)for(value in out){
  stopifnot(identical(vapply(value$original_checkpoints,function(c)c$state$assessment$components[[1]]$value,numeric(1)),c(.5,.75)),value$audit$decision$status=='PASS',value$negative_audit$decision$status=='BLOCKED',value$historical_replay$status=='REPRODUCED',value$batch_permutation_exact,value$early_revision$rows[[1]]$error_code=='RATING_REQUEST_UNAVAILABLE',identical(unlist(value$original_checkpoints[[1]]$state$event_statuses),c('APPLIED','DUPLICATE','DEFERRED')),identical(unlist(value$original_checkpoints[[2]]$state$event_statuses),c('APPLIED','DUPLICATE','APPLIED')))
 }
 list(products=out)
}
