# Seven sector histories pinned to actual component models and calibrators.
run_wholesale_sector_governance <- function(data=NULL,verify_reference=TRUE,artifact_paths=NULL,return_audit_artifacts=FALSE) {
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/wholesale_sector_governance.json',package='creditratingsystem'),simplifyVector=FALSE)
 rows<-list();audits<-list()
 for(case in data$cases) {
  path<-if(is.null(artifact_paths))system.file('extdata',case$model_resource,package='creditratingsystem')else artifact_paths[[case$model_resource]]
  model<-load_fitted_model(path);details<-.sector_journey(case,model);result<-details$summary
  if(verify_reference)stopifnot(abs(result$origination_pd-case$reference_pd)<1e-12,result$origination_decision==if(!is.null(case$reference_action))case$reference_action else if(case$gate$available_unrestricted_cashflow>=case$gate$debt_service_due)'APPROVE'else'REFER')
  rows[[length(rows)+1L]]<-result;audits[[length(audits)+1L]]<-details$audit
 }
 report<-list(data_origin='SYNTHETIC',scope=data$scope,cases=rows)
 if(return_audit_artifacts)list(report=report,audits=audits)else report
}
.sector_journey <- function(data,model){
 dates<-list(routing_start='2023-01-01',origination='2024-03-01',processing='2024-03-02',review='2024-03-03',finalize='2024-03-04',performing_review='2024-03-15',warning='2024-04-01',warning_known='2024-04-02',delinquency='2024-04-15',delinquency_known='2024-04-16',proposal='2024-04-20',default='2024-05-01',default_known='2024-05-02',closure='2024-09-03',replay='2025-01-02',later_registration='2025-02-01',later_replay='2025-03-01',payment_due='2024-03-15')
 supplied<-data$timeline
 if(length(setdiff(names(supplied),names(dates))))stop('Unknown timeline date')
 for(k in names(supplied))dates[[k]]<-supplied[[k]]
 for(v in dates)if(!is.character(v)||length(v)!=1L||is.na(as.Date(v))||format(as.Date(v),'%Y-%m-%d')!=v)stop('Timeline requires ISO dates')
 pairs<-list(c('routing_start','origination'),c('origination','processing'),c('origination','review'),c('review','finalize'),c('finalize','closure'),c('warning','warning_known'),c('delinquency','delinquency_known'),c('default','default_known'),c('closure','replay'),c('replay','later_registration'),c('later_registration','later_replay'),c('payment_due','delinquency'))
 for(pair in pairs)if(dates[[pair[1]]]>dates[[pair[2]]])stop('Inconsistent timeline chronology')
 for(k in c('warning_known','delinquency_known','default_known','proposal'))if(dates[[k]]<dates$origination||dates[[k]]>dates$closure)stop('Event outside historical interval')

 target<-data$target;sid<-data$case_id;mid<-model$value$model_id;facts<-data$facts;object<-setNames(list(),character())
 rule<-list(rule_id=sid,profile_id=sid,profile_version='1',priority=1,effective_from=dates$routing_start,effective_to=NULL,known_at=dates$routing_start,conditions=lapply(names(facts),function(k)list(field=k,operator='eq',value=facts[[k]])))
 contract<-list(contract_id=sid,version='1',required_features=as.list(names(facts)),allowed_values=lapply(facts,function(v)list(v)),support_bounds=object,maximum_age_days=lapply(facts,function(v)90))
 ref<-function(source,identity,...){setNames(list(list(source=source,id=identity,path=list(...))),'$ref')}
 recipe<-list(method='assessment_pipeline',version='2.0',parameters=list(routing_rules=list(rule),applicability_contract=contract,steps=list(list(step_id='pd',method_id='classification.predict_pd',arguments=list(fitted_model=ref('model','primary'),X=ref('input','features','value'),feature_order=data$feature_order,expected_target=target),argument_types=object,output_contract=list(shape='vector',unit='probability',target=target))),components=list(list(component_id='pd',quantity='PD',value=ref('step','pd','value',0),subject_id=sid,subject_type=if(is.null(data$subject_type))'obligor'else data$subject_type,variant='standalone',definition=target)),effects=list(),transitions=list(list(from_state='DRAFT',to_state='REVIEWED',event_type='review',required_gates=list('checked')),list(from_state='REVIEWED',to_state='FINAL',event_type='finalize',required_gates=list('approved')),list(from_state='FINAL',to_state='SUPERSEDED',event_type='exit',required_gates=list('settled'))),event_dependencies=list(list(assessment_id=paste0(sid,'-assessment'),dependency_type='subject',dependency_id=sid)),decision=NULL))
 recipe$parameters$steps[[2]]<-list(step_id='calibrated',method_id='calibration.apply_calibrator',arguments=list(calibrator=ref('artifact','calibrator'),values=ref('step','pd','value'),target=target,base_model_version='1'),argument_types=list(values='numeric_vector'),output_contract=list(shape='vector',unit='probability',target=target))
 extra<-data$calibrator_arguments
 if(length(setdiff(names(extra),'groups')))stop('Only explicit calibration groups are configurable')
 for(k in names(extra))recipe$parameters$steps[[2]]$arguments[[k]]<-extra[[k]]
 recipe$parameters$components[[1]]$value<-ref('step','calibrated','value',0)
 dependencies<-list(artifacts=list(calibrator=list(kind='calibrator',content=data$calibrator,sha256=content_fingerprint(data$calibrator)$value,known_at=dates$origination)))
 policy<-compile_credit_decision_policy(data$decision_policy)$value
 dependencies$artifacts$credit_policy<-list(kind='parameters',content=policy,sha256=content_fingerprint(policy)$value,known_at=dates$origination)
 timing<-list(as_of=dates$origination,known_at=dates$origination);gate<-data$gate
 recipe$parameters$decision<-c(list(policy=ref('artifact','credit_policy'),risk=c(timing,list(pd=ref('step','calibrated','value',0),target=target,freshness='CURRENT')),affordability=c(timing,list(status=if(gate$available_unrestricted_cashflow>=gate$debt_service_due)'AFFORDABLE'else'NOT_AFFORDABLE')),eligibility=c(timing,list(status=if(gate$legal_eligibility)'ELIGIBLE'else'INELIGIBLE')),operations=c(timing,list(status=gate$operations))),timing)
 registry<-register_rating_model_version(NULL,list(model=model$value,dependencies=dependencies,configuration=list(assessment_recipe=recipe)),list(model_id=mid,model_version='1',registered_at=dates$origination))$value
 assignment<-list(assignment_id='corporate-champion-1',profile_id=sid,purpose='PD',model_id=mid,model_version='1',model_hash=registry$entries[[1]]$model_hash,effective_from=dates$origination,effective_to=NULL,known_at=dates$origination)
 selection<-select_effective_model(registry,list(assignment),sid,'PD',dates$origination,dates$origination)$value
 snapshot<-list(subject_id=sid,subject_type=if(is.null(data$subject_type))'obligor'else data$subject_type,as_of=dates$origination,known_at=dates$origination,attributes=lapply(facts,function(v)list(value=v,observed_at=dates$origination,known_at=dates$origination)))
 subject<-list(subject_id=sid,version='1',attributes=snapshot$attributes,effective_from=dates$origination,effective_to=NULL,known_at=dates$origination)
 subjects<-build_rating_subjects(setNames(list(list(subject)),if(is.null(data$subject_type))'obligor'else data$subject_type),list(),dates$performing_review,dates$performing_review)$value
 request<-list(row_id='origination',known_at=dates$processing,model_id=mid,model_version='1',model_hash=assignment$model_hash,recipe=recipe,input=list(snapshot=snapshot,datasets=list(features=list(value=data$features,as_of=dates$origination,known_at=dates$origination)),assessment_id=paste0(sid,'-assessment'),assessment_known_at=dates$origination,calculated_at=dates$processing,lifecycle_events=list(),events=list()))
 original<-rate_subject_batch(list(request),registry,paste0(sid,'-origination'),dates$processing)$value$rows[[1]]
 if(original$status!='OK')stop(paste('Corporate pipeline:',original$error_code,original$error_message))
 assessment<-original$value$assessment
 fp<-list(maximum_age_days=90,maximum_information_age_days=90,trigger_types=list('financial_deterioration'))
 freshness<-check_assessment_freshness(assessment,fp,list(),dates$performing_review,dates$performing_review)$value
 trigger<-list(event_id='review-trigger',subject_id=sid,type='financial_deterioration',occurred_at=dates$warning,known_at=dates$warning_known)
 stale<-check_assessment_freshness(assessment,fp,list(trigger),dates$warning_known,dates$warning_known)$value
 history<-request;history$row_id<-'closed-history';history$known_at<-dates$closure
 history$input$lifecycle_events<-list(list(event_id='review',type='review',occurred_at=dates$review,known_at=dates$review,reason='Financial statements checked',gates=list(checked=TRUE)),list(event_id='approval',type='finalize',occurred_at=dates$finalize,known_at=dates$finalize,reason='Local approval',gates=list(approved=TRUE)),list(event_id='exit',type='exit',occurred_at=dates$closure,known_at=dates$closure,reason=data$observed$exit_reason,gates=list(settled=TRUE)))
 event<-list(event_id='credit-review',revision='1',occurred_at=dates$warning,known_at=dates$warning_known,dependencies=list(list(type='subject',id=sid)),payload=list(review_required=TRUE))
 default_event<-list(event_id='credit-default',revision='1',occurred_at=dates$default,known_at=dates$default_known,dependencies=list(list(type='subject',id=sid)),payload=list(default_observed=TRUE))
 delinquency<-list(event_id='credit-delinquency',revision='1',occurred_at=dates$delinquency,known_at=dates$delinquency_known,dependencies=list(list(type='subject',id=sid)),payload=list(due_date=dates$payment_due,scheduled_amount=data$observed$scheduled_due,received_amount=data$observed$received,days_past_due=as.numeric(as.Date(dates$delinquency)-as.Date(dates$payment_due))))
 proposal<-list(event_id='restructuring-proposal',revision='1',occurred_at=dates$proposal,known_at=dates$proposal,dependencies=list(list(type='subject',id=sid)),payload=list(treatment_id='extend',proposal_only=TRUE,contract_changed=FALSE))
 history$input$events<-list(event,event,delinquency,proposal,default_event)
 row<-rate_subject_batch(list(history),registry,paste0(sid,'-exit'),dates$closure)$value$rows[[1]]
 if(row$status!='OK')stop(paste('Corporate lifecycle:',row$error_code,row$error_message))
 replay<-replay_assessment_history(list(request,history),registry,list(original,row),dates$replay)$value
 tr<-list(X=list(list(0),list(0),list(1),list(1)),y=list(0,1,0,1),feature_order=data$feature_order);later_training<-data$later_training
 later_model<-fit_binary_glm(do.call(rbind,lapply(tr$X,unlist)),unlist(tr$y),weights=c(8,2,6,4),feature_order=unlist(tr$feature_order),link='logit',fit_intercept=TRUE,target=target,training=later_training,model_id=mid,model_version='2')
 later_registry<-register_rating_model_version(registry,list(model=later_model$model,dependencies=dependencies,configuration=list(assessment_recipe=recipe)),list(model_id=mid,model_version='2',registered_at=dates$later_registration))$value
 later_replay<-replay_assessment_history(list(request,history),later_registry,list(original,row),dates$later_replay)$value
 stopifnot(later_replay$status=='REPRODUCED')
 tampered<-row;tampered$event_state_hash<-'changed';rejected<-replay_assessment_history(list(history),registry,list(tampered),dates$replay)$value
 statuses<-vapply(row$value$event_trace,function(x)x$status,character(1))
 stopifnot(replay$status=='REPRODUCED',rejected$status=='NOT_REPRODUCIBLE',row$value$assessment$state=='SUPERSEDED',identical(statuses,c('APPLIED','DUPLICATE','APPLIED','APPLIED','APPLIED')),freshness$status=='CURRENT',stale$status=='STALE')
 summary<-list(origination_decision=original$value$decision$action,credit_gate_inputs=data$gate,product=data$product,case_id=sid,source_component=data$source_component,source_case_id=data$source_case_id,assessed_scope='CALIBRATED_STANDALONE_BEFORE_SUPPORT_TRANSFER',subject_count=length(subjects$subjects),selection=selection$status,initial_state=assessment$state,final_state=row$value$assessment$state,current=freshness$status,review_required=stale$status,event_statuses=as.list(statuses),replay=replay$status,replay_after_new_champion=later_replay$status,tampered_replay=rejected$status,origination_pd=assessment$components[[1]]$value,historical_pd=row$value$assessment$components[[1]]$value)
 list(summary=summary,audit=list(registry=registry,requests=list(request,history),results=list(original,row)))
}
