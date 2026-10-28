run_structured_lifecycle<-function(data=NULL,verify_reference=TRUE){
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/structured_lifecycle.json',package='creditratingsystem'),simplifyVector=FALSE)
 out<-list()
 for(product in names(data$variants)){
  c<-data$variants[[product]];calendar<-c$history_calendar
  if(!(calendar$origin<=calendar$complete_through&&calendar$complete_through<=calendar$known_at&&calendar$known_at<=data$training$known_at&&data$training$known_at<'2024-01-16'))stop('Historical transition sample must be complete and known before the decision')
  last_day<-as.Date(calendar$origin)+max(vapply(c(c$history,c$competing_panel),function(row)row$stop,numeric(1)))
  if(last_day>as.Date(calendar$complete_through))stop('Historical transitions exceed the declared complete-through date')
  model<-estimate_multistate_transitions(c$history,states=unlist(c$states),allowed_transitions=c$allowed_transitions,initial_state='performing',target=data$target,training=data$training,model_id=paste0(product,'-historical-transitions'),model_version='1')
  curves<-predict_event_curves(model,list(),c(0,14,15,31,121),feature_order=list(),target=data$target,event_states='default')$value
  conditional<-predict_event_curves(model,list(),c(0,16,106),feature_order=list(),target=data$target,initial_state='restructured',start_time=15,event_states='default')$value
  target<-data$target;target$horizon<-60
  competing<-fit_cause_specific_hazards(c$competing_panel,matrix(numeric(),4,0),c$event_types,feature_order=list(),causes=c('default','exit'),ties='breslow',target=target,training=data$training,model_id=paste0(product,'-historical-competing'),model_version='1')
  economic_parameters<-competing$model$parameters
  economic_parameters$cause_models<-lapply(economic_parameters$cause_models,function(m){m$diagnostics<-NULL;m})
  exit_curves<-predict_event_curves(competing,list(list(path_id='claim',start=0,stop=60,covariates=list(),stratum='all',covariate_time=0)),c(0,30,60),feature_order=list(),target=target)$value
  windows<-build_observation_windows(c$ratings,c$default_events,c$exits,30,'2024-02-15')$value
  immature<-build_observation_windows(c$ratings,c$default_events,c$exits,30,'2024-01-31')$value
  dependencies<-list(list(assessment_id=c$assessment_id,dependency_type=c$subject_type,dependency_id=c$subject_id))
  event<-function(kind,occurred,known)list(event_id=paste0(product,'-',kind),revision='1',occurred_at=occurred,known_at=known,dependencies=list(list(type=c$subject_type,id=c$subject_id)),payload=list(kind=kind,execution=if(kind=='payment_delay')'OBSERVED' else 'PROPOSAL_ONLY'))
  delay<-event('payment_delay','2024-01-15','2024-01-16')
  deferred<-apply_assessment_event(NULL,delay,dependencies,'2024-01-15')$value
  applied<-apply_assessment_event(NULL,delay,dependencies,'2024-01-16')$value
  proposed<-apply_assessment_event(applied$state,event('restructure_proposal','2024-01-16','2024-01-16'),dependencies,'2024-01-16')$value
  out[[product]]<-list(governance_assessment_hash=c$governance_assessment_hash,transition_model=model$model,transition_curves=curves,conditional_restructured_curves=conditional,competing_parameters=economic_parameters,competing_target=target,competing_curves=exit_curves,observation_windows=windows,immature_windows=immature,deferred_event=deferred,delinquency_event=applied,restructuring_event=proposed)
  if(verify_reference){
   v<-curves$curves[[1]];stopifnot(identical(unlist(v$first_hit_probability),c(0,0,0,.5,.5)),v$state_occupancy[[5]][[4]]==0,v$state_occupancy[[5]][[5]]==.5,identical(unlist(conditional$curves[[1]]$first_hit_probability),c(0,1,1)))
   v<-exit_curves$curves[[1]];stopifnot(abs(v$survival[[2]]-exp(-.5))<1e-12)
   for(cause in c('default','exit'))stopifnot(abs(v$cumulative_incidence[[cause]][[2]]-(1-exp(-.5))/2)<1e-12)
   stopifnot(windows[[1]]$target==1,is.null(windows[[2]]$target),windows[[2]]$label_status=='exit',is.null(immature[[1]]$target),deferred$status=='DEFERRED',identical(unlist(proposed$affected_assessment_ids),c$assessment_id))
  }
 }
 list(data_origin=data$data_origin,version=data$version,product=data$product,variants=out)
}
