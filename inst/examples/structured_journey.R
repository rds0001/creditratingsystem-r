# New2024claims reuse the verified dated lifecycle components.
run_structured_journey<-function(data=NULL,verify_reference=TRUE){
 load_data<-function(n)jsonlite::fromJSON(system.file(paste0('extdata/',n,'.json'),package='creditratingsystem'),simplifyVector=FALSE)
 if(is.null(data))data<-load_data('structured_journey')
 for(n in c('input_lineage','structured_governance','structured_lifecycle','structured_workout','structured_pool_review'))sys.source(system.file(paste0('examples/',n,'.R'),package='creditratingsystem'),envir=environment())
 lineage<-prepare_input_snapshot(system.file('extdata/structured_positions.csv',package='creditratingsystem'),system.file('extdata/structured_releases.csv',package='creditratingsystem'),data$lineage_config,data$lineage_config$cutoffs[[1]])
 stopifnot(lineage$audit$decision$status=='PASS')
 gd<-load_data('structured_governance');ld<-load_data('structured_lifecycle');wd<-load_data('structured_workout');products<-unlist(data$products,use.names=FALSE)
 gd$variants<-gd$variants[products];ld$variants<-ld$variants[products];wd$variants<-wd$variants[products]
 for(row in lineage$rows){
  p<-row$product_id;c<-gd$variants[[p]];stopifnot(c$subject_id==row$entity_id,row$x==0,row$balance==data$pools$cases[[p]]$position_binding$position_ead)
  c$snapshot$attributes$x$value<-row$x
  c$source_recipe_reference<-list(kind='OWN_2024_COLLATERAL_LEDGER',position_id=c$subject_id,source_snapshot_hash=lineage$selected_snapshot_hash,pool_contract_hash=content_fingerprint(data$pools$cases[[p]]$pool_input)$value)
  gd$variants[[p]]<-c
 }
 governance<-run_structured_governance(gd)$variants
 for(p in products)ld$variants[[p]]$governance_assessment_hash<-governance[[p]]$assessment$assessment_hash
 lifecycle<-run_structured_lifecycle(ld)$variants;workout<-run_structured_workout(wd)$variants
 pool_review<-run_structured_pool_review(data$pools,verify_reference=FALSE)$products
 records<-list();events<-list();snapshots<-list()
 for(p in products){
  c<-data$pools$cases[[p]];binding<-c$position_binding;g<-governance[[p]];w<-workout[[p]];sid<-binding$position_id
  stopifnot(sid==gd$variants[[p]]$subject_id,sid==ld$variants[[p]]$subject_id,sid==wd$variants[[p]]$subject_id)
  observed<-pool_review[[p]]$asset_pool_validation$metrics$observed_owned_flows$recovery_gross
  if(p=='SECURITIZATION_TRANCHE'){
   senior<-min(observed,binding$priority_contract$senior_principal);junior<-max(0,observed-senior);actual_claim_recovery<-senior
   claim_control<-list(senior_recovery=senior,junior_recovery=junior,cash_conservation=senior+junior-observed)
  }else if(p=='COVERED_BOND'){
   residual<-binding$position_ead-observed;estate<-min(residual,residual*binding$estate_recovery_fraction);actual_claim_recovery<-observed+estate
   claim_control<-list(cover_recovery=observed,residual_claim=residual,estate_recovery=estate,aggregate_recovery=actual_claim_recovery)
  }else{actual_claim_recovery<-observed;claim_control<-list(debtor_recovery=observed,seller_recourse=0,dilution=0)}
  stopifnot(abs(actual_claim_recovery-binding$actual_position_recovery)<1e-12,abs(binding$position_ead-actual_claim_recovery-w$closed_workout$economic_loss)<1e-12,length(pool_review[[p]]$asset_pool_validation$violations)==0)
  raw<-list(list('origin','2024-01-01',list(assessment_hash=g$assessment$assessment_hash)),list('performing','2024-01-10',list(position_id=sid,status='PERFORMING')),list('delay','2024-01-15',list(unpaid_collateral_principal=20)),list('proposal','2024-01-16',list(status='PROPOSAL_ONLY',contract_changed=FALSE)),list('default','2024-02-01',list(episode_id=wd$variants[[p]]$episode$episode_id,ead=binding$position_ead)),list('settlement','2024-05-01',list(position_recovery=actual_claim_recovery,closed_loss=w$closed_workout$economic_loss)))
  dependencies<-list(list(assessment_id=g$assessment$assessment_id,dependency_type=gd$variants[[p]]$subject_type,dependency_id=sid))
  state<-NULL;history<-list()
  for(i in seq_along(raw)){
   item<-raw[[i]];kind<-item[[1]];date<-item[[2]];payload<-item[[3]]
   event<-list(event_id=paste0(p,'-',kind),revision='1',occurred_at=date,known_at=date,dependencies=list(list(type=gd$variants[[p]]$subject_type,id=sid)),payload=payload)
   execution<-apply_assessment_event(state,event,dependencies,'2024-05-02')$value;state<-execution$state;history<-c(history,list(event))
   events<-c(events,list(list(event_id=event$event_id,idempotency_key=event$event_id,occurred_at=date,known_at=date,sequence=i-1,payload=payload,payload_hash=content_fingerprint(payload)$value,revision_of=NULL)))
  }
  snapshots<-c(snapshots,list(list(snapshot_id=paste0(p,'-contracts'),kind='SOURCE',content_hash=content_fingerprint(list(governance=gd$variants[[p]],collateral=c,lifecycle=ld$variants[[p]],workout=wd$variants[[p]]))$value,known_at='2024-05-02',effective_from='2024-05-02',effective_until=NULL)))
  cohort_input<-gd$variants[[p]]$cohort_input;forecast<-cohort_input$predictions[[1]]
  forecast$forecast_id<-paste0(p,'-main-performing');forecast$case_id<-paste0(p,'-main');forecast$entity_id<-sid;forecast$cluster_id<-sid;forecast$forecast_at<-'2024-01-15';forecast$known_at<-'2024-01-15';forecast$pd<-g$performing_assessment$components[[1]]$value
  cohort_input$predictions<-list(forecast);cohort_input$outcome_events<-list(list(event_id=paste0(p,'-actual-default'),entity_id=sid,event_at='2024-02-01',known_at='2024-02-02',event='default',default_definition=cohort_input$target_contract$default_definition));cohort_input$coverage<-list(list(entity_id=sid,complete_through='2024-05-01',known_at='2024-05-02'));cohort_input$evaluation_cutoff<-'2024-05-02'
  cohort<-do.call(build_monitoring_cohorts,cohort_input)$value
  main_monitoring<-monitor_rating_cohort(cohort,group_by=list('cohort'),reference_window=list(start='2024-01-01',end='2024-01-31'),minimum_count=1,uncertainty='none')$value
  stopifnot(main_monitoring[[1]]$metrics$observed_rate==1,main_monitoring[[1]]$metrics$brier==.25)
  records[[p]]<-list(position_binding=binding,source_row=Filter(function(r)r$product_id==p,lineage$rows)[[1]],main_cohort_input=cohort_input,main_cohort=cohort,main_monitoring=main_monitoring,governance=g,lifecycle=lifecycle[[p]],workout=w,pool_validation=pool_review[[p]],claim_control=claim_control,event_state=state,events=history)
 }
 replay_g<-run_structured_governance(gd)$variants;replay_l<-run_structured_lifecycle(ld)$variants;replay_w<-run_structured_workout(wd)$variants;replay_p<-run_structured_pool_review(data$pools,verify_reference=FALSE)$products
 stopifnot(identical(governance,replay_g),identical(lifecycle,replay_l),identical(workout,replay_w),identical(pool_review,replay_p))
 decision_state<-function(g)g[setdiff(names(g),c('feature_drift','calibration_drift'))]
 original<-list();replayed<-list();audits<-list();all_snapshots<-list()
 for(i in seq_along(products)){
  p<-products[[i]];snap<-snapshots[[i]];state<-NULL
  deps<-list(list(assessment_id=governance[[p]]$assessment$assessment_id,dependency_type=gd$variants[[p]]$subject_type,dependency_id=gd$variants[[p]]$subject_id))
  for(event in records[[p]]$events)state<-apply_assessment_event(state,event,deps,'2024-05-02')$value$state
  replay_cohort<-do.call(build_monitoring_cohorts,records[[p]]$main_cohort_input)$value
  replay_monitoring<-monitor_rating_cohort(replay_cohort,group_by=list('cohort'),reference_window=list(start='2024-01-01',end='2024-01-31'),minimum_count=1,uncertainty='none')$value
  stopifnot(identical(replay_cohort,records[[p]]$main_cohort),identical(replay_monitoring,records[[p]]$main_monitoring))
  event_log<-Filter(function(e)startsWith(e$event_id,paste0(p,'-')),events);local_snapshots<-list(snap)
  contents<-list(MODEL=gd$variants[[p]]$model,CALIBRATION=list(method='NO_POST_CALIBRATION',version='1',target=gd$variants[[p]]$model$target),POLICY=governance[[p]]$policy)
  for(kind in names(contents))local_snapshots<-c(local_snapshots,list(list(snapshot_id=paste0(p,'-',kind),kind=kind,content_hash=content_fingerprint(contents[[kind]])$value,known_at='2023-12-31',effective_from='2024-01-01',effective_until=NULL)))
  all_snapshots<-c(all_snapshots,local_snapshots)
  hashes<-setNames(lapply(local_snapshots,function(x)x$content_hash),vapply(local_snapshots,function(x)x$snapshot_id,character(1)))
  base<-list(checkpoint_id=paste0(p,'-settled'),as_of='2024-05-02',known_at='2024-05-02',snapshot_hashes=hashes,applied_event_ids=lapply(records[[p]]$events,function(e)e$event_id),event_log_hash=content_fingerprint(event_log)$value)
  orig<-c(base,list(mode='ORIGINAL',state=list(governance=decision_state(governance[[p]]),lifecycle=lifecycle[[p]],workout=workout[[p]],pool=pool_review[[p]],main_monitoring=records[[p]]$main_monitoring,events=records[[p]]$event_state)))
  rep<-c(base,list(mode='REPLAY',state=list(governance=decision_state(replay_g[[p]]),lifecycle=replay_l[[p]],workout=replay_w[[p]],pool=replay_p[[p]],main_monitoring=replay_monitoring,events=state)))
  original<-c(original,list(orig));replayed<-c(replayed,list(rep))
  contract<-list(contract_id=paste0(p,'-actual-2024-position-history'),version='1',reference='Actual component reexecution with separate forecast and realized servicing',ordering='KNOWN_AT_SEQUENCE_EVENT_ID',idempotency='FIRST_IDENTICAL_DELIVERY',corrections='APPEND_REVISION',required_checkpoint_ids=list(base$checkpoint_id),absolute_tolerance=0,relative_tolerance=0)
  audits[[p]]<-replay_consistency(event_log,list(orig),list(rep),snapshot_manifest=local_snapshots,replay_contract=contract)$value
 }
 if(verify_reference){
  stopifnot(all(vapply(audits,function(a)a$decision$status=='PASS',logical(1))))
  for(r in records)stopifnot(r$governance$decision$action=='APPROVE',r$lifecycle$observation_windows[[1]]$target==1,is.null(r$workout$incomplete_workout$economic_lgd),r$governance$retired$state=='SUPERSEDED',r$pool_validation$asset_pool_validation$metrics$default_probability$brier>.63)
  stopifnot(records$SECURITIZATION_TRANCHE$claim_control$senior_recovery==25,records$COVERED_BOND$claim_control$estate_recovery==32.5,records$TRADE_RECEIVABLES$claim_control$debtor_recovery==70)
 }
 list(full_component_reexecution_exact=TRUE,products=records,lineage=lineage,replay_audits=audits,snapshots=all_snapshots,original_checkpoints=original,replayed_checkpoints=replayed,interpretation='New2024position and collateral ledgers bind governance, observed lifecycle and workouts. Historical2023product examples remain separate economic-contract references. The analytic30day position monitor and121day collateral default forecast are distinct targets; neither is treated as the other.')
}
