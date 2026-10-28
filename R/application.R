# Application snapshots, economic deduplication and selection-aware risk.
.cr_app_object <- function(x,name) { if(!is.list(x)||is.null(names(x))) .cr_error('APPLICATION_SCHEMA',paste(name,'must be a named list')); x }
.cr_app_string <- function(x,name) { if(!is.character(x)||length(x)!=1L||is.na(x)||!nzchar(trimws(x))) .cr_error('APPLICATION_SCHEMA',paste(name,'must be a nonempty string')); x }
.cr_app_finite <- function(x,name,nonnegative=FALSE) { if(!is.numeric(x)||length(x)!=1L||!is.finite(x)||(nonnegative&&x<0)) .cr_error('APPLICATION_VALUE',paste(name,'must be finite and satisfy its sign constraint')); x }
.cr_app_apps <- function(applications) {
  apps<-.cr_data_rows(applications,'applications'); .cr_data_unique(apps,'application_id','applications')
  for(a in apps) { .cr_data_required(a,c('application_id','customer_id','product_id','population_id','application_at','information_cutoff','requested_terms')); if(.cr_data_date(a$application_at,'application_at')>.cr_data_date(a$information_cutoff,'information_cutoff')) .cr_error('APPLICATION_DATE_ORDER','Application after information cutoff'); .cr_app_object(a$requested_terms,'requested_terms') }; apps
}
.cr_app_snapshot <- function(snapshot) {
  if(inherits(snapshot,'CalcResult')) snapshot<-snapshot$value
  s<-.cr_app_object(snapshot,'snapshot'); expected<-s$snapshot_hash; s$snapshot_hash<-NULL
  if(is.null(expected)||!identical(.cr_hash(s),expected)) .cr_error('APPLICATION_SNAPSHOT_INTEGRITY','Snapshot missing hash or altered after freezing')
  if(!identical(s$schema_version,'1.0')) .cr_error('APPLICATION_SCHEMA','Unknown snapshot version'); s$snapshot_hash<-expected; s
}
build_application_snapshot <- function(applications,feature_records,term_events=NULL,required_features=NULL) {
  apps<-.cr_app_apps(applications); features<-.cr_data_rows(feature_records,'feature_records'); terms<-.cr_data_rows(if(is.null(term_events))list() else term_events,'term_events')
  app_ids<-vapply(apps,function(a)a$application_id,character(1)); required_features<-unlist(required_features)
  if(length(required_features)&&(!is.character(required_features)||anyDuplicated(required_features)||any(!nzchar(required_features)))) .cr_error('APPLICATION_SCHEMA','required_features must contain distinct names')
  for(r in features) {
    .cr_data_required(r,c('application_id','feature','observed_at','known_at','source_id')); .cr_app_string(r$feature,'feature')
    if(!('value' %in% names(r))) .cr_error('APPLICATION_SCHEMA','Feature requires value, possibly NULL')
    if(!(r$application_id %in% app_ids)) .cr_error('APPLICATION_FOREIGN_KEY','Feature refers to unknown application')
    if(.cr_data_date(r$known_at,'known_at')<.cr_data_date(r$observed_at,'observed_at')) .cr_error('APPLICATION_DATE_ORDER','Feature known before observation'); .cr_hash(r$value)
  }
  for(r in terms) {
    .cr_data_required(r,c('application_id','stage','terms','effective_at','known_at'))
    if(!(r$application_id %in% app_ids)) .cr_error('APPLICATION_FOREIGN_KEY','Terms refer to unknown application')
    if(!(r$stage %in% c('offered','accepted','booked'))) .cr_error('APPLICATION_SCHEMA','Invalid term stage')
    .cr_app_object(r$terms,'terms'); if(.cr_data_date(r$known_at,'known_at')<.cr_data_date(r$effective_at,'effective_at')) .cr_error('APPLICATION_DATE_ORDER','Term event known before effective date')
  }
  output<-list()
  for(i in seq_along(apps)) {
    a<-apps[[i]]; cutoff<-.cr_data_date(a$information_cutoff,'information_cutoff')
    candidates<-Filter(function(r)r$application_id==a$application_id&&.cr_data_date(r$known_at,'known_at')<=cutoff&&.cr_data_date(r$observed_at,'observed_at')<=cutoff,features)
    values<-.cr_empty_object(); sources<-.cr_empty_object()
    for(name in sort(unique(vapply(candidates,function(r)r$feature,character(1))))) {
      records<-Filter(function(r)r$feature==name,candidates); obs<-vapply(records,function(r)r$observed_at,character(1)); records<-records[obs==max(obs)]; kn<-vapply(records,function(r)r$known_at,character(1)); selected<-records[kn==max(kn)]
      if(length(selected)!=1L) .cr_error('APPLICATION_AMBIGUOUS_FEATURE',paste('Ambiguous feature',name))
      r<-selected[[1]]; values[name]<-list(r$value); sources[[name]]<-r[c('source_id','observed_at','known_at')]
    }
    stages<-list(requested=a$requested_terms,offered=NULL,accepted=NULL,booked=NULL); stage_sources<-.cr_empty_object()
    for(stage in c('offered','accepted','booked')) {
      records<-Filter(function(r)r$application_id==a$application_id&&r$stage==stage&&.cr_data_date(r$known_at,'known_at')<=cutoff&&.cr_data_date(r$effective_at,'effective_at')<=cutoff,terms)
      if(length(records)) { ef<-vapply(records,function(r)r$effective_at,character(1)); records<-records[ef==max(ef)]; kn<-vapply(records,function(r)r$known_at,character(1)); selected<-records[kn==max(kn)]; if(length(selected)!=1L) .cr_error('APPLICATION_AMBIGUOUS_TERMS','Ambiguous term revision'); stages[[stage]]<-selected[[1]]$terms; stage_sources[[stage]]<-selected[[1]][c('effective_at','known_at')] }
    }
    frozen<-c(list(schema_version='1.0'),a[c('application_id','customer_id','product_id','population_id','application_at','information_cutoff')],list(features=values,feature_sources=sources,terms=stages,term_sources=stage_sources,missing_features=as.list(required_features[vapply(required_features,function(f)is.null(values[[f]]),logical(1))])))
    frozen$snapshot_hash<-.cr_hash(frozen); output[[i]]<-frozen
  }
  .cr_result(output,'application.build_application_snapshot',diagnostics=list(hash_format='cr-canonical-v1',information_rule='observed_at and known_at <= information_cutoff'),limitations='Hashes detect mutation; returned in-memory objects are not write-protected.')
}

assemble_joint_application <- function(application_id,parties,income_items,obligation_items,currency) {
  .cr_app_string(application_id,'application_id'); .cr_app_string(currency,'currency'); parties<-.cr_data_rows(parties,'parties'); .cr_data_unique(parties,'party_id','parties'); pmap<-list()
  for(p in parties) { .cr_data_required(p,c('party_id','role','liability_share')); if(!(p$role %in% c('borrower','co_borrower','guarantor'))) .cr_error('APPLICATION_SCHEMA','Unsupported role'); share<-.cr_app_finite(p$liability_share,'liability_share'); if(share<0||share>1) .cr_error('APPLICATION_VALUE','Liability share outside [0,1]'); pmap[[p$party_id]]<-p }
  borrowers<-vapply(Filter(function(p)p$role!='guarantor',parties),function(p)p$party_id,character(1)); if(!length(borrowers)) .cr_error('APPLICATION_SCHEMA','Need at least one borrower')
  aggregate<-function(items,name) {
    unique<-list(); excluded<-list()
    for(r in .cr_data_rows(items,name)) {
      .cr_data_required(r,c('economic_id','party_id','amount_monthly','currency'))
      if(!(r$party_id %in% names(pmap))) .cr_error('APPLICATION_FOREIGN_KEY','Item refers to unknown party')
      amount<-.cr_app_finite(r$amount_monthly,'amount_monthly',TRUE); if(r$currency!=currency) .cr_error('APPLICATION_CURRENCY','Item currency mismatch')
      if(!(r$party_id %in% borrowers)) { excluded[[length(excluded)+1L]]<-list(economic_id=r$economic_id,party_id=r$party_id,reason='guarantor_only'); next }
      key<-r$economic_id
      if(!is.null(unique[[key]])&&unique[[key]]$amount_monthly!=amount) .cr_error('APPLICATION_AMBIGUOUS_ECONOMIC_ITEM','Shared economic ID has conflicting total amounts')
      if(is.null(unique[[key]])) unique[[key]]<-list(economic_id=key,amount_monthly=amount,party_ids=list())
      if(!(r$party_id %in% unlist(unique[[key]]$party_ids))) unique[[key]]$party_ids<-c(unique[[key]]$party_ids,list(r$party_id))
    }
    rows<-unname(unique[sort(names(unique))]); rows<-lapply(rows,function(r) {r$party_ids<-as.list(sort(unlist(r$party_ids)));r}); list(rows=rows,excluded=excluded)
  }
  income<-aggregate(income_items,'income_items'); debts<-aggregate(obligation_items,'obligation_items'); households<-unlist(lapply(parties,function(p)p$household_id))
  .cr_result(list(application_id=application_id,currency=currency,monthly_income=sum(vapply(income$rows,function(r)r$amount_monthly,numeric(1))),monthly_debt_service=sum(vapply(debts$rows,function(r)r$amount_monthly,numeric(1))),parties=parties,income_contributions=income$rows,obligation_contributions=debts$rows,excluded_items=c(income$excluded,debts$excluded),dependency_flags=list(shared_household=length(unique(households))<length(households),shared_income=any(vapply(income$rows,function(r)length(r$party_ids)>1,logical(1))),shared_obligation=any(vapply(debts$rows,function(r)length(r$party_ids)>1,logical(1))))),'application.assemble_joint_application',limitations=c('Item amounts are total economic amounts, not liability-share-weighted amounts; guarantor resources are excluded.','No aggregation of individual PDs; no inferred household identities.'))
}

build_application_targets <- function(applications,decisions,bookings,default_events,exits,horizon_days,evaluation_cutoff,performance_origin='first_draw_at') {
  apps<-.cr_app_apps(applications); decisions<-.cr_data_rows(decisions,'decisions'); bookings<-.cr_data_rows(bookings,'bookings'); cutoff<-.cr_data_date(evaluation_cutoff,'evaluation_cutoff'); .cr_data_days(horizon_days,'horizon_days',1)
  if(!(performance_origin %in% c('booked_at','first_draw_at'))) .cr_error('APPLICATION_SCHEMA','Invalid performance origin')
  app_ids<-vapply(apps,function(a)a$application_id,character(1)); app_map<-setNames(apps,app_ids)
  if(any(vapply(apps,function(a).cr_data_date(a$information_cutoff,'information_cutoff')>cutoff,logical(1)))) .cr_error('APPLICATION_COHORT_CUTOFF','Application information cutoff extends beyond evaluation cutoff')
  build_observation_windows(list(),default_events,exits,horizon_days,evaluation_cutoff,entity_key='facility_id')
  .cr_data_unique(decisions,'decision_id','decisions'); .cr_data_unique(bookings,'application_id','bookings'); .cr_data_unique(bookings,'facility_id','bookings')
  for(d in decisions) {
    .cr_data_required(d,c('decision_id','application_id','status','decided_at','known_at'))
    if(!(d$application_id %in% app_ids)||!(d$status %in% c('approved','rejected','pending','withdrawn'))) .cr_error('APPLICATION_SCHEMA','Invalid decision application/status')
    if(.cr_data_date(d$decided_at,'decided_at')<.cr_data_date(app_map[[d$application_id]]$application_at,'application_at')) .cr_error('APPLICATION_DATE_ORDER','Decision precedes application')
    if(.cr_data_date(d$known_at,'known_at')<.cr_data_date(d$decided_at,'decided_at')) .cr_error('APPLICATION_DATE_ORDER','Decision known before occurrence')
    if(!is.null(d$accepted_at)) { if(d$status!='approved') .cr_error('APPLICATION_SELECTION_CONFLICT','Acceptance requires an approved decision'); .cr_data_required(d,'accepted_known_at'); if(.cr_data_date(d$accepted_at,'accepted_at')<.cr_data_date(d$decided_at,'decided_at')||.cr_data_date(d$accepted_known_at,'accepted_known_at')<.cr_data_date(d$accepted_at,'accepted_at')) .cr_error('APPLICATION_DATE_ORDER','Invalid acceptance dates') }
  }
  for(b in bookings) {
    .cr_data_required(b,c('application_id','facility_id','booked_at','known_at')); if(!(b$application_id %in% app_ids)) .cr_error('APPLICATION_FOREIGN_KEY','Booking refers to unknown application')
    if(.cr_data_date(b$known_at,'known_at')<.cr_data_date(b$booked_at,'booked_at')) .cr_error('APPLICATION_DATE_ORDER','Booking known before occurrence')
    if(!is.null(b$first_draw_at)) { .cr_data_required(b,'first_draw_known_at'); if(.cr_data_date(b$first_draw_at,'first_draw_at')<.cr_data_date(b$booked_at,'booked_at')||.cr_data_date(b$first_draw_known_at,'first_draw_known_at')<.cr_data_date(b$first_draw_at,'first_draw_at')) .cr_error('APPLICATION_DATE_ORDER','Invalid first-draw dates') }
  }
  out<-list()
  for(i in seq_along(apps)) {
    a<-apps[[i]]; ds<-Filter(function(d)d$application_id==a$application_id&&.cr_data_date(d$known_at,'known_at')<=cutoff,decisions); decision<-NULL
    if(length(ds)) { ef<-vapply(ds,function(d)d$decided_at,character(1)); ds<-ds[ef==max(ef)]; kn<-vapply(ds,function(d)d$known_at,character(1)); selected<-ds[kn==max(kn)]; if(length(selected)!=1L) .cr_error('APPLICATION_AMBIGUOUS_DECISION','Multiple decisions tie'); decision<-selected[[1]] }
    bs<-Filter(function(b)b$application_id==a$application_id&&.cr_data_date(b$known_at,'known_at')<=cutoff,bookings); booking<-if(length(bs))bs[[1]] else NULL
    approved<-!is.null(decision)&&decision$status=='approved'; accepted<-approved&&!is.null(decision$accepted_at)&&.cr_data_date(decision$accepted_known_at,'accepted_known_at')<=cutoff
    if(!is.null(booking)&&!approved) .cr_error('APPLICATION_SELECTION_CONFLICT','Known booking requires known approved decision')
    origin_known<-!is.null(booking)&&(performance_origin=='booked_at'||!is.null(booking$first_draw_at)&&.cr_data_date(booking$first_draw_known_at,'first_draw_known_at')<=cutoff)
    if(!is.null(booking)&&!accepted) .cr_error('APPLICATION_SELECTION_CONFLICT','Known booking requires known acceptance')
    if(!is.null(booking)&&(.cr_data_date(booking$booked_at,'booked_at')<.cr_data_date(decision$accepted_at,'accepted_at')||.cr_data_date(booking$known_at,'known_at')<.cr_data_date(decision$accepted_known_at,'accepted_known_at'))) .cr_error('APPLICATION_DATE_ORDER','Booking precedes occurrence or knowledge of acceptance')
    selection<-if(origin_known)'funded' else if(!is.null(booking))'booked_not_drawn' else if(accepted)'accepted_not_booked' else if(approved)'approved_not_taken_up' else if(!is.null(decision))decision$status else 'pending_decision'
    base<-list(application_id=a$application_id,customer_id=a$customer_id,product_id=a$product_id,information_cutoff=a$information_cutoff,selection_status=selection,approved=approved,accepted=accepted,booked=!is.null(booking),funded=origin_known,performance_origin=performance_origin,facility_id=if(is.null(booking))NULL else booking$facility_id)
    if(origin_known) {
      rating<-list(observation_id=a$application_id,facility_id=booking$facility_id,as_of=a$information_cutoff,performance_start=booking[[performance_origin]],funded=TRUE)
      for(key in c('followup_end','outcome_available_at')) if(key %in% names(booking)) rating[key]<-list(booking[[key]])
      window<-build_observation_windows(list(rating),default_events,exits,horizon_days,evaluation_cutoff,entity_key='facility_id')$value[[1]]
      base<-c(base,window[c('performance_start','outcome_end','observed_until','target','censored','eligible','label_status','event_id')]); base$mature<-.cr_data_date(window$outcome_end,'outcome_end')<=cutoff
    } else base<-c(base,list(performance_start=NULL,outcome_end=NULL,observed_until=NULL,target=NULL,censored=FALSE,eligible=FALSE,label_status='not_funded',event_id=NULL,mature=FALSE))
    base$label_observed<-!is.null(base$target); out[[i]]<-base
  }
  .cr_result(out,'application.build_application_targets',unit='binary',diagnostics=list(target='funded_facility_first_default',performance_origin=performance_origin,horizon_days=horizon_days),limitations='Unfunded counterfactual loan outcomes remain missing; first-default excludes previously defaulted facilities.')
}

.cr_app_design <- function(snapshot,model,target,override_terms=NULL) {
  s<-.cr_app_snapshot(snapshot); target<-.cr_app_object(target,'target'); model<-.cr_model(model); contract<-model$application_contract
  if(!is.list(contract)) .cr_error('APPLICATION_MODEL_CONTRACT','Model requires application_contract')
  for(name in c('product_ids','population_ids')) { ids<-unlist(contract[[name]]); if(!is.character(ids)||!length(ids)||any(!nzchar(ids))) .cr_error('APPLICATION_MODEL_CONTRACT',paste('Missing applicable',name)) }
  if(!(s$product_id %in% unlist(contract$product_ids))||!(s$population_id %in% unlist(contract$population_ids))) .cr_error('APPLICATION_APPLICABILITY','Product or population outside model applicability')
  if(!(contract$performance_origin %in% c('booked_at','first_draw_at'))||!identical(target$performance_origin,contract$performance_origin)) .cr_error('APPLICATION_TARGET','Target origin mismatch')
  if(!identical(.cr_hash(target),.cr_hash(model$target))) .cr_error('APPLICATION_TARGET','Complete target must match model')
  if(!identical(target$probability_measure,'PHYSICAL')) .cr_error('APPLICATION_TARGET','Application inference requires physical probability')
  stage<-contract$term_stage; if(!(stage %in% c('requested','offered','accepted','booked'))) .cr_error('APPLICATION_MODEL_CONTRACT','Explicit term_stage required')
  mapping<-contract$term_features
  if(!is.list(mapping)||is.null(names(mapping))||any(!vapply(mapping,function(v)is.character(v)&&length(v)==1L,logical(1)))||anyDuplicated(unlist(mapping))) .cr_error('APPLICATION_MODEL_CONTRACT','term_features must map distinct term columns to features')
  if(any(!unlist(mapping) %in% unlist(model$feature_order))) .cr_error('APPLICATION_MODEL_CONTRACT','Term feature absent from model')
  terms<-if(!is.null(override_terms))override_terms else s$terms[[stage]]
  if(length(mapping)&&!is.list(terms)) .cr_error('APPLICATION_MISSING_TERMS',paste('No known',stage,'terms'))
  values<-s$features
  for(field in names(mapping)) {
    feature<-mapping[[field]]; if(is.null(terms[[field]])) .cr_error('APPLICATION_MISSING_TERMS',paste('Missing term',field))
    if(feature %in% names(values)) .cr_error('APPLICATION_FEATURE_COLLISION','Contract term features collide with customer features')
    values[[feature]]<-terms[[field]]
  }
  missing<-Filter(function(f)is.null(values[[f]]),model$feature_order)
  if(length(missing)) .cr_error('APPLICATION_MISSING_FEATURES','Required features missing',list(features=missing))
  x<-vapply(model$feature_order,function(f).cr_app_finite(values[[f]],f),numeric(1))
  list(snapshot=s,model=model,contract=contract,x=x)
}
.cr_app_predict <- function(snapshot,model,target,calibrator=NULL,offset=NULL,override_terms=NULL) {
  design<-.cr_app_design(snapshot,model,target,override_terms); s<-design$snapshot; model<-design$model
  raw<-as.numeric(predict_pd(model,matrix(design$x,nrow=1),feature_order=unlist(model$feature_order),expected_target=target,offset=offset)$value[1]); pd<-raw
  if(!is.null(calibrator)) {
    artifact<-if(inherits(calibrator,'CalcResult'))calibrator$value else calibrator
    if(identical(artifact$calibration_type,'isotonic')&&!identical(artifact$metadata$input_scale,'probability')) .cr_error('APPLICATION_CALIBRATION_SCALE','Isotonic calibrator must declare input_scale=probability')
    pd<-as.numeric(apply_calibrator(calibrator,c(raw),target=target,base_model_version=model$model_version)$value[1])
  }
  list(application_id=s$application_id,snapshot_hash=s$snapshot_hash,pd=pd,unadjusted_pd=raw,calibrated=!is.null(calibrator),model_id=model$model_id,model_version=model$model_version,target=target,product_id=s$product_id,population_id=s$population_id,term_stage=design$contract$term_stage,feature_coverage=list(required=length(model$feature_order),observed=length(design$x),missing=list()))
}
predict_application_risk <- function(snapshot,model,target,calibrator=NULL,offset=NULL) {
  .cr_result(.cr_app_predict(snapshot,model,target,calibrator,offset),'application.predict_application_risk',unit='probability',limitations='Risk inference is separate from affordability, eligibility and credit approval.')
}
compare_offer_risk <- function(snapshot,offers,model,target,support_bounds,calibrator=NULL,lgd=NULL,offset=NULL) {
  s<-.cr_app_snapshot(snapshot); artifact<-.cr_model(model); contract<-artifact$application_contract; mapping<-contract$term_features
  if(!identical(contract$term_stage,'offered')||!is.list(mapping)||!length(mapping)) .cr_error('APPLICATION_OFFER_MODEL','Need explicit offered-term conditional model')
  bounds<-.cr_app_object(support_bounds,'support_bounds'); if(!setequal(names(bounds),names(mapping))) .cr_error('APPLICATION_SUPPORT','Need bounds for all modelled terms')
  for(field in names(bounds)) { pair<-unlist(bounds[[field]]); if(length(pair)!=2L||.cr_app_finite(pair[1],field)>.cr_app_finite(pair[2],field)) .cr_error('APPLICATION_SUPPORT','Support bounds must be ordered [lower,upper]') }
  offers<-.cr_data_rows(offers,'offers'); .cr_data_unique(offers,'offer_id','offers'); if(!is.null(lgd)&&(.cr_app_finite(lgd,'lgd')<0||lgd>1)) .cr_error('APPLICATION_VALUE','LGD must lie in [0,1]')
  output<-list()
  for(i in seq_along(offers)) {
    offer<-offers[[i]]; .cr_data_required(offer,c('offer_id','terms')); terms<-.cr_app_object(offer$terms,'offer.terms')
    if(!setequal(names(terms),names(mapping))) .cr_error('APPLICATION_OFFER_MODEL','Every scenario term must be explicitly modelled')
    extra<-character(); for(field in names(bounds)) { pair<-unlist(bounds[[field]]); v<-.cr_app_finite(terms[[field]],field); if(v<pair[1]||v>pair[2]) extra<-c(extra,field) }
    risk<-.cr_app_predict(s,artifact,target,calibrator,offset,override_terms=terms); el<-NULL
    if(!is.null(lgd)) { if(!('amount' %in% names(terms))) .cr_error('APPLICATION_OFFER_MODEL','EL requires explicit amount as scenario EAD'); .cr_app_string(s$terms$requested$currency,'requested_terms.currency for EL'); el<-risk$pd*lgd*.cr_app_finite(terms$amount,'amount',TRUE) }
    output[[i]]<-list(offer_id=offer$offer_id,terms=terms,pd=risk$pd,unadjusted_pd=risk$unadjusted_pd,expected_loss=el,currency=if(is.null(lgd))NULL else s$terms$requested$currency,extrapolation=length(extra)>0,outside_support_terms=as.list(extra),snapshot_hash=s$snapshot_hash)
  }
  .cr_result(output,'application.compare_offer_risk',diagnostics=list(support_bounds=bounds,comparison='fixed_customer_conditional_scenarios',ead_assumption=if(is.null(lgd))NULL else 'offered amount'),limitations=c('Offer changes are conditional model scenarios, not identified causal effects or take-up predictions.','Bounds are user-supplied empirical support; marginal bounds do not establish joint support.','Optional EL treats offered amount as fixed EAD and supplied LGD as constant.'))
}
