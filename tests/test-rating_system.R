if(!exists('route_rating_profile')) library(creditratingsystem)
rs_error<-function(code,expr) {e<-tryCatch({force(expr);NULL},error=identity);stopifnot(inherits(e,'creditratingsystem_error'),identical(e$code,code))}
rs_empty<-structure(list(),names=character())
rs_model<-function(version='v1',coefficient=0)list(schema_version='1.0',model_id='M',model_version=version,method_id='classification.fit_binary_glm',feature_order=list('x'),parameters=list(link='logit',fit_intercept=TRUE,coefficients=list(coefficient,1),offset_required=FALSE),preprocessing=list(transformation='identity'),target=list(event='first_default',horizon=1,time_unit='years',probability_measure='PHYSICAL',conditioning='PIT'),training=list(data_fingerprint='reference',split_id='train',known_at='2023-12-01'),diagnostics=rs_empty,provenance=list(data_origin='REFERENCE',parameter_origin='USER_SUPPLIED'))
rs_registry<-function()register_rating_model_version(NULL,list(model=rs_model(),dependencies=rs_empty,configuration=rs_empty),list(model_id='M',model_version='v1',registered_at='2023-12-15'))$value
rs_snapshot<-function()list(subject_id='O',subject_type='obligor',as_of='2024-01-01',known_at='2024-01-02',attributes=list(income=list(value=2000,observed_at='2023-12-31',known_at='2024-01-01'),product=list(value='consumer',observed_at='2024-01-01',known_at='2024-01-01')))
rs_rules<-function() {
 base<-list(profile_version='1',priority=1,effective_from='2024-01-01',effective_to='2025-01-01',known_at='2023-12-01')
 list(c(base,list(rule_id='low',profile_id='P-low',conditions=list(list(field='income',operator='lt',value=2000)))),c(base,list(rule_id='high',profile_id='P',conditions=list(list(field='income',operator='ge',value=2000)))))
}
rs_composition<-function()list(assessment_id='A',subject=list(subject_id='O',subject_type='obligor'),components=list(list(component_id='PD-base',quantity='PD',value=0.1,subject_id='O',subject_type='obligor',variant='standalone',definition=rs_model()$target)),binding=list(profile_id='P',profile_version='1',model_id='M',model_version='v1',model_hash=rs_registry()$entries[[1]]$model_hash,input_hash='input-content-hash'),as_of='2024-01-01',known_at='2024-01-02',calculated_at='2024-01-03')
rs_request<-function(id='row1')list(row_id=id,known_at='2024-01-02',model_id='M',model_version='v1',model_hash=rs_registry()$entries[[1]]$model_hash,recipe=list(method='binary_glm',version='1.0',parameters=list(expected_target=rs_model()$target)),input=list(X=list(list(0),list(1)),feature_order=list('x')))
base<-list(version='1',effective_from='2023-01-01',effective_to=NULL,known_at='2023-01-01',attributes=rs_empty)
tables<-list(customer=list(c(list(subject_id='C'),base)),obligor=list(c(list(subject_id='O1'),base),c(list(subject_id='O2'),base)),facility=list(c(list(subject_id='F'),base)))
edge<-list(effective_from='2023-01-01',effective_to=NULL,known_at='2023-01-01',relation='legal_obligor',from_type='customer',from_id='C',to_type='obligor')
links<-list(c(edge,list(link_id='1',to_id='O1')),c(edge,list(link_id='2',to_id='O2')),c(edge,list(link_id='3',to_id='MISSING')))
out<-build_rating_subjects(tables,links,'2024-01-01','2024-01-02')$value
stopifnot(length(out$subjects)==4,length(out$links)==2,identical(out$unresolved_links,list(list(link_id='3',code='UNRESOLVED_ENDPOINT'))))
next_version<-tables$obligor[[1]];next_version$version<-'2';tables$obligor[[3]]<-next_version
out<-build_rating_subjects(tables,links,'2024-01-01','2024-01-02')$value
stopifnot(out$conflicts[[1]]$code=='OVERLAPPING_VERSIONS',!any(vapply(out$subjects,function(n)n$subject_id=='O1',logical(1))))
out<-route_rating_profile(rs_snapshot(),rs_rules(),'2024-01-01','2024-01-02')$value
stopifnot(out$profile_id=='P',identical(out$candidate_rule_ids,list('high')))
tied<-rs_rules();dup<-tied[[2]];dup$rule_id<-'tie';tied[[3]]<-dup
stopifnot(route_rating_profile(rs_snapshot(),tied,'2024-01-01','2024-01-02')$value$status=='AMBIGUOUS')
s<-rs_snapshot();s$attributes$income$known_at<-'2024-01-03'
stopifnot(route_rating_profile(s,rs_rules(),'2024-01-01','2024-01-02')$value$status=='NO_ROUTE')
s<-rs_snapshot();s$as_of<-'2025-01-01';s$known_at<-'2025-01-01'
stopifnot(route_rating_profile(s,rs_rules(),'2025-01-01','2025-01-01')$value$status=='NO_ROUTE')
app_contract<-list(contract_id='C',version='1',required_features=list('income'),allowed_values=list(product=list('consumer')),support_bounds=list(income=list(1000,5000)),maximum_age_days=list(income=30))
stopifnot(assess_rating_applicability(rs_snapshot(),app_contract,'2024-01-01','2024-01-02')$value$status=='APPLICABLE')
s<-rs_snapshot();s$attributes$income['value']<-list(NULL)
stopifnot(assess_rating_applicability(s,app_contract,'2024-01-01','2024-01-02')$value$status=='REFER')
s$attributes$product$value<-'mortgage'
stopifnot(assess_rating_applicability(s,app_contract,'2024-01-01','2024-01-02')$value$status=='NOT_APPLICABLE')
r<-rs_registry();bundle<-r$entries[[1]]$bundle;registration<-r$entries[[1]]$registration
same<-register_rating_model_version(r,bundle,registration)
stopifnot(same$diagnostics$idempotent,identical(same$value,r))
bundle$model$parameters$coefficients[[1]]<-10
rs_error('RATING_VERSION_CONFLICT',register_rating_model_version(r,bundle,registration))
bad<-r;bad$entries[[1]]$bundle$model$parameters$coefficients[[1]]<-10
rs_error('RATING_INTEGRITY',select_effective_model(bad,list(),'P','PD','2024-01-01','2024-01-02'))
newer<-register_rating_model_version(r,list(model=rs_model('v2',2),dependencies=rs_empty,configuration=rs_empty),list(model_id='M',model_version='v2',registered_at='2024-01-01'))$value
assignment<-list(assignment_id='old',profile_id='P',purpose='PD',model_id='M',model_version='v1',model_hash=r$entries[[1]]$model_hash,effective_from='2024-01-01',effective_to='2024-06-01',known_at='2023-12-15')
stopifnot(select_effective_model(newer,list(assignment),'P','PD','2024-01-01','2024-01-02')$value$selected$model_version=='v1',select_effective_model(newer,list(assignment),'P','PD','2024-06-01','2024-06-01')$value$status=='EXPIRED_MODEL',select_effective_model(newer,list(assignment),'P','PD','2023-12-20','2023-12-20')$value$status=='NOT_YET_EFFECTIVE')
args<-rs_composition();support<-args$components[[1]];support$component_id<-'PD-support';support$variant<-'supported';support$value<-0.05;args$components[[2]]<-support
args$effects<-list(list(effect_id='support',kind='support',order=1,level='obligor',input_component_id='PD-base',output_component_id='PD-support',reason='Explicit separate support calculation'))
a<-do.call(compose_risk_assessment,args)$value
stopifnot(a$composition_status=='CONSISTENT',a$components[[1]]$value==0.1)
bad<-args$components[[1]];bad$component_id<-'EL';bad$quantity<-'EL';bad$value<-100;bad$currency<-'EUR';bad$definition$horizon<-2;args$components[[3]]<-bad
stopifnot(do.call(compose_risk_assessment,args)$value$composition_status=='CONFLICT')
args$effects[[2]]<-list(effect_id='cycle',kind='override',order=2,level='obligor',input_component_id='PD-support',output_component_id='PD-base',reason='Invalid cycle')
rs_error('RATING_EFFECT_CONFLICT',do.call(compose_risk_assessment,args))
a<-do.call(compose_risk_assessment,rs_composition())$value
p<-list(maximum_age_days=30,maximum_information_age_days=30,trigger_types=list('income_change'))
stopifnot(check_assessment_freshness(a,p,list(),'2024-01-31','2024-01-31')$value$status=='CURRENT',check_assessment_freshness(a,p,list(),'2024-02-01','2024-02-01')$value$status=='STALE')
e<-list(event_id='E',subject_id='O',type='income_change',occurred_at='2023-12-31',known_at='2024-01-05')
stopifnot(check_assessment_freshness(a,p,list(e),'2024-01-04','2024-01-04')$value$status=='CURRENT',check_assessment_freshness(a,p,list(e),'2024-01-05','2024-01-05')$value$status=='STALE')
transitions<-list(list(from_state='DRAFT',to_state='REVIEWED',event_type='review',required_gates=list('checked')),list(from_state='REVIEWED',to_state='FINAL',event_type='finalize',required_gates=list('approved_local')),list(from_state='FINAL',to_state='SUPERSEDED',event_type='replace',required_gates=list()))
e<-list(event_id='E',type='review',occurred_at='2024-01-04',known_at='2024-01-04',reason='Evidence checked',gates=list(checked=FALSE))
rs_error('RATING_GATE',transition_assessment_state(a,e,transitions));e$gates$checked<-TRUE;b<-transition_assessment_state(a,e,transitions)$value
stopifnot(a$state=='DRAFT',b$state=='REVIEWED',identical(b$history[[1]]$previous_assessment_hash,a$assessment_hash))
e$event_id<-'F';e$type<-'finalize';e$gates<-list(approved_local=TRUE);final<-transition_assessment_state(b,e,transitions)$value
e$event_id<-'S';e$type<-'replace';e$gates<-rs_empty;superseded<-transition_assessment_state(final,e,transitions)$value
stopifnot(final$state=='FINAL',superseded$state=='SUPERSEDED',length(superseded$history)==3)
one<-rs_request('B');two<-rs_request('A');bad<-rs_request('C');bad$model_hash<-'wrong'
a<-rate_subject_batch(list(one,two,bad),r,'batch','2024-01-03')$value;b<-rate_subject_batch(list(bad,two,one),r,'batch','2024-01-03')$value
stopifnot(identical(a,b),identical(vapply(a$rows,function(row)row$row_id,character(1)),c('A','B','C')),a$rows[[1]]$value[[1]]==0.5,abs(a$rows[[1]]$value[[2]]-1/(1+exp(-1)))<1e-15,a$rows[[3]]$error_code=='RATING_INTEGRITY',is.null(a$rows[[3]]$value))
e<-list(event_id='payment',revision='1',occurred_at='2024-01-01',known_at='2024-01-02',dependencies=list(list(type='facility',id='F')),payload=list(amount=100));deps<-list(list(assessment_id='A',dependency_type='facility',dependency_id='F'),list(assessment_id='B',dependency_type='facility',dependency_id='G'))
a<-apply_assessment_event(NULL,e,deps,'2024-01-02')$value;b<-apply_assessment_event(a$state,e,deps,'2024-01-02')$value
stopifnot(identical(a$affected_assessment_ids,list('A')),identical(a$state,b$state),b$status=='DUPLICATE')
wrong<-e;wrong$payload$amount<-10
rs_error('RATING_EVENT_CONFLICT',apply_assessment_event(a$state,wrong,deps,'2024-01-02'))
wrong$revision<-'2';wrong$corrects_revision<-'1';wrong$known_at<-'2024-01-03';corrected<-apply_assessment_event(a$state,wrong,deps,'2024-01-03')$value
stopifnot(length(corrected$state$processed)==2,corrected$state$processed[[1]]$event$payload$amount==100)
q<-rs_request();batch<-rate_subject_batch(list(q),r,'original','2024-01-03')$value
newer<-register_rating_model_version(r,list(model=rs_model('v2',10),dependencies=rs_empty,configuration=rs_empty),list(model_id='M',model_version='v2',registered_at='2024-06-01'))$value
replay<-replay_assessment_history(list(q),newer,batch$rows,'2024-01-03')$value
stopifnot(replay$status=='REPRODUCED',identical(replay$batch$rows[[1]]$value,batch$rows[[1]]$value))
missing<-replay_assessment_history(list(q),NULL,batch$rows,'2024-01-03')$value
stopifnot(missing$status=='NOT_REPRODUCIBLE',missing$findings[[1]]$code=='RATING_MISSING_MODEL')
q$input$X[[1]][[1]]<-2;changed<-replay_assessment_history(list(q),r,batch$rows,'2024-01-03')$value
stopifnot(identical(changed$findings[[1]]$fields,list('input_hash','result_hash')))
cat('rating system native business cases passed\n')

# Application recipe dispatch equals the actual public single-subject calculation.
m<-rs_model();m$target$performance_origin<-'first_draw_at'
m$application_contract<-list(product_ids=list('consumer'),population_ids=list('new'),performance_origin='first_draw_at',term_stage='requested',term_features=rs_empty)
app_registry<-register_rating_model_version(NULL,list(model=m,dependencies=rs_empty,configuration=rs_empty),list(model_id='M',model_version='v1',registered_at='2023-12-15'))$value
apps<-list(list(application_id='A',customer_id='C',product_id='consumer',population_id='new',application_at='2024-01-01',information_cutoff='2024-01-01',requested_terms=rs_empty))
facts<-list(list(application_id='A',feature='x',value=0,observed_at='2024-01-01',known_at='2024-01-01',source_id='reference'))
s<-build_application_snapshot(apps,facts)$value[[1]]
q<-list(row_id='A',known_at='2024-01-01',model_id='M',model_version='v1',model_hash=app_registry$entries[[1]]$model_hash,input=list(snapshot=s),recipe=list(method='application_risk',version='1.0',parameters=list(target=m$target)))
batch<-rate_subject_batch(list(q),app_registry,'applications','2024-01-02')$value
stopifnot(identical(batch$rows[[1]]$value,predict_application_risk(s,m,m$target)$value),batch$rows[[1]]$value$pd==0.5)
q$known_at<-'2023-12-31'
stopifnot(rate_subject_batch(list(q),app_registry,'bad','2024-01-02')$value$rows[[1]]$error_code=='RATING_REQUEST_UNAVAILABLE')
