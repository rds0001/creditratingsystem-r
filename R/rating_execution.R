# Data-only local execution. Empirical fits are immutable development artifacts.
.cr_rx_dated <- function(value,cutoff,label) {
 if(is.list(value)) {
  if(!is.null(names(value))) for(key in intersect(names(value),c('known_at','information_cutoff','registered_at'))) if(!is.null(value[[key]]) && .cr_data_date(value[[key]],paste0(label,'.',key))>cutoff) .cr_error('RATING_REQUEST_UNAVAILABLE',paste(label,'contains information after cutoff'))
  for(item in value) .cr_rx_dated(item,cutoff,label)
 }
 invisible(NULL)
}
.cr_rx_path <- function(value,path) {
 if(!is.list(path) || !is.null(names(path))) .cr_error('RATING_REFERENCE','Reference path requires array')
 for(key in path) {
  if(is.list(value) && !is.null(names(value)) && is.character(key) && length(key)==1L && key %in% names(value)) value<-value[[key]]
  else if(is.list(value) && is.null(names(value)) && is.numeric(key) && length(key)==1L && is.finite(key) && key==floor(key) && key>=0 && key<length(value)) value<-value[[key+1L]]
  else .cr_error('RATING_REFERENCE','Missing or incompatible reference path')
 }
 value
}
.cr_rx_references <- function(value) {
 found<-list()
 if(is.list(value)) {
  if(!is.null(names(value)) && '$ref' %in% names(value)) {
   if(!identical(names(value),'$ref') || !is.list(value[['$ref']])) .cr_error('RATING_REFERENCE','Reference must be exact data-only object')
   ref<-value[['$ref']]
   if(!setequal(names(ref),c('source','id','path')) || !ref$source %in% c('input','artifact','step','model') || !is.character(ref$id)||length(ref$id)!=1L) .cr_error('RATING_REFERENCE','Invalid reference schema')
   found<-list(ref)
  } else for(item in value) found<-c(found,.cr_rx_references(item))
 }
 found
}
.cr_rx_resolve <- function(value,sources) {
 .cr_rx_references(value)
 if(is.list(value)) {
  if(!is.null(names(value)) && '$ref' %in% names(value)) {
   ref<-value[['$ref']];table<-sources[[ref$source]]
   if(!ref$id %in% names(table)) .cr_error('RATING_REFERENCE','Reference identity unavailable')
   return(.cr_rx_path(table[[ref$id]],ref$path))
  }
  return(lapply(value,.cr_rx_resolve,sources=sources))
 }
 value
}
.cr_rx_typed <- function(value,kind) {
 if(kind=='numeric_matrix') {
  if(!is.list(value)||!length(value)||!is.null(names(value))||!all(vapply(value,is.list,logical(1)))||length(unique(vapply(value,length,integer(1))))!=1L) .cr_error('RATING_ARGUMENT','Numeric matrix requires rectangular array')
  flat<-unlist(value,use.names=FALSE)
 } else if(kind %in% c('numeric_vector','character_vector','logical_vector')) {
  if(!is.list(value)||!is.null(names(value))) .cr_error('RATING_ARGUMENT','Typed vectors require arrays')
  flat<-unlist(value,use.names=FALSE)
 } else .cr_error('RATING_ARGUMENT','Unsupported argument type')
 values<-if(kind=='numeric_matrix')unlist(value,recursive=FALSE,use.names=FALSE) else value
 valid<-if(startsWith(kind,'numeric'))vapply(values,function(x)is.numeric(x)&&length(x)==1L&&is.finite(x),logical(1)) else if(kind=='character_vector')vapply(values,function(x)is.character(x)&&length(x)==1L&&!is.na(x),logical(1))else vapply(values,function(x)is.logical(x)&&length(x)==1L&&!is.na(x),logical(1))
 if(!all(valid)) .cr_error('RATING_ARGUMENT','Typed argument contains another type')
 if(kind=='numeric_matrix')return(matrix(as.numeric(flat),nrow=length(value),byrow=TRUE))
 if(kind=='numeric_vector')return(as.numeric(flat))
 if(kind=='character_vector')return(as.character(flat))
 as.logical(flat)
}
.cr_rx_portable <- function(value) {
 if(is.matrix(value))return(lapply(seq_len(nrow(value)),function(i)as.list(unname(value[i,]))))
 if(is.data.frame(value))return(lapply(seq_len(nrow(value)),function(i)lapply(value,function(v)v[[i]])))
 if(is.list(value))return(lapply(value,.cr_rx_portable))
 if(length(value)!=1L && !is.null(value))return(as.list(value))
 value
}
.cr_rx_output <- function(value,shape) {
 if(shape=='vector' && is.atomic(value)&&!is.null(value))value<-as.list(value)
 value<-.cr_rx_portable(value)
 if(shape=='scalar') {if(is.list(value)&&is.null(names(value))&&length(value)==1L)value<-value[[1]];if(is.null(value)||is.list(value)) .cr_error('RATING_OUTPUT','Expected scalar output')}
 else if(shape=='vector') {if(!is.list(value)||!is.null(names(value))||any(vapply(value,is.list,logical(1)))) .cr_error('RATING_OUTPUT','Expected vector output')}
 else if(shape=='matrix') {if(!is.list(value)||!is.null(names(value))||!all(vapply(value,is.list,logical(1)))||length(unique(vapply(value,length,integer(1))))>1L) .cr_error('RATING_OUTPUT','Expected matrix output')}
 else if(shape=='object') {if(!is.list(value)||is.null(names(value))) .cr_error('RATING_OUTPUT','Expected object output')}
 else if(shape=='records') {if(!is.list(value)||!is.null(names(value))||any(!vapply(value,function(x)is.list(x)&&!is.null(names(x)),logical(1)))) .cr_error('RATING_OUTPUT','Expected record array')}
 else .cr_error('RATING_OUTPUT','Explicit output shape required')
 .cr_hash(value);value
}
.cr_rx_invoke <- function(method,arguments,argument_types) {
 spec<-.cr_rating_operations[[method]]
 if(is.null(spec)) .cr_error('RATING_RECIPE','Operation outside static catalogue')
 if(spec$stage %in% c('PRE_FITTED','LIFECYCLE')) .cr_error('RATING_RECIPE','Operation requires frozen artifact or lifecycle stage')
 args<-arguments
 for(key in names(argument_types)) {if(!key %in% names(args)) .cr_error('RATING_ARGUMENT','Typed argument absent');args[key]<-list(.cr_rx_typed(args[[key]],argument_types[[key]]))}
 if(method=='explainability.estimate_credit_shapley_values') {
  predictor<-args$predictor
  if(!is.list(predictor)||!setequal(names(predictor),c('kind','model','feature_order','expected_target'))||!identical(predictor$kind,'FROZEN_PD')) .cr_error('RATING_RECIPE','Explanation requires data-only FROZEN_PD predictor contract')
  if(!identical(args$model_contract$output_scale,'probability')) .cr_error('RATING_RECIPE','Frozen PD adapter outputs probability only')
  model<-predictor$model
  for(k in c('model_id','model_version','feature_order','target'))if(!identical(.cr_hash(args$model_contract[[k]]),.cr_hash(model[[k]]))) .cr_error('RATING_INTEGRITY','Explanation contract differs from predictor')
  args$predictor<-function(X)predict_pd(model,X,feature_order=unlist(predictor$feature_order),expected_target=predictor$expected_target)
 }
 function_value<-get0(spec$function_name,envir=environment(.cr_rx_invoke),mode='function',inherits=TRUE)
 if(is.null(function_value)) .cr_error('RATING_RECIPE','Operation unavailable in installed version')
 form<-formals(function_value);required<-names(form)[vapply(form,function(x)identical(x,quote(expr=)),logical(1))]
 if(any(!names(args) %in% names(form))||any(!required %in% names(args))) .cr_error('RATING_ARGUMENT','Arguments differ from operation signature')
 tryCatch(do.call(function_value,args),error=function(e){if(inherits(e,"creditratingsystem_error"))stop(e);.cr_error("RATING_EXECUTION","Operation rejected malformed data or failed numerically")})
}
.cr_rs_execute_pipeline <- function(request,entry) {
 recipe<-request$recipe;p<-.cr_rs_object(recipe$parameters,'pipeline parameters')
 if(!setequal(names(p),c('routing_rules','applicability_contract','steps','components','effects','transitions','event_dependencies','decision'))) .cr_error('RATING_RECIPE','Pipeline requires explicit routing execution composition lifecycle decision fields')
 if(!identical(.cr_hash(entry$bundle$configuration$assessment_recipe),.cr_hash(recipe))) .cr_error('RATING_INTEGRITY','Recipe must be pinned inside model bundle')
 inputs<-.cr_rs_object(request$input,'pipeline input');.cr_data_required(inputs,c('snapshot','datasets','assessment_id','assessment_known_at','calculated_at','lifecycle_events','events'))
 snapshot<-.cr_rs_object(inputs$snapshot,'snapshot');.cr_data_required(snapshot,c('as_of','known_at'))
 base_known<-.cr_data_date(inputs$assessment_known_at,'assessment_known_at');request_known<-.cr_data_date(request$known_at,'request.known_at');calculated<-.cr_data_date(inputs$calculated_at,'calculated_at')
 if(base_known>calculated||calculated>request_known) .cr_error('RATING_DATE_ORDER','Calculation must follow knowledge and precede history cutoff')
 .cr_rs_snapshot(snapshot,snapshot$as_of,inputs$assessment_known_at);.cr_rx_dated(snapshot,base_known,'snapshot')
 if(.cr_data_date(entry$registered_at,'registered_at')>base_known) .cr_error('RATING_MISSING_MODEL','Model unavailable at assessment cutoff')
 .cr_rx_dated(entry$bundle$model,base_known,'model')
 datasets<-.cr_rs_object(inputs$datasets,'datasets')
 for(key in names(datasets)) {dataset<-datasets[[key]];.cr_data_required(dataset,c('value','as_of','known_at'));.cr_rx_dated(dataset,base_known,paste0('dataset.',key));if(.cr_data_date(dataset$as_of,'dataset.as_of')>.cr_data_date(snapshot$as_of,'snapshot.as_of')) .cr_error('RATING_SNAPSHOT_DATE','Dataset economic date follows assessment')}
 artifacts<-entry$bundle$dependencies$artifacts;if(is.null(artifacts))artifacts<-.cr_empty_object();artifacts<-.cr_rs_object(artifacts,'artifacts');content<-.cr_empty_object();manifest<-list()
 for(identity in sort(names(artifacts),method='radix')) {artifact<-artifacts[[identity]];.cr_data_required(artifact,c('kind','content','sha256','known_at'));if(!identical(artifact$sha256,.cr_hash(artifact$content))) .cr_error('RATING_INTEGRITY','Frozen dependency hash differs');.cr_rx_dated(artifact,base_known,paste0('artifact.',identity));content[identity]<-list(artifact$content);manifest[[length(manifest)+1L]]<-list(artifact_id=identity,kind=artifact$kind,sha256=artifact$sha256,known_at=artifact$known_at)}
 sources<-list(input=datasets,artifact=content,step=.cr_empty_object(),model=list(primary=entry$bundle$model))
 rules<-.cr_rx_resolve(p$routing_rules,sources);contract<-.cr_rx_resolve(p$applicability_contract,sources)
 route<-route_rating_profile(snapshot,rules,snapshot$as_of,inputs$assessment_known_at)$value;applicable<-assess_rating_applicability(snapshot,contract,snapshot$as_of,inputs$assessment_known_at)$value
 if(route$status!='ROUTED'||applicable$status!='APPLICABLE') .cr_error('RATING_APPLICABILITY','Assessment lacks unique applicable profile route')
 steps<-.cr_data_rows(p$steps,'steps');.cr_data_unique(steps,'step_id','steps');pending<-setNames(steps,vapply(steps,function(s)s$step_id,character(1)));trace<-list();all_ids<-names(pending)
 for(step in steps) {.cr_data_required(step,c('step_id','method_id','arguments','argument_types','output_contract'));if(!step$method_id %in% names(.cr_rating_operations)) .cr_error('RATING_RECIPE','Unknown operation');for(ref in .cr_rx_references(step$arguments))if(ref$source=='step'&&!ref$id %in% all_ids) .cr_error('RATING_REFERENCE','Unknown predecessor')}
 while(length(pending)) {
  ready<-sort(names(pending)[vapply(pending,function(step)all(vapply(.cr_rx_references(step$arguments),function(ref)ref$source!='step'||ref$id %in% names(sources$step),logical(1))),logical(1))],method='radix')
  if(!length(ready)) .cr_error('RATING_RECIPE','Execution graph contains cycle')
  for(identity in ready) {
   step<-pending[[identity]];pending[[identity]]<-NULL;method<-step$method_id;op<-.cr_rating_operations[[method]];arguments<-.cr_rs_object(.cr_rx_resolve(step$arguments,sources),'step arguments');.cr_rx_dated(arguments,base_known,'step arguments')
   output_contract<-.cr_rs_object(step$output_contract,'output contract');.cr_data_required(output_contract,c('shape','unit'));if(!'target' %in% names(output_contract)) .cr_error('RATING_OUTPUT','Explicit nullable target required')
   if(op$stage=='PRE_FITTED') {
    refs<-.cr_rx_references(step$arguments)
    if(!identical(names(arguments),'artifact')||length(refs)!=1L||refs[[1]]$source!='artifact') .cr_error('RATING_RECIPE','Empirical fit requires one frozen artifact reference')
    frozen<-arguments$artifact;if(!is.list(frozen)||!identical(frozen$method_id,method)) .cr_error('RATING_RECIPE','Frozen artifact method differs')
    raw_value<-frozen;unit<-'artifact';result_context<-.cr_empty_object();diagnostics<-list(execution='FROZEN_DEVELOPMENT_ARTIFACT')
   } else {
    result<-.cr_rx_invoke(method,arguments,.cr_rs_object(step$argument_types,'argument types'));if(!inherits(result,'CalcResult')) .cr_error('RATING_OUTPUT','Operation must return structured result')
    raw_value<-result$value;unit<-result$unit;result_context<-result$context;diagnostics<-result$diagnostics
   }
   if(!identical(unit,output_contract$unit)) .cr_error('RATING_OUTPUT','Result unit differs from pinned contract')
   target<-output_contract$target
   if(!is.null(target)) {
    if(!is.list(target)||is.null(names(target))) .cr_error('RATING_OUTPUT','Target must be object or null')
    actual_target<-diagnostics$target;if(is.null(actual_target)&&is.list(raw_value))actual_target<-raw_value$target
    if(!is.null(actual_target)&&!identical(.cr_hash(actual_target),.cr_hash(target))) .cr_error('RATING_OUTPUT','Actual target differs')
    for(key in intersect(names(target),c('probability_measure','horizon','time_unit','event')))if(!is.null(diagnostics[[key]])&&!identical(.cr_hash(diagnostics[[key]]),.cr_hash(target[[key]]))) .cr_error('RATING_OUTPUT','Method diagnostics differ from target')
    for(key in names(target))if(!is.null(result_context[[key]])&&!identical(.cr_hash(result_context[[key]]),.cr_hash(target[[key]]))) .cr_error('RATING_OUTPUT','Result context differs from target')
   }
   value<-.cr_rx_output(raw_value,output_contract$shape);fields<-output_contract$fields;if(is.null(fields))fields<-list();for(field in .cr_data_rows(fields,'output fields')){.cr_data_required(field,c('path','unit'));if(!all(c('target','target_path') %in% names(field))) .cr_error('RATING_OUTPUT','Explicit field target and target_path required');.cr_rx_path(value,field$path);if(!is.null(field$target_path)&&!identical(.cr_hash(.cr_rx_path(value,field$target_path)),.cr_hash(field$target))) .cr_error('RATING_OUTPUT','Nested target differs from field contract')};if(anyDuplicated(vapply(fields,function(f).cr_hash(f$path),character(1)))) .cr_error('RATING_OUTPUT','Duplicate field contract path');sources$step[identity]<-list(list(value=value,unit=unit,target=target,fields=fields));trace[[length(trace)+1L]]<-list(step_id=identity,method_id=method,stage=op$stage,arguments_hash=.cr_hash(arguments),output_contract=output_contract,value_hash=.cr_hash(value),value=value)
  }
 }
 components<-.cr_rx_resolve(p$components,sources);effects<-.cr_rx_resolve(p$effects,sources)
 for(component in components) {original<-Filter(function(c)c$component_id==component$component_id,p$components)[[1]];for(ref in .cr_rx_references(original$value))if(ref$source=='step'){produced<-sources$step[[ref$id]];matching<-Filter(function(f)identical(.cr_hash(ref$path),.cr_hash(c(list('value'),f$path))),produced$fields);field<-if(length(matching))matching[[1]] else produced;target<-field$target;required_unit<-switch(component$quantity,PD='probability',LGD='loss_fraction',GRADE='grade',SCORE='score',component$currency);if(!identical(field$unit,required_unit)) .cr_error('RATING_OUTPUT','Component quantity or currency differs from producing field unit');if(component$quantity %in% c('PD','LGD','EAD','EL')&&!identical(.cr_hash(target),.cr_hash(component$definition))) .cr_error('RATING_OUTPUT','Component definition differs from producing target')}}
 binding<-list(profile_id=route$profile_id,profile_version=route$profile_version,model_id=request$model_id,model_version=request$model_version,model_hash=entry$model_hash,input_hash=.cr_hash(inputs),recipe_hash=.cr_hash(recipe),artifact_manifest_hash=.cr_hash(manifest))
 assessment<-compose_risk_assessment(inputs$assessment_id,list(subject_id=snapshot$subject_id,subject_type=snapshot$subject_type),components,binding,snapshot$as_of,inputs$assessment_known_at,inputs$calculated_at,effects=effects)$value
 lifecycle<-.cr_data_rows(inputs$lifecycle_events,'lifecycle events')
 applied_lifecycle<-.cr_empty_object()
 for(event in lifecycle) {.cr_rx_dated(event,request_known,'lifecycle event');if(event$event_id %in% names(applied_lifecycle)){if(!identical(applied_lifecycle[[event$event_id]],.cr_hash(event))) .cr_error('RATING_EVENT_CONFLICT','Lifecycle event ID has different content');next};assessment<-transition_assessment_state(assessment,event,.cr_rx_resolve(p$transitions,sources))$value;applied_lifecycle[[event$event_id]]<-.cr_hash(event)}
 event_state<-NULL;event_trace<-list()
 for(event in .cr_data_rows(inputs$events,'events')) {applied<-apply_assessment_event(event_state,event,.cr_rx_resolve(p$event_dependencies,sources),request$known_at)$value;event_state<-applied$state;event_trace[[length(event_trace)+1L]]<-list(event_id=event$event_id,revision=event$revision,status=applied$status,affected_assessment_ids=applied$affected_assessment_ids)}
 if(is.null(event_state))event_state<-.cr_rs_seal(list(schema_version='1.0',processed=list(),applications=list()),'state_hash')
 decision<-NULL;decision_hash<-NULL
 if(!is.null(p$decision)){
  arguments<-.cr_rx_resolve(p$decision,sources);.cr_rx_dated(arguments,request_known,'decision')
  if(!is.list(arguments)||is.null(names(arguments))) .cr_error('RATING_ARGUMENT','Decision arguments must be an object')
  form<-formals(apply_credit_decision_policy);required<-names(form)[vapply(form,function(x)identical(x,quote(expr=)),logical(1))]
  if(any(!names(arguments) %in% names(form))||any(!required %in% names(arguments))) .cr_error('RATING_ARGUMENT','Decision arguments differ from operation signature')
  risk<-arguments$risk
  if(!is.list(risk)||is.null(names(risk))) .cr_error('RATING_DECISION','Decision risk must be an object')
  if(!any(vapply(components,function(c)c$quantity=='PD'&&identical(.cr_hash(c$value),.cr_hash(risk$pd))&&identical(.cr_hash(c$definition),.cr_hash(risk$target)),logical(1)))) .cr_error('RATING_DECISION','Decision risk must equal composed PD and target')
  decision<-tryCatch(do.call(apply_credit_decision_policy,arguments)$value,error=function(e){if(inherits(e,'creditratingsystem_error'))stop(e);.cr_error('RATING_EXECUTION','Decision operation rejected malformed data or failed numerically')})
  decision<-.cr_rx_portable(decision);decision_hash<-.cr_hash(decision)
 }
 output<-list(schema_version='2.0',assessment=assessment,routing=route,applicability=applicable,steps=trace,artifact_manifest=manifest,event_state=event_state,event_trace=event_trace,decision=decision,execution_role='LOCAL_ASSESSMENT_NOT_EMPIRICAL_VALIDATION')
 list(row_id=request$row_id,status='OK',value=output,error_code=NULL,input_hash=.cr_hash(inputs),recipe_hash=.cr_hash(recipe),model_hash=entry$model_hash,result_hash=.cr_hash(output),assessment_hash=assessment$assessment_hash,event_state_hash=event_state$state_hash,artifact_manifest_hash=.cr_hash(manifest),innovation_hash=.cr_hash(Filter(function(r)r$kind=='innovations',manifest)),decision_hash=decision_hash)
}
