# Sector-specific statements, native fits and dated support/override evidence.
fit_sector_case_models <- function(case,target) {
  training<-case$score_training
  prep<-fit_preprocessor(case$preprocessor_training,list(list(name='risk_ratio',type='numeric',imputation='error',scaling='standard')),target=target,training=training$training,model_id=paste0(case$case_id,'-preprocessor'),model_version='1')
  score<-fit_scorecard(do.call(rbind,lapply(training$X,unlist)),unlist(training$y),feature_order=unlist(training$feature_order),fit_spec=training$fit_spec,target=target,training=training$training,model_id=paste0(case$case_id,'-scorecard'),model_version='1',weights=unlist(training$weights))
  cal<-case$calibration;calibrator<-calibrate_logistic(unlist(cal$values),unlist(cal$outcomes),unlist(cal$weights),metadata=cal$metadata,tolerance=cal$fit_controls$tolerance,max_iterations=cal$fit_controls$max_iterations)
  list(preprocessor=prep,scorecard=score,calibrator=calibrator)
}
run_wholesale_sector_financial <- function(data=NULL,verify_reference=TRUE,return_fitted_models=FALSE) {
  if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata','wholesale_sector_financial.json',package='creditratingsystem'),simplifyVector=FALSE)
  out<-list();raw<-list();target<-data$target;as_of<-data$as_of;known<-data$known_at
  for(case in data$cases) {
    sid<-case$case_id;left<-list(list(entity_id=sid,as_of=as_of,known_at=known))
    selected<-join_as_of(left,case$statement_versions)$value[[1]];lateleft<-left;lateleft[[1]]$known_at<-'2024-04-02';later<-join_as_of(lateleft,case$statement_versions)$value[[1]]
    ratio<-build_financial_ratios(list(selected$statement),case$ratio_spec)$value[[1]]$ratios$risk_ratio$value
    late_ratio<-build_financial_ratios(list(later$statement),case$ratio_spec)$value[[1]]$ratios$risk_ratio$value
    fitted<-fit_sector_case_models(case,target)
    transformed<-transform_features(list(list(risk_ratio=ratio)),fitted$preprocessor)
    fundamental<-score_scorecard(fitted$scorecard,transformed$value$design_matrix,feature_order='risk_ratio',expected_target=target)
    calibrated<-apply_calibrator(fitted$calibrator,fundamental$value$pd,target=target,base_model_version='1');unrounded<-calibrated$value[[1]]
    if(verify_reference)stopifnot(abs(fundamental$value$pd[[1]]-case$reference$pd)<data$precision_contract$raw_fit_oracle_absolute_tolerance,abs(unrounded-case$reference$pd)<data$precision_contract$raw_fit_oracle_absolute_tolerance,abs(ratio-case$reference$ratio)<1e-12,abs(late_ratio-case$reference$late_ratio)<1e-12,selected$release_id=='initial',later$release_id=='revision')
    pd<-round(unrounded,data$precision_contract$pd_decimal_places)
    base<-list(entity_id=sid,pd=pd,stage='STANDALONE',target=target,applied_effects=list());parent<-list(entity_id=paste0(sid,'-sponsor'),pd=.02,stage='STANDALONE',target=target,applied_effects=list());joint<-min(pd*.25,.01)
    supported<-apply_parent_support(base,parent,case$support,joint_default_probability=joint)$value
    grade<-map_pd_to_grade(supported$pd,case$rating_scale)$value
    constrained<-apply_rating_constraints(list(pd=supported$pd,grade=grade),case$rating_scale,case$constraints,representative_pd_policy='representative_on_binding')$value
    request<-list(override_id=paste0(sid,'-review'),target='rating_grade',value='C',reason='Explicit synthetic qualitative deterioration review; no silent continuous-PD adjustment',author='local_analyst',approved_by='local_reviewer',recorded_at='2024-03-01',valid_from='2024-03-01',valid_until='2024-04-01')
    policy<-list(policy_id=paste0(sid,'-override-policy'),version='1',targets=list(rating_grade=list(path=list('grade'),kind='grade',allowed_values=list('A','B','C'))),require_dual_control=TRUE)
    override<-apply_override(constrained,request,policy,as_of=as_of,known_at=known)$value;expired<-apply_override(constrained,request,policy,as_of='2024-04-01',known_at='2024-04-01')$value
    snapshot<-list(subject_id=sid,subject_type='obligor',as_of=as_of,known_at=known,attributes=list(sector_variant=list(value=case$variant,observed_at=as_of,known_at=known),financial_evidence_verified=list(value=TRUE,observed_at=as_of,known_at=known)))
    rule<-list(rule_id=sid,profile_id=sid,profile_version='1',priority=1,effective_from='2023-01-01',effective_to=NULL,known_at='2023-01-01',conditions=list(list(field='sector_variant',operator='eq',value=case$variant),list(field='financial_evidence_verified',operator='eq',value=TRUE)))
    route<-route_rating_profile(snapshot,list(rule),as_of,known)$value;snapshot$attributes$financial_evidence_verified$value<-FALSE;negative<-route_rating_profile(snapshot,list(rule),as_of,known)$value
    if(verify_reference)stopifnot(abs(supported$pd-(pd-(pd-joint)*case$reference$rescue_success))<1e-12,override$status=='APPLIED',expired$status=='EXPIRED',negative$status=='NO_ROUTE',override$current$pd==constrained$pd)
    transformed_value<-transformed$value;transformed_value$design_matrix<-lapply(seq_len(nrow(transformed_value$design_matrix)),function(i)as.list(unname(transformed_value$design_matrix[i,])));transformed_value$feature_order<-as.list(transformed_value$feature_order)
    out[[length(out)+1L]]<-list(product=case$product,variant=case$variant,case_id=sid,economic_definition=case$economic_definition,selected_release=selected$release_id,later_release=later$release_id,ratio=ratio,later_ratio=late_ratio,transformed=transformed_value,reported_standalone_pd=pd,supported=supported,constrained=constrained,override=override,expired_override=expired,profile=route$profile_id,missing_evidence_status=negative$status)
    if(return_fitted_models)raw[[length(raw)+1L]]<-list(case_id=sid,preprocessor=fitted$preprocessor,scorecard=fitted$scorecard,calibrator=fitted$calibrator,fundamental=fundamental,calibrated=calibrated)
  }
  report<-list(data_origin='SYNTHETIC',precision_contract=data$precision_contract,cases=out)
  if(return_fitted_models)list(report=report,raw_native_fit_details=raw) else report
}
