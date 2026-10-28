# Conditional treatment economics, fitted open workouts and cohort monitoring.
run_wholesale_commitment_review <- function(data=NULL,verify_reference=TRUE,artifact_paths=NULL,engine_paths=NULL){
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/wholesale_commitment_review.json',package='creditratingsystem'),simplifyVector=FALSE)
 for(journey in data$monitoring$journeys){
  f<-journey$forecast_contract;path<-if(is.null(artifact_paths))system.file('extdata',f$model_resource,package='creditratingsystem')else artifact_paths[[f$model_resource]]
  model<-load_fitted_model(path);if(model$diagnostics$sha256!=f$model_hash)stop('Frozen forecast model hash mismatch')
  for(cohort in journey$cohorts){
   known<-min(vapply(cohort$predictions,function(x)x$known_at,character(1)))
   pd<-predict_pd(model,do.call(rbind,lapply(f$features,unlist)),feature_order=unlist(f$feature_order),expected_target=f$target,context=list(known_at=known))$value
   calibrated<-do.call(apply_calibrator,c(list(calibrator=f$calibrator,values=unlist(pd),target=f$target,base_model_version='1'),f$calibrator_arguments))$value[[1]]
   if(any(vapply(cohort$predictions,function(p)abs(p$pd-calibrated)>1e-12,logical(1)))||abs(journey$reference$cohort_pd-calibrated)>1e-12)stop('Monitoring forecast differs from pinned inference')
  }
 }
 result<-list(data_origin='SYNTHETIC');mapping<-c(review='specialized_risk_review',open_workout='specialized_open_workout',monitoring='specialized_monitoring')
 for(key in names(mapping)){
  example<-mapping[[key]];path<-if(is.null(engine_paths))system.file('examples',paste0(example,'.R'),package='creditratingsystem')else engine_paths[[key]]
  e<-new.env(parent=environment());sys.source(path,envir=e);result[[key]]<-e[[paste0('run_',example)]](data[[key]],verify_reference=verify_reference)
 }
 result
}
