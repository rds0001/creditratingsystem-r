# Seven observed sector cohorts use the shared dated-workout engine.
run_wholesale_sector_observed <- function(data=NULL,verify_reference=TRUE,engine_path=NULL,artifact_paths=NULL) {
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/wholesale_sector_observed.json',package='creditratingsystem'),simplifyVector=FALSE)
 if(is.null(engine_path))engine_path<-system.file('examples/specialized_observed_lifecycle.R',package='creditratingsystem')
 for(journey in data$journeys) {
  f<-journey$forecast_contract;path<-if(is.null(artifact_paths))system.file('extdata',f$model_resource,package='creditratingsystem')else artifact_paths[[f$model_resource]]
  model<-load_fitted_model(path);if(model$diagnostics$sha256!=f$model_hash)stop('Forecast model hash differs from frozen source')
  pd<-predict_pd(model,do.call(rbind,lapply(f$features,unlist)),feature_order=unlist(f$feature_order),expected_target=f$target,context=list(known_at=data$cohort_contract$forecast_known_at))$value
  calibrated<-apply_calibrator(f$calibrator,pd,target=f$target,base_model_version='1')$value[[1]]
  if(any(vapply(journey$monitoring$predictions,function(x)abs(x$pd-calibrated)>1e-12||x$model_id!=model$value$model_id,logical(1)))||abs(calibrated-journey$reference_pd)>1e-12)stop('Observation forecast differs from pinned model and calibration')
 }
 engine<-new.env(parent=environment());sys.source(engine_path,envir=engine)
 engine$run_specialized_observed_lifecycle(data,verify_reference=verify_reference)
}
