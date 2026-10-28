# Different contractual horizons, independently observed cohorts and closed workouts.
run_wholesale_commitment_observed <- function(data=NULL,verify_reference=TRUE,artifact_paths=NULL,engine_path=NULL){
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/wholesale_commitment_observed.json',package='creditratingsystem'),simplifyVector=FALSE)
 if(is.null(engine_path))engine_path<-system.file('examples/specialized_observed_lifecycle.R',package='creditratingsystem')
 e<-new.env(parent=environment());sys.source(engine_path,envir=e);rows<-list()
 for(cohort in data$cohorts){
  for(journey in cohort$journeys){
   f<-journey$forecast_contract;path<-if(is.null(artifact_paths))system.file('extdata',f$model_resource,package='creditratingsystem')else artifact_paths[[f$model_resource]]
   model<-load_fitted_model(path)
   if(model$diagnostics$sha256!=f$model_hash)stop('Forecast model hash mismatch')
   pd<-predict_pd(model,do.call(rbind,lapply(f$features,unlist)),feature_order=unlist(f$feature_order),expected_target=f$target,context=list(known_at=cohort$cohort_contract$forecast_known_at))$value
   calibrated<-do.call(apply_calibrator,c(list(calibrator=f$calibrator,values=unlist(pd),target=f$target,base_model_version='1'),f$calibrator_arguments))$value[[1]]
   if(abs(calibrated-journey$reference_pd)>1e-12||any(vapply(journey$monitoring$predictions,function(x)abs(x$pd-calibrated)>1e-12||x$model_id!=model$value$model_id,logical(1))))stop('Observed cohort forecast differs from pinned inference')
   horizon<-f$target$horizon
   if(f$target$time_unit=='years'){start<-as.Date(cohort$cohort_contract$forecast_at);finish<-as.Date(paste0(as.integer(format(start,'%Y'))+horizon,format(start,'-%m-%d')));horizon<-as.numeric(finish-start)}
   if(horizon!=cohort$cohort_contract$horizon_days)stop('Observation and prediction horizons differ')
  }
  rows[[length(rows)+1L]]<-e$run_specialized_observed_lifecycle(cohort,verify_reference=verify_reference)
 }
 list(data_origin='SYNTHETIC',scope=data$scope,cohorts=rows)
}
