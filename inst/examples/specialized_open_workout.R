# Actual residual settlement fits; ultimate-value oracle under declared flat curve.
run_specialized_open_workout <- function(data=NULL,verify_reference=TRUE) {
  if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata','specialized_open_workout.json',package='creditratingsystem'),simplifyVector=FALSE)
  out<-list()
  for(journey in data$journeys) {
    args<-journey$fit;args$X_train<-matrix(numeric(),nrow=length(args$episodes),ncol=0);args$feature_order<-character()
    fitted<-do.call(fit_recovery_hurdle,args)
    args<-journey$inference;args$X<-matrix(numeric(),nrow=length(args$episodes),ncol=0);args$feature_order<-character();args$fitted_recovery_model<-fitted
    prediction<-do.call(estimate_open_workout_lgd,args)$value;episode<-prediction$episodes[[1]]
    if(verify_reference)stopifnot(abs(episode$expected_final_lgd-journey$reference$expected_final_lgd)<1e-10,length(journey$fit$episodes)==16,sum(vapply(journey$fit$episodes,function(x)is.null(x$resolution_date),logical(1)))==4)
    out[[length(out)+1L]]<-list(product=journey$product,case_id=journey$case_id,training=journey$fit$training,stage_spec=journey$fit$stage_spec,reference=journey$reference,ead_at_default=episode$ead_at_default,known_discounted_net_cashflows=episode$known_discounted_net_cashflows,expected_final_loss=episode$expected_final_loss,expected_final_lgd=episode$expected_final_lgd,ratio_status=episode$ratio_status)
  }
  list(data_origin='SYNTHETIC',scope='Ultimate flat-curve financial quantities from actual hurdle and censored Weibull fits; timing-grid acceptance is separate',journeys=out)
}
