# Observed default/cure histories, feature drift and mature cohort residuals.
run_specialized_monitoring <- function(data=NULL,verify_reference=TRUE) {
  if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata','specialized_monitoring.json',package='creditratingsystem'),simplifyVector=FALSE)
  out<-list()
  for(journey in data$journeys) {
    args<-journey$multistate;args$states<-unlist(args$states);args$allowed_transitions<-lapply(args$allowed_transitions,unlist)
    model<-do.call(estimate_multistate_transitions,args)
    curves<-predict_event_curves(model,list(),c(0,1,2,3),feature_order=character(),target=journey$multistate$target,event_states='default')$value
    args<-journey$feature_drift;args$feature_order<-unlist(args$feature_order);args$permutations<-do.call(rbind,lapply(args$permutations,unlist))
    feature<-do.call(detect_credit_feature_drift,args)$value
    args<-journey$pd_drift;sources<-list();expected<-list();state<-0
    for(observed in journey$cohorts) {
      cohort<-do.call(build_monitoring_cohorts,observed)$value;rows<-Filter(function(row)row$eligible,cohort$rows)
      residual<-sum(vapply(rows,function(row)row$target,numeric(1)))/length(rows)-mean(vapply(rows,function(row)row$pd,numeric(1)))
      args$residuals[[length(args$residuals)+1L]]<-list(time=observed$evaluation_cutoff,value=residual,mature=TRUE)
      sources[[length(sources)+1L]]<-list(evaluation_cutoff=observed$evaluation_cutoff,eligible_count=length(rows),defaults=sum(vapply(rows,function(row)row$target,numeric(1))),mean_pd=mean(vapply(rows,function(row)row$pd,numeric(1))),residual=residual)
    }
    args$null_paths<-do.call(rbind,lapply(args$null_paths,unlist));drift<-do.call(detect_pd_calibration_drift,args)$value
    for(count in journey$reference$defaults) {state<-.5*state+.5*(count/20-(if(is.null(journey$reference$cohort_pd)).1 else journey$reference$cohort_pd));expected[[length(expected)+1L]]<-abs(state)}
    if(verify_reference) {
      curve<-curves$curves[[1]];p<-journey$reference$first_hit
      stopifnot(max(abs(unlist(curve$first_hit_probability)-c(0,p,p,p)))<1e-12,max(abs(vapply(curve$state_occupancy,function(x)x[[2]],numeric(1))-c(0,p,0,p)))<1e-12,abs(feature[[1]]$statistic-journey$reference$feature_wasserstein)<1e-12,max(abs(unlist(drift$statistic_path)-unlist(expected)))<1e-12)
    }
    out[[length(out)+1L]]<-list(product=journey$product,case_id=journey$case_id,multistate=curves,feature_drift=feature,pd_drift=drift,residual_sources=sources,independent_ewma=expected)
  }
  list(data_origin='SYNTHETIC',forecast_reference=data$forecast_reference,journeys=out)
}
