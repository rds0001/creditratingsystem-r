# Actual competing-exit fits and conditional specialized workout choices.
run_specialized_risk_review <- function(data=NULL,verify_reference=TRUE) {
  if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata','specialized_risk_review.json',package='creditratingsystem'),simplifyVector=FALSE)
  out<-list()
  for(journey in data$journeys) {
    args<-journey$survival; args$X<-matrix(numeric(),nrow=length(args$start_stop_panel),ncol=0); args$feature_order<-character(); args$causes<-unlist(args$causes)
    fit<-do.call(fit_cause_specific_hazards,args)
    curve<-predict_event_curves(fit,list(list(path_id=journey$case_id,start=0,stop=2,covariates=list(),stratum='all',covariate_time=0)),c(0,1,2),feature_order=character(),target=journey$survival$target)$value
    treatment<-do.call(evaluate_treatment_candidates,journey$treatment)$value
    ref<-journey$survival_reference;h<-(ref$defaults+ref$exits)/ref$risk_set;mass<-1-exp(-h)
    oracle<-list(default_incidence=mass*ref$defaults/(ref$defaults+ref$exits),exit_incidence=mass*ref$exits/(ref$defaults+ref$exits),survival=exp(-h))
    values<-lapply(journey$treatment$treatment_candidates,function(candidate) {
      accepted<-sum(vapply(candidate$cashflows,function(flow)(flow$expected_recovery-flow$expected_cost)*flow$discount_factor,numeric(1)))-candidate$accepted_upfront_cost
      candidate$uptake_probability*accepted+(1-candidate$uptake_probability)*candidate$rejected_present_value-candidate$offer_cost
    })
    if(verify_reference) {
      cc<-curve$curves[[1]]
      stopifnot(abs(cc$cumulative_incidence$default[[2]]-oracle$default_incidence)<1e-12,abs(cc$cumulative_incidence$exit[[2]]-oracle$exit_incidence)<1e-12,abs(oracle$default_incidence+oracle$exit_incidence+oracle$survival-1)<1e-12)
      expected_by_id<-setNames(values,vapply(journey$treatment$treatment_candidates,function(x)x$treatment_id,character(1)))
      for(candidate in treatment$candidates)stopifnot(abs(candidate$expected_present_value-expected_by_id[[candidate$treatment_id]])<1e-12)
    }
    out[[length(out)+1L]]<-list(product=journey$product,case_id=journey$case_id,historical_training=journey$survival$training,competing_curves=curve,treatment=treatment,independent_oracle=list(competing=oracle,treatment_present_values=values))
  }
  list(data_origin='SYNTHETIC',scope='Historical competing-exit fits and conditional treatment economics, not identified causal effects or executed arrangements',journeys=out)
}
