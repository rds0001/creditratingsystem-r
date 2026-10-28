# Native local severity fit and declared mortgage scenarios.
if(!exists("exposure_schedule"))library(creditratingsystem)
run_mortgage_risk<-function(data=NULL,verify_reference=TRUE){
 if(is.null(data))data<-jsonlite::fromJSON(system.file("extdata/mortgage_risk.json",package="creditratingsystem"),simplifyVector=FALSE)
 poolfit<-do.call(fit_retail_risk_pools,data$pool_fit)
 pool<-do.call(assign_retail_risk_pool,c(data$pool_assignment,list(fitted_model=poolfit)))$value
 a<-data$exposure;a$annual_interest_rates<-unlist(a$annual_interest_rates)
 exposure<-do.call(exposure_schedule,a)
 ead<-predict_ead(exposure,NULL,feature_order=list(),expected_target=NULL,reference_dates=as.list(rep(data$exposure$contract$as_of,2)),default_dates=data$exposure$period_end_dates,currency="EUR")
 a<-data$lgd_fit;a$X_train<-matrix(numeric(),nrow=length(a$X_train),ncol=0);a$lgd_targets<-unlist(a$lgd_targets);a$feature_order<-character()
 fit<-do.call(fit_fractional_lgd,a)
 severity<-predict_lgd(fit,matrix(numeric(),nrow=1,ncol=0),feature_order=character(),expected_target=data$lgd_fit$target,default_condition="given_default")
 pd<-marginal_pd_from_conditional(unlist(data$conditional_pd))$value
 loss<-unlist(ead$value$conditional_ead)*unlist(severity$value$mean_lgd)[1]
 lifetime<-lifetime_expected_loss(unlist(pd),loss,unlist(data$loss_discount_factors),c(1,2),currency="EUR",probability_measure="PHYSICAL")$value
 stress<-do.call(stress_affordability,data$stress)$value
 if(verify_reference){
  stopifnot(all(vapply(poolfit$model$parameters$segments[[1]]$pools,function(x)x$statistics$means$pd,numeric(1))==c(.25,.5)),pool$assignments[[1]]$score_bin==0)
  stopifnot(all(unlist(ead$value$conditional_ead)==c(95000,90000)),abs(unlist(severity$value$mean_lgd)[1]-.3)<1e-9,abs(lifetime$lifetime_expected_loss-1814.094)<1e-6)
  expected<-c(250,100000*.05/12,100000+100000*.05/12)
  for(i in 1:3)stopifnot(abs(stress$periods[[i]]$debt_service-expected[i])<1e-8)
  stopifnot(stress$first_liquidity_exhaustion=="2024-04-30")
 }
 list(data_origin=data$data_origin,pool_assignment=pool,exposure=exposure$value,ead=ead$value,conditional_lgd=severity$value,marginal_pd=pd,lifetime_loss=lifetime,stress=stress)
}
