# Specialized exposure timing, portfolio preparation and observed loss review.
run_specialized_portfolio_review <- function(data=NULL,verify_reference=TRUE) {
  if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata','specialized_portfolio_review.json',package='creditratingsystem'),simplifyVector=FALSE)
  graph<-build_entity_graph(lapply(data$positions,function(p)list(entity_id=p$entity_id)),list(),data$graph_policy,as_of=data$portfolio_contract$as_of,known_at=data$portfolio_contract$known_at)
  portfolio<-prepare_credit_portfolio(data$positions,graph,data$default_link_policy,data$portfolio_contract)$value
  concentration<-credit_concentration_measures(data$positions,weight_field='ead',group_fields=list('sector'),weight_unit='EUR')$value
  lgd<-lgd_backtest(data$loss_records,design=data$design,workout_policy=data$workout_policy,uncertainty='none')$value
  ead<-ead_backtest(data$loss_records,design=data$design,weighting_policy=data$weighting_policy,uncertainty='none')$value
  exposures<-list()
  for(record in data$exposures) {
    args<-record$arguments;args$annual_interest_rates<-unlist(args$annual_interest_rates)
    before<-do.call(exposure_schedule,args)$value;args$default_timing<-'AFTER_PERIOD_END_EVENTS';after<-do.call(exposure_schedule,args)$value
    if(verify_reference)stopifnot(abs(before$schedule[[1]]$conditional_ead-record$reference_ead_before)<1e-12,abs(after$schedule[[1]]$conditional_ead-record$reference_ead_after)<1e-12)
    exposures[[length(exposures)+1L]]<-list(product=record$product,before=before,after=after)
  }
  total<-sum(vapply(data$positions,function(p)p$ead,numeric(1)));hhi<-sum(vapply(data$positions,function(p)(p$ead/total)^2,numeric(1)))
  lgd_bias<-sum(vapply(data$loss_records,function(x)(x$predicted_lgd-x$realized_lgd)*x$ead_at_default,numeric(1)))/sum(vapply(data$loss_records,function(x)x$ead_at_default,numeric(1)))
  ead_bias<-mean(vapply(data$loss_records,function(x)x$predicted_ead-x$realized_ead,numeric(1)))
  if(verify_reference)stopifnot(abs(concentration$dimensions[[1]]$hhi-hhi)<1e-12,abs(lgd$metrics$overall$ead_weighted$bias-lgd_bias)<1e-12,abs(ead$metrics$ead$overall$selected_weighting$bias-ead_bias)<1e-12)
  list(data_origin='SYNTHETIC',forecast_sources=data$forecast_sources,prepared_portfolio=portfolio,concentration=concentration,exposures=exposures,lgd_backtest=lgd,ead_backtest=ead,independent_oracle=list(hhi=hhi,lgd_bias=lgd_bias,ead_bias=ead_bias))
}
