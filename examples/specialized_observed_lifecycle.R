# Observed specialized-credit cohorts and legally closed workout accounting.
run_specialized_observed_lifecycle <- function(data=NULL,verify_reference=TRUE) {
  if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata','specialized_governance.json',package='creditratingsystem'),simplifyVector=FALSE)
  contract<-list(forecast_at='2023-01-01',forecast_known_at='2023-01-01',horizon_days=731,outcome_known_at='2025-01-01',evaluation_cutoff='2025-01-02',reference_window=list(start='2023-01-01',end='2023-12-31'),total_count=22,mature_count=20,censored_count=2,reporting_currency='EUR')
  for(k in names(data$cohort_contract))contract[[k]]<-data$cohort_contract[[k]]
  out<-list()
  for(journey in data$journeys) {
    observed<-journey$observed
    windows<-do.call(build_observation_windows,journey$observation_windows)$value
    cohort<-do.call(build_monitoring_cohorts,journey$monitoring)
    summary<-monitor_rating_cohort(cohort,group_by=list('cohort'),reference_window=contract$reference_window,minimum_count=10,uncertainty='iid_homogeneous_binomial')$value
    args<-journey$recovery_episodes
    args$closures<-list(list(closure_id=paste0(journey$case_id,'-legal-close'),episode_id=args$default_events[[1]]$episode_id,closed_at=observed$closure_at,known_at=observed$closure_known_at,closure_reason='SETTLEMENT'))
    episodes<-do.call(build_recovery_episodes,args)$value
    recovery<-do.call(discount_recoveries,journey$recovery_value)
    closed<-workout_lgd(observed$ead,recovery,episode_status='CLOSED',default_date=observed$default_at,evaluation_cutoff=observed$closure_known_at,currency=if(is.null(observed$currency))contract$reporting_currency else observed$currency)$value
    incomplete<-workout_lgd(observed$ead,recovery,episode_status='OPEN',default_date=observed$default_at,evaluation_cutoff=observed$closure_known_at,currency=if(is.null(observed$currency))contract$reporting_currency else observed$currency)$value
    panel<-lapply(windows,function(row) {row$prior_default<-row$label_status=='prior_default';row$default_definition<-'first_default';row$outcome_known_at<-contract$outcome_known_at;row$grade<-'B';row})
    forecasts<-lapply(panel,function(row)list(observation_id=row$observation_id,pd=journey$reference_pd,forecast_at=contract$forecast_at,known_at=contract$forecast_known_at,default_definition='first_default',horizon_days=contract$horizon_days,unit='obligor',probability_measure='PHYSICAL',model_id=journey$generation_model_id,model_version='1'))
    counts<-default_count_backtest(panel,forecasts,list('grade'),unit='obligor',horizon_days=contract$horizon_days,default_definition='first_default',redefault_policy='exclude_previous_default',evaluation_cutoff=contract$evaluation_cutoff)$value
    days<-as.numeric(as.Date(observed$settlement_at)-as.Date(observed$default_at))
    oracle_pv<-(observed$gross_recovery-observed$direct_cost)/(1+observed$annual_discount_rate)^(days/365)
    oracle_lgd<-1-oracle_pv/observed$ead
    if(verify_reference) {
      stopifnot(length(windows)==contract$total_count,sum(vapply(windows,function(w)w$censored,logical(1)))==contract$censored_count,sum(vapply(windows,function(w)if(is.null(w$target))0 else w$target,numeric(1)))==journey$cohort_defaults)
      stopifnot(counts$overall$observed_count==contract$mature_count,counts$overall$observed_defaults==journey$cohort_defaults,summary[[1]]$metrics$n==contract$mature_count)
      pd<-journey$reference_pd;n<-contract$mature_count;d<-journey$cohort_defaults
      stopifnot(abs(summary[[1]]$metrics$brier-(d*(1-pd)^2+(n-d)*pd^2)/n)<1e-12,episodes$episodes[[1]]$status=='closed',abs(recovery$value$discounted_net_recoveries-oracle_pv)<1e-12,abs(closed$economic_lgd-oracle_lgd)<1e-12,is.null(incomplete$economic_lgd))
    }
    out[[length(out)+1L]]<-list(product=journey$product,case_id=journey$case_id,observed_source=observed$source,windows=windows,cohort_summary=summary,default_backtest=counts,recovery_episodes=episodes,discounted_recoveries=recovery$value,closed_workout=closed,incomplete_workout=incomplete,independent_oracle=list(discount_days=days,present_value=oracle_pv,lgd=oracle_lgd))
  }
  list(data_origin='SYNTHETIC',scope='Observed monitoring, default counts and closed settlement accounting; simulation labels never treated as observations',journeys=out)
}
