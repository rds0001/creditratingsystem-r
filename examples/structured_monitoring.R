run_structured_monitoring<-function(data=NULL,verify_reference=TRUE){
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/structured_monitoring.json',package='creditratingsystem'),simplifyVector=FALSE)
 out<-list()
 for(product in names(data$variants)){
  c<-data$variants[[product]]
  graph<-build_entity_graph(c$entities,list(),list(policy_id=paste0(product,'-legal'),version='1',edge_types=list('CROSS_DEFAULT')),as_of='2024-01-16',known_at='2024-01-16')
  policy<-list(policy_id=paste0(product,'-group'),version='1',edge_types=list('CROSS_DEFAULT'),direction_policy='mutual_only',transitive=TRUE)
  contract<-list(portfolio_id=paste0(product,'-holdings'),version='1',currency='EUR',as_of='2024-01-16',known_at='2024-01-16',horizon=30,time_unit='days',probability_measure='PHYSICAL',conditioning='initially_performing',event='first_default')
  portfolio<-prepare_credit_portfolio(c$positions,graph,policy,contract)$value
  concentration<-credit_concentration_measures(c$positions,weight_field='ead',group_fields='entity_id',weight_unit='EUR')$value
  cohort<-do.call(build_monitoring_cohorts,c$cohort_input)$value
  counts<-default_count_backtest(c$observation_panel,c$frozen_forecasts,'segment',unit='obligor',horizon_days=30,default_definition=c$default_definition,redefault_policy='exclude_previous_default',evaluation_cutoff='2024-05-02')$value
  immature<-default_count_backtest(c$observation_panel,c$frozen_forecasts,'segment',unit='obligor',horizon_days=30,default_definition=c$default_definition,redefault_policy='exclude_previous_default',evaluation_cutoff='2024-02-02')$value
  lgd<-lgd_backtest(c$loss_records,design=c$loss_design,workout_policy=list(method='closed_only',reference='Explicit closed-claim descriptive cohort; unfinished workouts excluded'),uncertainty='none')$value
  ead<-NULL
  if(c$ead_applicability=='APPLICABLE')ead<-ead_backtest(c$loss_records[1],design=c$loss_design,weighting_policy=list(method='equal',reference='Fully funded claim balance at actual default'),uncertainty='none')$value
  out[[product]]<-list(portfolio=portfolio,concentration=concentration,cohort=cohort,default_counts=counts,immature_default_counts=immature,lgd_backtest=lgd,ead_backtest=ead,ead_applicability=c$ead_applicability,ead_inapplicability_reason=c$ead_inapplicability_reason)
  if(verify_reference){
   stopifnot(concentration$dimensions[[1]]$hhi==.25,concentration$dimensions[[2]]$hhi==.375,counts$deduplicated_rows==1,counts$overall$observed_count==3,counts$overall$observed_defaults==2,counts$overall$expected_defaults==1.5,abs(counts$overall$observed_expected_ratio-4/3)<1e-12,immature$overall$observed_count==0,immature$overall$expected_defaults==0,lgd$coverage$included_count==1,lgd$coverage$exclusion_counts$OPEN_WORKOUT==1,abs(lgd$metrics$overall$count_weighted$bias-c$expected_lgd_bias)<1e-12)
   if(!is.null(ead))stopifnot(ead$metrics$ead$overall$selected_weighting$bias==0,ead$diagnostics$ccf_undefined_count==1)
  }
 }
 list(data_origin=data$data_origin,version=data$version,product=data$product,variants=out)
}
