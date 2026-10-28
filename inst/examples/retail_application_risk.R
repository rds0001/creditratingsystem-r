# Same six original applications: prior-known conditional 30-day credit losses.
library(creditratingsystem)
.rar_helper<-function(){if(!exists('rj_fit',mode='function')){p<-system.file('examples','retail_application_journey.R',package='creditratingsystem');if(!nzchar(p))p<-'creditratingsystem-r/examples/retail_application_journey.R';sys.source(p,envir=environment(.rar_helper))}}
.rar_one<-function(rows,key,value){z<-Filter(function(r)identical(r[[key]],value),rows);if(length(z)!=1)stop('Expected one identity');z[[1]]}
.rar_evaluate<-function(product,c,r,target,chain=NULL,verify_reference=TRUE){
 app<-.rar_one(c$applications,'application_id',r$application_id);D<-app$information_cutoff;key<-app$application_id
 if(app$customer_id!=r$customer_id||r$account_id!=paste0('facility-',key)||r$assumptions_known_at>D)stop('Same application identity and prior-known assumptions required')
 if(r$horizon_end!='2025-01-30'||r$proposed_funding_at!='2024-12-31'||target$horizon!=30)stop('Exact30day proposed funding window required')
 expected_ead<-if(c$terms$kind=='OPERATING_RENT')c$terms$rent else app$requested_terms$amount
 if(r$conditional_ead!=expected_ead)stop('Claim differs from requested principal or unpaid rent')
 if(!is.null(r$asset)&&c$terms$kind=='OPERATING_RENT'&&r$waterfall$claims[[1]]$subject_id==app$customer_id)stop('Lessor investment cannot become borrower debt')
 if(is.null(chain)){
  .rar_helper();fitted<-rj_fit(product,c,target);feature<-.rar_one(c$features,'application_id',key);if(feature$known_at>D)stop('Future feature')
  snapshot<-build_application_snapshot(list(app),list(feature),required_features='driver')$value[[1]];pd<-predict_application_risk(snapshot,fitted$model,target,calibrator=fitted$calibrator)$value$pd
 }else{origin<-chain$products[[product]];row<-Filter(function(a)a$application$application_id==key,origin$applications)[[1]];if(!identical(row$application,app)||!identical(row$risk$target,target))stop('Upstream origin identity/target mismatch');fitted<-origin$fitted;pd<-row$risk$pd}
 prior<-lapply(r$prior_workouts,function(w){
  if(!(w$default_date<=w$recovery_date&&w$recovery_date<=w$closed_at&&w$closed_at<=w$known_at&&w$known_at<D)||w$entity_id==app$customer_id)stop('Independent completed prior workouts required')
  discounted<-discount_recoveries(list(list(cashflow_id=w$episode_id,cashflow_date=w$recovery_date,known_at=w$known_at,currency='EUR',kind='RECOVERY',amount=w$recovery)),list(anchor_date=w$default_date,known_at=w$default_date,discount_basis='Declared synthetic zero-rate prior valuation',points=list(list(cashflow_date=w$recovery_date,discount_factor=w$discount_factor))),list(reporting_currency='EUR',known_at=w$default_date,conversion_basis='cashflow_date',rates=list(list(currency='EUR',date=w$recovery_date,rate=1))),default_date=w$default_date,evaluation_cutoff=w$known_at,reporting_currency='EUR')
  workout_lgd(w$ead,discounted,episode_status='CLOSED',default_date=w$default_date,evaluation_cutoff=w$known_at,currency='EUR')$value
 })
 lt<-list(event='economic_loss',horizon=30,time_unit='days',probability_measure='PHYSICAL',conditioning='given_default');known<-max(vapply(r$prior_workouts,function(w)w$known_at,''))
 model<-fit_fractional_lgd(matrix(numeric(),length(prior),0),vapply(prior,function(w)w$economic_lgd,0.0),feature_order=character(),link='logit',covariance='HC0',target=lt,training=list(known_at=known,data_fingerprint=content_fingerprint(r$prior_workouts)$value,split_id=paste0(product,'-prior-workouts'),data_origin='SYNTHETIC',workout_population='closed_defaulted_episodes'),model_id=paste0(product,'-claim-lgd'),model_version='1')
 lgd<-predict_lgd(model,matrix(numeric(),1,0),feature_order=character(),expected_target=lt,default_condition='given_default')$value$mean_lgd[[1]]
 schedule<-exposure_schedule(list(facility_id=r$account_id,currency='EUR',as_of=r$proposed_funding_at,drawn_principal=r$conditional_ead,credit_limit=r$conditional_ead,accrued_interest=0),list(),0,period_end_dates=r$horizon_end,day_count='ACT/365F',default_timing='BEFORE_PERIOD_END_EVENTS',allow_overlimit=FALSE,currency='EUR')
 ead<-predict_ead(schedule,NULL,feature_order=character(),expected_target=NULL,reference_dates=r$proposed_funding_at,default_dates=r$horizon_end,currency='EUR')$value$conditional_ead[[1]]
 loss<-lifetime_expected_loss(pd,ead*lgd,r$loss_discount_factor,30/365,currency='EUR',probability_measure='PHYSICAL')$value
 out<-list(application_id=key,customer_id=app$customer_id,account_id=r$account_id,household_id=r$household_id,forecast_at=D,proposed_funding_at=r$proposed_funding_at,horizon_days=30,application_pd=pd,conditional_ead=ead,conditional_lgd=lgd,thirty_day_expected_loss=loss$lifetime_expected_loss,ead_scope=r$ead_scope,prior_workout_lgd=lapply(prior,function(w)w$economic_lgd),prior_known_at=model$model$training$known_at)
 raw<-list(application_fit=fitted,lgd_model=model$model,prior_workouts=prior,exposure_schedule=schedule$value,loss_result=loss)
 if(!is.null(r$asset)){
  asset<-do.call(simulate_asset_values,r$asset)$value;args<-r$waterfall;args$realization_paths[[1]]$gross_value<-asset$paths[[1]]$asset_value;recovery<-do.call(simulate_recovery_waterfall,args)$value
  out$asset_scenario<-list(scope=r$asset_scope,gross_value=asset$paths[[1]]$asset_value,cost=50,economic_loss=recovery$claim_results[[1]]$economic_loss,not_added_to_credit_expected_loss=TRUE);raw$asset<-asset;raw$waterfall<-recovery
 }
 if(!is.null(r$bnpl)){
  obligations<-do.call(build_bnpl_obligation_features,r$bnpl)$value;graph<-do.call(build_customer_household_graph,r$household_graph)$value
  household<-aggregate_household_obligations(r$obligations,graph,r$bnpl$provider_coverage,currency='EUR',period='month',guarantee_budget_policy='exclude')$value
  ht<-list(event='default',horizon=30,time_unit='days',probability_measure='PHYSICAL',conditioning='initially_performing');tr<-r$hazard_training
  if(tr$outcome_end>tr$known_at||tr$known_at>=D)stop('Hazard cohort must mature before origin')
  hm<-fit_discrete_hazard(r$hazard_panel,matrix(numeric(),10,0),feature_order=character(),period_grid=c(0,30),link='logit',partial_period_policy='exclude_censored',target=ht,training=tr,model_id=paste0(product,'-prior-discrete'),model_version='1')
  hc<-predict_event_curves(hm,list(list(path_id=r$account_id,start=0,stop=30,covariates=list(),stratum='all',covariate_time=0)),30,feature_order=character(),target=ht)$value$curves[[1]]
  out$bnpl<-list(outstanding=obligations$total_outstanding,near_term_due=obligations$known_near_term_due,active_plans=obligations$active_plan_count,household_debt=household$households[[1]]$total_linked_debt_stock,household_service=household$households[[1]]$total_linked_debt_service,prior_hazard_pd=1-hc$survival[[1]],hazard_role='INDEPENDENT_PRIOR_BENCHMARK_NOT_REPLACEMENT_APPLICATION_PD')
  raw$bnpl<-obligations;raw$household<-household;raw$hazard_model<-hm$model;raw$hazard_curves<-hc
 }
 if(!is.null(r$expert)){
  prior_beta<-do.call(fit_elicited_beta_prior,r$expert)$value;update<-do.call(update_expert_beta_binomial,c(list(prior=prior_beta),r$expert_update))$value
  offers<-lapply(r$limit_offers,function(o){o$pd<-update$posterior_mean;o});frontier<-compute_limit_frontier(offers,r$limit_profile,as_of=D)$value
  out$micro<-list(expert_prior_alpha=prior_beta$alpha,expert_prior_beta=prior_beta$beta,posterior_alpha=update$posterior$alpha,posterior_beta=update$posterior$beta,posterior_mean=update$posterior_mean,maximum_feasible_amount=frontier$maximum_feasible_amount,expert_role='SEPARATE_POPULATION_OFFER_SCENARIO_NOT_APPLICATION_PD');raw$expert_prior<-prior_beta;raw$expert_update<-update;raw$frontier<-frontier
 }
 if(verify_reference){
  cell<-Filter(function(a)a$driver==1,c$calibration);pd_oracle<-mean(vapply(cell,function(a)a$outcome,0.0));stopifnot(abs(pd-pd_oracle)<1e-9,abs(loss$lifetime_expected_loss-pd_oracle*ead*r$expected_prior_mean_lgd)<1e-8)
  stopifnot(abs(lgd-r$expected_prior_mean_lgd)<1e-9,ead==r$conditional_ead,abs(out$thirty_day_expected_loss-pd*ead*r$expected_prior_mean_lgd)<1e-8)
  if(!is.null(out$asset_scenario))stopifnot(out$asset_scenario$gross_value==600,out$asset_scenario$economic_loss==450)
  if(!is.null(out$bnpl))stopifnot(out$bnpl$outstanding==1000,out$bnpl$household_debt==1000,out$bnpl$near_term_due==500,out$bnpl$household_service==500,abs(out$bnpl$prior_hazard_pd-.2)<1e-8)
  if(!is.null(out$micro))stopifnot(out$micro$posterior_alpha==4,out$micro$posterior_beta==16,out$micro$maximum_feasible_amount==1000)
 }
 out$thirty_day_expected_loss<-round(out$thirty_day_expected_loss,2)
 list(summary=out,raw=raw)
}
run_retail_application_risk<-function(data=NULL,application_data=NULL,application_chain=NULL,product_ids=NULL,verify_reference=TRUE,return_artifacts=FALSE){
 resource<-function(n){p<-system.file('extdata',n,package='creditratingsystem');if(!nzchar(p))p<-file.path('creditratingsystem-r/inst/extdata',n);jsonlite::fromJSON(p,simplifyVector=FALSE)}
 if(is.null(data))data<-resource('retail_application_risk.json');if(is.null(application_data))application_data<-resource('retail_application_journey.json');rows<-list();raw<-list()
 for(p in names(data$products)){if(!is.null(product_ids)&&!p%in%product_ids)next;x<-.rar_evaluate(p,application_data$products[[p]],data$products[[p]],application_data$target,application_chain,verify_reference);rows[[p]]<-x$summary;raw[[p]]<-x$raw}
 out<-list(data_origin=data$data_origin,forecast_scope=data$forecast_scope,presentation_contract='Expected loss displayed to cents; original unrounded calculation remains in raw_artifacts.loss_result.',products=rows);if(return_artifacts)out$raw_artifacts<-raw;out
}
