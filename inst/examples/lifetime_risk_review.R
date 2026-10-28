# Competing exits, conditional lifetime losses and independent held-out outcomes.
if(!exists('fit_cause_specific_hazards',mode='function'))library(creditratingsystem)
run_lifetime_risk_review<-function(data=NULL,verify_reference=TRUE){
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/lifetime_risk_review.json',package='creditratingsystem'),simplifyVector=FALSE)
 d<-data;target<-d$target;grid<-unlist(d$horizons);train<-d$training;origin<-d$origin;times<-unlist(d$training_times);dates<-unlist(d$dates)
 if(as.character(as.Date(d$training_origin)+max(times))>train$known_at||train$known_at>origin)stop('Training event support must precede model availability and forecast origin')
 rows<-lapply(seq_along(times),function(i)list(entity_id=paste0('train-',i-1),start=0,stop=times[i],event=as.integer(!is.null(d$training_events[[i]])),covariate_time=0))
 cs<-fit_cause_specific_hazards(rows,matrix(numeric(),length(rows),0),d$training_events,feature_order=character(),causes=c('default','prepay'),ties='breslow',target=target,training=train,model_id='lifetime-cause',model_version='1')
 fg<-fit_fine_gray(times,d$training_events,matrix(numeric(),length(rows),0),cause_of_interest='default',feature_order=character(),censoring_spec=list(estimator='kaplan_meier',independence_assumption='independent_within_censoring_group'),target=target,training=train,model_id='lifetime-fine-gray',model_version='1')
 path<-list(list(path_id='account',start=0,stop=60,covariates=list(),stratum='all',covariate_time=0))
 curves<-predict_event_curves(cs,path,grid,feature_order=character(),target=target)$value$curves[[1]];alternate<-predict_event_curves(fg,path,grid,feature_order=character(),target=target)$value$curves[[1]]
 cumulative<-unlist(curves$cumulative_incidence$default);marginal<-diff(c(0,cumulative))
 events<-lapply(seq_along(dates),function(i)list(event_id=paste0('repay-',i-1),event_date=dates[i],known_at=origin,sequence=0,kind='PRINCIPAL_REPAYMENT',amount=c(10,20,30)[i]))
 schedule<-exposure_schedule(list(facility_id='amortizing',currency='EUR',as_of=origin,drawn_principal=100,credit_limit=100,accrued_interest=0),events,c(0,0,0),period_end_dates=dates,day_count='ACT/365F',default_timing='BEFORE_PERIOD_END_EVENTS',allow_overlimit=FALSE,currency='EUR')
 ead<-unlist(predict_ead(schedule,NULL,feature_order=character(),expected_target=NULL,reference_dates=rep(origin,3),default_dates=dates,currency='EUR')$value$conditional_ead)
 lt<-list(event='economic_loss',horizon=60,time_unit='days',probability_measure='PHYSICAL',conditioning='given_default');ltrain<-train;ltrain$workout_population<-'closed_defaulted_episodes'
 lf<-fit_fractional_lgd(matrix(numeric(),length(d$lgd_training_targets),0),unlist(d$lgd_training_targets),feature_order=character(),link='logit',covariance='HC0',target=lt,training=ltrain,model_id='lifetime-lgd',model_version='1')
 lgd<-unlist(predict_lgd(lf,matrix(numeric(),3,0),feature_order=character(),expected_target=lt,default_condition='given_default')$value$mean_lgd);lgd<-round(lgd,12);conditional<-ead*lgd;discount<-unlist(d$discount_factors)
 loss<-lifetime_expected_loss(marginal,conditional,discount,grid/365,currency='EUR',probability_measure='PHYSICAL')$value
 design<-list(model_id='lifetime-cause',model_version='1',parameter_version='1',data_fingerprint='independent-heldout-account-events',population_id='first-default-before-exit',split_id='holdout-2024',prediction_origin='HOLDOUT',evaluation_cutoff='2024-06-01',days_per_time_unit=1,target=target)
 obs<-lapply(d$holdout,function(r){
  if(!is.null(r$terminal_exit_date)&&(r$event||!identical(r$coverage_end,'2024-03-02')))stop('Terminal prepaid noncase needs explicit observed first-default-before-exit target coverage')
  list(prediction_id=r$id,entity_id=r$id,cluster_id=r$id,segment='all',as_of=origin,forecast_known_at=origin,horizon_end=dates[3],model_id='lifetime-cause',model_version='1',parameter_version='1',outcome_known_at='2024-03-03',observed_time=r$time,event=r$event)
 })
 censor<-list(method='frozen_ipcw',reference='Independent administrative censor cohort: 2/8 at day30, remaining6 supported through90',independence_assumption='censoring_independent_of_event_features_and_across_clusters',tie_rule='event_before_censoring',model_fingerprint='independent-administrative-censor-eight',model_known_at='2022-04-03',times=list(30),survival=list(.75),support_end=90,minimum_survival=.1)
 groups<-vapply(d$holdout,function(r)r$id,'');draws<-list(groups,rev(groups),rep(groups[1],6),rep(groups[6],6))
 survival<-survival_backtest(matrix(rep(cumulative,each=length(obs)),nrow=length(obs)),obs,horizons=grid,censoring=censor,design=design,uncertainty='cluster_bootstrap',cluster_resamples=draws)$value
 default_rows<-list()
 for(i in seq_along(d$holdout)){
  r<-d$holdout[[i]];if(!r$event)next;j<-match(r$time,grid);closed<-identical(r$workout_status,'CLOSED');o<-obs[[i]]
  o$default_date<-r$default_date;o$outcome_known_at<-'2024-05-01';o$predicted_ead<-ead[j];o$realized_ead<-r$ead;o$realized_ead_date<-r$default_date;o$drawn_at_reference<-100;o$limit_at_reference<-100;o$currency<-'EUR';o$predicted_lgd<-lgd[j];o['realized_lgd']<-list(r$lgd);o$ead_at_default<-r$ead;o$workout_status<-if(closed)'CLOSED'else'OPEN';o['workout_closed_at']<-list(if(closed)'2024-05-01'else NULL);default_rows[[length(default_rows)+1]]<-o
 }
 dd<-design;dd$target$conditioning<-'default';dd$currency<-'EUR'
 ev<-ead_backtest(default_rows,design=dd,weighting_policy=list(method='equal',reference='Matched observed default balances'),uncertainty='none')$value
 lv<-lgd_backtest(default_rows,design=dd,workout_policy=list(method='closed_only',reference='Actual finalized claims only; excludes unresolved claim'),uncertainty='none')$value
 w<-d$open_workout;oa<-list(episodes=list(w$episode),cashflows=w$cashflows,fitted_recovery_model=d$residual_model,X=matrix(numeric(),1,0),valuation_contracts=list(w$valuation),future_cost_cashflows=w$future_costs,feature_order=character(),expected_target=d$recovery_target,stage_spec=d$stage_spec,as_of='2024-03-29',currency='EUR',extrapolation='parametric')
 open_value<-do.call(estimate_open_workout_lgd,oa)$value;unknown<-oa;unknown$valuation_contracts[[1]]['tail_discount_factor']<-list(NULL);unknown$valuation_contracts[[1]]['tail_assumption_reference']<-list(NULL)
 unidentified<-do.call(estimate_open_workout_lgd,unknown)$value
 out<-list(data_origin=d$data_origin,target_definition=d$target_definition,cause_specific_curves=curves,fine_gray_curves=alternate,fine_gray_baseline=fg$model$parameters$baseline,marginal_first_default=as.list(marginal),conditional_ead=as.list(ead),conditional_lgd=as.list(lgd),lifetime_loss=loss,survival_validation=survival,ead_validation=ev,lgd_validation=lv,open_workout=open_value,unidentified_tail=unidentified,ignored_prepayment_probability=1-exp(-5/6),incorrect_cumulative_mass_loss=sum(cumulative*conditional*discount))
 if(verify_reference){
  expected<-c(0,exp(-.2)*(1-exp(-1/3)),exp(-.2)*(1-exp(-5/6)));stopifnot(max(abs(cumulative-expected))<1e-12,max(abs(unlist(curves$survival)+cumulative+unlist(curves$cumulative_incidence$prepay)-1))<1e-12,all(ead==c(100,90,70)),max(abs(lgd-.4))<1e-12)
  stopifnot(max(abs(vapply(out$fine_gray_baseline,function(r)r$increment,0)-c(1/3.75,1/2.75)))<1e-12,abs(loss$lifetime_expected_loss-(expected[2]*90*.4*.97+(expected[3]-expected[2])*70*.4*.95))<1e-10,out$incorrect_cumulative_mass_loss>loss$lifetime_expected_loss)
  b<-c(0,(2*(1-expected[2])^2+3*expected[2]^2)/4.5,(3*(1-expected[3])^2+2*expected[3]^2)/4.5)
  stopifnot(max(abs(vapply(survival$metrics$by_horizon,function(r)r$brier,0)-b))<1e-12,abs(ev$metrics$ead$overall$selected_weighting$bias+10)<1e-12,abs(lv$metrics$overall$ead_weighted$bias-4/180)<1e-10,lv$coverage$exclusion_counts$OPEN_WORKOUT==1,is.null(unidentified$episodes[[1]]$expected_final_lgd))
 };out
}
