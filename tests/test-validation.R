if(!exists('auc_gini'))library(creditratingsystem)
v_error<-function(code,expr){e<-tryCatch({force(expr);NULL},error=identity);stopifnot(inherits(e,'creditratingsystem_error'),identical(e$code,code))}
v_near<-function(x,y,tol=1e-13)stopifnot(isTRUE(all.equal(x,y,tolerance=tol,check.attributes=FALSE)))
y<-c(0,1,0,1);score<-c(0,1,1,2);weights<-c(2,2,3,4)
v<-auc_gini(y,score,weights,score_direction='higher_is_risk')$value
v_near(v$auc,27/30);v_near(v$gini,0.8);stopifnot(v$pair_count==4,v$tied_pair_count==1);v_near(v$tied_pair_mass,6/30)
v_near(auc_gini(y,score,weights,score_direction='lower_is_risk')$value$auc,0.1)
v<-ks_discrimination(y,score,weights,score_direction='higher_is_risk')$value
v_near(v$ks,2/3);stopifnot(v$threshold==1)
# Deterministic pair enumeration is an independent oracle, not a second rank implementation.
y<-rep(c(0,1,1,0,1),8);scores<-(seq_along(y)*7)%%11;weights<-(seq_along(y)%%7)/3
num<-0;den<-0;pair_count<-0;tied_count<-0
for(i in seq_along(y))for(j in seq_along(y))if(y[i]==1&&y[j]==0&&weights[i]>0&&weights[j]>0){den<-den+weights[i]*weights[j];num<-num+weights[i]*weights[j]*((scores[i]>scores[j])+0.5*(scores[i]==scores[j]));pair_count<-pair_count+1;tied_count<-tied_count+(scores[i]==scores[j])}
v<-auc_gini(y,scores,weights,score_direction='higher_is_risk')$value
v_near(v$auc,num/den);stopifnot(v$pair_count==pair_count,v$tied_pair_count==tied_count)
perm<-rev(seq_along(y));v_near(auc_gini(y[perm],scores[perm],weights[perm],score_direction='higher_is_risk')$value$auc,v$auc)
stopifnot(auc_gini(c(0,1),c(0,1),score_direction='higher_is_risk')$value$auc==1,ks_discrimination(c(0,1),c(0,1),score_direction='higher_is_risk')$value$ks==1,auc_gini(c(0,1),c(2,2),score_direction='higher_is_risk')$value$auc==0.5,ks_discrimination(c(0,1),c(2,2),score_direction='higher_is_risk')$value$ks==0)
for(fn in list(auc_gini,ks_discrimination)) {v<-fn(c(0,1),c(0,1),c(1,0),score_direction='higher_is_risk')$value;stopifnot(v$applicability=='NOT_APPLICABLE',v$reason=='ONE_CLASS')}
v<-brier_score(c(0,1),c(.25,.75),c(1,3))$value
stopifnot(v$brier==.0625,identical(v$contributions,list(.015625,.046875)),v$outcome_variance_baseline==.1875,brier_score(c(0,1),c(0,1))$value$brier==0)
stopifnot(log_loss(c(0,1),c(0,1))$value$log_loss==0)
v<-log_loss(c(0,1),c(0,0))$value
stopifnot(v$loss_status=='INFINITE',is.null(v$log_loss),identical(v$contributions,list(0,NULL)),identical(v$infinite_loss_flags,list(FALSE,TRUE)),log_loss(c(0,1),c(0,0),c(1,0))$value$log_loss==0)
v<-log_loss(c(0,1),c(0,0),boundary_policy='clip',epsilon=.01)
v_near(v$value$log_loss,(log(100)-log(.99))/2);stopifnot(v$diagnostics$clipped_count==2)
v_near(log_loss(1,1e-300)$value$log_loss,300*log(10))
v_error('INVALID_BOUNDARY_POLICY',log_loss(c(0,1),c(0,1),boundary_policy='clip',epsilon=1e-300))
for(alternative in c('less','two-sided','greater')) {
 v<-binomial_pd_test(4,0,.5,alternative=alternative,independence_assumption='independent_common_probability')$value
 expected_p<-switch(alternative,less=1/16,'two-sided'=2/16,greater=1)
 expected_upper<-switch(alternative,less=1-.05^.25,'two-sided'=1-.025^.25,greater=1)
 v_near(v$p_value,expected_p);v_near(v$upper,expected_upper,1e-12);stopifnot(v$lower==0)
}
# Nonzero count probability-ordering oracle: n=5,p=1/2,k=1 includes masses 1,5,5,1 over 32.
v<-binomial_pd_test(5,1,.5,alternative='two-sided',independence_assumption='independent_common_probability')$value
v_near(v$p_value,12/32)
for(p in c(0,1))for(k in c(0,5)) {v<-binomial_pd_test(5,k,p,alternative='two-sided',independence_assumption='independent_common_probability')$value;stopifnot(v$p_value==if(k==5*p)1 else 0)}
v<-binomial_pd_test(0,0,.02,alternative='greater',independence_assumption='independent_common_probability')$value
stopifnot(is.null(v$p_value),v$applicability=='NOT_APPLICABLE')
v_error('INCOMPATIBLE_ASSUMPTIONS',binomial_pd_test(4,0,.5,alternative='greater',independence_assumption='correlated'))
for(fn in list(brier_score,log_loss)) {
 v_error('INVALID_OUTCOMES',fn(c(0,2),c(.1,.2)))
 v_error('INCOMPATIBLE_SHAPE',fn(c(0,1),.1))
 v_error('INVALID_WEIGHTS',fn(c(0,1),c(.1,.2),c(0,0)))
 v_error('INVALID_PROBABILITY',fn(c(0,1),c(.1,2)))
 v_error('NON_FINITE_INPUT',fn(c(0,NaN),c(.1,.2)))
 v_error('NUMERICAL_RANGE',fn(c(0,1),c(.1,.2),c(1e308,1e308)))
}
cat('validation native independent cases passed\n')

v_design<-function(p,origin='HOLDOUT',ids=NULL) {d<-list(forecast_fingerprint=creditratingsystem_hash_for_validation(p),prediction_origin=origin,forecast_known_at='2023-12-31',evaluation_at='2025-01-01',split_id='test',target=list(event='first_default',horizon=1,time_unit='years',probability_measure='PHYSICAL',conditioning='PIT'));if(!is.null(ids))d$observation_ids<-as.list(ids);d}
# Use the package canonical contract to bind inputs; numerical expectations below are hand derived.
creditratingsystem_hash_for_validation<-function(p) {hash<-if(exists('.cr_hash'))get('.cr_hash') else getFromNamespace('.cr_hash','creditratingsystem');hash(as.list(as.numeric(p)))}
p<-rep(c(.25,.5,.75),each=4);y<-c(0,0,0,1,0,0,1,1,0,1,1,1);d<-v_design(p)
v<-calibration_intercept_slope(p,y,design=d,covariance_policy='iid')$value
stopifnot(v$converged);v_near(v$intercept,0,1e-10);v_near(v$slope,1,1e-10);v_near(v$covariance[[1]][[1]],.4,1e-10);v_near(v$covariance[[2]][[2]],1/(1.5*log(3)^2),1e-10)
v<-calibration_intercept_slope(p,y,design=d,covariance_policy='cluster_cr1',cluster_ids=rep(c('A','B','C','D'),3))$value
v_near(v$covariance[[1]][[1]],88/75,1e-10);v_near(v$covariance[[2]][[2]],88/(135*log(3)^2),1e-10);v_near(v$covariance[[1]][[2]],0,1e-10)
for(case in list(list(p=c(.2,.2),y=c(0,1),reason='CONSTANT_PD'),list(p=c(0,.5),y=c(0,1),reason='BOUNDARY_PD'),list(p=c(.2,.8),y=c(0,1),reason='SEPARATION'))) {v<-calibration_intercept_slope(case$p,case$y,design=v_design(case$p),covariance_policy='iid')$value;stopifnot(v$reason==case$reason,is.null(v$slope))}
d$forecast_fingerprint<-'changed'
v_error('FORECAST_INTEGRITY',calibration_intercept_slope(p,y,design=d,covariance_policy='none'))
p<-c(0,.25,.5,1);y<-c(0,1,0,1)
v<-calibration_curve(p,y,c(0,.25,.5,.75,.9,1),design=v_design(p),interval_method='clopper_pearson',dependence_assumption='independent_common_probability_within_bins')$value
stopifnot(identical(unlist(v$assignments),c(1L,2L,3L,5L)),v$bins[[4]]$status=='EMPTY',is.null(v$bins[[4]]$observed_rate),is.null(v$bins[[4]]$lower));v_near(v$bins[[1]]$upper,.975,1e-12)
stopifnot(sum(vapply(v$bins,function(b)if(b$count)b$count*b$observed_rate else 0,numeric(1)))==sum(y))
p<-c(.1,.2);y<-c(0,1)
v<-spiegelhalter_test(p,y,design=v_design(p),alternative='two-sided',independence_assumption='independent_fixed_bernoulli')$value
v_near(v$numerator,.4);v_near(v$variance,.1152);v_near(v$z,.4/sqrt(.1152))
v<-spiegelhalter_test(c(.5,.5),y,design=v_design(c(.5,.5)),alternative='greater',independence_assumption='independent_fixed_bernoulli')$value
stopifnot(v$reason=='ZERO_VARIANCE',is.null(v$p_value))
p<-c(.2,.5,.8);v<-poisson_binomial_pd_test(p,0,design=v_design(p),alternative='two-sided',independence_assumption='independent_fixed_bernoulli')$value
v_near(unlist(v$probability_mass),c(.08,.42,.42,.08));v_near(v$p_value,.16)
p<-rep(.2,12)
for(k in c(0,2,8,12)) {v<-poisson_binomial_pd_test(p,k,design=v_design(p),alternative='two-sided',independence_assumption='independent_fixed_bernoulli')$value;ref<-stats::binom.test(k,12,.2)$p.value;v_near(v$p_value,ref,1e-12);v_near(sum(unlist(v$probability_mass)),1,1e-14)}
p<-c(0,1);stopifnot(poisson_binomial_pd_test(p,0,design=v_design(p),alternative='less',independence_assumption='independent_fixed_bernoulli')$value$p_value==0)
p<-rep(c(.25,.75),each=20);y<-c(rep(1,4),rep(0,16),rep(1,16),rep(0,4))
v<-hosmer_lemeshow_test(p,y,c(0,.5,1),design=v_design(p),degrees_of_freedom_policy='external_common_bin_pd',independence_assumption='independent_fixed_bernoulli')$value
v_near(v$statistic,8/15);stopifnot(v$degrees_of_freedom==2);v_near(v$p_value,exp(-4/15))
v<-hosmer_lemeshow_test(c(.1,.9),c(0,1),c(0,.5,1),design=v_design(c(.1,.9)),degrees_of_freedom_policy='external_common_bin_pd',independence_assumption='independent_fixed_bernoulli')$value
stopifnot(v$reason=='SMALL_EXPECTED_CELL',is.null(v$p_value))
v_error('INVALID_VALIDATION_DESIGN',hosmer_lemeshow_test(p,y,c(0,.5,1),design=v_design(p),degrees_of_freedom_policy='fitted_logistic_approximation',independence_assumption='independent_fixed_bernoulli'))
v_dependent<-function(rho=1)list(predicted_pd=c(.5,.5),outcomes=c(1,1),cluster_ids=c('G','G'),time_ids=c('2024','2024'),design=v_design(c(.5,.5),ids=c('O1','O2')),null_model=list(family='gaussian_factor',asset_correlation=rho,factor_correlation=list(list(1)),parameter_uncertainty='conditional_fixed'),innovations=list(draw_assumption='independent_standard_normal_rows',observation_ids=list('O1','O2'),factor_keys=list(list(cluster_id='G',time_id='2024')),factor_normals=list(list(-2),list(-.5),list(.5),list(2)),idiosyncratic_normals=list(list(-1,-1),list(-1,1),list(1,-1),list(1,1))),alternative='greater')
v<-do.call(dependent_pd_backtest,v_dependent())$value
stopifnot(identical(unlist(v$null_counts),c(2,2,0,0)),v$p_value==3/5,identical(unlist(v$null_count_interval),c(0,2)))
v<-do.call(dependent_pd_backtest,v_dependent(0))$value
stopifnot(identical(unlist(v$null_counts),c(2,1,1,0)),v$p_value==2/5)
args<-v_dependent();args$innovations$observation_ids<-rev(args$innovations$observation_ids)
v_error('INNOVATION_ALIGNMENT',do.call(dependent_pd_backtest,args))
args<-v_dependent(0);args$null_model$parameter_uncertainty<-'supplied_predictive_pd_draws';args$null_model$parameter_draw_source<-'REFERENCE: four fixed parameter scenarios';args$innovations$parameter_pd_draws<-list(list(0,0),list(1,1),list(0,1),list(1,0))
v<-do.call(dependent_pd_backtest,args)$value
stopifnot(identical(unlist(v$null_counts),c(0,2,1,1)))
base<-list(entity_id='O',as_of='2024-01-01',performance_start='2024-01-01',outcome_end='2024-01-31',eligible=TRUE,prior_default=FALSE,default_definition='first_default',target=1,outcome_known_at='2024-01-15',grade='B')
rows<-list(c(base,list(observation_id='a',facility_id='F1')),c(base,list(observation_id='b',facility_id='F2')))
third<-base;third$entity_id<-'Q';third$target<-0;third$outcome_known_at<-'2024-01-31';third$grade<-'A';rows[[3]]<-c(third,list(observation_id='c',facility_id='F3'))
fourth<-base;fourth$entity_id<-'R';fourth['target']<-list(NULL);fourth['outcome_known_at']<-list(NULL);rows[[4]]<-c(fourth,list(observation_id='d',facility_id='F4'))
forecasts<-lapply(rows,function(row)list(observation_id=row$observation_id,pd=if(row$entity_id=='O').1 else .2,forecast_at='2023-12-31',known_at='2023-12-31',default_definition='first_default',horizon_days=30,unit='obligor',probability_measure='PHYSICAL',model_id='M',model_version='1'))
args<-list(observation_panel=rows,frozen_forecasts=forecasts,group_by=list('grade'),unit='obligor',horizon_days=30,default_definition='first_default',redefault_policy='exclude_previous_default',evaluation_cutoff='2024-02-01')
v<-do.call(default_count_backtest,args)$value
stopifnot(v$deduplicated_rows==1,v$overall$observed_count==2,v$overall$observed_defaults==1,identical(v$excluded,list(list(observation_id='d',reason='MISSING_OUTCOME'))));v_near(v$overall$expected_defaults,.3);v_near(v$overall$observed_expected_ratio,10/3)
args$evaluation_cutoff<-'2024-01-20';v<-do.call(default_count_backtest,args)$value
stopifnot(v$overall$observed_count==0,v$overall$expected_defaults==0,all(vapply(v$excluded,function(e)e$reason=='IMMATURE',logical(1))))
args$evaluation_cutoff<-'2024-02-01';args$frozen_forecasts[[2]]$pd<-.5
v_error('DUPLICATE_UNIT_CONFLICT',do.call(default_count_backtest,args))
args$frozen_forecasts<-forecasts;args$frozen_forecasts[[1]]$known_at<-'2024-01-02'
v_error('FORECAST_LEAKAGE',do.call(default_count_backtest,args))
cat('extended validation native hand-derived cases passed\n')
stopifnot(identical(brier_score(1,.2,2)$value,brier_score(c(1),c(.2),c(2))$value),auc_gini(1,3,score_direction='higher_is_risk')$value$reason=='ONE_CLASS')
for(fn in list(brier_score,log_loss)) {v_error('INVALID_INPUT',fn(matrix(c(0,1),nrow=1),c(.1,.2)));v_error('INCOMPATIBLE_SHAPE',fn(c(0,1),matrix(c(.1,.2),nrow=1)));v_error('INVALID_INPUT',fn(c(0,1),c(.1,.2),matrix(c(1,1),nrow=1)))}
args<-v_dependent(1);args$time_ids<-c('2023','2024');args$null_model$factor_correlation<-list(list(1,.6),list(.6,1));args$innovations$factor_keys<-list(list(cluster_id='G',time_id='2023'),list(cluster_id='G',time_id='2024'));args$innovations$factor_normals<-list(list(-1,-.2),list(1,.2),list(.1,-1),list(-.1,1))
v<-do.call(dependent_pd_backtest,args)$value
stopifnot(identical(unlist(v$null_counts),c(2,0,1,1)))
p<-rep(.5,1100);v<-poisson_binomial_pd_test(p,0,design=v_design(p),alternative='less',independence_assumption='independent_fixed_bernoulli')
stopifnot(v$value$reason=='NUMERICAL_UNDERFLOW',is.null(v$value$p_value),v$diagnostics$underflowed_support_cells>0)
cat('validation scalar, temporal-factor and underflow cases passed\n')
