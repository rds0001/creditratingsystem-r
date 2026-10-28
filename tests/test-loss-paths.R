if(!exists('expected_loss_from_paths'))library(creditratingsystem)
lp_error<-function(code,expr){e<-tryCatch({force(expr);NULL},error=identity);stopifnot(inherits(e,'creditratingsystem_error'),identical(e$code,code))}
lp_near<-function(x,y,tol=1e-12)stopifnot(isTRUE(all.equal(x,y,tolerance=tol,check.attributes=FALSE)))
lp_paths<-function(){list(default_indicator=matrix(c(1,0,1,0,0,1,0,0),4,2,byrow=TRUE),lgd_paths=matrix(c(.2,0,.8,0,0,.5,0,0),4,2,byrow=TRUE),ead_paths=matrix(c(100,0,200,0,0,80,0,0),4,2,byrow=TRUE),scenario_weights=rep(.25,4),discount_factors=c(1,.9),scenario_ids=c('a','b','c','d'),time_grid=c(1,2),currency='EUR',probability_measure='PHYSICAL')}
args<-lp_paths();args$uncertainty<-'iid';r<-do.call(expected_loss_from_paths,args)$value
lp_near(unlist(r$path_losses),c(20,160,36,0));lp_near(r$expected_loss,54);lp_near(unlist(r$period_contributions),c(45,9));lp_near(r$conditional_independence_loss,46.5);lp_near(r$conditional_lgd_exposure_covariance_contribution,7.5);lp_near(r$standard_error,sqrt(15632/12))
r<-lifetime_expected_loss(c(.5,.25),c(90,40),c(1,.9),c(1,2),currency='EUR',probability_measure='PHYSICAL')$value;lp_near(r$lifetime_expected_loss,54)
# Full Boolean partition: transfer costs in nondefault state and default/transfer overlap counted exactly once.
p<-c(.7,.1,.15,.05);loss<-c(0,20,60,70);states<-lapply(0:3,function(i)list(state_id=as.character(i),events=list(default=i>=2,transfer=i%%2==1),probability=p[i+1],conditional_loss=loss[i+1]))
r<-event_conditioned_loss(states,c('default','transfer'),'EUR','PHYSICAL')$value;lp_near(r$expected_loss,14.5)
bad<-states;bad[[4]]$events<-states[[1]]$events;lp_error('OVERLAPPING_STATES',event_conditioned_loss(bad,c('default','transfer'),'EUR','PHYSICAL'))
r<-counterparty_expected_loss(matrix(c(1,0),2,1),matrix(c(100,-20),2,1),matrix(c(.4,.4),2,1),c(.5,.5),1,'C','N',c('bad','good'),1,'EUR','PHYSICAL')
lp_near(r$value$expected_loss,30);lp_near(r$value$independent_default_exposure_loss,15);lp_near(r$value$default_exposure_dependence_contribution,15);stopifnot(r$diagnostics$negative_net_values_floored==1)
lp_portfolio<-function(){list(facility_loss_paths=list(list(facility_id='A',currency='EUR',scenario_ids=c('low','mid','high'),losses=c(0,10,30)),list(facility_id='B',currency='USD',scenario_ids=c('high','low','mid'),losses=c(5,0,5))),portfolio_mapping=list(list(facility_id='A',portfolio_id='P',share=1),list(facility_id='B',portfolio_id='P',share=1)),scenario_weights=c(.5,.3,.2),fx_rates=list(EUR=1,USD=2),scenario_ids=c('low','mid','high'),reporting_currency='EUR',probability_measure='PHYSICAL',quantile_levels=.75)}
r<-do.call(aggregate_portfolio_loss,lp_portfolio())$value$portfolios[[1]]
lp_near(unlist(r$path_losses),c(0,20,40));lp_near(r$expected_loss,14);tail<-r$tail_metrics[[1]];lp_near(tail$value_at_risk,20);lp_near(tail$expected_shortfall,36);lp_near(unlist(tail$facility_es_contributions),c(A=26,B=10))
# Equal aggregate boundary losses but distinct facilities must share boundary mass proportionally.
args<-lp_portfolio();args$scenario_weights<-c(.5,.25,.25);args$facility_loss_paths[[1]]$losses<-c(0,10,0);args$facility_loss_paths[[2]]$losses<-c(5,0,0)
tail<-do.call(aggregate_portfolio_loss,args)$value$portfolios[[1]]$tail_metrics[[1]];lp_near(tail$expected_shortfall,10);lp_near(unlist(tail$facility_es_contributions),c(A=5,B=5))
# Zero-weight high losses cannot affect tail metrics; negative economic losses retained.
args<-lp_portfolio();args$scenario_weights<-c(1,0,0);args$facility_loss_paths[[1]]$losses<-c(-10,10,30);r<-do.call(aggregate_portfolio_loss,args)$value$portfolios[[1]];lp_near(r$expected_loss,-10);lp_near(r$tail_metrics[[1]]$expected_shortfall,-10)
# Allocations and scenario FX use global scenario order, even with local row permutation.
args<-lp_portfolio();args$fx_rates$USD<-c(1,2,3);args$portfolio_mapping[[1]]$share<-.5;args$portfolio_mapping[[2]]$share<-.5;args$portfolio_mapping[[3]]<-list(facility_id='A',portfolio_id='Q',share=.5)
r<-do.call(aggregate_portfolio_loss,args)$value;lp_near(unlist(r$portfolios[[1]]$path_losses),c(0,10,22.5));lp_near(r$unallocated_shares$A,0);lp_near(r$unallocated_shares$B,.5)
for(case in list(list('INVALID_DEFAULT_PATH','default_indicator',matrix(1,4,2)),list('INVALID_IDENTIFIERS','scenario_ids',rep('a',4)),list('INVALID_WEIGHTS','scenario_weights',rep(.5,4)),list('INVALID_WEIGHTS','scenario_weights',rep(1e308,4)),list('INVALID_TIME_GRID','time_grid',c(2,1)),list('INVALID_DISCOUNT','discount_factors',c(1,-1)))){args<-lp_paths();args[[case[[2]]]]<-case[[3]];lp_error(case[[1]],do.call(expected_loss_from_paths,args))}
args<-lp_paths();args$scenario_weights<-c(.1,.2,.3,.4);args$uncertainty<-'iid';lp_error('INCOMPATIBLE_UNCERTAINTY',do.call(expected_loss_from_paths,args))
args<-list(default_indicator=matrix(1,1,1),lgd_paths=matrix(-1e-308,1,1),ead_paths=matrix(1e308,1,1),scenario_weights=1,discount_factors=1,scenario_ids='x',time_grid=1,currency='EUR',probability_measure='PHYSICAL');r<-do.call(expected_loss_from_paths,args)$value;lp_near(r$expected_loss,-1)
args$lgd_paths<-matrix(1e308,1,1);args$discount_factors<-1e-308;r<-do.call(expected_loss_from_paths,args)$value;lp_near(r$expected_loss/1e308,1)
args$discount_factors<-1;lp_error('NUMERICAL_FAILURE',do.call(expected_loss_from_paths,args))
lp_error('INVALID_PD_MASS',lifetime_expected_loss(c(.6,.8),c(10,10),c(1,1),c(1,2),'EUR','PHYSICAL'))
args<-lp_portfolio();args$portfolio_mapping[[3]]<-list(facility_id='A',portfolio_id='Q',share=.1);lp_error('DOUBLE_COUNTING',do.call(aggregate_portfolio_loss,args))
args<-lp_paths();args$context<-list(currency='USD');lp_error('TARGET_MISMATCH',do.call(expected_loss_from_paths,args))
args<-lp_paths();args$default_indicator[,]<-0;r<-do.call(expected_loss_from_paths,args)$value;lp_near(r$expected_loss,0);stopifnot(all(vapply(r$default_weighted_lgd,is.null,logical(1))))
cat('loss paths native independent finite partition and tail oracles passed\n')

lp_error('NUMERICAL_FAILURE',expected_loss_from_paths(matrix(1,2,1),matrix(c(1.7e154,-5.5e154),2,1),matrix(c(1e154,1),2,1),c(.5,.5),1,c('a','b'),1,'EUR','PHYSICAL'))
cat('loss covariance difference overflow rejected before result serialization\n')
