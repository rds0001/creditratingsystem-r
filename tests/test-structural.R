library(creditratingsystem)
option<-merton_equity_value(100,100,.2,.05,1,currency="EUR")$value
oracle<-integrate(function(z)pmax(100*exp(.03+.2*z)-100,0)*dnorm(z)*exp(-.05),-.15,10,abs.tol=1e-11)$value
stopifnot(abs(option$equity_value-oracle)<2e-12,abs(option$equity_value-10.450583572185565)<2e-12)
for(parameters in list(c(100,100,.2,.05,1,0),c(130,80,.5,-.01,2,.03),c(100,0,.2,.05,1,.02),c(40,100,.8,.03,1,0))) {
 a<-parameters[1];d<-parameters[2];s<-parameters[3];r<-parameters[4];t<-parameters[5];q<-parameters[6]
 equity<-merton_equity_value(a,d,s,r,t,q,currency="EUR")$value
 inferred<-infer_merton_assets(equity$equity_value,a*equity$delta*s/equity$equity_value,d,r,t,c(1,10000),c(.001,4),"EUR",payout=q)$value
 stopifnot(abs(inferred$asset_value/a-1)<1e-8,abs(inferred$asset_volatility/s-1)<1e-8)
}
expect_code<-function(expr,code){actual<-tryCatch({force(expr);"NO_ERROR"},creditratingsystem_error=function(e)e$code);stopifnot(identical(actual,code))}
expect_code(infer_merton_assets(50,.2,100,.05,1,c(1,2),c(.001,.01),"EUR"),"NONCONVERGENCE")
stopifnot(merton_default_probability(100,80,.2,.1,1,"PHYSICAL")$value<merton_default_probability(100,80,.2,.03,1,"RISK_NEUTRAL")$value,
 merton_default_probability(100,100,0,0,0,"PHYSICAL")$value==1,
 distance_to_default(100,100,0,0,0)$value$boundary_status=="DETERMINISTIC_AT_BARRIER")
for(drift in c(-.2,.02,.3)) {
 x<-log(100/80);s<-.2;m<-drift-.5*s*s
 integral<-integrate(function(t)x/(s*sqrt(2*pi*t^3))*exp(-(x+m*t)^2/(2*s*s*t)),0,1,abs.tol=1e-11)$value
 pd<-first_passage_pd(100,list(kind="constant",initial_level=80),s,drift,1,"PHYSICAL")$value
 stopifnot(abs(pd-integral)<1e-11,pd>=merton_default_probability(100,80,s,drift,1,"PHYSICAL")$value)
}
reflection<-2*pnorm(-log(100/80)/.2)
stopifnot(abs(first_passage_pd(100,list(kind="exponential",initial_level=80,growth_rate=.03),.2,.05,1,"PHYSICAL")$value-reflection)<1e-14,
 first_passage_pd(80,list(kind="constant",initial_level=80),.2,.02,0,"PHYSICAL")$value==1)
z<-matrix(c(0,1,-1,0),nrow=2,byrow=TRUE)
dynamics<-list(asset_value=100,volatility=.2,drift=.02)
paths<-simulate_structural_paths(dynamics,c(80,80,80),c(0,.5,1),z,"PHYSICAL")
expected<-100*exp(cbind(c(0,0),.2*sqrt(.5)*z[,1],.2*sqrt(.5)*rowSums(z)))
stopifnot(max(abs(paths$value$asset_paths-expected))<1e-12)
bridge<-simulate_structural_paths(dynamics,c(90,90),c(0,1),matrix(0,1,1),"PHYSICAL",bridge_uniforms=matrix(.1,1,1))
stopifnot(bridge$value$survival_curve[2]==0)
jump<-list(asset_value=100,volatility=0,drift=0,jump_intensity=1,jump_log_mean=log(.5),jump_log_sd=0)
jumped<-simulate_structural_paths(jump,c(80,80),c(0,1),matrix(0,1,1),"PHYSICAL",jump_counts=matrix(1,1,1),jump_innovations=matrix(0,1,1))
stopifnot(abs(jumped$value$asset_paths[1,2]-50*exp(.5))<1e-12)
meta<-list(target=list(event="default",horizon=1,time_unit="years",probability_measure="PHYSICAL",conditioning="observed_DD"),calibration=list(start="2020-01-01",end="2021-01-01",known_at="2022-01-01",split_id="calibration",prediction_origin="HOLDOUT"),base_model_version="1",population_definition="test firms",outcome_policy="complete one-year outcomes",weight_type="frequency",data_origin="SYNTHETIC")
mapping<-calibrate_distance_pd_mapping(c(0,1,2,3),c(1,0,1,0),meta,bootstrap_indices=matrix(c(0,1,2,3,0,0,3,3),2,byrow=TRUE),out_of_bounds="clip")
stopifnot(max(abs(mapping$value$pd-c(1,.5,.5,0)))<1e-14,!mapping$diagnostics$out_of_sample_validation_performed,mapping$diagnostics$bootstrap_samples==2)
