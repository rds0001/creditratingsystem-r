library(creditratingsystem)
for(name in c('.cb_prediction','.cb_diagnostics'))if(!exists(name))assign(name,getFromNamespace(name,'creditratingsystem'))
public_check<-function(m,x,features,expected) {stopifnot(max(abs(unlist(predict_pd(m,x,feature_order=features,expected_target=target)$value)-expected))<1e-12);for(z in list(list(offset=0),list(context=list(horizon=2)))) {e<-tryCatch(do.call(predict_pd,c(list(m,x,feature_order=features,expected_target=target),z)),error=identity);stopifnot(inherits(e,'error'),e$code%in%c('OFFSET_MISMATCH','TARGET_MISMATCH'))}}
target<-list(event='default',horizon=1,time_unit='years',probability_measure='PHYSICAL',conditioning='performing');meta<-list(target=target,training=list(data_fingerprint='bayes-analytic',split_id='train',known_at='2025-01-01',data_origin='SYNTHETIC'),model_id='bayes',model_version='1')
fit<-function(...)do.call(fit_bayesian_pd,c(list(...),meta));set.seed(981)
sampler<-function(p=1L,steps=7000L)list(initial_positions=matrix(0,4,p),proposal_cholesky=diag(.9,p),normal_innovations=array(rnorm(4*steps*p),c(4,steps,p)),uniforms=matrix(runif(4*steps),4,steps),warmup=1000L,thin=2L,innovation_contract='INDEPENDENT_STANDARD_NORMAL_AND_UNIFORM')
extended<-identical(tolower(Sys.getenv('CREDITRATINGSYSTEM_EXTENDED_TESTS','false')),'true')
if(extended) {
for(link in c('logit','probit')) {
 m<-fit(matrix(numeric(),12,0),c(rep(0,11),1),feature_order=character(),link=link,prior_mean=-1,prior_sd=1.5,sampler_spec=sampler())
 result<-.cb_prediction(m$model,matrix(numeric(),1,0),character());cdf<-if(link=='logit')plogis else pnorm
 density<-function(b)cdf(b)*(1-cdf(b))^11*dnorm(b,-1,1.5)
 z<-integrate(density,-15,15,abs.tol=1e-12)$value;expected<-integrate(function(b)cdf(b)*density(b),-15,15,abs.tol=1e-12)$value/z
 stopifnot(abs(result$probability-expected)<4*result$diagnostics$mean_pd_mcse[[1]]+.001,m$diagnostics$parameter_diagnostics[[1]]$rhat<1.03,m$diagnostics$parameter_diagnostics[[1]]$ess_bulk>400)
}
stuck<-.cb_diagnostics(matrix(0,4,20));stopifnot(is.null(stuck$rhat),stuck$ess_bulk==0)
nonmixed<-matrix(rnorm(400),4,100)+c(0,4,8,12);stopifnot(.cb_diagnostics(nonmixed)$rhat>1.1)
m<-fit(matrix(numeric(),8,0),c(0,0,0,1,0,1,1,1),feature_order=character(),link='logit',prior_mean=0,prior_sd=1,group_spec=list(levels=c('a','b'),labels=rep(c('a','b'),each=4),prior_sd=.7),sampler_spec=sampler(p=3L,steps=2000L))
result<-.cb_prediction(m$model,list(X=matrix(numeric(),2,0),groups=c('a','b')),character());stopifnot(result$probability[1]<result$probability[2])

one_stuck<-.cb_diagnostics(rbind(rep(0,100),matrix(rnorm(300),3,100)));stopifnot(is.null(one_stuck$rhat),one_stuck$ess_bulk==0)

}
for(link in c('logit','probit')) {
 spec<-sampler(steps=80L);spec$warmup<-20L;spec$thin<-1L
 m<-fit(matrix(numeric(),4,0),c(0,0,0,1),feature_order=character(),link=link,prior_mean=-1,prior_sd=1,sampler_spec=spec)
 result<-.cb_prediction(m$model,matrix(numeric(),1,0),character())
 stopifnot(result$probability>0,result$probability<1);public_check(m,matrix(numeric(),1,0),character(),result$probability)
}
stuck<-.cb_diagnostics(matrix(0,4,20));stopifnot(is.null(stuck$rhat),stuck$ess_bulk==0)
