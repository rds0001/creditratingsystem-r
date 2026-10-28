.cb_diagnostics <- function(draws) {
 a<-as.matrix(draws);m<-nrow(a);n<-ncol(a)
 if(n<8 || m<2).cr_error('INSUFFICIENT_DRAWS','Two chains with eight retained draws required')
 half<-n%/%2L;a<-rbind(a[,seq_len(half),drop=FALSE],a[,seq.int(n-half+1,n),drop=FALSE]);m<-nrow(a);n<-ncol(a)
 statistics<-function(z) {
  W<-mean(apply(z,1,stats::var));B<-n*stats::var(rowMeans(z));variance<-(n-1)/n*W+B/n
  if(any(apply(z,1,stats::var)==0))return(list(rhat=NULL,ess=0))
  rhat<-sqrt(variance/W);center<-z-rowMeans(z);size<-2^ceiling(log2(2*n-1));autocov<-t(vapply(seq_len(m),function(i) {fourier<-stats::fft(c(center[i,],rep(0,size-n)));Re(stats::fft(fourier*Conj(fourier),inverse=TRUE))[seq_len(n)]/(size*n)},numeric(n)))
  rho<-1-(W-colMeans(autocov))/variance;rho[1]<-1;pairs<-numeric()
  for(k in seq.int(1,n-1,by=2)) {pair<-rho[k]+rho[k+1];if(pair<0)break;pairs<-c(pairs,if(length(pairs))min(pair,tail(pairs,1)) else pair)}
  tau<-max(1/log10(max(m*n,10)),-1+2*sum(pairs));list(rhat=rhat,ess=min(m*n,m*n/tau))
 }
 ranks<-function(z)matrix(stats::qnorm((rank(as.vector(z),ties.method='average')-3/8)/(length(z)+1/4)),nrow(z),ncol(z))
 ranked<-statistics(ranks(a));folded<-statistics(ranks(abs(a-stats::median(a))));limits<-stats::quantile(a,c(.05,.95),names=FALSE);low<-statistics(1*(a<=limits[1]));high<-statistics(1*(a>=limits[2]));raw<-statistics(a)
 list(rhat=if(is.null(ranked$rhat)||is.null(folded$rhat))NULL else max(ranked$rhat,folded$rhat),ess_bulk=ranked$ess,ess_tail=min(low$ess,high$ess),ess_mean=raw$ess,split_chains=m,draws_per_split=n)
}
.cb_logposterior <- function(beta,d,y,link,mean,sd) {
 eta<-as.vector(d%*%beta);likelihood<-if(link=='logit')sum(y*eta-pmax(eta,0)-log1p(exp(-abs(eta)))) else sum(ifelse(y==1,stats::pnorm(eta,log.p=TRUE),stats::pnorm(-eta,log.p=TRUE)))
 likelihood-.5*sum(((beta-mean)/sd)^2)-sum(log(sd))
}
.cb_array <- function(x,name) {
 if(is.array(x) && length(dim(x))==3L)return(.cr_numeric(x,name))
 if(!is.list(x) || !length(x)).cr_error('INVALID_SAMPLER',paste(name,'requires chain/step/parameter tensor'))
 matrices<-lapply(x,.cm_matrix);dims<-dim(matrices[[1]])
 if(any(vapply(matrices,function(m)!identical(dim(m),dims),logical(1)))).cr_error('INVALID_SAMPLER','Tensor shapes differ')
 a<-array(0,c(length(x),dims));for(i in seq_along(x))a[i,,]<-matrices[[i]];.cr_numeric(a,name)
}
fit_bayesian_pd <- function(X_train,y_train,feature_order,link,prior_mean,prior_sd,sampler_spec,group_spec=NULL,credible_level=.95,target,training,model_id,model_version,context=NULL) {
 design<-.cr_glm_design(X_train,feature_order,TRUE);x<-design$x;d<-design$d;y<-.cm_y(y_train,nrow(x),require_both=FALSE)
 if(!link %in% c('logit','probit')).cr_error('INVALID_INPUT','Bayesian link logit/probit required')
 level<-.cm_number(credible_level,'credible_level',strict=TRUE);if(level>=1).cr_error('INVALID_INPUT','credible_level must be below one')
 mean<-.cr_numeric(unlist(prior_mean),'prior_mean',vector_only=TRUE);sd<-.cr_numeric(unlist(prior_sd),'prior_sd',vector_only=TRUE)
 if(length(mean)!=ncol(d) || length(sd)!=ncol(d) || any(sd<=0)).cr_error('INVALID_PRIOR','Proper mean and positive prior SD for intercept/slopes required')
 groups<-NULL
 if(!is.null(group_spec)) {
  levels<-as.character(unlist(group_spec$levels));labels<-as.character(unlist(group_spec$labels));gsd<-.cm_number(group_spec$prior_sd,'group prior_sd',strict=TRUE)
  if(!length(levels) || anyDuplicated(levels) || anyNA(levels) || any(!nzchar(levels)) || length(labels)!=nrow(x) || anyNA(labels) || any(!labels %in% levels) || !setequal(labels,levels)).cr_error('INVALID_GROUP','Distinct observed group levels and known training labels required')
  z<-outer(labels,levels,'==')*1;d<-cbind(d,z);mean<-c(mean,rep(0,length(levels)));sd<-c(sd,rep(gsd,length(levels)));groups<-list(levels=as.list(levels),prior_sd=gsd,unknown_policy='ERROR',hyperparameter_inference='FIXED_GROUP_PRIOR_SD')
 }
 if(!is.list(sampler_spec) || !identical(sampler_spec$innovation_contract,'INDEPENDENT_STANDARD_NORMAL_AND_UNIFORM')).cr_error('INVALID_SAMPLER','Explicit independent innovation contract required')
 mat<-function(a,name)if(is.matrix(a)).cr_numeric(a,name) else .cr_numeric(.cm_matrix(a),name)
 initial<-mat(sampler_spec$initial_positions,'initial_positions');proposal<-mat(sampler_spec$proposal_cholesky,'proposal_cholesky');noise<-.cb_array(sampler_spec$normal_innovations,'normal_innovations');uniform<-mat(sampler_spec$uniforms,'uniforms');p<-ncol(d)
 if(ncol(initial)!=p || nrow(initial)<2 || !all(dim(proposal)==c(p,p)) || any(diag(proposal)<=0) || any(proposal[upper.tri(proposal)]!=0)).cr_error('INVALID_SAMPLER','Multiple initial chains and positive diagonal lower proposal Cholesky required')
 if(dim(noise)[1]!=nrow(initial) || dim(noise)[3]!=p || !all(dim(uniform)==dim(noise)[1:2]) || any(uniform<=0 | uniform>=1)).cr_error('INVALID_SAMPLER','Aligned innovations and open interval uniforms required')
 warmup<-sampler_spec$warmup;thin<-if(is.null(sampler_spec$thin))1L else sampler_spec$thin
 if(length(warmup)!=1 || is.logical(warmup) || warmup!=floor(warmup) || warmup<0 || warmup>=dim(noise)[2] || length(thin)!=1 || is.logical(thin) || thin!=floor(thin) || thin<1).cr_error('INVALID_SAMPLER','Integer warmup and positive thinning required')
 retained<-seq.int(warmup+1L,dim(noise)[2],by=thin);if(length(retained)<8).cr_error('INSUFFICIENT_DRAWS','Eight retained draws per chain required')
 draws<-array(0,c(nrow(initial),length(retained),p));accept<-numeric(nrow(initial));positions<-initial;post<-vapply(seq_len(nrow(initial)),function(i).cb_logposterior(initial[i,],d,y,link,mean,sd),numeric(1));ri<-rep(NA_integer_,dim(noise)[2]);ri[retained]<-seq_along(retained)
 if(any(!is.finite(post))).cr_error('INVALID_SAMPLER','Initial posterior densities must be finite')
 for(chain in seq_len(nrow(initial))) {
  accepted<-0L
  for(step in seq_len(dim(noise)[2])) {candidate<-positions[chain,]+as.vector(proposal%*%noise[chain,step,]);logp<-.cb_logposterior(candidate,d,y,link,mean,sd);if(is.finite(logp) && log(uniform[chain,step])<logp-post[chain]) {positions[chain,]<-candidate;post[chain]<-logp;accepted<-accepted+1L};if(!is.na(ri[step]))draws[chain,ri[step],]<-positions[chain,]}
  accept[chain]<-accepted/dim(noise)[2]
 }
 metrics<-lapply(seq_len(p),function(j).cb_diagnostics(matrix(draws[,,j],nrow(initial),length(retained))));converged<-all(vapply(metrics,function(v)!is.null(v$rhat)&&v$rhat<=1.01&&v$ess_bulk>=400&&v$ess_tail>=400,logical(1)))
 diagnostics<-list(sampler='FIXED_PROPOSAL_RANDOM_WALK_METROPOLIS',initial_positions=.cm_rows(initial),innovation_fingerprint=.cr_hash(list(normal_innovations=lapply(seq_len(dim(noise)[1]),function(i).cm_rows(matrix(noise[i,,],dim(noise)[2],dim(noise)[3]))),uniforms=.cm_rows(uniform))),convergence_status=if(converged)'DIAGNOSTICS_PASSED' else 'DIAGNOSTICS_FAILED_OR_INSUFFICIENT',parameter_diagnostics=metrics,acceptance_rates=as.list(accept),warmup=warmup,thin=thin,steps=dim(noise)[2],chains=nrow(initial),retained_per_chain=length(retained),diagnostic_thresholds=list(rhat_max=1.01,ess_bulk_min=400,ess_tail_min=400),innovation_assumption=sampler_spec$innovation_contract,prior_sensitivity='REQUIRES_REFITTING_DECLARED_ALTERNATIVE_PRIORS',uncertainty='POSTERIOR_CONDITIONAL_ON_PRIOR_AND_MODEL',training_only=TRUE)
 parameters<-list(kind='bayesian',link=link,fit_intercept=TRUE,prior_mean=as.list(mean),prior_sd=as.list(sd),posterior_draws=lapply(seq_len(nrow(initial)),function(i).cm_rows(matrix(draws[i,,],length(retained),p))),groups=groups,credible_level=level,proposal_cholesky=.cm_rows(proposal))
 .cm_model('fit_bayesian_pd',feature_order,parameters,list(transformation='identity'),diagnostics,target,training,model_id,model_version,context)
}
.cb_prediction <- function(model,X,feature_order) {
 params<-model$parameters;groups<-params$groups
 if(!is.null(groups)) {
  if(!is.list(X) || is.null(X$X)).cr_error('INVALID_GROUP','Grouped Bayesian prediction requires X/groups')
  design<-.cr_glm_design(X$X,feature_order,TRUE);labels<-as.character(unlist(X$groups));levels<-as.character(unlist(groups$levels))
  if(length(labels)!=nrow(design$x) || anyNA(labels) || any(!labels %in% levels)).cr_error('UNKNOWN_GROUP','Known trained group label per prediction row required')
  d<-cbind(design$d,outer(labels,levels,'==')*1)
 } else d<-.cr_glm_design(X,feature_order,TRUE)$d
 draws<-.cb_array(params$posterior_draws,'posterior_draws');means<-sd<-variance<-numeric(nrow(d));intervals<-matrix(0,nrow(d),2);metrics<-mcse<-list();q<-(1-params$credible_level)/2
 for(i in seq_len(nrow(d))) {
  eta<-matrix(0,dim(draws)[1],dim(draws)[2]);for(chain in seq_len(nrow(eta)))eta[chain,]<-matrix(draws[chain,,],dim(draws)[2],dim(draws)[3])%*%d[i,]
  probability<-if(params$link=='logit')plogis(eta) else pnorm(eta);means[i]<-mean(probability);intervals[i,]<-stats::quantile(probability,c(q,1-q),names=FALSE);sd[i]<-stats::sd(as.vector(probability));variance[i]<-means[i]*(1-means[i]);metrics[[i]]<-.cb_diagnostics(probability);mcse[i]<-list(if(metrics[[i]]$ess_mean>0)sd[i]/sqrt(metrics[[i]]$ess_mean) else NULL)
 }
 list(probability=means,diagnostics=list(credible_level=params$credible_level,pd_credible_intervals=.cm_rows(intervals),posterior_sd=as.list(sd),posterior_predictive_event_variance=as.list(variance),mean_pd_mcse=mcse,prediction_chain_diagnostics=metrics,fit_convergence_status=model$diagnostics$convergence_status,interpretation='Intervals for latent conditional event probability, not prediction intervals for individual Bernoulli outcome'))
}
