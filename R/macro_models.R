.cr_m_vecm_design <- function(x,p,deterministic) {
 n<-nrow(x);k<-ncol(x);delta<-x[-1L,,drop=FALSE]-x[-n,,drop=FALSE];response<-delta[p:(n-1L),,drop=FALSE];levels<-x[p:(n-1L),,drop=FALSE]
 outside<-deterministic %in% c('constant_outside','constant_outside_trend_inside','constant_trend_outside')
 if(deterministic=='constant_inside')levels<-cbind(levels,1)else if(deterministic=='constant_outside_trend_inside')levels<-cbind(levels,p:(n-1L))
 short<-matrix(numeric(),n-p,0L)
 if(p>1L)for(j in seq_len(p-1L))short<-cbind(short,delta[(p-j):(n-1L-j),,drop=FALSE])
 if(outside)short<-cbind(short,1)
 if(deterministic=='constant_trend_outside')short<-cbind(short,(p+1L):n)
 list(response=response,levels=levels,short=short)
}
fit_vecm <- function(level_history,rank,lags,deterministic='constant_inside',integration_order='I1',training,target,model_id,model_version,context=NULL) {
 dat<-.cr_m_history(level_history,training);x<-dat$x;dates<-dat$dates;h<-dat$h;n<-nrow(x);k<-ncol(x);p<-.cr_m_integer(lags,'lags',1);r<-.cr_m_integer(rank,'rank',0)
 if(k<2L||r>=k||length(deterministic)!=1L||!deterministic %in% c('none','constant_inside','constant_outside','constant_outside_trend_inside','constant_trend_outside')||!identical(integration_order,'I1')) .cr_error('MACRO_VECM_SPEC','Require >=2 I(1) level series, rank 0..k-1 and supported deterministic terms')
 if(!is.null(h$inverse_state)&&any(vapply(h$inverse_state,function(s)s$transform %in% c('difference','log_difference'),logical(1)))) .cr_error('MACRO_VECM_SPEC','VECM requires levels, not already differenced history')
 design<-.cr_m_vecm_design(x,p,deterministic);y<-design$response;z<-design$levels;d<-design$short
 if(nrow(y)<=ncol(d)+ncol(z)+k||(ncol(d)>0L&&qr(d)$rank<ncol(d))) .cr_error('INSUFFICIENT_OBSERVATIONS','Full-rank short-run design and enough observations required')
 r0<-if(ncol(d))stats::lm.fit(d,y)$residuals else y;r1<-if(ncol(d))stats::lm.fit(d,z)$residuals else z
 s00<-crossprod(r0)/nrow(y);s11<-crossprod(r1)/nrow(y);s01<-crossprod(r0,r1)/nrow(y)
 linear<-tryCatch({lower<-t(chol(s11));middle<-t(s01)%*%solve(s00,s01);whitened<-solve(lower,middle)%*%solve(t(lower));eig<-eigen((whitened+t(whitened))/2,symmetric=TRUE);list(lower=lower,eig=eig)},error=identity)
 if(inherits(linear,'error')) .cr_error('RANK_DEFICIENT','Johansen covariance is singular')
 eig<-linear$eig;eigenvalues<-eig$values[seq_len(k)]
 if(r>0L&&p>=2L&&deterministic %in% c('constant_inside','constant_outside','constant_outside_trend_inside')) {
  colnames(x)<-paste0('v',seq_len(k));ecdet<-switch(deterministic,constant_inside='const',constant_outside='none',constant_outside_trend_inside='trend')
  fitted<-tryCatch(urca::ca.jo(x,type='trace',ecdet=ecdet,K=p,spec='transitory'),error=identity)
  if(inherits(fitted,'error')) .cr_error('RANK_DEFICIENT','Native urca Johansen fit failed',list(reason=conditionMessage(fitted)))
  beta<-fitted@V[,seq_len(r),drop=FALSE];backend<-'urca::ca.jo Johansen ML'
 } else {
  # Same reduced-rank likelihood for K=1, rank=0, and deterministic designs
  # not admitted by ca.jo; no extra lag or constant is silently introduced.
  beta<-if(r>0L)solve(t(linear$lower),eig$vectors[,seq_len(r),drop=FALSE])else matrix(numeric(),ncol(z),0L)
  backend<-'Native generalized-eigen Johansen ML'
 }
 if(r>0L){norm<-tryCatch(solve(beta[seq_len(r),,drop=FALSE]),error=identity);if(inherits(norm,'error')) .cr_error('IDENTIFICATION_FAILURE','First rank variables cannot identify cointegration basis');beta<-beta%*%norm}
 full<-cbind(z%*%beta,d)
 if(ncol(full)){fitted<-stats::lm.fit(full,y);coefficient<-as.matrix(fitted$coefficients);residual<-as.matrix(fitted$residuals)}else{coefficient<-matrix(numeric(),0L,k);residual<-y}
 alpha<-t(coefficient[seq_len(r),,drop=FALSE]);gamma<-t(coefficient[r+seq_len(k*(p-1L)),,drop=FALSE]);outside<-t(coefficient[r+k*(p-1L)+seq_len(nrow(coefficient)-r-k*(p-1L)),,drop=FALSE])
 covariance<-crossprod(residual)/nrow(y);factorization<-.cr_m_sqrt_cov(covariance)$diagnostics;pi<-alpha%*%t(beta[seq_len(k),,drop=FALSE]);coefs<-rep(list(matrix(0,k,k)),p);coefs[[1]]<-diag(k)+pi
 if(p>1L)for(j in seq_len(p-1L)){g<-gamma[,(j-1L)*k+seq_len(k),drop=FALSE];coefs[[j]]<-coefs[[j]]+g;coefs[[j+1L]]<-coefs[[j+1L]]-g}
 intercept<-rep(0,k);trend<-rep(0,k)
 if(deterministic %in% c('constant_outside','constant_outside_trend_inside','constant_trend_outside'))intercept<-outside[,1]
 if(deterministic=='constant_trend_outside')trend<-outside[,2]
 if(deterministic=='constant_inside'&&r>0L)intercept<-intercept+as.vector(alpha%*%beta[k+1L,])
 if(deterministic=='constant_outside_trend_inside'&&r>0L){trend<-trend+as.vector(alpha%*%beta[k+1L,]);intercept<-intercept-as.vector(alpha%*%beta[k+1L,])}
 companion<-matrix(0,k*p,k*p);companion[seq_len(k),]<-do.call(cbind,coefs);if(p>1L)companion[(k+1L):(k*p),seq_len(k*(p-1L))]<-diag(k*(p-1L))
 roots<-eigen(companion,only.values=TRUE)$values;roots<-roots[order(Re(roots),Im(roots))];radius<-max(Mod(roots))
 if(any(eigenvalues>1+1e-8|eigenvalues< -1e-8)) .cr_error('NUMERICAL_FAILURE','Johansen eigenvalues outside covariance projection domain')
 traces<-vapply(seq_len(k),function(j)-nrow(y)*sum(log1p(-pmin(pmax(eigenvalues[j:k],0),1-1e-15))),numeric(1))
 params<-list(dynamics='vecm',lags=p,rank=r,integration_order='I1',deterministic=deterministic,alpha=.cr_m_rows(alpha),beta=.cr_m_rows(beta[seq_len(k),,drop=FALSE]),cointegration_deterministic=.cr_m_rows(beta[k+seq_len(nrow(beta)-k),,drop=FALSE]),short_run=.cr_m_rows(gamma),outside_deterministic=.cr_m_rows(outside),lag_coefficients=lapply(coefs,.cr_m_rows),intercept=as.list(unname(intercept)),trend=as.list(unname(trend)),covariance=.cr_m_rows(covariance),covariance_estimator='mle',initial_history=.cr_m_rows(x[(n-p+1L):n,,drop=FALSE]),next_time_index=n+1L,'_training_values'=.cr_m_rows(x))
 diagnostics<-list(backend=backend,observations=nrow(y),residuals=.cr_m_rows(residual),residual_dates=as.list(dates[(p+1L):n]),rank_assumed=r,integration_order_assumed='I1',normalization='FIRST_R_VARIABLE_ROWS_IDENTITY',johansen_eigenvalues=as.list(eigenvalues),rank_trace_statistics=lapply(seq_len(k),function(j)list(null_rank=j-1L,statistic=traces[j])),critical_values_not_inferred=TRUE,cointegration_residuals=.cr_m_rows(z%*%beta),companion_eigenvalues=lapply(roots,function(v)list(Re(v),Im(v))),spectral_radius=radius,expected_unit_roots=k-r,explosive=radius>1+1e-8,eigenvalue_repair='NONE',factorization=factorization,training_only=TRUE)
 .cr_m_model('fit_vecm',params,h,dates,training,target,model_id,model_version,diagnostics,context)
}
.cr_m_lse <- function(x){m<-max(x);if(!is.finite(m))return(m);m+log(sum(exp(x-m)))}
.cr_m_hmm_expectation <- function(y,design,coefficients,covariances,transition,initial) {
 n<-nrow(y);k<-ncol(y);r<-length(initial);emission<-matrix(0,n,r)
 for(j in seq_len(r)){
  cov<-covariances[[j]];corr<-cov/sqrt(outer(diag(cov),diag(cov)))
  if(any(diag(cov)<=0)||min(eigen(corr,symmetric=TRUE,only.values=TRUE)$values)<=1e-12) .cr_error('REGIME_DEGENERACY','Positive definite regime covariance required without inferred ridge')
  residual<-y-design%*%coefficients[[j]];emission[,j]<--.5*(k*log(2*pi)+as.numeric(determinant(cov,logarithm=TRUE)$modulus)+rowSums(residual*t(solve(cov,t(residual)))))
 }
 logp<-log(transition);alpha<-matrix(0,n,r);normal<-numeric(n);alpha[1,]<-log(initial)+emission[1,];normal[1]<-.cr_m_lse(alpha[1,]);alpha[1,]<-alpha[1,]-normal[1]
 if(n>1L)for(t in 2:n){for(j in seq_len(r))alpha[t,j]<-emission[t,j]+.cr_m_lse(alpha[t-1L,]+logp[,j]);normal[t]<-.cr_m_lse(alpha[t,]);alpha[t,]<-alpha[t,]-normal[t]}
 beta<-matrix(0,n,r);xi<-matrix(0,r,r)
 if(n>1L)for(t in (n-1L):1L){for(j in seq_len(r))beta[t,j]<-.cr_m_lse(logp[j,]+emission[t+1L,]+beta[t+1L,])-normal[t+1L];pair<-sweep(sweep(logp,1,alpha[t,],'+'),2,emission[t+1L,]+beta[t+1L,],'+');xi<-xi+exp(pair-.cr_m_lse(pair))}
 gamma<-alpha+beta;for(t in seq_len(n))gamma[t,]<-exp(gamma[t,]-.cr_m_lse(gamma[t,]))
 list(ll=sum(normal),gamma=gamma,xi=xi,filtered=exp(alpha))
}
fit_regime_switching <- function(transformed_history,regime_count,lags=0,initialization,max_iterations=200,tolerance=1e-8,training,target,model_id,model_version,context=NULL) {
 dat<-.cr_m_history(transformed_history,training);x<-dat$x;n<-nrow(x);k<-ncol(x);r<-.cr_m_integer(regime_count,'regime_count',1);p<-.cr_m_integer(lags,'lags',0);maxit<-.cr_m_integer(max_iterations,'max_iterations',1)
 if(length(tolerance)!=1L||!is.finite(tolerance)||tolerance<=0) .cr_error('INVALID_TOLERANCE','Positive finite convergence tolerance required')
 if(n-p<=1+k*p||!is.list(initialization)||!length(initialization)) .cr_error('REGIME_SPEC','Sufficient observations and explicit starts required')
 design<-matrix(1,n-p,1);if(p)for(j in seq_len(p))design<-cbind(design,x[(p-j+1L):(n-j),,drop=FALSE]);y<-x[(p+1L):n,,drop=FALSE];reports<-list();best<-NULL
 for(start_id in seq_along(initialization)) {
  result<-tryCatch({
   start<-initialization[[start_id]];transition<-.cr_m_matrix(start$transition,'transition');initial<-.cr_numeric(unlist(start$initial_probabilities),'initial_probabilities',vector_only=TRUE);coef<-lapply(start$coefficients,function(v).cr_m_matrix(v,'coefficients'));cov<-lapply(start$covariances,function(v).cr_m_matrix(v,'covariances'))
   if(!identical(dim(transition),c(r,r))||length(initial)!=r||length(coef)!=r||length(cov)!=r||any(!vapply(coef,function(v)identical(dim(v),c(1L+k*p,k)),logical(1)))||any(!vapply(cov,function(v)identical(dim(v),c(k,k)),logical(1)))) .cr_error('INCOMPATIBLE_SHAPE','Regime start dimensions inconsistent')
   if(any(transition<0)||any(initial<0)||any(abs(rowSums(transition)-1)>1e-12)||abs(sum(initial)-1)>1e-12) .cr_error('INVALID_TRANSITION','Initial and transition probabilities must normalize')
   for(c in cov).cr_m_sqrt_cov(c);trace<-numeric();converged<-FALSE
   for(iteration in 0:maxit){
    ex<-.cr_m_hmm_expectation(y,design,coef,cov,transition,initial);trace<-c(trace,ex$ll)
    if(length(trace)>1L){change<-tail(trace,1)-trace[length(trace)-1L];if(change< -1e-7*(1+abs(trace[length(trace)-1L]))) .cr_error('NUMERICAL_FAILURE','EM likelihood decreased beyond rounding');if(abs(change)<=tolerance*(1+abs(trace[length(trace)-1L]))){converged<-TRUE;break}}
    if(iteration==maxit)break
    for(j in seq_len(r)){weights<-ex$gamma[,j];weighted<-design*sqrt(weights);if(sum(weights)<=ncol(design)||qr(weighted)$rank<ncol(design)) .cr_error('REGIME_DEGENERACY','Insufficient occupancy or weighted design');coef[[j]]<-stats::lm.fit(weighted,y*sqrt(weights))$coefficients;coef[[j]]<-matrix(coef[[j]],nrow=ncol(design),ncol=k);residual<-y-design%*%coef[[j]];cov[[j]]<-crossprod(residual*sqrt(weights))/sum(weights)}
    transition<-ex$xi/rowSums(ex$xi);initial<-ex$gamma[1,]
   }
   list(ll=ex$ll,coef=coef,cov=cov,transition=transition,initial=initial,gamma=ex$gamma,filtered=ex$filtered,iteration=iteration,converged=converged,trace=trace)
  },creditratingsystem_error=function(e)e)
  if(inherits(result,'creditratingsystem_error')){reports[[length(reports)+1L]]<-list(start_index=start_id-1L,status='FAILED',error_code=result$code);next}
  reports[[length(reports)+1L]]<-list(start_index=start_id-1L,status=if(result$converged)'CONVERGED'else 'MAX_ITERATIONS',log_likelihood=result$ll,iterations=result$iteration,likelihood_trace=as.list(result$trace))
  if(is.null(best)||result$ll>best$ll){best<-result;best$start_id<-start_id-1L}
 }
 if(is.null(best)) .cr_error('REGIME_ESTIMATION_FAILED','All explicit HMM starts failed')
 intercepts<-do.call(rbind,lapply(best$coef,function(v)v[1,]));order<-do.call(order,c(lapply(seq_len(k),function(j)intercepts[,j]),list(seq_len(r))));coef<-best$coef[order];cov<-best$cov[order];transition<-best$transition[order,order,drop=FALSE];initial<-best$initial[order];gamma<-best$gamma[,order,drop=FALSE];filtered<-best$filtered[,order,drop=FALSE]
 params<-list(dynamics='regime',regime_count=r,lags=p,coefficients=lapply(coef,.cr_m_rows),covariances=lapply(cov,.cr_m_rows),transition=.cr_m_rows(transition),initial_probabilities=as.list(initial),filtered_probabilities_end=as.list(filtered[nrow(filtered),]),initial_history=if(p).cr_m_rows(tail(x,p))else list(),'_training_values'=.cr_m_rows(x))
 diag<-list(backend='native Gaussian autoregressive HMM EM',log_likelihood=best$ll,converged=best$converged,selected_start=best$start_id,starts=reports,filtered_probabilities=.cr_m_rows(filtered),smoothed_training_probabilities=.cr_m_rows(gamma),probability_dates=as.list(dat$dates[(p+1L):n]),ordering_rule='LEXICOGRAPHIC_INTERCEPT_ASCENDING',original_regime_order=as.list(order-1L),transition_orientation='ROW_ORIGIN_COLUMN_DESTINATION',training_only=TRUE,global_optimum_claimed=FALSE,smoothing_uses_entire_training_window=TRUE,emission_specification='OBSERVED_LAGS_REGIME_SPECIFIC_COEFFICIENTS')
 .cr_m_model('fit_regime_switching',params,dat$h,dat$dates,training,target,model_id,model_version,diag,context)
}
.cr_m_simulate_latent <- function(model,innovations,initial_state=NULL,regime_uniforms=NULL,output_space='model',sampling_contract=NULL,context=NULL) {
 p<-model$parameters;ctx<-.cr_m_measure(model,context)
 if(p$dynamics=='dynamic_factor')return(.cr_m_simulate_factor(model,innovations,initial_state,regime_uniforms,output_space,sampling_contract,context))
 input<-.cr_m_scenario_inputs(innovations,model$feature_order,model$preprocessing$frequency);s<-dim(input$z)[1];h<-dim(input$z)[2];k<-dim(input$z)[3]
 if(input$kind!='STANDARD_NORMAL') .cr_error('MACRO_INNOVATIONS','Regime-dependent covariance requires standard-normal draws')
 u<-.cr_m_matrix(regime_uniforms,'regime_uniforms');r<-.cr_m_integer(p$regime_count,'regime_count',1);lags<-.cr_m_integer(p$lags,'lags',0)
 if(!identical(dim(u),c(s,h+1L))||any(u<0|u>=1)) .cr_error('MACRO_INNOVATIONS','Regime uniforms need scenario by initial plus forecast axes in [0,1)')
 transition<-.cr_m_matrix(p$transition,'transition');initial<-unlist(p$filtered_probabilities_end)
 if(!identical(dim(transition),c(r,r))||any(transition<0)||any(abs(rowSums(transition)-1)>1e-10)||length(initial)!=r||any(initial<0)||abs(sum(initial)-1)>1e-10) .cr_error('INVALID_MODEL','Invalid frozen regime probabilities')
 coef<-lapply(p$coefficients,function(v).cr_m_matrix(v,'coefficients'));cov<-lapply(p$covariances,function(v).cr_m_matrix(v,'covariances'));roots<-lapply(cov,function(v).cr_m_sqrt_cov(v)$root)
 if(length(coef)!=r||any(!vapply(coef,function(v)identical(dim(v),c(1L+k*lags,k)),logical(1)))||length(roots)!=r||any(!vapply(roots,function(v)identical(dim(v),c(k,k)),logical(1)))) .cr_error('INVALID_MODEL','Invalid regime coefficient dimensions')
 sampling<-if(is.null(sampling_contract))list(law='FORECAST_CONDITIONAL')else sampling_contract
 if(identical(sampling$law,'FORECAST_CONDITIONAL')){if(identical(ctx$conditioning,'TTC')) .cr_error('TTC_INITIALIZATION','Filtered regime forecast does not constitute TTC');sampling$conditioning_scope<-'PIT';sampling$initialization<-'FILTERED_REGIME_POSTERIOR'}else if(identical(sampling$law,'STATIONARY_MARKOV_EMISSION')){
  if(lags>0L||!is.null(initial_state)) .cr_error('TTC_INITIALIZATION','Stationary Markov emission requires zero observed lags')
  if(any(!vapply(c('cycle_law_id','equilibrium_assumptions'),function(v)is.character(sampling[[v]])&&length(sampling[[v]])==1L&&nzchar(trimws(sampling[[v]])),logical(1)))) .cr_error('TTC_INITIALIZATION','Cycle-law and assumptions required')
  if(qr(diag(r)-t(transition))$rank!=r-1L) .cr_error('NONSTATIONARY_TTC','Unique stationary regime law required')
  initial<-as.vector(qr.solve(rbind(diag(r)-t(transition),rep(1,r)),c(rep(0,r),1)));if(any(initial< -1e-10)) .cr_error('NONSTATIONARY_TTC','Invalid stationary weights');initial<-pmax(initial,0);initial<-initial/sum(initial);sampling$conditioning_scope<-'TTC_STATIONARY';sampling$stationary_regime_probabilities<-as.list(initial)
 }else .cr_error('TTC_INITIALIZATION','Regimes require filtered posterior or stationary zero-lag Markov law')
 state<-if(lags).cr_m_matrix(if(is.null(initial_state))p$initial_history else initial_state,'initial_state')else matrix(numeric(),0,k)
 if(!identical(dim(state),c(lags,k))) .cr_error('INCOMPATIBLE_SHAPE','Regime initial history shape differs')
 paths<-array(0,c(s,h,k));regimes<-matrix(0,s,h)
 for(i in seq_len(s)){
  previous<-min(r,which(u[i,1]<cumsum(initial))[1]);current<-state
  for(t in seq_len(h)){j<-min(r,which(u[i,t+1L]<cumsum(transition[previous,]))[1]);regimes[i,t]<-j-1L;design<-c(1,if(lags)as.vector(t(current[lags:1L,,drop=FALSE]))else numeric());value<-as.vector(design%*%coef[[j]]+roots[[j]]%*%input$z[i,t,]);paths[i,t,]<-value;previous<-j;if(lags)current<-rbind(if(lags>1L)current[-1L,,drop=FALSE]else matrix(numeric(),0,k),value)}
 }
 if(output_space=='level'){if(!is.null(initial_state)||sampling$law!='FORECAST_CONDITIONAL') .cr_error('MACRO_TRANSFORM','Alternative initial law requires explicit original anchors');paths<-.cr_m_inverse_paths(paths,model$preprocessing$inverse_state)}else if(output_space!='model') .cr_error('MACRO_TRANSFORM','output_space must be model or level')
 .cr_m_cube(model,paths,input$ids,input$times,input$w,list(output_space=output_space,sampling_contract=sampling,regime_paths=.cr_m_rows(regimes),regime_uniform_hash=.cr_hash(.cr_m_rows(u)),innovation_hash=.cr_hash(innovations),shared_macro_across_entities=TRUE),context,'simulate_macro_scenarios')
}
.cr_m_factor_layout <- function(k,q) {
 out<-matrix(integer(),0,2);for(i in seq_len(k))for(j in seq_len(q))if(i>q||j<i)out<-rbind(out,c(i,j));out
}
.cr_m_factor_unpack <- function(theta,k,q) {
 positions<-.cr_m_factor_layout(k,q);load<-matrix(0,k,q);load[seq_len(q),]<-diag(q)
 if(nrow(positions))for(a in seq_len(nrow(positions)))load[positions[a,1],positions[a,2]]<-theta[a]
 offset<-nrow(positions);m<-q+k;list(load=load,mean=theta[offset+seq_len(k)],ar=theta[offset+k+seq_len(m)],variance=exp(theta[offset+k+m+seq_len(m)]))
}
.cr_m_factor_filter <- function(x,load,mean,ar,variance,states=FALSE) {
 k<-nrow(load);q<-ncol(load);m<-q+k;y<-sweep(x,2,mean);design<-cbind(load,diag(k))
 SSMcustom<-KFAS::SSMcustom
 model<-KFAS::SSModel(y ~ -1 + SSMcustom(Z=design,index=seq_len(k),T=diag(ar,m),R=diag(m),Q=diag(variance,m),a1=rep(0,m),P1=diag(variance/(1-ar^2),m),P1inf=matrix(0,m,m)),H=matrix(0,k,k))
 if(states){fit<-KFAS::KFS(model,filtering='state',smoothing='none');return(list(ll=as.numeric(stats::logLik(model)),filtered=fit$att,covariance=fit$Ptt))}
 as.numeric(stats::logLik(model))
}
fit_dynamic_factor <- function(transformed_history,factor_count,initialization,ar_limit=.995,variance_bounds=c(1e-8,1e4),max_iterations=300,tolerance=1e-8,training,target,model_id,model_version,context=NULL) {
 dat<-.cr_m_history(transformed_history,training,missing=TRUE);x<-dat$x;n<-nrow(x);k<-ncol(x);q<-.cr_m_integer(factor_count,'factor_count',1);m<-q+k
 if(q>=k||any(colSums(is.finite(x))<=2*m)) .cr_error('FACTOR_IDENTIFICATION','More variables than factors and sufficient observed entries required')
 limits<-.cr_numeric(unlist(variance_bounds),'variance_bounds',vector_only=TRUE)
 if(length(limits)!=2L||limits[1]<=0||limits[2]<=limits[1]||length(ar_limit)!=1L||!is.finite(ar_limit)||ar_limit<=0||ar_limit>=1) .cr_error('FACTOR_SPEC','Positive variance bounds and stationary AR bound required')
 if(!is.list(initialization)) .cr_error('FACTOR_SPEC','Explicit identified initial parameters required')
 load<-.cr_m_matrix(initialization$loadings,'loadings');mean<-.cr_numeric(unlist(initialization$mean),'mean',vector_only=TRUE);ar<-.cr_numeric(unlist(initialization$ar),'ar',vector_only=TRUE);variance<-.cr_numeric(unlist(initialization$variances),'variances',vector_only=TRUE)
 if(!identical(dim(load),c(k,q))||length(mean)!=k||length(ar)!=m||length(variance)!=m||any(abs(ar)>ar_limit)||any(variance<limits[1]|variance>limits[2])) .cr_error('FACTOR_SPEC','Initial parameter dimensions or bounds invalid')
 anchor<-load[seq_len(q),,drop=FALSE];if(any(diag(anchor)!=1)||any(anchor[upper.tri(anchor)]!=0)) .cr_error('FACTOR_IDENTIFICATION','Unit-diagonal lower-triangular loading anchors required')
 positions<-.cr_m_factor_layout(k,q);theta<-c(if(nrow(positions))vapply(seq_len(nrow(positions)),function(a)load[positions[a,1],positions[a,2]],numeric(1))else numeric(),mean,ar,log(variance));free<-nrow(positions)+k;lower<-c(rep(-Inf,free),rep(-ar_limit,m),rep(log(limits[1]),m));upper<-c(rep(Inf,free),rep(ar_limit,m),rep(log(limits[2]),m))
 objective<-function(v){pars<-.cr_m_factor_unpack(v,k,q);-.cr_m_factor_filter(x,pars$load,pars$mean,pars$ar,pars$variance)}
 initial_objective<-objective(theta);maxit<-.cr_m_integer(max_iterations,'max_iterations',1)
 if(length(tolerance)!=1L||!is.finite(tolerance)||tolerance<=0) .cr_error('INVALID_TOLERANCE','Positive tolerance required')
 fit<-stats::optim(theta,objective,method='L-BFGS-B',lower=lower,upper=upper,control=list(maxit=maxit,factr=tolerance/.Machine$double.eps,pgtol=tolerance,ndeps=rep(1e-6,length(theta))))
 pars<-.cr_m_factor_unpack(fit$par,k,q);filtered<-.cr_m_factor_filter(x,pars$load,pars$mean,pars$ar,pars$variance,TRUE);design<-cbind(pars$load,diag(k))
 if(!is.finite(filtered$ll)||filtered$ll< -initial_objective-1e-7) .cr_error('NUMERICAL_FAILURE','State-space optimizer returned inferior or invalid likelihood')
 names<-c(paste0('factor_',seq_len(q)),paste0('idiosyncratic_',dat$variables));training_values<-lapply(seq_len(n),function(i)lapply(seq_len(k),function(j)if(is.na(x[i,j]))NULL else x[i,j]))
 params<-list(dynamics='dynamic_factor',factor_count=q,loadings=.cr_m_rows(pars$load),mean=as.list(pars$mean),ar=as.list(pars$ar),variances=as.list(pars$variance),state_transition=.cr_m_rows(diag(pars$ar,m)),state_covariance=.cr_m_rows(diag(pars$variance,m)),observation_matrix=.cr_m_rows(design),state_ids=as.list(names),filtered_state_end=as.list(unname(filtered$filtered[n,])),filtered_covariance_end=.cr_m_rows(filtered$covariance[,,n]),stationary_covariance=.cr_m_rows(diag(pars$variance/(1-pars$ar^2),m)),'_training_values'=training_values)
 diag<-list(backend='KFAS Kalman likelihood / stats optim L-BFGS-B',converged=fit$convergence==0L,optimizer_status=fit$convergence,optimizer_message=if(is.null(fit$message))''else fit$message,function_evaluations=unname(fit$counts[['function']]),log_likelihood=filtered$ll,initial_log_likelihood= -initial_objective,identification='FIRST_FACTOR_ROWS_UNIT_DIAGONAL_LOWER_TRIANGULAR',factor_lag_order=1,idiosyncratic_lag_order=1,ar_limit=ar_limit,variance_bounds=as.list(limits),boundary_parameters=as.list(which(abs(fit$par-lower)<1e-6|abs(fit$par-upper)<1e-6)-1L),missing_entries=sum(is.na(x)),missing_policy='OBSERVED_SUBVECTOR_KALMAN_LIKELIHOOD',filtered_states=.cr_m_rows(filtered$filtered),state_dates=as.list(dat$dates),training_only=TRUE,global_optimum_claimed=FALSE)
 .cr_m_model('fit_dynamic_factor',params,dat$h,dat$dates,training,target,model_id,model_version,diag,context)
}
.cr_m_factor_state <- function(model,initial_state,sampling_contract,scenario_count,context) {
 p<-model$parameters;ctx<-.cr_m_measure(model,context);m<-length(p$state_ids);sampling<-if(is.null(sampling_contract))list(law='FILTERED_GAUSSIAN')else sampling_contract;law<-sampling$law
 if(identical(law,'FILTERED_GAUSSIAN')){
  if(identical(ctx$conditioning,'TTC')) .cr_error('TTC_INITIALIZATION','Filtered latent posterior is PIT')
  mean<-unlist(p$filtered_state_end);cov<-.cr_m_matrix(p$filtered_covariance_end,'filtered_covariance_end');sampling$conditioning_scope<-'PIT';sampling$initialization<-'FILTERED_STATE_DISTRIBUTION'
 }else if(identical(law,'STATIONARY_GAUSSIAN')){
  if(any(!vapply(c('cycle_law_id','equilibrium_assumptions'),function(v)is.character(sampling[[v]])&&length(sampling[[v]])==1L&&nzchar(trimws(sampling[[v]])),logical(1)))) .cr_error('TTC_INITIALIZATION','Cycle law and assumptions required')
  transition<-.cr_m_matrix(p$state_transition,'state_transition');radius<-max(Mod(eigen(transition,only.values=TRUE)$values));if(radius>=1-1e-10) .cr_error('NONSTATIONARY_TTC','Stationary latent law requires stable transition')
  mean<-rep(0,m);cov<-.cr_m_matrix(p$state_covariance,'state_covariance');power<-transition;converged<-FALSE
  for(i in seq_len(100L)){addition<-power%*%cov%*%t(power);cov<-cov+addition;power<-power%*%power;if(max(abs(addition))<=1e-12*max(max(abs(cov)),.Machine$double.xmin)){converged<-TRUE;break}}
  if(!converged) .cr_error('NUMERICAL_FAILURE','Latent stationary covariance failed to converge')
  sampling$conditioning_scope<-'TTC_STATIONARY';sampling$spectral_radius<-radius
 }else if(identical(law,'DECLARED_FIXED_STATE')){
  if(is.null(initial_state)) .cr_error('MACRO_INITIAL_STATE','Fixed-state law requires explicit state')
  mean<-.cr_numeric(unlist(initial_state),'initial_state',vector_only=TRUE);cov<-matrix(0,m,m);sampling$conditioning_scope<-'PIT';sampling$initialization<-'DECLARED_FIXED_STATE';if(identical(ctx$conditioning,'TTC')) .cr_error('TTC_INITIALIZATION','Fixed latent state is not TTC')
 }else .cr_error('TTC_INITIALIZATION','Unknown factor initial law')
 if(length(mean)!=m||!identical(dim(cov),c(m,m))) .cr_error('INVALID_MODEL','Invalid latent initial distribution dimensions')
 if(law!='DECLARED_FIXED_STATE'&&!is.null(initial_state)) .cr_error('MACRO_INITIAL_STATE','Use fixed-state law to override initial distribution')
 if(law=='DECLARED_FIXED_STATE')states<-matrix(mean,scenario_count,m,byrow=TRUE)else{
  draws<-.cr_m_matrix(sampling$initial_state_draws,'initial_state_draws');sampling$initial_state_draws<-NULL
  if(!identical(dim(draws),c(as.integer(scenario_count),m))) .cr_error('INCOMPATIBLE_SHAPE','Initial latent draws need scenario by state axes')
  fd<-.cr_m_sqrt_cov(cov);states<-sweep(draws%*%t(fd$root),2,mean,'+');sampling$initial_draw_hash<-.cr_hash(.cr_m_rows(draws));sampling$initial_factorization<-fd$diagnostics
 }
 sampling$initial_mean<-as.list(mean);sampling$initial_covariance<-.cr_m_rows(cov);sampling$state_ids<-p$state_ids
 list(states=states,mean=mean,covariance=cov,sampling=sampling)
}
.cr_m_simulate_factor <- function(model,innovations,initial_state,regime_uniforms,output_space,sampling_contract,context) {
 p<-model$parameters;if(!is.null(regime_uniforms)) .cr_error('MACRO_INNOVATIONS','Linear factors do not consume regime draws')
 input<-.cr_m_scenario_inputs(innovations,p$state_ids,model$preprocessing$frequency);s<-dim(input$z)[1];h<-dim(input$z)[2];m<-dim(input$z)[3]
 if(input$kind!='STANDARD_NORMAL') .cr_error('MACRO_INNOVATIONS','Latent Gaussian requires standard-normal innovations')
 initial<-.cr_m_factor_state(model,initial_state,sampling_contract,s,context);states<-initial$states;fd<-.cr_m_sqrt_cov(p$state_covariance);transition<-.cr_m_matrix(p$state_transition,'state_transition');design<-.cr_m_matrix(p$observation_matrix,'observation_matrix');mean<-unlist(p$mean);k<-length(mean);paths<-array(0,c(s,h,k))
 if(!identical(dim(transition),c(m,m))||!identical(dim(design),c(k,m))) .cr_error('INVALID_MODEL','Invalid latent dynamics dimensions')
 for(t in seq_len(h)){states<-states%*%t(transition)+matrix(input$z[,t,],nrow=s,ncol=m)%*%t(fd$root);paths[,t,]<-sweep(states%*%t(design),2,mean,'+')}
 if(output_space=='level'){if(initial$sampling$law!='FILTERED_GAUSSIAN') .cr_error('MACRO_TRANSFORM','Alternative law requires original-level anchors');paths<-.cr_m_inverse_paths(paths,model$preprocessing$inverse_state)}else if(output_space!='model') .cr_error('MACRO_TRANSFORM','output_space must be model or level')
 .cr_m_cube(model,paths,input$ids,input$times,input$w,list(output_space=output_space,sampling_contract=initial$sampling,factorization=fd$diagnostics,innovation_hash=.cr_hash(innovations),shared_macro_across_entities=TRUE),context,'simulate_macro_scenarios')
}
.cr_m_factor_moments <- function(model,horizon,initial_state=NULL,sampling_contract=NULL) {
 p<-model$parameters;m<-length(p$state_ids);contract<-if(is.null(sampling_contract))list(law='FILTERED_GAUSSIAN')else sampling_contract
 if(contract$law!='DECLARED_FIXED_STATE')contract$initial_state_draws<-list(as.list(rep(0,m)))
 initial<-.cr_m_factor_state(model,initial_state,contract,1L,NULL);state<-initial$mean;cov<-initial$covariance;transition<-.cr_m_matrix(p$state_transition,'state_transition');q<-.cr_m_matrix(p$state_covariance,'state_covariance');design<-.cr_m_matrix(p$observation_matrix,'observation_matrix');offset<-unlist(p$mean);k<-length(offset);means<-matrix(0,horizon,k);state_cov<-list()
 for(t in seq_len(horizon)){state<-as.vector(transition%*%state);cov<-transition%*%cov%*%t(transition)+q;means[t,]<-offset+as.vector(design%*%state);state_cov[[t]]<-cov}
 covariance<-matrix(0,horizon*k,horizon*k)
 for(t in seq_len(horizon)){power<-diag(m);for(u in t:horizon){block<-design%*%state_cov[[t]]%*%t(power)%*%t(design);it<-((t-1L)*k+1L):(t*k);iu<-((u-1L)*k+1L):(u*k);covariance[it,iu]<-block;covariance[iu,it]<-t(block);power<-transition%*%power}}
 list(mean=as.vector(t(means)),covariance=covariance)
}
