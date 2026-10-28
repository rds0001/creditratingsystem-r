.cr_dependence_ids <- function(x,name){if(is.list(x)&&is.null(names(x))&&all(vapply(x,function(v)is.character(v)&&length(v)==1L,logical(1))))x<-unlist(x,use.names=FALSE);if(!is.character(x)||!length(x)||anyNA(x)||any(!nzchar(x))||anyDuplicated(x)) .cr_error('DEPENDENCE_IDS',paste(name,'requires distinct nonempty string IDs'));x}
validate_dependence_matrix <- function(matrix,variable_ids,matrix_kind,repair_policy='none',tolerance=1e-10,context=NULL) {
 a<-.cr_numeric(matrix,'matrix');ids<-.cr_dependence_ids(variable_ids,'variable_ids');tol<-.cr_numeric(tolerance,'tolerance')
 if(length(tol)!=1L||!is.null(dim(tol))||tol<=0||tol>=1) .cr_error('DEPENDENCE_TOLERANCE','Tolerance must be scalar in (0,1)')
 if(!identical(dim(a),c(length(ids),length(ids)))) .cr_error('INCOMPATIBLE_SHAPE','Square matrix must match ordered variable IDs')
 if(length(matrix_kind)!=1L||!(matrix_kind%in%c('correlation','covariance'))||length(repair_policy)!=1L||!(repair_policy%in%c('none','spectral'))) .cr_error('DEPENDENCE_CONTRACT','Declare correlation/covariance and none/spectral repair')
 scale<-max(abs(a));if(scale==0)scale<-1;asymmetry<-max(abs(a/scale-t(a)/scale))
 if(asymmetry>tol&&repair_policy=='none') .cr_error('ASYMMETRIC_MATRIX','Dependence matrix is not symmetric at declared tolerance')
 symmetric<-a/2+t(a)/2;e<-eigen(symmetric/scale,symmetric=TRUE);e$values<-e$values*scale;values<-rev(e$values);if(any(!is.finite(values))) .cr_error('NUMERICAL_FAILURE','Absolute matrix eigenvalues exceed finite range')
 if(matrix_kind=='correlation'&&max(abs(diag(a)-1))>tol&&repair_policy=='none') .cr_error('INVALID_CORRELATION','Correlation diagonal must equal one')
 if(matrix_kind=='covariance'&&any(diag(a)<0)&&repair_policy=='none') .cr_error('INVALID_COVARIANCE','Negative marginal variance')
 if(values[1]< -tol*scale&&repair_policy=='none') .cr_error('NON_PSD_MATRIX','Negative eigenvalue exceeds declared numerical tolerance')
 out<-a
 if(repair_policy=='spectral'){
  out<-sweep(e$vectors,2,pmax(e$values,0),'*')%*%t(e$vectors)
  if(matrix_kind=='correlation'){diagonal<-diag(out);if(any(diagonal<=0)) .cr_error('NON_REPAIRABLE_MATRIX','Spectral projection has a zero marginal variance; correlation scaling is undefined');out<-sweep(sweep(out,1,sqrt(diagonal),'/'),2,sqrt(diagonal),'/');diag(out)<-1}
  out<-out/2+t(out)/2
 }
 repaired<-rev(eigen((out/2+t(out)/2)/scale,symmetric=TRUE,only.values=TRUE)$values)*scale;difference<-out-a;change_max<-max(abs(difference));change_norm<-if(change_max==0)0 else change_max*norm(difference/change_max,'F');if(any(!is.finite(out))||any(!is.finite(repaired))||!is.finite(change_norm)) .cr_error('NUMERICAL_FAILURE','Matrix repair or absolute diagnostics exceed finite range')
 .cr_result(list(matrix=lapply(seq_len(nrow(out)),function(i)as.list(out[i,])),variable_ids=as.list(ids),matrix_kind=matrix_kind,eigenvalues=as.list(repaired)),'dependence.validate_dependence_matrix','dependence_matrix',context,
 diagnostics=list(repair_policy=repair_policy,input_eigenvalues=as.list(values),relative_asymmetry=asymmetry,frobenius_change=change_norm,maximum_absolute_change=change_max,numerical_rank=sum(repaired>tol*scale),tolerance=tol,strictly_positive_definite=repaired[1]>tol*scale),
 limitations=c('Numerical PSD acceptance does not guarantee suitability for an inverse or Cholesky factor. Tiny negative eigenvalues within tolerance remain explicit when no repair is requested.','Spectral repair truncates negative eigenvalues and rescales correlations; it is not claimed to be the nearest correlation matrix.'))
}
.cr_addressed_uniform <- function(seed,scenario,time,factor,stream,lane){address<-.cr_hash(list('creditratingsystem-innovations-v1',seed,scenario,time,factor,stream,lane));value<-strtoi(substr(address,1,4),16)*65536+strtoi(substr(address,5,8),16);(value+.5)/4294967296}
make_shared_innovations <- function(scenario_ids,time_grid,factor_ids,stream_spec,seed,output_directory,chunk_rows=1024,context=NULL) {
 scenarios<-.cr_dependence_ids(scenario_ids,'scenario_ids');factors<-.cr_dependence_ids(factor_ids,'factor_ids');times<-.cr_numeric(time_grid,'time_grid',vector_only=TRUE)
 if(any(times<0)||any(diff(times)<=0)) .cr_error('INVALID_TIME_GRID','Times must be nonnegative and strictly increasing')
 .cr_string(seed,'seed','INNOVATION_SEED')
 if(!is.numeric(chunk_rows)||is.logical(chunk_rows)||length(chunk_rows)!=1L||!is.finite(chunk_rows)||chunk_rows<1||chunk_rows!=floor(chunk_rows)) .cr_error('INNOVATION_CHUNK','chunk_rows must be a positive integer')
 if(!is.list(stream_spec)||!length(stream_spec)||any(!vapply(stream_spec,is.list,logical(1)))) .cr_error('INNOVATION_STREAM','Named stream specifications required')
 .cr_dependence_ids(lapply(stream_spec,function(s)s$stream_id),'stream_ids')
 for(s in stream_spec)if(!setequal(names(s),c('stream_id','distribution'))||anyDuplicated(names(s))||length(s$distribution)!=1L||!(s$distribution%in%c('uniform','normal'))) .cr_error('INNOVATION_STREAM','Each stream declares stream_id and distribution uniform/normal')
 .cr_string(output_directory,'output_directory','INNOVATION_OUTPUT')
 if(file.exists(output_directory)&&(!dir.exists(output_directory)||length(list.files(output_directory,all.files=TRUE,no..=TRUE)))) .cr_error('OUTPUT_EXISTS','Output directory must be absent or empty')
 if(!dir.exists(output_directory)&&!dir.create(output_directory,recursive=TRUE)) .cr_error('INNOVATION_OUTPUT','Cannot create output directory')
 entries<-list()
 for(number in seq_along(stream_spec)){
  s<-stream_spec[[number]];filename<-sprintf('stream-%04d.f64le',number-1L);path<-file.path(output_directory,filename);con<-file(path,'wb');count<-0
  tryCatch({for(offset in seq.int(1L,length(scenarios),by=chunk_rows))for(scenario in scenarios[seq.int(offset,min(length(scenarios),offset+chunk_rows-1L))])for(time in times)for(factor in factors){
   u<-.cr_addressed_uniform(seed,scenario,time,factor,s$stream_id,0)
   if(s$distribution=='normal'){v<-.cr_addressed_uniform(seed,scenario,time,factor,s$stream_id,1);u<-sqrt(-2*log(u))*cos(2*pi*v)}
   writeBin(as.double(u),con,size=8L,endian='little');count<-count+1
  }},finally=close(con))
  entries[[number]]<-list(stream_id=s$stream_id,distribution=s$distribution,file=filename,sha256=digest::digest(file=path,algo='sha256'),values=count,shape=as.list(c(length(scenarios),length(times),length(factors))))
 }
 manifest<-list(format='creditratingsystem-f64le-addressed-v1',seed=seed,scenario_ids=as.list(scenarios),time_grid=as.list(times),factor_ids=as.list(factors),order='scenario,time,factor (factor varies fastest)',streams=entries,algorithm='canonical SHA256 address first32bits midpoint; normal Box-Muller cosine with separately addressed lanes0,1')
 path<-file.path(output_directory,'manifest.json');jsonlite::write_json(manifest,path,auto_unbox=TRUE,digits=NA,null='null',pretty=TRUE)
 .cr_result(list(directory=output_directory,manifest=manifest,manifest_sha256=digest::digest(file=path,algo='sha256')),'dependence.make_shared_innovations','innovations_bundle',context,diagnostics=list(chunk_rows=chunk_rows,address_order_invariant=TRUE),provenance=list(data_origin='SYNTHETIC'),limitations=c('Addressed streams are deterministic simulation inputs, not a cryptographic randomness service. Uniform draws have 32-bit resolution.','Uniform binary files are exact across runtimes. Normal transcendental transforms may differ by floating-point rounding; share one frozen file for exact path replay.'))
}
.cr_copula_model <- function(model){
 model<-.cr_model(model);p<-model$parameters
 if(!identical(model$method_id,'dependence.fit_copula')||length(p$family)!=1L||!(p$family%in%c('gaussian','t','clayton','gumbel'))) .cr_error('DEPENDENCE_MODEL','Frozen supported copula model required')
 d<-length(model$feature_order);if(d<2||!identical(as.numeric(p$dimension),as.numeric(d))) .cr_error('DEPENDENCE_MODEL','Copula dimension must match variable order')
 if(p$family%in%c('gaussian','t')){
  r<-if(is.matrix(p$correlation))p$correlation else do.call(rbind,lapply(p$correlation,unlist));checked<-validate_dependence_matrix(r,model$feature_order,matrix_kind='correlation');p$correlation<-do.call(rbind,lapply(checked$value$matrix,unlist));if(any(p$correlation!=t(p$correlation))||any(diag(p$correlation)!=1)||any(abs(p$correlation)>1)) .cr_error('DEPENDENCE_MODEL','Frozen correlation must be symmetric with exact unit diagonal and entries in [-1,1]')
  if(p$family=='t'&&(!is.numeric(p$df)||length(p$df)!=1L||!is.finite(p$df)||p$df<=0)) .cr_error('DEPENDENCE_MODEL','Positive finite t degrees of freedom required')
 }else{theta<-.cr_numeric(p$theta,'theta');if(length(theta)!=1L||!is.null(dim(theta))||theta<(if(p$family=='clayton')0 else 1)) .cr_error('DEPENDENCE_MODEL','Supported Clayton theta>=0 or Gumbel theta>=1 required')}
 list(model=model,p=p)
}
fit_copula <- function(observations,variable_ids,family,input_kind,tie_policy,correlation_repair='none',df=NULL,df_bounds=c(1,100),target,training,model_id,model_version,context=NULL){
 ids<-.cr_dependence_ids(variable_ids,'variable_ids');x<-.cr_numeric(observations,'observations');d<-length(ids)
 if(length(dim(x))!=2L||ncol(x)!=d||d<2||nrow(x)<3) .cr_error('DEPENDENCE_DATA','At least three aligned rows and two named variables required')
 if(length(family)!=1L||!(family%in%c('gaussian','t','clayton','gumbel'))||length(input_kind)!=1L||!(input_kind%in%c('observations','uniforms'))||length(tie_policy)!=1L||!(tie_policy%in%c('error','average'))) .cr_error('DEPENDENCE_CONTRACT','Declare family, observations/uniforms and error/average tie policy')
 ties<-vapply(seq_len(d),function(j)nrow(x)-length(unique(x[,j])),integer(1))
 if(any(ties>0)&&tie_policy=='error') .cr_error('TIED_OBSERVATIONS','Continuous copula estimation requires explicit handling of tied observations')
 if(any(vapply(seq_len(d),function(j)all(x[,j]==x[1,j]),logical(1)))) .cr_error('NON_IDENTIFIABLE','Constant marginal observations do not identify dependence')
 if(input_kind=='observations')u<-apply(x,2,rank,ties.method='average')/(nrow(x)+1) else{if(any(x<=0|x>=1)) .cr_error('INVALID_PROBABILITY','Copula uniforms must lie strictly inside (0,1)');u<-x}
 tau<-stats::cor(x,method='kendall');parameters<-list(family=family,dimension=d)
 rows<-function(a)lapply(seq_len(nrow(a)),function(i)as.list(a[i,]))
 diagnostics<-list(estimator='INVERSION_OF_KENDALL_TAU',n_rows=nrow(x),tie_counts=as.list(ties),tie_policy=tie_policy,tied_continuous_approximation=any(ties>0),input_kind=input_kind,kendall_tau=rows(tau),uncertainty='NOT_ESTIMATED')
 if(family%in%c('gaussian','t')){
  raw<-sin(pi*tau/2);diag(raw)<-1;checked<-validate_dependence_matrix(raw,ids,matrix_kind='correlation',repair_policy=correlation_repair);r<-do.call(rbind,lapply(checked$value$matrix,unlist))
  if(min(eigen(r,symmetric=TRUE,only.values=TRUE)$values)<=1e-12) .cr_error('SINGULAR_COPULA','Fitted likelihood requires strictly positive definite dependence; perfect ranks need a declared singular scenario model')
  parameters$correlation<-rows(r);diagnostics$correlation_validation<-checked$diagnostics
  if(family=='t'){
   bounds<-.cr_numeric(df_bounds,'df_bounds',vector_only=TRUE);if(length(bounds)!=2L||bounds[1]<=0||bounds[2]<=bounds[1]) .cr_error('INVALID_DF','Positive increasing profile bounds required')
   objective<-function(nu){z<-matrix(stats::qt(u,nu),nrow(u));-sum(mvtnorm::dmvt(z,sigma=r,df=nu,log=TRUE)-rowSums(matrix(stats::dt(z,nu,log=TRUE),nrow(z))))}
   if(is.null(df)){
    fit<-stats::optimize(objective,bounds,tol=1e-8);if(!is.finite(fit$objective)) .cr_error('NON_CONVERGENCE','t copula degrees-of-freedom profile failed')
    candidates<-c(fit$minimum,bounds);losses<-vapply(candidates,objective,numeric(1));index<-which.min(losses);nu<-candidates[index];boundary<-nu%in%bounds
    diagnostics<-c(diagnostics,list(df_bounds=as.list(bounds),df_on_boundary=boundary,df_identification=if(boundary)'BOUND_LIMITED' else 'INTERIOR_PROFILE_ESTIMATE',conditional_log_likelihood=-losses[index]));diagnostics$estimator<-'KENDALL_CORRELATION_THEN_DF_PROFILE'
   }else{value<-.cr_numeric(df,'df');if(length(value)!=1L||!is.null(dim(value))||value<=0) .cr_error('INVALID_DF','Fixed t degrees of freedom must be positive');nu<-as.numeric(value);diagnostics$df_identification<-'FIXED_BY_USER';diagnostics$conditional_log_likelihood<- -objective(nu)}
   parameters$df<-nu
  }else if(!is.null(df)) .cr_error('INVALID_DF','Degrees of freedom only apply to the t family')
 }else{
  if(!is.null(df)) .cr_error('INVALID_DF','Degrees of freedom only apply to the t family')
  tm<-mean(tau[upper.tri(tau)]);if(tm<0||tm>=1) .cr_error('ARCHIMEDEAN_DOMAIN','This exchangeable Archimedean fit requires mean Kendall tau in [0,1); negative dependence needs a different family')
  theta<-if(family=='clayton')2*tm/(1-tm) else 1/(1-tm);parameters$theta<-theta;diagnostics$mean_kendall_tau<-tm;diagnostics$pair_tau_range<-as.list(range(tau[upper.tri(tau)]));diagnostics$exchangeable<-TRUE
 }
 pair_ll<-numeric();tail<-list()
 for(j in 2:d)for(k in seq_len(j-1L)){
  if(family=='gaussian'){cop<-copula::normalCopula(r[j,k]);lower<-upper<-0
  }else if(family=='t'){cop<-copula::tCopula(r[j,k],df=nu,df.fixed=TRUE);lower<-upper<-2*stats::pt(-sqrt((nu+1)*(1-r[j,k])/(1+r[j,k])),nu+1)
  }else if(family=='clayton'){cop<-if(theta==0)NULL else copula::claytonCopula(theta);lower<-if(theta==0)0 else 2^(-1/theta);upper<-0
  }else{cop<-if(theta==1)NULL else copula::gumbelCopula(theta);lower<-0;upper<-2-2^(1/theta)}
  ll<-if(is.null(cop))0 else sum(copula::dCopula(u[,c(k,j),drop=FALSE],cop,log=TRUE));if(!is.finite(ll)) .cr_error('NUMERICAL_FAILURE','Pairwise copula density outside representable range');pair_ll<-c(pair_ll,ll);tail[[length(tail)+1L]]<-list(variables=as.list(ids[c(k,j)]),lower=lower,upper=upper)
 }
 diagnostics$pairwise_composite_log_likelihood<-sum(pair_ll);diagnostics$tail_dependence<-tail;diagnostics$likelihood_note<-'Pairwise composite likelihood is a descriptive fit diagnostic, not a joint MLE or an IID standard-error estimator.'
 model<-.cr_model(list(schema_version='1.0',model_id=model_id,model_version=model_version,method_id='dependence.fit_copula',feature_order=as.list(ids),parameters=parameters,preprocessing=list(input_kind=input_kind,tie_policy=tie_policy,rank_denominator=if(input_kind=='observations')nrow(x)+1 else NULL),target=target,training=training,diagnostics=diagnostics,provenance=list(data_origin=if(is.null(training$data_origin))'USER_SUPPLIED' else training$data_origin,parameter_origin='ESTIMATED'),input_fingerprint=.cr_hash(rows(x))))
 .cr_fit_result(model,'dependence.fit_copula',diagnostics=diagnostics,context=context)
}
.cr_psd_cholesky <- function(r,tolerance=1e-12){
 n<-nrow(r);l<-matrix(0,n,n)
 for(j in seq_len(n)){
  prior<-seq_len(j-1L);pivot<-r[j,j]-sum(l[j,prior]^2)
  if(pivot< -tolerance) .cr_error('NON_PSD_MATRIX','Semidefinite factorization encountered a negative pivot')
  if(pivot<=tolerance){if(j<n)for(i in seq.int(j+1L,n))if(abs(r[i,j]-sum(l[i,prior]*l[j,prior]))>tolerance) .cr_error('NON_PSD_MATRIX','Zero pivot has a nonzero residual covariance')
  }else{l[j,j]<-sqrt(pivot);if(j<n)for(i in seq.int(j+1L,n))l[i,j]<-(r[i,j]-sum(l[i,prior]*l[j,prior]))/l[j,j]}
 };l
}
simulate_copula <- function(dependence_model,independent_uniforms,variable_ids,auxiliary_uniforms=NULL,context=NULL){
 m<-.cr_copula_model(dependence_model);model<-m$model;p<-m$p;.cr_copula_context(model,context);ids<-.cr_dependence_ids(variable_ids,'variable_ids');u<-.cr_numeric(independent_uniforms,'independent_uniforms')
 if(!identical(as.list(ids),model$feature_order)||length(dim(u))!=2L||ncol(u)!=length(ids)) .cr_error('FEATURE_MISMATCH','Uniform columns must match frozen variable order')
 if(any(u<=0|u>=1)) .cr_error('INVALID_PROBABILITY','Innovation uniforms must be strictly inside (0,1)')
 family<-p$family;need<-if(family=='t'||(family=='clayton'&&p$theta>0))1L else if(family=='gumbel'&&p$theta>1)2L else 0L;aux<-if(is.null(auxiliary_uniforms))NULL else .cr_numeric(auxiliary_uniforms,'auxiliary_uniforms')
 if(need>0){if(is.null(aux)||!identical(dim(aux),c(nrow(u),need))||any(aux<=0|aux>=1)) .cr_error('AUXILIARY_DRAWS','Required open auxiliary uniforms have wrong shape or domain')
 }else if(!is.null(aux)) .cr_error('AUXILIARY_DRAWS','No auxiliary innovations apply at this family/boundary')
 if(family%in%c('gaussian','t')){
  root<-.cr_psd_cholesky(p$correlation);z<-matrix(stats::qnorm(u),nrow(u))%*%t(root)
  if(family=='gaussian')out<-matrix(stats::pnorm(z),nrow(u)) else{scaling<-stats::qchisq(aux[,1],p$df)/p$df;if(any(scaling<=0)||any(!is.finite(scaling))) .cr_error('NUMERICAL_FAILURE','t scale quantile is outside representable positive range');out<-matrix(stats::pt(z/sqrt(scaling),p$df),nrow(u))};factor_error<-max(abs(root%*%t(root)-p$correlation))
 }else if(need==0){out<-u;factor_error<-NULL
 }else if(family=='clayton'){frailty<-stats::qgamma(aux[,1],shape=1/p$theta);if(any(frailty<=0)||any(!is.finite(frailty))) .cr_error('NUMERICAL_FAILURE','Gamma frailty quantile is outside representable positive range');out<-exp(-log1p(-log(u)/frailty)/p$theta);factor_error<-NULL
 }else{alpha<-1/p$theta;angle<-pi*aux[,1];exponential<- -log(aux[,2]);log_s<-log(sin(alpha*angle))-log(sin(angle))/alpha+(1-alpha)/alpha*(log(sin((1-alpha)*angle))-log(exponential));out<-exp(-exp(alpha*(log(-log(u))-log_s)));factor_error<-NULL}
 if(any(!is.finite(out))) .cr_error('NUMERICAL_FAILURE','Copula transform produced a nonfinite value')
 rows<-function(a)lapply(seq_len(nrow(a)),function(i)as.list(a[i,]))
 .cr_result(out,'dependence.simulate_copula','uniform_probability',context,diagnostics=list(family=family,variable_ids=as.list(ids),model_fingerprint=.cr_hash(model),draw_fingerprint=.cr_hash(list(uniforms=rows(u),auxiliary=if(is.null(aux))NULL else rows(aux))),numerical_boundary_count=sum(out==0|out==1),factorization_maximum_residual=factor_error),provenance=model$provenance,limitations='Only the supplied innovation distribution and frozen copula establish these marginal/dependence assumptions. Empirical dependence and scenario realism require separate validation.')
}
joint_default_probability <- function(p_a,p_b,event_order,dependence_model=NULL,joint_probability=NULL,context=NULL){
 a<-.cr_probability(p_a,'p_a');b<-.cr_probability(p_b,'p_b');ids<-.cr_dependence_ids(event_order,'event_order')
 if(length(a)!=1L||!is.null(dim(a))||length(b)!=1L||!is.null(dim(b))||length(ids)!=2L) .cr_error('DEPENDENCE_EVENTS','Two ordered event IDs and scalar marginal probabilities required')
 lo<-max(0,sum(c(a,b,-1)));hi<-min(a,b);error<-0
 if(is.null(dependence_model)==is.null(joint_probability)) .cr_error('DEPENDENCE_CONTRACT','Provide exactly one copula model or explicit intersection')
 if(!is.null(joint_probability)){j<-.cr_probability(joint_probability,'joint_probability');if(length(j)!=1L||!is.null(dim(j))) .cr_error('DEPENDENCE_EVENTS','Intersection must be scalar');family<-'explicit'
 }else{
  m<-.cr_copula_model(dependence_model);model<-m$model;p<-m$p;.cr_copula_context(model,context);if(!identical(model$feature_order,as.list(ids))) .cr_error('FEATURE_MISMATCH','Bivariate event order must exactly match model');family<-p$family
  if(hi==0)j<-0 else if(a==1||b==1)j<-hi else if(family%in%c('gaussian','t')){
   rho<-p$correlation[1,2]
   if(rho==1)j<-hi else if(rho== -1)j<-lo else if(family=='gaussian'&&rho==0)j<-a*b else{
    threshold<-if(family=='gaussian')stats::qnorm(b) else stats::qt(b,p$df)
    integrand<-function(v){x<-if(family=='gaussian')stats::qnorm(a*v) else stats::qt(a*v,p$df);if(family=='gaussian')stats::pnorm((threshold-rho*x)/sqrt(1-rho*rho)) else stats::pt((threshold-rho*x)*sqrt((p$df+1)/((p$df+x*x)*(1-rho*rho))),p$df+1)}
    fit<-stats::integrate(integrand,0,1,abs.tol=1e-10,rel.tol=1e-10,subdivisions=300L);j<-a*fit$value;error<-a*fit$abs.error
   }
  }else if(family=='clayton'){
   theta<-p$theta;if(theta==0)j<-a*b else{x<- -log(a);y<- -log(b);mx<-max(x,y);exponent<-if(theta*mx<500)log1p(expm1(theta*x)+expm1(theta*y))/theta else mx+log(exp(theta*(x-mx))+exp(theta*(y-mx))-exp(-theta*mx))/theta;j<-exp(-exponent)}
  }else{theta<-p$theta;x<-log(-log(a));y<-log(-log(b));mx<-max(x,y);j<-exp(-exp(mx)*(exp(theta*(x-mx))+exp(theta*(y-mx)))^(1/theta))}
 }
 if(!is.finite(j)||!is.finite(error)) .cr_error('NUMERICAL_FAILURE','Copula intersection or quadrature error is nonfinite');unadjusted<-j
 if(j<lo||j>hi){if(!is.null(dependence_model)&&min(abs(j-lo),abs(j-hi))<=max(error,8*.Machine$double.eps))j<-min(hi,max(lo,j)) else .cr_error('INVALID_JOINT_PROBABILITY','Intersection lies outside Frechet bounds')}
 .cr_result(j,'dependence.joint_default_probability','probability',context,diagnostics=list(family=family,event_order=as.list(ids),frechet_lower=lo,frechet_upper=hi,quadrature_absolute_error=error,frechet_roundoff_adjustment=j-unadjusted),limitations='Defaults are lower-tail uniform events U<=PD. Latent correlation is not Bernoulli default correlation. A finite-df t copula with zero correlation is generally dependent.')
}

.cr_copula_context <- function(model,context){ctx<-.cr_result(NULL,"dependence.context",context=context)$context;for(key in c("horizon","time_unit","probability_measure","conditioning"))if(!is.null(ctx[[key]])&&!identical(.cr_hash(ctx[[key]]),.cr_hash(model$target[[key]]))) .cr_error("TARGET_MISMATCH","Context conflicts with frozen copula target");ctx}
