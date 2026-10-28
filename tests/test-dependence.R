library(creditratingsystem)
r<-validate_dependence_matrix(matrix(1,2,2),c('a','b'),matrix_kind='correlation');stopifnot(r$diagnostics$numerical_rank==1,!r$diagnostics$strictly_positive_definite)
e<-tryCatch(validate_dependence_matrix(matrix(c(1,2,2,1),2),c('a','b'),matrix_kind='correlation'),error=identity);stopifnot(inherits(e,'creditratingsystem_NON_PSD_MATRIX'))
r<-validate_dependence_matrix(matrix(c(1,2,2,1),2),c('a','b'),matrix_kind='correlation',repair_policy='spectral');stopifnot(max(abs(unlist(r$value$matrix)-1))<1e-15,abs(r$diagnostics$frobenius_change-sqrt(2))<1e-14)
e<-tryCatch(validate_dependence_matrix(matrix(c(1,2,2,1),2)*1e-20,c('a','b'),matrix_kind='covariance'),error=identity);stopifnot(inherits(e,'creditratingsystem_NON_PSD_MATRIX'))
a<-matrix(c(1,.2+1e-12,.2,1),2);r<-validate_dependence_matrix(a,c('a','b'),matrix_kind='correlation');stopifnot(r$diagnostics$frobenius_change==0)
root<-tempfile('innovations-',tmpdir=tempdir());dir.create(root)
spec<-list(list(stream_id='common',distribution='uniform'),list(stream_id='idiosyncratic',distribution='normal'))
a<-make_shared_innovations(c('one','two'),c(0,1),c('X','Y'),spec,'experiment',file.path(root,'a'),chunk_rows=1)
b<-make_shared_innovations(c('one','two'),c(0,1),c('X','Y'),spec,'experiment',file.path(root,'b'),chunk_rows=20)
reordered<-make_shared_innovations(c('two','one'),c(0,1),c('X','Y'),spec,'experiment',file.path(root,'c'))
for(i in 1:2){stopifnot(a$value$manifest$streams[[i]]$sha256==b$value$manifest$streams[[i]]$sha256);x<-readBin(file.path(root,'a',a$value$manifest$streams[[i]]$file),'double',n=8,endian='little');y<-readBin(file.path(root,'c',reordered$value$manifest$streams[[i]]$file),'double',n=8,endian='little');stopifnot(identical(x,y[c(5:8,1:4)]))}
stopifnot(a$provenance$data_origin=='SYNTHETIC')
target<-list(event='joint_default',horizon=1,time_unit='years',probability_measure='PHYSICAL',conditioning='performing')
training<-list(data_fingerprint='copula-oracle',split_id='train',known_at='2024-01-01',data_origin='SYNTHETIC')
frozen<-function(family,parameters)list(schema_version='1.0',model_id='analytic-copula',model_version='1',method_id='dependence.fit_copula',feature_order=list('a','b'),parameters=c(list(family=family,dimension=2),parameters),preprocessing=list(transformation='identity'),target=target,training=training,diagnostics=structure(list(),names=character()),provenance=list(data_origin='SYNTHETIC',parameter_origin='SYNTHETIC_REFERENCE'))
correlation<-list(list(1,.5),list(.5,1));cases<-list(list('gaussian',list(correlation=correlation),1/3),list('t',list(correlation=correlation,df=4),1/3),list('clayton',list(theta=1),1/3),list('gumbel',list(theta=2),2^(-sqrt(2))))
for(case in cases){m<-frozen(case[[1]],case[[2]]);r<-joint_default_probability(.5,.5,c('a','b'),m);stopifnot(abs(r$value-case[[3]])<2e-10,joint_default_probability(0,.5,c('a','b'),m)$value==0,joint_default_probability(1,.3,c('a','b'),m)$value==.3)}
x<-cbind(1:4,c(1,3,2,4))
for(family in c('gaussian','clayton','gumbel')){
 fit<-fit_copula(x,c('a','b'),family,'observations','error',target=target,training=training,model_id='copula',model_version='1')
 if(family=='gaussian')stopifnot(abs(fit$model$parameters$correlation[[1]][[2]]-sin(pi/3))<1e-14) else stopifnot(abs(fit$model$parameters$theta-if(family=='clayton')4 else 3)<1e-14)
}
u<-matrix(c(.1,.3,.5,.7,.9,.2,.4,.6),ncol=2)
for(family in c('clayton','gumbel')){m<-frozen(family,list(theta=if(family=='clayton')0 else 1));stopifnot(identical(simulate_copula(m,u,c('a','b'))$value,u))}
r<-simulate_copula(frozen('gaussian',list(correlation=list(list(1,1),list(1,1)))),u,c('a','b'));stopifnot(identical(r$value[,1],r$value[,2]))
e<-tryCatch(validate_dependence_matrix(matrix(1e308,2,2),c('a','b'),matrix_kind='covariance'),error=identity);stopifnot(inherits(e,'creditratingsystem_NUMERICAL_FAILURE'))
r<-joint_default_probability(.1,.2,c('a','b'),frozen('clayton',list(theta=1e308)));stopifnot(abs(r$value-.1)<1e-15)
e<-tryCatch(simulate_copula(frozen('clayton',list(theta=1e5)),matrix(c(.2,.8),1),c('a','b'),matrix(.5,1)),error=identity);stopifnot(inherits(e,'creditratingsystem_NUMERICAL_FAILURE'))

unlink(root, recursive=TRUE)
