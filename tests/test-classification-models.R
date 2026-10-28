library(creditratingsystem)
if(!exists(".cm_predict_pd")) .cm_predict_pd <- getFromNamespace(".cm_predict_pd","creditratingsystem")
target <- list(event="default",horizon=1,time_unit="years",probability_measure="PHYSICAL",conditioning="performing")
training <- list(data_fingerprint="analytic-classification",split_id="train",known_at="2025-01-01",data_origin="SYNTHETIC")
meta <- list(target=target,training=training,model_id="classification-oracle",model_version="1")
fit <- function(fun,...)do.call(fun,c(list(...),meta))
pred <- function(m,x,f)unlist(.cm_predict_pd(m,x,feature_order=f,expected_target=target)$value)
close <- function(a,b,tol=1e-8)stopifnot(max(abs(a-b))<tol)
error <- function(expr,code) {e<-tryCatch({force(expr);NULL},error=function(e)e);stopifnot(inherits(e,"error"),identical(e$code,code))}
x <- rbind(c(-2,-1),c(-2,1),c(0,-1),c(0,1),c(1,-2),c(1,0),c(3,-2),c(3,0)); y <- rep(0:1,each=4)
for(est in c("mle","unbiased")) {
 lda <- fit(fit_discriminant_pd,x,y,feature_order=c("x","z"),variant="lda",covariance_estimator=est)
 qda <- fit(fit_discriminant_pd,x,y,feature_order=c("x","z"),variant="qda",covariance_estimator=est)
 z <- rbind(c(0,0),c(1,.5),c(-1,.2)); variance <- if(est=="mle")1 else 4/3
 a <- dnorm(z[,1],-1,sqrt(variance))*dnorm(z[,2],0,sqrt(variance)); b <- dnorm(z[,1],2,sqrt(variance))*dnorm(z[,2],-1,sqrt(variance))
 close(pred(lda,z,c("x","z")),b/(a+b));close(pred(qda,z,c("x","z")),b/(a+b))
 ref <- MASS::lda(x,factor(y),method=if(est=="mle")"mle" else "moment")
 close(pred(lda,z,c("x","z")),predict(ref,z)$posterior[,2])
}
x <- list(list("a","yes"),list("a","no"),list("b","no"),list("b","yes"),list("b","yes"),list("b","no"));y<-rep(0:1,each=3)
m <- fit(fit_naive_bayes_pd,x,y,feature_order=c("a","b"),feature_types=rep("categorical",2),variant="categorical",alpha=1,unknown_policy="reserved",missing_policy="category")
close(pred(m,list(list("b","yes")),c("a","b")),.75)
close(pred(m,list(list("unseen",NULL)),c("a","b")),.5)
x <- rbind(c(-2,-1),c(-1,1),c(0,0),c(1,0),c(2,2),c(3,1))
g <- fit(fit_naive_bayes_pd,x,y,feature_order=c("a","b"),feature_types=rep("gaussian",2),variant="gaussian",missing_policy="marginalize")
close(pred(g,list(list(NULL,NULL)),c("a","b")),.5)
for(kernel in c("linear","rbf")) {
 m <- fit(fit_svm_score,matrix(c(-1,1),ncol=1),c(0,1),feature_order="x",kernel=kernel,C=10,gamma=if(kernel=="rbf").5 else NULL,scale=FALSE)
 score <- predict_model_score(m,matrix(c(-1,0,1),ncol=1),feature_order="x",expected_target=target)
 close(unlist(score$value),c(-1,0,1),1e-7);stopifnot(score$diagnostics$scale=="SVM_MARGIN")
 error(pred(m,matrix(0,ncol=1),"x"),"UNCALIBRATED_SCORE")
}
m <- fit(fit_firth_logit,matrix(numeric(),8,0),c(rep(0,7),1),feature_order=character())
close(unlist(m$model$parameters$coefficients),log(1.5/7.5),1e-7)
m <- fit(fit_firth_logit,matrix(c(-2,-1,1,2),ncol=1),c(0,0,1,1),feature_order="x")
stopifnot(all(is.finite(unlist(m$model$parameters$coefficients))))
x <- matrix(c(-2,-1,0,1,2,3),ncol=1);y<-c(0,1,0,1,0,1);weights<-c(2,1,1,2,1,1)
ordinary <- fit(fit_binary_glm,x,y,link="logit",feature_order="x",fit_intercept=TRUE)
for(penalty in c("ridge","lasso","elasticnet")) {
 kw <- list(X_train=x,y_train=y,penalty=penalty,lambda_path=c(10,.1,0),selected_lambda=0,feature_order="x",scale=FALSE)
 if(penalty=="elasticnet")kw$l1_ratio <- .4
 m <- do.call(fit_penalized_glm,c(kw,meta));close(unlist(m$model$parameters$coefficients),unlist(ordinary$model$parameters$coefficients),1e-7)
 if(penalty=="lasso")stopifnot(m$diagnostics$path[[1]]$coefficients[[2]]==0)
 kw$selected_lambda <- .1
 weighted <- do.call(fit_penalized_glm,c(kw,list(weights=weights),meta))
 kw$X_train <- x[rep(seq_len(nrow(x)),weights),,drop=FALSE];kw$y_train <- rep(y,weights)
 replicated <- do.call(fit_penalized_glm,c(kw,meta));close(pred(weighted,x,"x"),pred(replicated,x,"x"),1e-7)
}
for(link in c("logit","probit")) {
 m <- fit(fit_ordinal_model,matrix(numeric(),10,0),c(rep("good",5),rep("weak",3),rep("default",2)),feature_order=character(),ordered_categories=c("good","weak","default"),link=link,default_categories="default")
 close(pred(m,matrix(numeric(),1,0),character()),.2)
}
x <- matrix(rep(-2:2,each=12),ncol=1); counts <- rbind(c(9,2,1),c(7,3,2),c(4,4,4),c(2,3,7),c(1,2,9));labels<-unlist(lapply(seq_len(5),function(i)rep(c("good","weak","default"),counts[i,])))
for(link in c("logit","probit")) {
 m <- fit(fit_ordinal_model,x,labels,feature_order="x",ordered_categories=c("good","weak","default"),link=link,default_categories="default")
 stopifnot(m$model$parameters$coefficients[[1]]>0,m$diagnostics$parallel_slopes_diagnostic$status=="ESTIMATED",m$diagnostics$parallel_slopes_diagnostic$p_value>.5)
}
m <- fit(fit_firth_logit,matrix(numeric(),8,0),rep(0,8),feature_order=character())
close(pred(m,matrix(numeric(),1,0),character()),.5/9,1e-9)
m <- fit(fit_svm_score,matrix(c(1,-1),ncol=1),c(1,0),feature_order="x",kernel="linear",C=10,scale=FALSE)
close(unlist(predict_model_score(m,matrix(c(-1,1),ncol=1),feature_order="x",expected_target=target)$value),c(-1,1))
error(fit(fit_svm_score,matrix(c(1,-1),ncol=1),c(1,0),feature_order="x",kernel="linear",C=10,max_iter=100),"UNSUPPORTED_CONTROL")

# Intercept-only fitted artifacts preserve an empty, numeric preprocessing vector.
intercept_only <- fit(fit_penalized_glm,matrix(numeric(),8,0),c(rep(0,6),rep(1,2)),feature_order=character(),penalty="ridge",lambda_path=.1,selected_lambda=.1,scale=TRUE)
close(unlist(predict_pd(intercept_only,matrix(numeric(),2,0),feature_order=character(),expected_target=target)$value),c(.25,.25),1e-7)

# Named bank input matrices must preserve the same positional Firth model.
x_named <- matrix(rep(c(1.5,.5),each=40),ncol=1,dimnames=list(NULL,"cashflow_ratio"))
y_named <- c(rep(1,10),rep(0,30),rep(1,30),rep(0,10))
f_named <- fit(fit_firth_logit,x_named,y_named,feature_order="cashflow_ratio")
f_plain <- fit(fit_firth_logit,unname(x_named),y_named,feature_order="cashflow_ratio")
close(unlist(f_named$model$parameters$coefficients),unlist(f_plain$model$parameters$coefficients),1e-12)
close(pred(f_named,matrix(c(1.5,.5),ncol=1),"cashflow_ratio"),c(21/82,61/82),1e-8)
stopifnot(identical(unlist(f_named$model$feature_order),"cashflow_ratio"))

# Independent regression oracles for dimensional and binary-ordinal boundaries.
local({
if(!exists('fit_discriminant_pd'))library(creditratingsystem)
target<-list(event='default',horizon=30,time_unit='days',probability_measure='PHYSICAL',conditioning='performing')
meta<-list(target=target,training=list(data_fingerprint='independent-binary-cell-oracle',split_id='train',known_at='2020-01-01',data_origin='SYNTHETIC'),model_id='edge-oracle',model_version='1')
for(variant in c('lda','qda')) {
 m<-do.call(fit_discriminant_pd,c(list(X_train=matrix(0:3,ncol=1),y_train=c(0,0,1,1),feature_order='x',variant=variant),meta))
 p<-predict_pd(m,matrix(c(1,1.5,2),ncol=1),feature_order='x',expected_target=target)$value
 stopifnot(max(abs(unlist(p)-plogis(c(-4,0,4))))<1e-12)
}
x<-matrix(rep(c(-1,1),each=8),ncol=1);y<-c(rep('good',6),rep('default',2),rep('good',2),rep('default',6))
for(link in c('logit','probit')) {
 m<-do.call(fit_ordinal_model,c(list(X_train=x,y_train=y,feature_order='x',ordered_categories=c('good','default'),default_categories='default',link=link),meta))
 p<-predict_pd(m,matrix(c(-1,0,1),ncol=1),feature_order='x',expected_target=target)$value
 stopifnot(max(abs(unlist(p)-c(.25,.5,.75)))<1e-8,abs(unlist(m$model$parameters$thresholds))<1e-8)
 expected<-if(link=='logit')log(3)else qnorm(.75)
 stopifnot(abs(unlist(m$model$parameters$coefficients)-expected)<1e-8,m$diagnostics$parallel_slopes_diagnostic$status=='NOT_APPLICABLE')
}

})
