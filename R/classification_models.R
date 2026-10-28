# Portable classification artifacts; fitting never consumes prediction rows.
.cm_rows <- function(x) lapply(seq_len(nrow(x)),function(i)as.list(unname(x[i,])))
.cm_matrix <- function(x) do.call(rbind,lapply(x,function(z)as.numeric(unlist(z))))
.cm_model <- function(method,features,parameters,preprocessing,diagnostics,target,training,model_id,model_version,context) {
 m <- .cr_model(list(schema_version="1.0",model_id=model_id,model_version=model_version,method_id=paste0("classification.",method),feature_order=as.list(features),parameters=parameters,preprocessing=preprocessing,target=target,training=training,diagnostics=diagnostics,provenance=list(data_origin=if(is.null(training$data_origin))"USER_SUPPLIED" else training$data_origin,parameter_origin="ESTIMATED")))
 .cr_fit_result(m,paste0("classification.",method),diagnostics=diagnostics,context=context)
}
.cm_y <- function(y,n,require_both=TRUE) {
 y <- .cr_glm_vector(y,n,"y_train",NA_real_)
 if(!n || anyNA(y) || any(!y %in% c(0,1))) .cr_error("INVALID_OUTCOME","Training requires nonempty observed numeric zero/one labels")
 if(require_both && length(unique(y))!=2L) .cr_error("INSUFFICIENT_CLASS_SUPPORT","Both outcome classes must occur in training")
 as.integer(y)
}
.cm_priors <- function(priors,y) {
 p <- if(is.null(priors))tabulate(y+1L,2L)/length(y) else .cr_numeric(priors,"priors",vector_only=TRUE)
 if(length(p)!=2 || any(p<=0) || abs(sum(p)-1)>1e-12) .cr_error("INVALID_PRIORS","Priors must be positive probabilities for [0,1] summing to one")
 p
}
.cm_number <- function(x,name,lower=0,strict=FALSE) {
 x <- .cr_numeric(x,name,vector_only=TRUE)
 if(length(x)!=1 || if(strict)x<=lower else x<lower) .cr_error("INVALID_INPUT",paste(name,"is outside its permitted range"))
 as.numeric(x)
}
fit_discriminant_pd <- function(X_train,y_train,feature_order,variant,covariance_estimator="mle",covariance_ridge=0,priors=NULL,target,training,model_id,model_version,context=NULL) {
 x <- .cr_glm_design(X_train,feature_order,FALSE)$x; y <- .cm_y(y_train,nrow(x)); p <- .cm_priors(priors,y)
 if(!variant %in% c("lda","qda") || !covariance_estimator %in% c("mle","unbiased")) .cr_error("INVALID_INPUT","Variant lda/qda and estimator mle/unbiased required")
 ridge <- .cm_number(covariance_ridge,"covariance_ridge"); counts <- tabulate(y+1,2)
 if(any(counts<2)) .cr_error("INSUFFICIENT_CLASS_SUPPORT","Two observations per class required for covariance")
 means <- do.call(rbind,lapply(0:1,function(k)colMeans(x[y==k,,drop=FALSE])))
 scatters <- lapply(0:1,function(k)crossprod(sweep(x[y==k,,drop=FALSE],2,means[k+1,],"-")))
 if(variant=="lda") {cov <- (scatters[[1]]+scatters[[2]])/(nrow(x)-if(covariance_estimator=="unbiased")2 else 0); covs <- list(cov,cov)} else covs <- lapply(1:2,function(k)scatters[[k]]/(counts[k]-(covariance_estimator=="unbiased")))
 covs <- lapply(covs,function(c)c+diag(ridge,ncol(x))); conditions <- numeric(2)
 for(k in 1:2) {tryCatch(chol(covs[[k]]),error=function(e).cr_error("SINGULAR_COVARIANCE","Covariance singular; choose explicit positive ridge")); conditions[k] <- kappa(covs[[k]],exact=TRUE); if(conditions[k]>1e14).cr_error("SINGULAR_COVARIANCE","Covariance condition number exceeds 1e14")}
 diagnostics <- list(class_counts=as.list(counts),covariance_estimator=covariance_estimator,covariance_condition_numbers=as.list(conditions),calibration="MODEL_BASED_POSTERIOR_NOT_EMPIRICALLY_CALIBRATED",uncertainty="NOT_ESTIMATED",training_only=TRUE)
 .cm_model("fit_discriminant_pd",feature_order,list(kind="discriminant",variant=variant,priors=as.list(p),means=.cm_rows(means),covariances=lapply(covs,.cm_rows),covariance_ridge=ridge),list(transformation="identity"),diagnostics,target,training,model_id,model_version,context)
}
.cm_table <- function(X,features) {
 if(!is.character(features) || !length(features) || anyNA(features) || any(!nzchar(features)) || anyDuplicated(features)) .cr_error("FEATURE_MISMATCH","Distinct ordered feature names required")
 if(is.data.frame(X)) X <- lapply(seq_len(nrow(X)),function(i)lapply(X,function(c)c[[i]]))
 if(is.matrix(X)) X <- lapply(seq_len(nrow(X)),function(i)as.list(unname(X[i,])))
 if(!is.list(X) || any(vapply(X,length,integer(1))!=length(features))) .cr_error("FEATURE_MISMATCH","Table rows must match feature order")
 X
}
.cm_missing <- function(v) is.null(v) || (length(v)==1 && is.na(v))
.cm_category <- function(v) {if(!is.character(v) || length(v)!=1 || is.na(v) || !nzchar(v)) .cr_error("INVALID_CATEGORY","Categories must be nonempty strings, null reserved for missing"); v}
fit_naive_bayes_pd <- function(X_train,y_train,feature_order,feature_types,variant,alpha=1,variance_floor=0,missing_policy="error",unknown_policy="error",priors=NULL,target,training,model_id,model_version,context=NULL) {
 x <- .cm_table(X_train,feature_order); y <- .cm_y(y_train,length(x)); p <- .cm_priors(priors,y); types <- unlist(feature_types)
 if(length(types)!=length(feature_order) || any(!types %in% c("gaussian","categorical"))) .cr_error("INVALID_INPUT","One Gaussian/categorical type per feature required")
 if(!variant %in% c("gaussian","categorical","mixed") || (variant!="mixed" && any(types!=variant)) || (variant=="mixed" && length(unique(types))!=2)) .cr_error("INVALID_INPUT","Variant must agree with feature types")
 if(!missing_policy %in% c("error","marginalize","category") || !unknown_policy %in% c("error","reserved")) .cr_error("INVALID_INPUT","Unsupported missing/unknown policy")
 alpha <- .cm_number(alpha,"alpha",strict=TRUE); floor <- .cm_number(variance_floor,"variance_floor"); specs <- list()
 for(j in seq_along(types)) {
  vals <- lapply(x,function(r)r[[j]]); missing <- vapply(vals,.cm_missing,logical(1)); typ <- types[j]
  if(any(missing) && (missing_policy=="error" || (typ=="gaussian" && missing_policy=="category"))) .cr_error("MISSING_FEATURE","Missing feature incompatible with declared policy")
  if(typ=="gaussian") {
   means <- variances <- counts <- numeric(2)
   for(k in 0:1) {z <- .cr_numeric(unlist(vals[y==k & !missing]),"Gaussian feature",vector_only=TRUE); if(length(z)<2).cr_error("INSUFFICIENT_CLASS_SUPPORT","Two observed Gaussian values per class required"); v <- mean((z-mean(z))^2)+floor; if(v<=0).cr_error("SINGULAR_COVARIANCE","Zero Gaussian variance requires explicit positive variance_floor"); means[k+1] <- mean(z); variances[k+1] <- v; counts[k+1] <- length(z)}
   specs[[j]] <- list(type=typ,means=as.list(means),variances=as.list(variances),observed_counts=as.list(counts))
  } else {
   categories <- sort(unique(vapply(vals[!missing],.cm_category,character(1))),method="radix"); hm <- missing_policy=="category"; hu <- unknown_policy=="reserved"; size <- length(categories)+hm+hu
   if(!size).cr_error("INSUFFICIENT_CLASS_SUPPORT","Categorical feature has no supported values")
   counts <- matrix(0,2,size)
   for(i in seq_along(vals)) {if(missing[i] && !hm)next; index <- if(missing[i])length(categories)+1L else match(vals[[i]],categories); counts[y[i]+1,index] <- counts[y[i]+1,index]+1}
   probs <- (counts+alpha)/(rowSums(counts)+alpha*size)
   specs[[j]] <- list(type=typ,categories=as.list(categories),missing_index=if(hm)length(categories) else NULL,unknown_index=if(hu)length(categories)+as.integer(hm) else NULL,counts=.cm_rows(counts),probabilities=.cm_rows(probs))
  }
 }
 diagnostics <- list(class_counts=as.list(tabulate(y+1,2)),conditional_independence_assumed=TRUE,calibration="MODEL_BASED_POSTERIOR_NOT_EMPIRICALLY_CALIBRATED",uncertainty="NOT_ESTIMATED",training_only=TRUE)
 .cm_model("fit_naive_bayes_pd",feature_order,list(kind="naive_bayes",variant=variant,priors=as.list(p),features=specs,alpha=alpha,variance_floor=floor,missing_policy=missing_policy,unknown_policy=unknown_policy),list(transformation="identity"),diagnostics,target,training,model_id,model_version,context)
}
fit_svm_score <- function(X_train,y_train,feature_order,kernel,C,gamma=NULL,scale=TRUE,class_weights=NULL,tolerance=1e-7,max_iter=NULL,target,training,model_id,model_version,context=NULL) {
 x <- .cr_glm_design(X_train,feature_order,FALSE)$x; y <- .cm_y(y_train,nrow(x)); C <- .cm_number(C,"C",strict=TRUE)
 if(!kernel %in% c("linear","rbf") || !is.logical(scale) || length(scale)!=1 || is.na(scale)) .cr_error("INVALID_INPUT","Kernel linear/rbf and boolean scale required")
 g <- if(kernel=="rbf").cm_number(gamma,"gamma",strict=TRUE) else NULL
 if(kernel=="linear" && !is.null(gamma)).cr_error("INVALID_INPUT","Linear kernel does not use gamma")
 tol <- .cm_number(tolerance,"tolerance",strict=TRUE)
 if(!is.null(max_iter)).cr_error("UNSUPPORTED_CONTROL","Native e1071 does not expose an iteration cap; use max_iter=NULL and convergence KKT diagnostics")
 cw <- if(is.null(class_weights))c(1,1) else .cr_numeric(class_weights,"class_weights",vector_only=TRUE)
 if(length(cw)!=2 || any(cw<=0)).cr_error("INVALID_WEIGHTS","Positive class weights for [0,1] required")
 center <- if(scale)colMeans(x) else rep(0,ncol(x)); scaling <- if(scale)sqrt(colMeans(sweep(x,2,center,"-")^2)) else rep(1,ncol(x)); scaling[scaling==0] <- 1; z <- sweep(sweep(x,2,center,"-"),2,scaling,"/")
 fit <- e1071::svm(z,factor(y,levels=c(0,1)),type="C-classification",kernel=if(kernel=="rbf")"radial" else "linear",cost=C,gamma=if(is.null(g))1 else g,class.weights=setNames(cw,c("0","1")),scale=FALSE,tolerance=tol,shrinking=FALSE,probability=FALSE)
 # LIBSVM in e1071 directs positive margin to its first internal class.
 orientation <- if(fit$levels[fit$labels[1]]=="1")1 else -1
 coef <- as.numeric(fit$coefs)*orientation; intercept <- -fit$rho*orientation
 kern <- if(kernel=="linear")z%*%t(fit$SV) else exp(-g*outer(seq_len(nrow(z)),seq_len(nrow(fit$SV)),Vectorize(function(i,j)sum((z[i,]-fit$SV[j,])^2))))
 score <- as.vector(kern%*%coef+intercept); margin <- (2*y-1)*score
 # Native libsvm has no public iteration cap/status; verify its dual KKT certificate.
 alpha <- numeric(length(y)); alpha[fit$index] <- abs(coef); bounds <- C*cw[y+1]; eps <- max(1e-7,tol*10)
 residual <- max(c(pmax(1-margin[alpha<bounds-eps],0),pmax(margin[alpha>eps]-1,0),abs(sum(coef))))
 if(residual>max(1e-5,tol*20)).cr_error("NON_CONVERGENCE","LIBSVM dual KKT residual exceeds tolerance")
 diagnostics <- list(converged=TRUE,iterations=NULL,iteration_cap_supported=FALSE,requested_max_iter=max_iter,kkt_residual=residual,support_vector_count=nrow(fit$SV),hinge_loss=mean(pmax(0,1-margin)),training_only=TRUE,score_orientation="HIGHER_MEANS_DEFAULT",calibration="UNCALIBRATED_SCORE",uncertainty="NOT_ESTIMATED")
 .cm_model("fit_svm_score",feature_order,list(kind="svm",kernel=kernel,C=C,gamma=g,support_vectors=.cm_rows(fit$SV),dual_coefficients=as.list(coef),intercept=as.numeric(intercept),class_weights=as.list(cw)),list(transformation="standardize",center=as.list(center),scale=as.list(scaling)),diagnostics,target,training,model_id,model_version,context)
}
.cm_extended_scores <- function(fitted_model,X,feature_order,expected_target) {
 model <- .cr_model(fitted_model)
 if(!identical(as.character(unlist(feature_order)),as.character(unlist(model$feature_order)))).cr_error("FEATURE_MISMATCH","Feature order must exactly match frozen artifact")
 if(!identical(expected_target,model$target)).cr_error("TARGET_MISMATCH","Prediction target must match frozen artifact")
 p <- model$parameters; kind <- p$kind
 if(kind %in% c("neural","sequence"))return(list(score=.cn_raw(model,X,feature_order),scale="LINEAR_PREDICTOR_LOGIT"))
 if(kind=="bayesian")return(list(score=.cb_prediction(model,X,feature_order)$probability,scale="POSTERIOR_MEAN_PROBABILITY_SCORE"))
 if(kind %in% c("gam","random_forest","gradient_boosting")) {
  if(kind=="random_forest")return(list(score=.cf_forest_probability(model,X,feature_order),scale="LEAF_PROBABILITY_SCORE"))
  return(list(score=if(kind=="gam").cf_gam_raw(model,X,feature_order) else .cf_boost_raw(model,X,feature_order),scale="LINEAR_PREDICTOR_LOGIT"))
 }
 if(kind=="naive_bayes") {
  x <- .cm_table(X,feature_order); scores <- matrix(rep(log(unlist(p$priors)),each=length(x)),ncol=2)
  for(j in seq_along(p$features)) {s <- p$features[[j]]; for(i in seq_along(x)) {
   v <- x[[i]][[j]]; missing <- .cm_missing(v)
   if(missing) {if(p$missing_policy=="marginalize")next; if(s$type=="gaussian" || is.null(s$missing_index)).cr_error("MISSING_FEATURE","Missing feature not supported"); idx <- s$missing_index+1L} else if(s$type=="categorical") {.cm_category(v); idx <- match(v,unlist(s$categories)); if(is.na(idx)) {if(is.null(s$unknown_index)).cr_error("UNKNOWN_CATEGORY","Previously unseen category not supported");idx <- s$unknown_index+1L}}
   if(s$type=="gaussian") {v <- .cm_number(v,"Gaussian feature",lower=-Inf); variances <- unlist(s$variances); scores[i,] <- scores[i,]-.5*(log(2*pi*variances)+(v-unlist(s$means))^2/variances)} else scores[i,] <- scores[i,]+log(.cm_matrix(s$probabilities)[,idx])
  }}
  return(list(score=scores[,2]-scores[,1],scale="LOG_POSTERIOR_ODDS"))
 }
 if(kind=="ordinal") {x <- .cr_glm_design(X,feature_order,TRUE)$x;return(list(score=as.vector(x%*%as.numeric(unlist(p$coefficients))),scale=paste0("ORDINAL_LATENT_RISK_",toupper(p$link))))}
 if(kind %in% c("penalized_logit","firth_logit")) {
  x <- .cr_numeric(X,"X",allow_empty=TRUE)
  if(model$preprocessing$transformation=="standardize")x <- sweep(sweep(x,2,as.numeric(unlist(model$preprocessing$center)),"-"),2,as.numeric(unlist(model$preprocessing$scale)),"/")
  d <- .cr_glm_design(x,feature_order,p$fit_intercept)$d
  return(list(score=as.vector(d%*%unlist(p$coefficients)),scale="LINEAR_PREDICTOR_LOGIT"))
 }
 x <- .cr_glm_design(X,feature_order,FALSE)$x
 if(kind=="discriminant") {
  means <- .cm_matrix(p$means); scores <- matrix(0,nrow(x),2)
  for(k in 1:2) {c <- .cm_matrix(p$covariances[[k]]); delta <- sweep(x,2,means[k,],"-"); ch <- chol(c); v <- forwardsolve(t(ch),t(delta)); scores[,k] <- log(p$priors[[k]])-sum(log(diag(ch)))-.5*colSums(v*v)}
  return(list(score=scores[,2]-scores[,1],scale="LOG_POSTERIOR_ODDS"))
 }
 if(kind=="svm") {
  prep <- model$preprocessing; z <- sweep(sweep(x,2,unlist(prep$center),"-"),2,unlist(prep$scale),"/"); sv <- .cm_matrix(p$support_vectors)
  kern <- if(p$kernel=="linear")z%*%t(sv) else exp(-p$gamma*outer(seq_len(nrow(z)),seq_len(nrow(sv)),Vectorize(function(i,j)sum((z[i,]-sv[j,])^2))))
  return(list(score=as.vector(kern%*%unlist(p$dual_coefficients)+p$intercept),scale="SVM_MARGIN"))
 }
 .cr_error("UNSUPPORTED_MODEL","Frozen model kind not supported by extended score prediction")
}
predict_model_score <- function(fitted_model,X,feature_order,expected_target,context=NULL) {
 model <- .cr_model(fitted_model)
 ctx <- .cr_result(NULL,"classification.predict_model_score",context=context)$context
 for(key in c("horizon","time_unit","probability_measure","conditioning"))if(!is.null(ctx[[key]]) && !isTRUE(all.equal(ctx[[key]],model$target[[key]]))).cr_error("TARGET_MISMATCH",paste("Context",key,"conflicts with frozen target"))
 if(model$method_id=="classification.fit_binary_glm") {
  p <- model$parameters
  if(p$offset_required).cr_error("OFFSET_MISMATCH","Use explicit GLM prediction for offset-dependent models")
  if(!identical(as.character(unlist(feature_order)),as.character(unlist(model$feature_order))) || !identical(expected_target,model$target)).cr_error("MODEL_MISMATCH","Features and target must match frozen artifact")
  d <- .cr_glm_design(X,feature_order,p$fit_intercept)$d; result <- list(score=as.vector(d%*%unlist(p$coefficients)),scale=paste0("LINEAR_PREDICTOR_",toupper(p$link)))
 } else result <- .cm_extended_scores(model,X,feature_order,expected_target)
 diagnostics <- list(scale=result$scale,orientation="HIGHER_MEANS_DEFAULT",model_id=model$model_id,model_version=model$model_version,calibration="NO_CALIBRATION_APPLIED")
 if(identical(model$parameters$kind,"ordinal")) {diagnostics$category_probabilities <- .cm_rows(.cm_ordinal_probabilities(model,X,feature_order,expected_target));diagnostics$ordered_categories <- model$parameters$ordered_categories}
 .cr_result(as.list(unname(result$score)),"classification.predict_model_score",unit="score",context=context,diagnostics=diagnostics,provenance=model$provenance)
}
.cm_predict_pd <- function(fitted_model,X,feature_order,expected_target,context=NULL) {
 model <- .cr_model(fitted_model)
 if(model$parameters$kind=="bayesian") {
  if(!identical(as.character(unlist(feature_order)),as.character(unlist(model$feature_order))) || !identical(expected_target,model$target)).cr_error("MODEL_MISMATCH","Features and target must match frozen model")
  result<-.cb_prediction(model,X,feature_order);diagnostics<-c(result$diagnostics,list(model_id=model$model_id,model_version=model$model_version,target=model$target))
  return(.cr_result(as.list(result$probability),"classification.predict_pd",unit="probability",context=context,diagnostics=diagnostics,provenance=model$provenance))
 }
 if(model$parameters$kind=="ordinal") {
  probabilities <- .cm_ordinal_probabilities(model,X,feature_order,expected_target);p <- model$parameters
  if(!length(p$default_categories)).cr_error("UNMAPPED_DEFAULT","Ordinal categories require explicit default-category mapping before PD prediction")
  pd <- rowSums(probabilities[,seq.int(ncol(probabilities)-length(p$default_categories)+1,ncol(probabilities)),drop=FALSE])
  return(.cr_result(as.list(unname(pd)),"classification.predict_pd",unit="probability",context=context,diagnostics=list(model_id=model$model_id,model_version=model$model_version,target=model$target,category_probabilities=.cm_rows(probabilities),ordered_categories=p$ordered_categories),provenance=model$provenance))
 }
 result <- .cm_extended_scores(model,X,feature_order,expected_target)
 if(result$scale=="SVM_MARGIN").cr_error("UNCALIBRATED_SCORE","SVM margins require separate calibration before becoming PDs")
 probabilities <- if(result$scale=="LEAF_PROBABILITY_SCORE")result$score else stats::plogis(result$score)
 .cr_result(as.list(unname(probabilities)),"classification.predict_pd",unit="probability",context=context,diagnostics=list(model_id=model$model_id,model_version=model$model_version,target=model$target,calibration="MODEL_BASED_POSTERIOR"),provenance=model$provenance)
}
.cm_controls <- function(max_iter,tolerance) {
 if(length(max_iter)!=1 || !is.numeric(max_iter) || !is.finite(max_iter) || max_iter<1 || max_iter!=floor(max_iter)).cr_error("INVALID_INPUT","max_iter must be positive integer")
 tolerance <- .cm_number(tolerance,"tolerance",strict=TRUE);if(tolerance>=1).cr_error("INVALID_INPUT","tolerance must be below one")
 list(max_iter=as.integer(max_iter),tolerance=tolerance)
}
fit_firth_logit <- function(X_train,y_train,feature_order,fit_intercept=TRUE,max_iter=1000L,tolerance=1e-9,target,training,model_id,model_version,context=NULL) {
 design <- .cr_glm_design(X_train,feature_order,fit_intercept); d <- design$d; y <- .cm_y(y_train,nrow(d),require_both=FALSE); ctl <- .cm_controls(max_iter,tolerance)
 singular <- svd(d,nu=0,nv=0)$d
 if(sum(singular>singular[1]*1e-10)<ncol(d)).cr_error("RANK_DEFICIENT","Firth requires full rank; Jeffreys determinant otherwise zero")
 # brglm2 indexes coefficients by design names; cbind may leave an empty intercept name.
 # Backend names are positional and must not depend on supplied matrix dimnames.
 colnames(d) <- paste0("design_",seq_len(ncol(d)))
 fitted <- brglm2::brglmFit(d,y,family=stats::binomial("logit"),intercept=fit_intercept,singular.ok=FALSE,control=brglm2::brglmControl(type="MPL_Jeffreys",a=.5,epsilon=tolerance,maxit=max_iter))
 beta <- unname(fitted$coefficients); eta <- as.vector(d%*%beta); p <- plogis(eta); w <- p*(1-p); info <- crossprod(d,d*w); ch <- chol(info); inverse <- chol2inv(ch); h <- w*rowSums((d%*%inverse)*d)
 gradient <- as.vector(crossprod(d,y-p+h*(.5-p)));residual <- max(abs(gradient)); ll <- sum(y*eta-pmax(eta,0)-log1p(exp(-abs(eta)))); objective <- ll+sum(log(diag(ch)))
 if(!isTRUE(fitted$converged) || any(!is.finite(beta)) || residual>max(1e-7,sqrt(tolerance))).cr_error("NON_CONVERGENCE","Firth adjusted score has not converged",list(adjusted_score_max=residual))
 diagnostics <- list(converged=TRUE,iterations=fitted$iter,penalized_log_likelihood=objective,adjusted_score_max=residual,rank=ncol(d),jeffreys_logdet_multiplier=.5,uncertainty="EXPECTED_INFORMATION_LOCAL_APPROXIMATION_NOT_PROFILE_INTERVAL",training_only=TRUE)
 .cm_model("fit_firth_logit",feature_order,list(kind="firth_logit",link="logit",fit_intercept=fit_intercept,coefficients=as.list(beta),expected_information_inverse=.cm_rows(inverse)),list(transformation="identity"),diagnostics,target,training,model_id,model_version,context)
}
fit_penalized_glm <- function(X_train,y_train,feature_order,penalty,lambda_path,selected_lambda,l1_ratio=NULL,fit_intercept=TRUE,scale=TRUE,weights=NULL,max_iter=10000L,tolerance=1e-9,target,training,model_id,model_version,context=NULL) {
 x <- .cr_glm_design(X_train,feature_order,fit_intercept)$x; y <- .cm_y(y_train,nrow(x));ctl <- .cm_controls(max_iter,tolerance)
 if(!penalty %in% c("ridge","lasso","elasticnet") || !is.logical(scale) || length(scale)!=1 || is.na(scale)).cr_error("INVALID_INPUT","Explicit penalty and boolean scale required")
 alpha <- if(penalty=="ridge")0 else if(penalty=="lasso")1 else .cm_number(l1_ratio,"l1_ratio",strict=TRUE)
 if(penalty=="elasticnet" && alpha>=1).cr_error("INVALID_INPUT","Elastic net l1_ratio must be strictly between zero and one")
 if(penalty!="elasticnet" && !is.null(l1_ratio)).cr_error("INVALID_INPUT","l1_ratio only for elasticnet")
 lambdas <- .cr_numeric(lambda_path,"lambda_path",vector_only=TRUE)
 if(any(lambdas<0) || anyDuplicated(lambdas) || any(diff(lambdas)>=0)).cr_error("INVALID_INPUT","lambda_path unique strictly decreasing nonnegative required")
 selected <- .cm_number(selected_lambda,"selected_lambda");if(!selected %in% lambdas).cr_error("INVALID_INPUT","Selected lambda must be exact member of path")
 w <- .cr_glm_vector(weights,nrow(x),"weights",1)
 if(any(w<0) || sum(w)<=0 || any(vapply(0:1,function(k)sum(w[y==k]),numeric(1))==0)).cr_error("INVALID_WEIGHTS","Positive total weight in both classes required")
 center <- if(scale)colSums(x*w)/sum(w) else rep(0,ncol(x)); scaling <- if(scale)sqrt(colSums(sweep(x,2,center,"-")^2*w)/sum(w)) else rep(1,ncol(x));scaling[scaling==0] <- 1
 z <- sweep(sweep(x,2,center,"-"),2,scaling,"/");d <- .cr_glm_design(z,feature_order,fit_intercept)$d;mask <- rep(1,ncol(d));if(fit_intercept)mask[1] <- 0
 # glmnet requires at least two columns; zero padding has zero coefficient and no penalty contribution.
 zg <- if(ncol(z)<2)cbind(z,matrix(0,nrow(z),2-ncol(z))) else z
 path <- list()
 positive <- lambdas[lambdas>0]
 backend <- if(length(positive) && ncol(z)>0)glmnet::glmnet(zg,y,family="binomial",alpha=alpha,lambda=positive,weights=w,standardize=FALSE,intercept=fit_intercept,thresh=min(tolerance^2,1e-14),maxit=max_iter) else NULL
 for(i in seq_along(lambdas)) {
  lam <- lambdas[i]
  if(lam==0 || ncol(z)==0) {
   active <- w>0;if(qr(d[active,,drop=FALSE],tol=1e-10)$rank<ncol(d)).cr_error("RANK_DEFICIENT","Zero penalty needs full-rank design")
   .cr_glm_separation(d[active,,drop=FALSE],y[active]);fitted <- stats::glm.fit(d,y,weights=w,family=stats::binomial(),intercept=fit_intercept,control=stats::glm.control(epsilon=tolerance,maxit=max_iter));beta <- unname(fitted$coefficients)
  } else {
   if(backend$jerr!=0).cr_error("NON_CONVERGENCE","glmnet did not converge along requested path")
   coeff <- as.numeric(stats::coef(backend,s=lam));beta <- if(fit_intercept)coeff[seq_len(ncol(z)+1)] else coeff[seq_len(ncol(z))+1]
  }
  eta <- as.vector(d%*%beta);gradient <- as.vector(crossprod(d,w*(plogis(eta)-y)))/sum(w)+lam*(1-alpha)*mask*beta
  kkt <- ifelse(mask==0,abs(gradient),ifelse(abs(beta)>tolerance,abs(gradient+lam*alpha*sign(beta)),pmax(abs(gradient)-lam*alpha,0)))
  residual <- max(kkt);objective <- sum(w*(pmax(eta,0)+log1p(exp(-abs(eta)))-y*eta))/sum(w)+lam*((1-alpha)/2*sum(mask*beta^2)+alpha*sum(mask*abs(beta)))
  if(any(!is.finite(beta)) || residual>max(1e-6,sqrt(tolerance))).cr_error("NON_CONVERGENCE","Elastic-net KKT tolerance not met",list(lambda=lam,kkt_residual=residual))
  path[[i]] <- list(lambda_value=lam,coefficients=as.list(beta),objective=objective,kkt_residual=residual)
 }
 chosen <- path[[match(selected,lambdas)]]
 diagnostics <- list(converged=TRUE,path=path,selection="CALLER_DECLARED_LAMBDA_NO_AUTOMATIC_HOLDOUT_ACCESS",intercept_penalized=FALSE,objective_normalization="WEIGHTED_MEAN_NEGATIVE_LOG_LIKELIHOOD",weight_type="NONNEGATIVE_CASE_WEIGHT",training_only=TRUE,uncertainty="NOT_ESTIMATED_POST_SELECTION")
 .cm_model("fit_penalized_glm",feature_order,list(kind="penalized_logit",link="logit",penalty=penalty,l1_ratio=alpha,selected_lambda=selected,fit_intercept=fit_intercept,coefficients=chosen$coefficients),list(transformation="standardize",center=as.list(center),scale=as.list(scaling)),diagnostics,target,training,model_id,model_version,context)
}
.cm_ordinal_diagnostic <- function(x,y,link) {
 if(!ncol(x))return(list(status="NOT_APPLICABLE",reason="No slopes"))
 d <- cbind(1,x);k <- length(unique(y))-1L
 if(k<2)return(list(status="NOT_APPLICABLE",reason="Only one cumulative threshold"))
 estimates <- list();influences <- list()
 good <- tryCatch({for(j in seq_len(k)) {
  binary <- as.numeric(y>j-1L);.cr_glm_separation(d,binary);fitted <- stats::glm.fit(d,binary,family=stats::binomial(link),control=stats::glm.control(epsilon=1e-11,maxit=1000))
  if(!isTRUE(fitted$converged))stop("Threshold regression failed")
  eta <- as.vector(d%*%fitted$coefficients);p <- pmin(pmax(fitted$fitted.values,1e-12),1-1e-12);dp <- if(link=="logit")p*(1-p) else dnorm(eta)
  bread <- crossprod(d,d*(dp^2/(p*(1-p))));scores <- d*((binary-p)*dp/(p*(1-p)));influences[[j]] <- scores%*%solve(bread);estimates[[j]] <- unname(fitted$coefficients)
 };TRUE},error=function(e)FALSE)
 if(!good)return(list(status="NOT_ESTIMABLE",reason="Separated or singular cumulative binary regression"))
 influence <- do.call(cbind,influences);covariance <- crossprod(influence);beta <- unlist(estimates);contrast <- matrix(0,(k-1)*ncol(x),k*ncol(d))
 for(j in 2:k)for(h in seq_len(ncol(x))) {contrast[(j-2)*ncol(x)+h,h+1] <- -1;contrast[(j-2)*ncol(x)+h,(j-1)*ncol(d)+h+1] <- 1}
 diff <- as.vector(contrast%*%beta);variance <- contrast%*%covariance%*%t(contrast)
 if(qr(variance)$rank<length(diff))return(list(status="NOT_ESTIMABLE",reason="Singular joint slope-contrast covariance"))
 statistic <- as.numeric(crossprod(diff,solve(variance,diff)))
 list(status="ESTIMATED",method="STACKED_BINARY_SANDWICH_WALD",statistic=statistic,degrees_of_freedom=length(diff),p_value=stats::pchisq(statistic,length(diff),lower.tail=FALSE),threshold_slopes=lapply(estimates,function(b)as.list(b[-1])),assumptions="IID rows; asymptotic joint estimating-equation sandwich; exploratory diagnostic, not proof of proportionality")
}
fit_ordinal_model <- function(X_train,y_train,feature_order,ordered_categories,link,default_categories=NULL,max_iter=1000L,tolerance=1e-9,target,training,model_id,model_version,context=NULL) {
 x <- .cr_glm_design(X_train,feature_order,TRUE)$x;ctl <- .cm_controls(max_iter,tolerance);cats <- as.character(unlist(ordered_categories))
 if(length(cats)<2 || anyDuplicated(cats) || anyNA(cats) || any(!nzchar(cats))).cr_error("INVALID_INPUT","Distinct ordered category labels required")
 if(!link %in% c("logit","probit")).cr_error("INVALID_INPUT","Ordinal link logit or probit required")
 ylabels <- as.character(unlist(y_train));if(length(ylabels)!=nrow(x) || anyNA(ylabels) || any(!ylabels %in% cats)).cr_error("INVALID_OUTCOME","Every observed outcome must belong to ordered categories")
 y <- match(ylabels,cats)-1L;counts <- tabulate(y+1L,length(cats));if(any(counts==0)).cr_error("INSUFFICIENT_CLASS_SUPPORT","Every ordered category must occur in training")
 defaults <- as.character(unlist(default_categories));if(length(defaults) && (anyDuplicated(defaults) || !identical(defaults,tail(cats,length(defaults))))).cr_error("INVALID_TARGET","Default categories must be ordered terminal tail of risk grades")
 if(ncol(x)) {
  if(qr(cbind(1,x),tol=1e-10)$rank<ncol(x)+1).cr_error("RANK_DEFICIENT","Ordinal slopes full rank without intercept required")
  if(length(cats)==2L) {
   design <- cbind(1,x);.cr_glm_separation(design,y)
   fitted <- stats::glm.fit(design,y,family=stats::binomial(link),control=stats::glm.control(epsilon=tolerance,maxit=max_iter))
   if(!isTRUE(fitted$converged) || any(!is.finite(fitted$coefficients))).cr_error("NON_CONVERGENCE","Binary ordinal likelihood failed to converge")
   raw <- unname(fitted$coefficients);beta <- raw[-1];thresholds <- -raw[1]
   nll <- function(b) {eta<-as.vector(design%*%b);lp<-if(link=="logit")stats::plogis(eta,log.p=TRUE)else stats::pnorm(eta,log.p=TRUE);lq<-if(link=="logit")stats::plogis(eta,lower.tail=FALSE,log.p=TRUE)else stats::pnorm(eta,lower.tail=FALSE,log.p=TRUE);-sum(ifelse(y==1,lp,lq))}
   hessian <- stats::optimHess(raw,nll);if(any(!is.finite(hessian)) || min(eigen(hessian,symmetric=TRUE,only.values=TRUE)$values)<=0).cr_error("NON_IDENTIFIABLE","Binary ordinal information is singular")
   transform <- rbind(cbind(0,diag(ncol(x))),c(-1,rep(0,ncol(x))))
   cov <- transform%*%solve(hessian)%*%t(transform);ll <- -nll(raw);gradient <- NULL
  } else {
  data <- data.frame(.outcome=ordered(ylabels,levels=cats),x);names(data)[-1] <- paste0("x",seq_len(ncol(x)))
  fitted <- MASS::polr(.outcome~.,data=data,method=if(link=="logit")"logistic" else "probit",Hess=TRUE,control=list(maxit=max_iter,reltol=tolerance))
  if(fitted$convergence!=0).cr_error("NON_CONVERGENCE","Ordinal likelihood failed to converge")
  beta <- unname(fitted$coefficients);thresholds <- unname(fitted$zeta);cov <- unname(stats::vcov(fitted));ll <- as.numeric(stats::logLik(fitted));gradient <- NULL
  # MASS vcov uses actual thresholds; portable covariance uses first/log-spacing coordinates.
  jac <- diag(nrow(cov));p <- length(beta)
  if(length(thresholds)>1)for(j in 2:length(thresholds)) {jac[p+j,p+j] <- 1/(thresholds[j]-thresholds[j-1]);jac[p+j,p+j-1] <- -jac[p+j,p+j]}
  cov <- jac%*%cov%*%t(jac)
  }
 } else {
  cum <- cumsum(counts)[-length(counts)]/nrow(x);thresholds <- if(link=="logit")qlogis(cum) else qnorm(cum);beta <- numeric();cov <- NULL;gradient <- 0;ll <- sum(counts*log(counts/nrow(x)))
 }
 if(any(diff(thresholds)<=0) || any(!is.finite(thresholds))).cr_error("NON_IDENTIFIABLE","Ordinal thresholds finite strictly increasing required")
 diagnostics <- list(converged=TRUE,log_likelihood=ll,score_max=gradient,class_counts=as.list(counts),parallel_slopes_diagnostic=.cm_ordinal_diagnostic(x,y,link),covariance_parameterization=if(is.null(cov))"NOT_ESTIMATED" else "SLOPES_FIRST_THRESHOLD_LOG_THRESHOLD_SPACINGS",uncertainty=if(is.null(cov))"NOT_ESTIMATED" else "MODEL_BASED_ASYMPTOTIC",training_only=TRUE)
 .cm_model("fit_ordinal_model",feature_order,list(kind="ordinal",link=link,coefficients=as.list(beta),thresholds=as.list(thresholds),ordered_categories=as.list(cats),default_categories=as.list(defaults),covariance=if(is.null(cov))NULL else .cm_rows(cov)),list(transformation="identity"),diagnostics,target,training,model_id,model_version,context)
}
.cm_ordinal_probabilities <- function(model,X,feature_order,expected_target) {
 if(!identical(as.character(unlist(feature_order)),as.character(unlist(model$feature_order))) || !identical(expected_target,model$target)).cr_error("MODEL_MISMATCH","Feature order and target must match ordinal model")
 x <- .cr_glm_design(X,feature_order,TRUE)$x;p <- model$parameters;eta <- as.vector(x%*%as.numeric(unlist(p$coefficients)));thresholds <- unlist(p$thresholds)
 cdf <- outer(eta,thresholds,function(e,t)if(p$link=="logit")plogis(t-e) else pnorm(t-e));cumulative <- cbind(0,cdf,1)
 cumulative[,-1,drop=FALSE]-cumulative[,-ncol(cumulative),drop=FALSE]
}
