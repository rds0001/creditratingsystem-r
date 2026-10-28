.cf_gam_spline <- function(v,spec,derivative=0L) {
 knots <- unlist(spec$full_knots);degree <- spec$degree;lo <- knots[degree+1];hi <- knots[length(knots)-degree];clamped<-pmin(pmax(v,lo),hi)
 basis <- splines::splineDesign(knots,clamped,ord=degree+1,derivs=rep(derivative,length(v)),outer.ok=FALSE)
 outside <- v<lo | v>hi
 if(any(outside)) {
  if(spec$extrapolation=="error").cr_error("OUTSIDE_SUPPORT","Spline predictor outside frozen boundary knots")
  if(spec$extrapolation=="constant") {if(derivative>0)basis[outside,] <- 0} else if(derivative==0)basis <- basis+(v-clamped)*splines::splineDesign(knots,clamped,ord=degree+1,derivs=rep(1L,length(v))) else if(derivative>1)basis[outside,] <- 0
 }
 basis%*%.cm_matrix(spec$coefficient_transform)
}
.cf_gam_design <- function(x,parameters,features,derivative_feature=NULL) {
 columns <- list(matrix(if(is.null(derivative_feature))1 else 0,nrow(x),1))
 for(name in unlist(parameters$linear_features)) {j <- match(name,features);columns[[length(columns)+1L]] <- if(is.null(derivative_feature))x[,j,drop=FALSE] else matrix(as.numeric(name==derivative_feature),nrow(x),1)}
 for(s in parameters$smooths) {j <- match(s$feature,features);columns[[length(columns)+1L]] <- if(is.null(derivative_feature) || derivative_feature==s$feature).cf_gam_spline(x[,j],s,if(is.null(derivative_feature))0L else 1L) else matrix(0,nrow(x),length(s$coefficient_transform[[1]]))}
 do.call(cbind,columns)
}
.cf_gam_recession <- function(d,y,penalty,lower) {
 eig<-eigen(penalty,symmetric=TRUE);positive<-eig$values>max(max(eig$values)*1e-10,0);scaling<-pmax(apply(abs(d),2,max),1);signed<-sweep(d,2,scaling,'/')*(2*y-1);equality<-sweep(t(eig$vectors[,positive,drop=FALSE]),2,scaling,'/');p<-ncol(d)
 bounded<-which(is.finite(lower));a<-rbind(cbind(signed,-signed),cbind(equality,-equality),cbind(diag(p),diag(p)))
 dirs<-c(rep('>=',nrow(d)),rep('=',nrow(equality)),rep('<=',p));rhs<-c(rep(0,nrow(d)+nrow(equality)),rep(1,p))
 if(length(bounded)) {zero<-matrix(0,length(bounded),2*p);for(i in seq_along(bounded))zero[i,p+bounded[i]]<-1;a<-rbind(a,zero);dirs<-c(dirs,rep('=',length(bounded)));rhs<-c(rhs,rep(0,length(bounded)))}
 result<-lpSolve::lp('max',c(colMeans(signed),-colMeans(signed)),a,dirs,rhs)
 if(result$status!=0).cr_error('NUMERICAL_FAILURE','GAM null-penalty recession LP failed')
 objective<-mean(signed%*%(result$solution[seq_len(p)]-result$solution[p+seq_len(p)]))
 if(objective>1e-8).cr_error('SEPARATION','Unpenalized feasible GAM direction separates outcomes; finite optimum unavailable')
 objective
}

fit_gam_pd <- function(X_train,y_train,feature_order,linear_features,smooth_specs,variant,max_iter=2000L,tolerance=1e-9,target,training,model_id,model_version,context=NULL) {
 x <- .cr_glm_design(X_train,feature_order,TRUE)$x;y <- .cm_y(y_train,nrow(x));ctl <- .cm_controls(max_iter,tolerance)
 if(!variant %in% c("unconstrained","monotone")).cr_error("INVALID_INPUT","GAM variant unconstrained/monotone required")
 linear <- as.character(unlist(linear_features));smooths<-list();names<-linear;penalties<-list();lower<-rep(-Inf,1+length(linear));constrained<-FALSE
 for(spec in smooth_specs) {
  if(!is.list(spec)).cr_error("INVALID_INPUT","Every smooth spec must be a list")
  name<-spec$feature;names<-c(names,name);degree<-if(is.null(spec$degree))3L else spec$degree
  if(length(degree)!=1 || !degree %in% 1:3 || is.logical(degree)).cr_error("INVALID_INPUT","Spline degree 1,2,3 required")
  boundary <- .cr_numeric(unlist(spec$boundary_knots),"boundary_knots",vector_only=TRUE);interior<-if(is.null(spec$interior_knots))numeric() else .cr_numeric(unlist(spec$interior_knots),"interior_knots",vector_only=TRUE,allow_empty=TRUE)
  if(length(boundary)!=2 || boundary[1]>=boundary[2] || any(diff(interior)<=0) || any(interior<=boundary[1] | interior>=boundary[2])).cr_error("INVALID_INPUT","Strict interior knots must lie within increasing boundaries")
  mono<-if(is.null(spec$monotonicity))"none" else spec$monotonicity;extra<-if(is.null(spec$extrapolation))"error" else spec$extrapolation;smoothing<-.cm_number(spec$smoothing,"smoothing")
  if(!mono %in% c("none","increasing","decreasing") || !extra %in% c("error","constant","linear")).cr_error("INVALID_INPUT","Invalid monotonicity/extrapolation")
  if(variant=="unconstrained" && mono!="none").cr_error("INVALID_INPUT","Unconstrained variant cannot declare monotone terms")
  knots<-c(rep(boundary[1],degree+1),interior,rep(boundary[2],degree+1));nb<-length(knots)-degree-1L
  transform<-if(mono=="none")rbind(rep(0,nb-1),diag(nb-1)) else (if(mono=="increasing")1 else -1)*outer(seq_len(nb),seq_len(nb-1),">" )
  penalty<-if(nb>2)diff(diag(nb),differences=2)%*%transform else matrix(0,0,nb-1)
  penalties[[length(penalties)+1L]]<-smoothing*crossprod(penalty);lower<-c(lower,rep(if(mono=="none")-Inf else 0,nb-1));constrained<-constrained || mono!="none"
  smooths[[length(smooths)+1L]]<-list(feature=name,degree=degree,full_knots=as.list(knots),boundary_knots=as.list(boundary),interior_knots=as.list(interior),monotonicity=mono,extrapolation=extra,smoothing=smoothing,coefficient_transform=.cm_rows(transform))
 }
 if(anyDuplicated(names) || !setequal(names,feature_order) || length(names)!=length(feature_order)).cr_error("FEATURE_MISMATCH","Each feature must occur exactly once as linear or smooth")
 if(variant=="monotone" && !constrained).cr_error("INVALID_INPUT","Monotone variant requires genuinely constrained smooth")
 params<-list(kind="gam",variant=variant,linear_features=as.list(linear),smooths=smooths);d<-.cf_gam_design(x,params,feature_order);penalty<-matrix(0,ncol(d),ncol(d));offset<-1L+length(linear)
 for(p in penalties) {index<-seq.int(offset+1L,offset+nrow(p));penalty[index,index]<-p;offset<-offset+nrow(p)}
 if(qr(d)$rank<ncol(d) && qr(crossprod(d)+penalty)$rank<ncol(d)).cr_error("RANK_DEFICIENT","Spline likelihood plus penalty not identifiable")
 recession<-.cf_gam_recession(d,y,penalty,lower)
 objective<-function(beta) {eta<-as.vector(d%*%beta);mean(pmax(eta,0)+log1p(exp(-abs(eta)))-y*eta)+as.numeric(crossprod(beta,penalty%*%beta))/2}
 gradient<-function(beta)as.vector(crossprod(d,plogis(d%*%beta)-y)/length(y)+penalty%*%beta)
 fitted<-stats::optim(rep(0,ncol(d)),objective,gradient,method="L-BFGS-B",lower=lower,control=list(maxit=max_iter,pgtol=tolerance,factr=max(1,tolerance*1e-4/.Machine$double.eps)))
 beta<-fitted$par;grad<-gradient(beta);free<-!is.finite(lower) | beta>max(tolerance,1e-8);kkt<-ifelse(free,abs(grad),pmax(-grad,0));residual<-max(kkt)
 if(!is.finite(fitted$value) || residual>max(1e-6,sqrt(tolerance)) || fitted$convergence!=0).cr_error("NON_CONVERGENCE","GAM projected score/KKT residual exceeds tolerance",list(kkt_residual=residual))
 prob<-as.vector(plogis(d%*%beta));h<-crossprod(d,d*(prob*(1-prob)/length(y)));hf<-(h+penalty)[free,free,drop=FALSE]
 inverse<-tryCatch(solve(hf),error=function(e).cr_error("NON_IDENTIFIABLE","Active penalized information singular"));edf<-sum(diag(inverse%*%h[free,free,drop=FALSE]));params$coefficients<-as.list(unname(beta))
 diagnostics<-list(converged=TRUE,null_penalty_separation_objective=recession,null_penalty_rank_tolerance=1e-10,iterations=unname(fitted$counts[[1]]),iteration_measure="FUNCTION_EVALUATIONS",objective=fitted$value,kkt_residual=residual,effective_degrees_of_freedom=edf,active_constraint_count=sum(!free),smoothing_selection="CALLER_DECLARED_TRAINING_ONLY",uncertainty="NOT_ESTIMATED_CONSTRAINED_OR_SMOOTHING_SELECTION",training_only=TRUE)
 .cm_model("fit_gam_pd",feature_order,params,list(transformation="FROZEN_B_SPLINE_DESIGN"),diagnostics,target,training,model_id,model_version,context)
}
.cf_gam_raw <- function(model,X,feature_order,derivative_feature=NULL) {
 x<-.cr_glm_design(X,feature_order,TRUE)$x
 as.vector(.cf_gam_design(x,model$parameters,feature_order,derivative_feature)%*%unlist(model$parameters$coefficients))
}
.cf_forest_tree <- function(tree,x) {
 vapply(seq_len(nrow(x)),function(i) {node<-tree;while(is.null(node$leaf_probability))node<-if(x[i,node$feature_index+1L]<=node$threshold)node$left else node$right;node$leaf_probability},numeric(1))
}
fit_random_forest_pd <- function(X_train,y_train,feature_order,sampling_plan,max_depth,min_samples_leaf=1L,min_weight_leaf=0,criterion="gini",weights=NULL,target,training,model_id,model_version,context=NULL) {
 x<-.cr_glm_design(X_train,feature_order,FALSE)$x;y<-.cm_y(y_train,nrow(x))
 if(!criterion %in% c("gini","log_loss")).cr_error("INVALID_INPUT","criterion must be gini or log_loss")
 if(length(max_depth)!=1 || is.logical(max_depth) || max_depth!=floor(max_depth) || max_depth<1 || max_depth>20).cr_error("INVALID_INPUT","max_depth integer 1..20 required")
 if(length(min_samples_leaf)!=1 || is.logical(min_samples_leaf) || min_samples_leaf!=floor(min_samples_leaf) || min_samples_leaf<1).cr_error("INVALID_INPUT","Positive integer min_samples_leaf required")
 minw<-.cm_number(min_weight_leaf,"min_weight_leaf");w<-.cr_glm_vector(weights,nrow(x),"weights",1)
 if(any(w<0) || any(vapply(0:1,function(k)sum(w[y==k]),numeric(1))<=0)).cr_error("INVALID_WEIGHTS","Positive support in both classes required")
 if(!is.list(sampling_plan) || !length(sampling_plan)).cr_error("INVALID_INPUT","Nonempty explicit forest sampling plan required")
 impurity<-function(p)if(criterion=="gini")2*p*(1-p) else if(p>0 && p<1)-p*log(p)-(1-p)*log1p(-p) else 0
 trees<-list();used<-list();oob_total<-numeric(nrow(x));oob_count<-integer(nrow(x))
 for(draw in sampling_plan) {
  indices<-.cr_numeric(unlist(draw$sample_indices),"sample_indices",vector_only=TRUE)
  if(any(indices!=floor(indices)) || any(indices<0 | indices>=nrow(x))).cr_error("INVALID_SAMPLING_PLAN","Bootstrap indices zero-based training rows required")
  indices<-as.integer(indices)+1L;features<-draw$node_features
  if(!is.list(features) || sum(w[indices])<=0).cr_error("INVALID_SAMPLING_PLAN","Feature candidate draws and positive sampled weight required")
  visited<-.cr_empty_object()
  build<-function(rows,depth,path) {
   ww<-w[rows];total<-sum(ww);positive<-sum(ww*y[rows]);prob<-positive/total;leaf<-list(leaf_probability=prob,weighted_count=total,sample_count=length(rows),default_weight=positive)
   if(depth==max_depth || length(rows)<2*min_samples_leaf || prob %in% c(0,1))return(leaf)
   key<-if(nzchar(path))path else "root";candidates<-features[[key]]
   if(is.null(candidates)).cr_error("INVALID_SAMPLING_PLAN",paste("Missing feature candidates for",key))
   cand<-.cr_numeric(unlist(candidates),"node_features",vector_only=TRUE)
   if(any(cand!=floor(cand)) || any(cand<0 | cand>=ncol(x)) || anyDuplicated(cand)).cr_error("INVALID_SAMPLING_PLAN","Distinct valid zero-based feature candidates required")
   visited[[key]]<<-as.list(as.integer(cand));best<-NULL;bestgain<-0;parent<-impurity(prob)
   for(j in as.integer(cand)+1L) {
    values<-sort(unique(x[rows,j]));thresholds<-if(length(values)>1)head(values,-1)+(tail(values,-1)-head(values,-1))/2 else numeric()
    for(threshold in thresholds) {
     left<-x[rows,j]<=threshold;nl<-sum(left);nr<-length(rows)-nl
     if(min(nl,nr)<min_samples_leaf)next
     wl<-sum(ww[left]);wr<-total-wl;if(wl<=0 || wr<=0 || min(wl,wr)<minw)next
     pl<-sum(ww[left]*y[rows[left]])/wl;pr<-(positive-wl*pl)/wr;gain<-parent-(wl*impurity(pl)+wr*impurity(pr))/total
     if(gain>bestgain+1e-14) {bestgain<-gain;best<-list(j=j,threshold=threshold,left=left)}
    }
   }
   if(is.null(best))return(leaf)
   list(feature_index=best$j-1L,threshold=best$threshold,gain=bestgain,weighted_count=total,sample_count=length(rows),left=build(rows[best$left],depth+1L,paste0(path,"L")),right=build(rows[!best$left],depth+1L,paste0(path,"R")))
  }
  tree<-build(indices,0L,"");trees[[length(trees)+1L]]<-tree;used[[length(used)+1L]]<-list(sample_indices=as.list(indices-1L),node_features=visited);oob<-rep(TRUE,nrow(x));oob[indices]<-FALSE;oob_total[oob]<-oob_total[oob]+.cf_forest_tree(tree,x[oob,,drop=FALSE]);oob_count[oob]<-oob_count[oob]+1L
 }
 has<-oob_count>0;oob<-rep(NA_real_,nrow(x));oob[has]<-oob_total[has]/oob_count[has];valid<-has & w>0;brier<-if(any(valid))stats::weighted.mean((oob[valid]-y[valid])^2,w[valid]) else NULL
 diagnostics<-list(training_only=TRUE,tree_count=length(trees),sampling_plan=used,oob_counts=as.list(oob_count),oob_probabilities=lapply(oob,function(v)if(is.na(v))NULL else v),oob_brier=brier,oob_coverage=mean(has),oob_assumption="Rows independent; OOB is not a temporal or obligor-group holdout",calibration="UNCALIBRATED_EMPIRICAL_LEAF_PROBABILITIES",uncertainty="NOT_ESTIMATED",split_tie_policy="CANDIDATE_ORDER_THEN_INCREASING_THRESHOLD_GAIN_TOL_1E-14")
 .cm_model("fit_random_forest_pd",feature_order,list(kind="random_forest",trees=trees,aggregation="ARITHMETIC_MEAN_PROBABILITY",max_depth=max_depth,min_samples_leaf=min_samples_leaf,min_weight_leaf=minw,criterion=criterion),list(transformation="identity"),diagnostics,target,training,model_id,model_version,context)
}
.cf_forest_probability <- function(model,X,feature_order) {
 x<-.cr_glm_design(X,feature_order,FALSE)$x
 Reduce(`+`,lapply(model$parameters$trees,function(t).cf_forest_tree(t,x)))/length(model$parameters$trees)
}
.cf_float32 <- function(x) {if(any(abs(x)>3.402823466e38)).cr_error("NUMERICAL_FAILURE","Boosting inputs must be representable in float32");matrix(readBin(writeBin(as.numeric(x),raw(),size=4),numeric(),n=length(x),size=4),nrow=nrow(x),ncol=ncol(x))}
.cf_boost_tree <- function(node,row) {while(is.null(node$leaf_value))node<-if(row[node$feature_index+1L]<node$threshold)node$left else node$right;node$leaf_value}
.cf_boost_raw <- function(model,X,feature_order) {
 x<-.cf_float32(.cr_glm_design(X,feature_order,FALSE)$x);p<-model$parameters
 vapply(seq_len(nrow(x)),function(i)p$base_margin+sum(vapply(p$trees,.cf_boost_tree,numeric(1),row=x[i,])),numeric(1))
}
fit_gradient_boosting_pd <- function(X_train,y_train,feature_order,train_row_ids,iterations,max_depth,learning_rate,monotonic_constraints=NULL,l2_regularization=1,min_child_weight=1,min_split_loss=0,max_bin=256L,inner_validation=NULL,early_stopping_rounds=NULL,target,training,model_id,model_version,context=NULL) {
 x<-.cr_glm_design(X_train,feature_order,FALSE)$x;y<-.cm_y(y_train,nrow(x))
 for(name in c("iterations","max_depth","max_bin")) {v<-get(name);lo<-if(name=="max_bin")2 else 1;if(length(v)!=1 || is.logical(v) || !is.numeric(v) || v!=floor(v) || v<lo).cr_error("INVALID_INPUT",paste(name,"must be integer >=",lo))}
 rate<-.cm_number(learning_rate,"learning_rate",strict=TRUE);if(rate>1).cr_error("INVALID_INPUT","learning_rate must not exceed one")
 reg<-.cm_number(l2_regularization,"l2_regularization");mcw<-.cm_number(min_child_weight,"min_child_weight");gamma<-.cm_number(min_split_loss,"min_split_loss")
 ids<-as.character(unlist(train_row_ids));if(length(ids)!=nrow(x) || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)).cr_error("INVALID_SPLIT","Distinct nonempty training row_ids required")
 constraints<-if(is.null(monotonic_constraints))rep(0,ncol(x)) else .cr_numeric(unlist(monotonic_constraints),"monotonic_constraints",vector_only=TRUE)
 if(length(constraints)!=ncol(x) || any(!constraints %in% c(-1,0,1))).cr_error("INVALID_INPUT","One -1/0/1 monotonic constraint per feature required")
 if(!is.null(early_stopping_rounds) && (length(early_stopping_rounds)!=1 || is.logical(early_stopping_rounds) || early_stopping_rounds<1 || early_stopping_rounds!=floor(early_stopping_rounds))).cr_error("INVALID_INPUT","Early stopping rounds positive integer required")
 if(!is.null(early_stopping_rounds) && is.null(inner_validation)).cr_error("INVALID_SPLIT","Early stopping requires explicit inner validation")
 train<-xgboost::xgb.DMatrix(.cf_float32(x),label=y,nthread=1L);evals<-list(train=train);validation_contract<-NULL
 if(!is.null(inner_validation)) {
  val<-inner_validation;vx<-.cr_glm_design(val$X,feature_order,FALSE)$x;vy<-.cm_y(val$y,nrow(vx),require_both=FALSE);vid<-as.character(unlist(val$row_ids))
  if(length(vid)!=nrow(vx) || anyDuplicated(vid) || any(vid %in% ids) || anyNA(vid) || any(!nzchar(vid))).cr_error("INVALID_SPLIT","Inner validation IDs unique and disjoint required")
  if(!identical(val$role,"inner_validation") || is.null(val$split_id) || !nzchar(val$split_id) || identical(val$split_id,training$split_id)).cr_error("INVALID_SPLIT","Distinct explicit inner-validation role and split_id required")
  cutoff<-tryCatch(as.POSIXct(val$known_at,tz="UTC"),error=function(e)NA);traincut<-tryCatch(as.POSIXct(training$known_at,tz="UTC"),error=function(e)NA)
  if(length(cutoff)!=1 || length(traincut)!=1 || is.na(cutoff) || is.na(traincut)).cr_error("INVALID_SPLIT","Comparable ISO information cutoffs required")
  if(cutoff>traincut).cr_error("LOOKAHEAD","Inner validation labels cannot be known after training cutoff")
  evals$inner_validation<-xgboost::xgb.DMatrix(.cf_float32(vx),label=vy,nthread=1L);validation_contract<-val[c("row_ids","role","split_id","known_at")];validation_contract$row_ids<-as.list(vid)
 }
 base<-mean(y);params<-list(objective="binary:logistic",eval_metric="logloss",tree_method="hist",nthread=1L,max_depth=as.integer(max_depth),eta=rate,lambda=reg,min_child_weight=mcw,gamma=gamma,max_bin=as.integer(max_bin),base_score=base,subsample=1,colsample_bytree=1,monotone_constraints=paste0("(",paste(constraints,collapse=","),")"))
 # XGBoost 3 uses evals and stores callback results as attributes.
 modern_xgb <- "evals" %in% names(formals(xgboost::xgb.train))
 train_args <- list(params=params,data=train,nrounds=as.integer(iterations),early_stopping_rounds=early_stopping_rounds,verbose=0)
 train_args[[if(modern_xgb) "evals" else "watchlist"]] <- evals
 backend <- do.call(xgboost::xgb.train,train_args)
 selected <- if(is.null(early_stopping_rounds)) as.integer(iterations) else if(modern_xgb) as.integer(xgboost::xgb.attr(backend,"best_iteration"))+1L else backend$best_iteration
 evaluation_log <- if(modern_xgb) attr(backend,"evaluation_log",exact=TRUE) else backend$evaluation_log
 convert<-function(node) {
  if(!is.null(node$leaf))return(list(leaf_value=node$leaf))
  index<-as.integer(sub("^f","",node$split));children<-node$children;child<-function(id)children[[which(vapply(children,function(c)c$nodeid,numeric(1))==id)]]
  list(feature_index=index,threshold=node$split_condition,left=convert(child(node$yes)),right=convert(child(node$no)))
 }
 dumped<-jsonlite::fromJSON(xgboost::xgb.dump(backend,dump_format="json"),simplifyVector=FALSE);trees<-lapply(dumped[seq_len(selected)],convert)
 history<-list(train=list(logloss=as.list(evaluation_log$train_logloss)))
 if(!is.null(inner_validation))history$inner_validation<-list(logloss=as.list(evaluation_log$inner_validation_logloss))
 parameters<-list(kind="gradient_boosting",trees=trees,base_margin=qlogis(base),base_probability=base,learning_rate=rate,leaf_values_include_shrinkage=TRUE,input_cast="float32",split_rule="STRICT_LESS_THAN",monotonic_constraints=as.list(as.integer(constraints)),max_depth=as.integer(max_depth),l2_regularization=reg,min_child_weight=mcw,min_split_loss=gamma,max_bin=as.integer(max_bin))
 diagnostics<-list(training_only=TRUE,backend="XGBOOST_CPU_HIST",backend_version=as.character(utils::packageVersion("xgboost")),selected_iterations=selected,requested_iterations=as.integer(iterations),evaluated_iterations=nrow(evaluation_log),loss_path=history,inner_validation=validation_contract,early_stopping_rounds=early_stopping_rounds,uncertainty="NOT_ESTIMATED",calibration="BERNOULLI_MODEL_NOT_HOLDOUT_CALIBRATED",train_row_ids=as.list(ids))
 .cm_model("fit_gradient_boosting_pd",feature_order,parameters,list(transformation="FLOAT32_CAST"),diagnostics,target,training,model_id,model_version,context)
}
