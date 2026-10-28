library(creditratingsystem)
near_cm<-function(a,b,tol=1e-8) stopifnot(length(a)==length(b),all(abs(a-b)<=tol))
error_cm<-function(expr,code=NULL){e<-tryCatch(force(expr),error=identity);stopifnot(inherits(e,"creditratingsystem_error"));if(!is.null(code)) stopifnot(e$code==code)}
meta_cm<-list(target=list(event="first_default",horizon=12,time_unit="months",probability_measure="PHYSICAL",conditioning="PIT"),calibration=list(start="2014-01-01",end="2014-12-31",known_at="2015-12-31",split_id="calibration",prediction_origin="HOLDOUT"),base_model_version="fixed-v1",population_definition="Independent synthetic algebra",outcome_policy="Mature binary event",weight_type="frequency",data_origin="SYNTHETIC")
apply_cm<-function(a,p,groups=NULL) apply_calibrator(a,p,meta_cm$target,"fixed-v1",groups=groups)$value
p<-rep(c(.1,.2,.4,.7,.9),each=2);y<-rep(c(1,0),5);w<-c(1,9,2,8,4,6,7,3,9,1)
beta_cm<-calibrate_beta(p,y,w,metadata=meta_cm);near_cm(unlist(beta_cm$value$parameters[c("intercept","a","b")]),c(0,1,1));near_cm(apply_cm(beta_cm,c(.01,.5,.99)),c(.01,.5,.99))
restored<-jsonlite::fromJSON(jsonlite::toJSON(beta_cm$value,auto_unbox=TRUE,digits=17,null="null"),simplifyVector=FALSE);near_cm(apply_cm(restored,.2),.2);restored$parameters$a<-2;error_cm(apply_cm(restored,.2),"INTEGRITY_ERROR")
rev_cm<-calibrate_beta(rep(c(0,.2,.5,.8,1),each=2),y,rev(w),metadata=meta_cm,boundary_policy="clip");stopifnot(all(diff(apply_cm(rev_cm,seq(0,1,.1)))>= -1e-12))
grade_cm<-calibrate_grade_pd(c(100,100,100,20),c(1,4,2,20),grades=c("A","B","C","D"),default_grade="D")$value;near_cm(vapply(grade_cm$grades,function(r) r$pd,numeric(1)),c(.01,.03,.03,1));near_cm(grade_cm$grades[[2]]$upper,qbeta(.975,7,194))
grade_target_cm<-calibrate_grade_pd(c(10,20,30),c(0,1,5),prior_alpha=1,prior_beta=1,target_default_rate=.2)$value;near_cm(grade_target_cm$achieved_mean_pd,.2)
ind_cm<-low_default_bound(0,100)$value;near_cm(ind_cm$bounds[[1]]$upper_pd,1-.05^.01,1e-12);near_cm(low_default_bound(3,100)$value$bounds[[1]]$upper_pd,qbeta(.95,4,97),1e-12)
corr_cm<-low_default_bound(0,100,dependence="gaussian_one_factor",correlation=.1)$value;stopifnot(corr_cm$bounds[[1]]$upper_pd>ind_cm$bounds[[1]]$upper_pd)
for(mode in c("additive","multiplicative","logit")){x<-if(mode=="additive") c(-.2,.5,1.2) else if(mode=="multiplicative") c(.1,.5,1.2) else c(.1,.4,.8);target<-switch(mode,additive=.7,multiplicative=.8,logit=.6);a<-calibrate_lgd_level(x,target,c(1,2,3),metadata=meta_cm,transformation=mode);near_cm(sum(apply_cm(a,x)*c(1,2,3))/6,target,1e-10)}
hierarchy_cm<-fit_hierarchical_calibration(rep(.2,6),rep(c(1,0),3),rep(c("A","B","C"),each=2),c(1,19,8,12,16,4),metadata=meta_cm);hp<-hierarchy_cm$value$parameters;stopifnot(hp$variance>0,all(diff(vapply(hp$groups,function(r) r$mode,numeric(1)))>0));oracle<-integrate(function(z) plogis(qlogis(.3)+hp$intercept+sqrt(hp$variance)*z)*dnorm(z),-10,10,abs.tol=1e-11)$value;near_cm(apply_cm(hierarchy_cm,.3,"NEW"),oracle,2e-7)
components_cm<-list(list(source_id="a",category="data",adverse_bias=.01,standard_error=.02),list(source_id="b",category="general",adverse_bias=0,standard_error=.03));moc_cm<-estimate_margin_of_conservatism(.1,components_cm,correlation=matrix(c(1,.5,.5,1),2))$value;near_cm(moc_cm$margin,.01+qnorm(.95)*sqrt(.02^2+.03^2+.02*.03));stopifnot(estimate_margin_of_conservatism(.1,components_cm,already_included=c("a","b"))$value$margin==0);error_cm(estimate_margin_of_conservatism(.1,c(components_cm,components_cm[1])),"DUPLICATE_UNCERTAINTY")
weights_cm<-population_reweighting(c("a","a","a","b"),list(a=.5,b=.5))$value;near_cm(unlist(weights_cm$weights),c(2/3,2/3,2/3,2));near_cm(weights_cm$effective_sample_size,3);error_cm(population_reweighting("a",list(a=.5,missing=.5)),"NO_COMMON_SUPPORT")
# Independent repeated-binomial cells, not random outcomes; explicit dated split.
x_cm<-matrix(numeric(),ncol=2);y_cm<-numeric();rows_cm<-list()
for(block in 0:3) for(level in 1:5) for(j in 0:19){x_cm<-rbind(x_cm,c(level-3,(-1)^j));y_cm<-c(y_cm,as.numeric(j<c(2,5,10,15,18)[level]));i<-length(y_cm)-1L;year<-2010+2*block;rows_cm[[i+1L]]<-list(entity_id=paste0("e",i),as_of=paste0(year,"-01-01"),label_known_at=paste0(year+1L,"-01-01"),role=if(block<2) "development" else if(block==2) "calibration" else "test")}
plan_cm<-list(rows=rows_cm,folds=list(list(train=as.list(0:99),validation=as.list(100:199))));features_cm<-c("signal","noise");step_results_cm<-list();extended_cm<-identical(tolower(Sys.getenv("CREDITRATINGSYSTEM_EXTENDED_TESTS")),"true")
for(direction in c("forward","backward","bidirectional")) for(criterion in c("AIC","BIC","LR")) for(estimator in c("OLS","WLS","GLM")) {
 if(!extended_cm&&!((direction=="forward"&&criterion=="AIC"&&estimator=="GLM")||(direction=="backward"&&criterion=="BIC"&&estimator=="WLS"))) next
 x<-x_cm[1:100,];y<-if(estimator=="GLM") y_cm[1:100] else 2+3*x[,1]+.1*x[,2]+sin(0:99);w<-if(estimator=="WLS") 1+(0:99)%%3 else NULL
 a<-fit_stepwise(x,y,features_cm,list(signal="signal",noise="noise"),direction=direction,criterion=tolower(criterion),estimator=tolower(estimator),weights=w)$value;stopifnot("signal" %in% unlist(a$selected_groups),a$selected_model$row_count==100);m<-a$selected_model
 if(estimator!="GLM"){D<-cbind(1,x[,match(unlist(m$feature_order),features_cm),drop=FALSE]);ww<-if(is.null(w)) rep(1,100) else w;b<-drop(solve(crossprod(D,D*ww),crossprod(D,ww*y)));near_cm(c(m$intercept,unlist(m$coefficients)),b,1e-10);ll<--50*(log(2*pi)+1+log(sum(ww*(y-D%*%b)^2)/100))+.5*sum(log(ww));near_cm(m$log_likelihood,ll)}
 if(criterion!="LR") near_cm(a$path[[length(a$path)]]$criterion_value,-2*m$log_likelihood+(if(criterion=="AIC") 2 else log(100))*m$parameter_count)
 step_results_cm[[paste(direction,criterion,estimator,sep="_")]]<-a
}
paths_cm<-list()
for(alpha in if(extended_cm) c(0,.5,1) else 0){a<-fit_regularization_path(x_cm,y_cm,features_cm,c(0,.1,1),plan_cm,elastic_net_alpha=alpha)$value;altered<-x_cm;altered[351,]<-c(1e12,-1e12);yy<-y_cm;yy[351]<-1-yy[351];b<-fit_regularization_path(altered,yy,features_cm,c(0,.1,1),plan_cm,elastic_net_alpha=alpha)$value;stopifnot(a$selected_model$artifact_hash==b$selected_model$artifact_hash);near_cm(unlist(a$path[[1]]$folds[[1]]$training_center),c(0,0));paths_cm[[length(paths_cm)+1L]]<-a}
bad<-plan_cm;bad$rows[[101]]$entity_id<-bad$rows[[1]]$entity_id;error_cm(fit_regularization_path(x_cm,y_cm,features_cm,.1,bad),"LEAKAGE")
x<-matrix(0:5,ncol=1);y<-c(0,1,1,4,3,6);ix<-list(as.list(0:5),as.list(c(0,1,0,1,4,5)),as.list(c(2,3,2,3,4,5)));units<-rep(c("A","B","C"),each=2)
resample_cm<-resample_fit_uncertainty(x,y,"x",ix,units,"cluster",estimator="OLS")$value;stopifnot(resample_cm$effective_replicates==3);slopes<-vapply(ix,function(idx){idx<-unlist(idx)+1L;coef(lm(y[idx]~x[idx,1]))[2]},numeric(1));near_cm(unlist(resample_cm$coefficient_intervals[[2]]),unname(quantile(slopes,c(.025,.975))))
error_cm(resample_fit_uncertainty(x,y,"x",list(as.list(c(0,1,2))),units,"cluster",estimator="OLS"),"INVALID_RESAMPLING")
candidates_cm<-list(list(id="signal",features=list("signal"),penalty_grid=list(0,.1)),list(id="bad",features=list("missing")))
protocol_cm<-fit_model_selection_protocol(x_cm,y_cm,features_cm,candidates_cm,plan_cm,meta_cm,2)$value;yy<-y_cm;yy[301:400]<-1-yy[301:400];b<-fit_model_selection_protocol(x_cm,yy,features_cm,candidates_cm,plan_cm,meta_cm,2)$value;stopifnot(protocol_cm$selected_model$artifact_hash==b$selected_model$artifact_hash,protocol_cm$calibration_artifact$artifact_hash==b$calibration_artifact$artifact_hash,!is.null(protocol_cm$candidate_evidence[[2]]$failure),is.null(protocol_cm$test_metrics))
if(nzchar(Sys.getenv("CREDITRATING_CALIBRATION_MODELS_REPORT"))) {
 lgd_cm<-list(additive=calibrate_lgd_level(c(-.2,.5,1.2),.7,c(1,2,3),metadata=meta_cm)$value,multiplicative=calibrate_lgd_level(c(.1,.5,1.2),.8,c(1,2,3),metadata=meta_cm,transformation="multiplicative")$value,logit=calibrate_lgd_level(c(.1,.4,.8),.6,c(1,2,3),metadata=meta_cm,transformation="logit")$value)
 report<-list(lgd=lgd_cm,beta=beta_cm$value,grade=grade_cm,grade_target=grade_target_cm,independent=ind_cm,correlated=corr_cm,hierarchical=hierarchy_cm$value,moc=moc_cm,weights=weights_cm,stepwise=step_results_cm,paths=paths_cm,resample=resample_cm,protocol=protocol_cm)
 writeLines(jsonlite::toJSON(report,auto_unbox=TRUE,digits=17,null="null"),Sys.getenv("CREDITRATING_CALIBRATION_MODELS_REPORT"))
}

# Common public fitted-model dispatch and serialization, with no test fitting.
frozen_cm<-protocol_cm$selected_frozen_model
xp_cm<-matrix(c(-1,1),ncol=1);score_cm<-predict_pd(frozen_cm,xp_cm,feature_order="signal",expected_target=meta_cm$target)$value
sm_cm<-protocol_cm$selected_model;expected_cm<-plogis(sm_cm$intercept+(xp_cm[,1]-unlist(sm_cm$center))/unlist(sm_cm$scale)*unlist(sm_cm$coefficients));near_cm(score_cm,expected_cm,1e-12)
path_cm<-tempfile("selection-model-",fileext=".json");serialize_fitted_model(frozen_cm,path_cm);loaded_cm<-load_fitted_model(path_cm)$value;near_cm(predict_pd(loaded_cm,xp_cm,feature_order="signal",expected_target=meta_cm$target)$value,score_cm,1e-12)
bad_cm<-frozen_cm;bad_cm$parameters$selection_model$intercept<-bad_cm$parameters$selection_model$intercept+1;error_cm(predict_pd(bad_cm,xp_cm,feature_order="signal",expected_target=meta_cm$target),"INTEGRITY_ERROR")
error_cm(predict_pd(frozen_cm,xp_cm,feature_order="signal",expected_target=meta_cm$target,offset=c(0,0)),"OFFSET_MISMATCH")
exposure_meta_cm<-meta_cm;exposure_meta_cm$weight_type<-"exposure";exposure_cm<-calibrate_lgd_level(c(.1,.9),.6,c(.25,.75),metadata=exposure_meta_cm);near_cm(sum(apply_cm(exposure_cm,c(.1,.9))*c(.25,.75)),.6)
near_cm(calibrate_grade_pd(c(100,100),c(10,30),target_weights=c(.25,.75),target_default_rate=.4)$value$achieved_mean_pd,.4)

error_cm(calibrate_beta(c(.1,.2,.7,.9),c(0,0,1,1),metadata=meta_cm),"SEPARATION")
