library(creditratingsystem)
near_sb<-function(a,b,tol=1e-8)stopifnot(length(a)==length(b),all(abs(a-b)<=tol))
error_sb<-function(expr,code){e<-tryCatch(force(expr),error=identity);stopifnot(inherits(e,'creditratingsystem_error'),e$code==code)}
target_sb<-function(event='default',conditioning='POTENTIAL_OWN_CONTRACT')list(event=event,horizon=12,time_unit='months',probability_measure='PHYSICAL',conditioning=conditioning)
training_sb<-list(known_at='2025-12-31',split_id='train',data_fingerprint='fixture',data_origin='SYNTHETIC')
rows_sb<-function(n)lapply(seq_len(n)-1L,function(i)list(entity_id=paste0('e',i),as_of=if(i<n/2)'2020-01-01'else'2022-01-01',label_known_at=if(i<n/2)'2021-01-01'else'2023-01-01',features_known_at=if(i<n/2)'2019-12-31'else'2021-12-31'))
lineage_sb<-function(n)list(rows=rows_sb(n),folds=list(list(train=as.list(0:(n/2-1L)),validation=as.list((n/2):(n-1L)))),nuisance_prediction_origin='OUT_OF_FOLD')
apps_sb<-lapply(0:3,function(i)list(application_id=as.character(i),entity_id=as.character(i),submitted_at='2020-01-01'))
dec_sb<-lapply(0:3,function(i)list(application_id=as.character(i),as_of='2020-01-02',known_at='2020-01-02',decision=if(i==0)'rejected'else'offered',policy_version='p1'))
book_sb<-lapply(1:3,function(i)list(application_id=as.character(i),as_of='2020-01-03',known_at='2020-01-03',taken_up=i>1,funded=i>1))
out_sb<-list(list(application_id='3',as_of='2021-01-03',known_at='2021-01-04',outcome_scope='own_contract',status='observed',outcome=1,target=target_sb()))
panel_sb<-build_selection_dataset(apps_sb,dec_sb,book_sb,out_sb,'2021-02-01','all applicants',target_sb())$value
stopifnot(identical(vapply(panel_sb$rows,`[[`,character(1),'stratum'),c('rejected','offered_not_taken_up','funded_immature','funded_observed')),is.null(panel_sb$rows[[1]]$outcome))
ipw_sb<-selection_ipw_weights(c(1,1,0),matrix(c(.5,.5,1,.5,.1,.2),ncol=2,byrow=TRUE),stabilization=c(.5,.5))$value
near_sb(unlist(ipw_sb$weights),c(1,.5,0));near_sb(ipw_sb$effective_sample_size,1.8)
stopifnot(selection_ipw_weights(c(1,1,0),c(.25,.5,0),trim=c(.1,1))$value$estimand_changed)
error_sb(selection_ipw_weights(c(1,0),c(.5,0)),'POSITIVITY_FAILURE')
fuzzy_sb<-fuzzy_reject_augmentation(list(1,NULL),c(1,0),c(.2,.3),c('a','b'),2)$value
near_sb(vapply(fuzzy_sb$rows,`[[`,numeric(1),'weight'),c(1,1.4,.6));stopifnot(!fuzzy_sb$rows[[2]]$validation_eligible)
parcel_sb<-parcel_rejected_applications(c('a','b'),c(2,5),c(10,10),c(3,0),log(2),list(c(.1,.4,.9),numeric()))$value
near_sb(unlist(parcel_sb$assumed_reject_bad_rate),c(1/3,2/3));near_sb(unlist(parcel_sb$hypothetical_labels),c(1,0,0))
bounds_sb<-reject_inference_bounds(list(1,0,NULL,NULL),c(1,1,0,0),c(2,1,3,4))$value;near_sb(c(bounds_sb$lower,bounds_sb$upper),c(.2,.9))
restricted_sb<-reject_inference_bounds(list(1,0,NULL,NULL),c(1,1,0,0),c(2,1,3,4),c('a','a','a','b'),list(a=c(.2,.4),b=c(.5,.6)))$value;near_sb(c(restricted_sb$lower,restricted_sb$upper),c(.46,.56))
sensitivity_sb<-reject_inference_sensitivity(c(.9,.2,0,1),c(1,0,0,0),list(0,NULL,NULL,NULL),c(-1000,0,log(4),1000),threshold=.35)$value;near_sb(vapply(sensitivity_sb$surface,`[[`,numeric(1),'population_pd'),c(.25,.3,.375,.5));stopifnot(!sensitivity_sb$validation_eligible)
aipw_sb<-estimate_population_risk_aipw(list(0,1,0,1,1,NULL,0,NULL),c(1,1,1,1,1,0,1,0),rep(.25,8),rep(.5,8),lineage_sb(8),c(rep(0,4),rep(1,4)),c(rep('warm',4),'a','a','b','b'))$value
near_sb(unlist(aipw_sb$augmented_contributions)[5:8],c(1.75,.25,-.25,.25));near_sb(c(aipw_sb$population_pd,aipw_sb$standard_error),c(.5,.5))
propensity_sb<-fit_selection_propensity(matrix(numeric(),16,0),rep(c(0,0,0,1),4),character(),'funding',lineage_sb(16)$folds,rows_sb(16),target_sb('funding','eligible_offers'),training_sb,'prop','1')
stopifnot(all(vapply(propensity_sb$diagnostics$out_of_fold_propensity[1:8],is.null,logical(1))));near_sb(unlist(propensity_sb$diagnostics$out_of_fold_propensity),rep(.25,8));near_sb(unlist(propensity_sb$model$parameters$coefficients),log(1/3))
error_sb(fit_selection_propensity(matrix(rep(c(-1,1),8),ncol=1),rep(c(0,1),8),'policy_score','decision',lineage_sb(16)$folds,rows_sb(16),target_sb('decision','eligible'),training_sb,'p','1'),'SEPARATION')
# Independent conditional-normal integral, rather than a second copy of quadrature.
bvn_sb<-if(exists('.cr_sb_bvn',envir=.GlobalEnv,inherits=FALSE))get('.cr_sb_bvn',envir=.GlobalEnv)else getFromNamespace('.cr_sb_bvn','creditratingsystem')
for(rho in c(-.95,-.4,0,.6,.95)) {
 near_sb(bvn_sb(0,0,rho),.25+asin(rho)/(2*pi),2e-13)
 for(pair in list(c(-1,.3),c(.4,-.7),c(2,1))) {
  ref<-integrate(function(t)dnorm(t)*pnorm((pair[2]-rho*t)/sqrt(1-rho^2)),-Inf,pair[1],abs.tol=1e-12,rel.tol=1e-11)$value
  near_sb(bvn_sb(pair[1],pair[2],rho),ref,3e-12)
 }
}
x_sb<-matrix(numeric(),ncol=1);z_sb<-matrix(numeric(),ncol=1);s_sb<-numeric();y_sb<-list()
for(xx in c(-1,1))for(zz in c(-1,1))for(i in 0:19){selected<-if(zz<0)8 else 12;bad<-if(xx<0)2 else 6;x_sb<-rbind(x_sb,xx);z_sb<-rbind(z_sb,zz);s_sb<-c(s_sb,as.integer(i<selected));y_sb[length(y_sb)+1L]<-list(if(i<selected)as.integer(i<bad)else NULL)}
joint_sb<-fit_joint_selection_model(x_sb,z_sb,s_sb,y_sb,'x','instrument',target_sb(),training_sb,'joint','1',rho_grid=0,selected_rho=0)
near_sb(unlist(joint_sb$model$parameters$outcome_coefficients),c((qnorm(.2)+qnorm(.6))/2,(qnorm(.6)-qnorm(.2))/2),2e-7);near_sb(unlist(joint_sb$model$parameters$selection_coefficients),c(0,qnorm(.6)),2e-7)
candidates_sb<-list(list(model_id='m',model_hash='frozen',training_known_at='2020-01-01',training_entities=list('trained')))
eval_sb<-lapply(seq_len(3),function(i)list(application_id=as.character(i-1L),entity_id=as.character(i-1L),as_of='2021-01-01',known_at='2022-01-01',label_origin=c('PSEUDO','EXTERNAL_CONTRACT','OBSERVED_OWN_CONTRACT')[i],outcome=0,target=target_sb(),funded=TRUE,mature=TRUE))
evaluation_sb<-prepare_reject_evaluation(candidates_sb,eval_sb,'2023-01-01',target_sb())$value;stopifnot(identical(vapply(evaluation_sb$rows,`[[`,logical(1),'validation_eligible'),c(FALSE,FALSE,TRUE)))
if(identical(tolower(Sys.getenv('CREDITRATINGSYSTEM_EXTENDED_TESTS')),'true')) {
 profiles_sb<-fit_joint_selection_model(x_sb,z_sb,s_sb,y_sb,'x','instrument',target_sb(),training_sb,'joint','1',rho_grid=c(-.4,0,.4),selected_rho=0);stopifnot(length(profiles_sb$diagnostics$profiles)==3L);near_sb(profiles_sb$diagnostics$profiles[[2]]$model_population_pd,.4)
}
if(nzchar(Sys.getenv('CREDITRATING_SELECTION_REPORT'))) {
 report<-list(panel=panel_sb,ipw=ipw_sb,fuzzy=fuzzy_sb,parcel=parcel_sb,bounds=bounds_sb,restricted=restricted_sb,sensitivity=sensitivity_sb,aipw=aipw_sb,propensity=propensity_sb$model,joint=joint_sb$model,evaluation=evaluation_sb)
 writeLines(jsonlite::toJSON(report,auto_unbox=TRUE,digits=17,null='null'),Sys.getenv('CREDITRATING_SELECTION_REPORT'))
}
# Extended estimated-rho recovery uses counts from independent normal integration.
if(identical(tolower(Sys.getenv('CREDITRATINGSYSTEM_EXTENDED_TESTS')),'true')) {
 counts<-matrix(c(24,8,54,14,82,17,24,13,54,24,82,31,24,17,54,35,82,48),ncol=2,byrow=TRUE)
 x<-matrix(numeric(),ncol=1);z<-matrix(numeric(),ncol=1);s<-numeric();y<-list();cell<-0L
 for(xx in c(-1,0,1))for(zz in c(-1,0,1)){cell<-cell+1L;for(i in 0:99){x<-rbind(x,xx);z<-rbind(z,zz);s<-c(s,as.integer(i<counts[cell,1]));y[length(y)+1L]<-list(if(i<counts[cell,1])as.integer(i<counts[cell,2])else NULL)}}
 fitted<-fit_joint_selection_model(x,z,s,y,'x','instrument',target_sb(),training_sb,'joint','1',exclusion=list(features=list('instrument'),justification='Independent synthetic selection-only instrument.'))
 near_sb(fitted$model$parameters$rho,.35,.03);near_sb(unlist(fitted$model$parameters$outcome_coefficients),c(-.4,.5),.012);near_sb(unlist(fitted$model$parameters$selection_coefficients),c(.1,.8),.012);stopifnot(!fitted$diagnostics$weak_identification)
}
meta<-rows_sb(16);meta[[9]]$features_known_at<-'2023-01-01';error_sb(fit_selection_propensity(matrix(numeric(),16,0),rep(c(0,0,0,1),4),character(),'funding',lineage_sb(16)$folds,meta,target_sb('funding','eligible'),training_sb,'p','1'),'LEAKAGE')
error_sb(fit_joint_selection_model(x_sb,z_sb,s_sb,y_sb,'x','z',target_sb(),training_sb,'m','1'),'IDENTIFICATION_REQUIRED')
error_sb(estimate_population_risk_aipw(rep(0,8),rep(1,8),rep(.2,8),rep(1,8),lineage_sb(8)),'LEAKAGE')
# Local immutable artifact replay and integrity rejection.
predict_joint_sb<-if(exists('.cr_sb_predict_joint',envir=.GlobalEnv,inherits=FALSE))get('.cr_sb_predict_joint',envir=.GlobalEnv)else getFromNamespace('.cr_sb_predict_joint','creditratingsystem')
path_sb<-tempfile('joint-selection-',fileext='.json');serialize_fitted_model(joint_sb,path_sb);loaded_sb<-load_fitted_model(path_sb)$value;near_sb(predict_joint_sb(loaded_sb,matrix(c(-1,1,1,-1),ncol=2,byrow=TRUE),c('x','instrument'),target_sb())$value,c(.2,.6),1e-7)
envelope_sb<-jsonlite::fromJSON(path_sb,simplifyVector=FALSE);envelope_sb$model$parameters$rho<-.5;writeLines(jsonlite::toJSON(envelope_sb,auto_unbox=TRUE,digits=17,null='null'),path_sb);error_sb(load_fitted_model(path_sb),'INTEGRITY_ERROR')
# Exact probability-mass enumeration proves either-nuisance robustness of mean.
y_dr<-c(as.list(rep(0,6)),list(1,0,NULL,1,0,NULL));s_dr<-c(rep(1,6),1,1,0,1,1,0);w_dr<-c(rep(0,6),.1,.4,.5,.15,.1,.75)
estimate_dr<-function(m,e)estimate_population_risk_aipw(y_dr,s_dr,m,e,lineage_sb(12),w_dr)$value$population_pd
near_sb(estimate_dr(rep(.8,12),c(rep(.5,9),rep(.25,3))),.4);near_sb(estimate_dr(c(rep(.2,9),rep(.6,3)),rep(.8,12)),.4);near_sb(estimate_dr(rep(.8,12),rep(.8,12)),.58125)
near_sb(unlist(selection_ipw_weights(c(1,1),c(1,1),stabilization=1)$value$weights),c(1,1))
