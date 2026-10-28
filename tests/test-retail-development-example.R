if(!exists('fit_firth_logit'))library(creditratingsystem)
example_path<-system.file('examples','retail_development.R',package='creditratingsystem')
if(!nzchar(example_path))example_path<-'creditratingsystem-r/examples/retail_development.R'
source(example_path)
extended<-identical(Sys.getenv('CREDITRATINGSYSTEM_EXTENDED_TESTS'),'true')
v<-run_retail_development(include_holdout=extended);a<-v$selection_analysis;m<-if(extended)v$holdout$benchmark$metrics$by_model$scorecard else NULL
if(extended)stopifnot(abs(m$brier-3/16)<1e-12,abs(m$auc-.75)<1e-12,abs(m$log_loss+.25*log(.25)+.75*log(.75))<1e-12)
stopifnot(abs(a$ipw$effective_sample_size-64^2/(16*2^2+24*(4/3)^2))<1e-7,abs(a$aipw$population_pd-.5)<1e-8,a$bounds$lower==20/64,a$bounds$upper==44/64,a$overlap_failure=='POSITIVITY_FAILURE')
stopifnot(max(abs(unlist(v$models$scorecard$parameters$coefficients)-c(0,log(3))))<1e-8)
thin<-v$thin_file;slope<-2*log(21/61);stopifnot(max(abs(unlist(thin$model$parameters$coefficients)-c(-slope,slope)))<2e-7)
for(name in c('no_hit','stale')){c<-thin$cases[[name]];stopifnot(c$income$total_selected_income==100,c$cashflow_ratio==1,abs(c$raw_application_risk$pd-.5)<1e-8)}
stopifnot(is.null(thin$cases$outside_support$raw_application_risk))
p<-thin$hierarchical_calibrator$parameters;integral<-integrate(function(z)plogis(p$intercept+sqrt(p$variance)*z)*dnorm(z),-12,12,abs.tol=1e-12)$value
stopifnot(abs(thin$unknown_group_reference[[1]]-integral)<2e-7,thin$population_bounds$BUREAU_NO_HIT$lower==13/16,thin$population_bounds$BUREAU_STALE$lower==7/48,thin$population_bounds$BUREAU_STALE$upper==31/48)
rows<-v$prepared$rows;roles<-lapply(c('train','calibration','test'),function(role)Filter(function(r)r$role==role,rows))
stopifnot(identical(vapply(roles,length,integer(1)),c(128L,64L,64L)))
groups<-lapply(roles,function(rr)vapply(rr,function(r)r$entity_id,character(1)))
stopifnot(!length(intersect(groups[[1]],groups[[2]])),!length(intersect(groups[[1]],groups[[3]])),!length(intersect(groups[[2]],groups[[3]])))
for(r in rows){if(!r$selected)stopifnot(is.null(r$outcome));stopifnot(r$features_known_at<=r$as_of,r$snapshot$features$x!=99)}
p<-v$models$joint$parameters;q<-qnorm(.75)
stopifnot(max(abs(unlist(p$outcome_coefficients)-c(0,q)))<1e-7,max(abs(unlist(p$selection_coefficients)-c(q/2,q/2)))<1e-7,p$rho==0)
stopifnot(max(abs(unlist(v$selection_analysis$parcel$assumed_reject_bad_rate)-c(2/5,6/7)))<1e-12,!v$selection_analysis$parcel$validation_eligible)
hypothetical<-Filter(function(r)r$label_origin=='HYPOTHETICAL',v$selection_analysis$fuzzy$rows)
mass<-tapply(vapply(hypothetical,function(r)r$weight,numeric(1)),vapply(hypothetical,function(r)r$application_id,character(1)),sum)
stopifnot(length(mass)==48,max(abs(mass-1))<1e-12,all(vapply(hypothetical,function(r)!r$validation_eligible,logical(1))))
if(extended){pseudo<-Filter(function(r)r$label_origin=='PSEUDO',v$holdout$evaluation_roles$rows);stopifnot(length(pseudo)==1,!pseudo[[1]]$validation_eligible)}
