.cr_m_satellite_design <- function(target_series,macro_history,feature_spec,split_plan,training) {
 if(!is.list(target_series)||!length(target_series)) .cr_error('SATELLITE_DATA','Dated target records required')
 ids<-.cr_m_ids(lapply(target_series,`[[`,'observation_id'),'observation_ids');rows<-stats::setNames(target_series,ids)
 if(!is.list(split_plan)||length(split_plan$mode)!=1L||!split_plan$mode %in% c('TEMPORAL','ENTITY_TIME')||!identical(split_plan$split_id,training$split_id)) .cr_error('LEAKAGE','Matching temporal SplitPlan required')
 roles<-lapply(c('train_ids','calibration_ids','validation_ids','test_ids'),function(v)unlist(split_plan[[v]]));names(roles)<-c('train_ids','calibration_ids','validation_ids','test_ids');assigned<-unlist(roles)
 if(anyDuplicated(assigned)||!setequal(assigned,ids)||!length(roles$train_ids)||!length(unlist(roles[-1L]))) .cr_error('LEAKAGE','Exhaustive disjoint training and later holdout IDs required')
 cutoff<-.cr_date(training$known_at,'known_at');start<-.cr_date(training$start,'start');end<-.cr_date(training$end,'end')
 if(!identical(split_plan$training_cutoff,cutoff)||start>end||end>cutoff) .cr_error('LEAKAGE','Training cutoff/window must agree')
 byrole<-list()
 for(role in names(roles)){
  keys<-roles[[role]];dates<-vapply(keys,function(v).cr_date(rows[[v]]$date,'date'),character(1));entities<-vapply(keys,function(v){e<-rows[[v]]$entity_id;if(!is.character(e)||length(e)!=1L||!nzchar(e)) .cr_error('INVALID_IDENTIFIERS','Target entity IDs required');e},character(1))
  if(length(dates))byrole[[length(byrole)+1L]]<-list(first=min(dates),last=max(dates),entities=unique(entities))
  if(role=='train_ids'&&any(dates<start|dates>end)) .cr_error('LEAKAGE','Training row outside declared window')
 }
 if(length(byrole)>1L&&byrole[[2L]]$first<=cutoff) .cr_error('LEAKAGE','Holdout must follow model information cutoff')
 if(length(byrole)>1L)for(i in 2:length(byrole))if(byrole[[i-1L]]$last>=byrole[[i]]$first) .cr_error('LEAKAGE','Split roles must be strictly time ordered')
 if(split_plan$mode=='ENTITY_TIME'&&length(byrole)>1L)for(i in seq_len(length(byrole)-1L))for(j in (i+1L):length(byrole))if(length(intersect(byrole[[i]]$entities,byrole[[j]]$entities))) .cr_error('LEAKAGE','Entity-time roles need disjoint entities')
 h<-.cr_m_unwrap(macro_history);if(!is.null(h$fit_window)){if(.cr_date(h$fit_window$known_at,'transform_known_at')>cutoff||.cr_date(h$fit_window$end,'transform_end')>end) .cr_error('LEAKAGE','Macro transformation fitted beyond satellite training window')}
 dates<-.cr_m_dates(h$dates,h$frequency);factors<-.cr_m_ids(h$variable_ids,'variable_ids');x<-.cr_m_matrix(h$values,'macro_values');units<-unlist(h$units);releases<-h$release_dates
 if(!identical(dim(x),c(length(dates),length(factors)))||length(units)!=length(factors)||!is.list(releases)||length(releases)!=length(dates)) .cr_error('SATELLITE_DATA','Aligned macro values units/releases required')
 if(!is.list(feature_spec)||!length(feature_spec)) .cr_error('SATELLITE_SPEC','Explicit lag feature specification required')
 feature_names<-.cr_m_ids(lapply(feature_spec,`[[`,'feature_id'),'feature_ids');spec<-feature_spec
 for(j in seq_along(spec)){
  v<-spec[[j]];v$lag<-.cr_m_integer(v$lag,'lag',0);name<-v$variable_id
  if(length(name)!=1L||!name %in% factors||!identical(v$unit,units[match(name,factors)])||length(v$expected_sign)!=1L||!v$expected_sign %in% c('positive','negative','unrestricted')) .cr_error('SATELLITE_SPEC','Factor unit and sign hypothesis required')
  spec[[j]]<-v
 }
 train_rows<-unname(rows[roles$train_ids]);ord<-order(vapply(train_rows,`[[`,character(1),'date'),vapply(train_rows,`[[`,character(1),'entity_id'),vapply(train_rows,`[[`,character(1),'observation_id'));train_rows<-train_rows[ord];seen<-character();design<-matrix(0,length(train_rows),length(spec));outcomes<-numeric(length(train_rows));lineage<-list()
 for(i in seq_along(train_rows)){
  row<-train_rows[[i]];key<-paste(row$entity_id,row$date,sep='\r');if(key %in% seen) .cr_error('DUPLICATE_RECORD','One target per entity/date required');seen<-c(seen,key)
  known<-.cr_date(row$known_at,'target_known_at');info<-.cr_date(row$information_date,'information_date')
  if(known<row$date||known>cutoff||info>row$date||info>cutoff) .cr_error('LEAKAGE','Target maturity or information violates cutoff')
  t<-match(row$date,dates);if(is.na(t)) .cr_error('SATELLITE_DATA','Target period absent from macro calendar')
  for(j in seq_along(spec)){
   v<-spec[[j]];ix<-t-v$lag;factor<-match(v$variable_id,factors);if(ix<1L) .cr_error('INSUFFICIENT_HISTORY','Lag requires earlier observations')
   release<-.cr_date(releases[[ix]][[factor]],'release_date');if(release<dates[ix]||release>info) .cr_error('LEAKAGE','Macro vintage not released at target information date')
   design[i,j]<-x[ix,factor];lineage[[length(lineage)+1L]]<-list(observation_id=row$observation_id,feature_id=v$feature_id,macro_date=dates[ix],release_date=release)
  }
  val<-.cr_numeric(row$value,'training_targets');if(length(val)!=1L) .cr_error('SATELLITE_DATA','Scalar target required');outcomes[i]<-val
 }
 list(x=design,y=outcomes,rows=train_rows,h=h,spec=spec,names=feature_names,lineage=lineage)
}
fit_satellite_model <- function(target_series,macro_history,feature_spec,estimator,link,split_plan,training,target,target_unit,model_id,model_version,weights=NULL,covariance='classical',hac_lags=0,residual_process='white_noise',context=NULL) {
 dat<-.cr_m_satellite_design(target_series,macro_history,feature_spec,split_plan,training);x<-dat$x;y<-dat$y;rows<-dat$rows;h<-dat$h;spec<-dat$spec;feature_names<-dat$names;n<-nrow(x);d<-ncol(x);design<-cbind(1,x);dimension<-d+1L
 if(!is.character(target_unit)||length(target_unit)!=1L||!nzchar(target_unit)||!estimator %in% c('OLS','WLS','GLM')||!covariance %in% c('classical','HC1','HAC','cluster')||!residual_process %in% c('none','white_noise','AR1')) .cr_error('SATELLITE_SPEC','Explicit estimator unit covariance/residual process required')
 if(n<=dimension||qr(design)$rank<dimension) .cr_error('RANK_DEFICIENT','Training design needs full rank and positive residual degrees of freedom')
 if(is.null(weights))w<-rep(1,n)else{
  keys<-vapply(rows,`[[`,character(1),'observation_id');if(!is.list(weights)||!setequal(names(weights),keys)) .cr_error('INVALID_WEIGHTS','Weights must map training observation IDs exactly');w<-.cr_numeric(unlist(weights[keys]),'weights',vector_only=TRUE)
 }
 if(any(w<=0)||(estimator=='OLS'&&any(w!=1))) .cr_error('INVALID_WEIGHTS','Positive WLS precision or unit OLS weights required')
 transformed<-y;binary<-estimator=='GLM'
 if(binary){
  if(!link %in% c('logit','probit','cloglog')||residual_process!='none'||covariance!='classical') .cr_error('SATELLITE_SPEC','GLM requires explicit link Fisher covariance and no Gaussian residual process')
  glm_training<-training;glm_training$data_fingerprint<-.cr_hash(list(x=.cr_m_rows(x),y=as.list(y),w=as.list(w)))
  glm<-fit_binary_glm(x,y,link=link,feature_order=feature_names,fit_intercept=TRUE,target=target,training=glm_training,model_id=model_id,model_version=model_version,weights=w)
  coefficients<-unlist(glm$model$parameters$coefficients);cv<-.cr_m_matrix(glm$model$parameters$covariance,'covariance');backend<-'stats glm Bernoulli';residual<-rep(0,n);phi<-0;sigma<-0;pairs<-0
 }else{
  if(!link %in% c('identity','log')) .cr_error('SATELLITE_SPEC','Continuous satellites require identity/log transform')
  if(link=='log'){if(any(y<=0)) .cr_error('SATELLITE_DOMAIN','Log response requires positive outcomes');transformed<-log(y)}
  fitted<-stats::lm.wfit(design,transformed,w);coefficients<-unname(fitted$coefficients);residual<-as.vector(transformed-design%*%coefficients);bread<-solve(crossprod(design*sqrt(w)));scores<-design*(w*residual)
  if(covariance=='classical')cv<-bread*sum(w*residual^2)/(n-dimension)else{
   if(covariance=='HC1')meat<-crossprod(scores)*n/(n-dimension)else if(covariance=='cluster'){
    entities<-sort(unique(vapply(rows,`[[`,character(1),'entity_id')));g<-length(entities);if(g<2L) .cr_error('INSUFFICIENT_CLUSTERS','Cluster covariance needs at least two entities')
    summed<-do.call(rbind,lapply(entities,function(v)colSums(scores[vapply(rows,function(r)r$entity_id==v,logical(1)),,drop=FALSE])));meat<-crossprod(summed)*g/(g-1)*(n-1)/(n-dimension)
   }else{
    length<-.cr_m_integer(hac_lags,'hac_lags',0);if(length(unique(vapply(rows,`[[`,character(1),'entity_id')))!=1L||length>=n) .cr_error('SATELLITE_SPEC','HAC requires one contiguous entity and lag below sample size')
    .cr_m_dates(lapply(rows,`[[`,'date'),h$frequency);meat<-crossprod(scores)
    if(length>0L)for(lag in seq_len(length)){cross<-crossprod(scores[(lag+1L):n,,drop=FALSE],scores[seq_len(n-lag),,drop=FALSE]);meat<-meat+(1-lag/(length+1))*(cross+t(cross))}
   }
   cv<-bread%*%meat%*%bread
  }
  phi<-0;pairs<-0;errors<-residual
  if(residual_process=='AR1'){
   pair_list<-matrix(integer(),0,2);frequency<-switch(h$frequency,monthly=1L,quarterly=3L,annual=12L)
   for(entity in sort(unique(vapply(rows,`[[`,character(1),'entity_id')))){
    ix<-which(vapply(rows,function(r)r$entity_id==entity,logical(1)));if(length(ix)>1L)for(t in 2:length(ix)){
     a<-ix[t-1L];b<-ix[t];da<-rows[[a]]$date;db<-rows[[b]]$date;gap<-(as.integer(substr(db,1,4))-as.integer(substr(da,1,4)))*12+as.integer(substr(db,6,7))-as.integer(substr(da,6,7));if(gap==frequency)pair_list<-rbind(pair_list,c(a,b))
    }
   }
   if(nrow(pair_list)<3L) .cr_error('INSUFFICIENT_HISTORY','Residual AR1 needs three within-entity consecutive pairs')
   previous<-residual[pair_list[,1]];current<-residual[pair_list[,2]];den<-sum(previous^2);if(den<=0) .cr_error('NON_IDENTIFIABLE','Residual AR1 variance zero');phi<-sum(previous*current)/den
   if(abs(phi)>=1) .cr_error('NONSTATIONARY_RESIDUAL','Estimated residual AR1 not stationary')
   errors<-current-phi*previous;pairs<-nrow(pair_list)
  }
  sigma<-if(residual_process!='none')sqrt(mean(errors^2))else 0;backend<-'stats lm.wfit'
 }
 train<-training;train$data_fingerprint<-.cr_hash(list(rows=lapply(rows,function(r)r[c('observation_id','date','entity_id','known_at','information_date')]),x=.cr_m_rows(x),y=as.list(y),weights=as.list(w)))
 violations<-list();for(j in seq_along(spec))if((spec[[j]]$expected_sign=='positive'&&coefficients[j+1L]<0)||(spec[[j]]$expected_sign=='negative'&&coefficients[j+1L]>0))violations[[length(violations)+1L]]<-spec[[j]]$feature_id
 parameters<-list(dynamics='satellite',estimator=estimator,link=link,coefficients=as.list(coefficients),covariance=.cr_m_rows(cv),residual_process=residual_process,residual_ar=phi,residual_sigma=sigma,target_unit=target_unit)
 prep<-list(feature_spec=spec,macro_factor_ids=h$variable_ids,macro_units=h$units,frequency=h$frequency,target_transform=link,application_modes=list('absolute','baseline_delta'))
 diagnostics<-list(backend=backend,training_only=TRUE,training_rows=n,training_ids=lapply(rows,`[[`,'observation_id'),split_plan_hash=.cr_hash(split_plan),coefficient_covariance=covariance,hac_lags=if(covariance=='HAC')hac_lags else NULL,residual_ar_pairs=pairs,residuals=as.list(residual),feature_lineage=dat$lineage,sign_hypothesis_violations=violations,sign_constraints_enforced=FALSE,log_inverse_interpretation=if(link=='log')'CONDITIONAL_MEDIAN_WITHOUT_STOCHASTIC_DRAWS'else NULL,holdout_used_for_estimation=FALSE)
 model<-.cr_model(list(schema_version='1.0',method_id='macro.fit_satellite_model',model_id=model_id,model_version=model_version,feature_order=as.list(feature_names),parameters=parameters,preprocessing=prep,target=.cr_cal_target(target),training=train,diagnostics=diagnostics,provenance=list(data_origin=if(is.null(training$data_origin))'USER_SUPPLIED'else training$data_origin,parameter_origin='ESTIMATED')));.cr_m_measure(model,context)
 .cr_fit_result(model,'macro.fit_satellite_model',diagnostics,context)
}
apply_satellite_model <- function(fitted_satellite_model,scenario_cube,idiosyncratic_innovations,initial_target_state,context=NULL) {
 model<-.cr_model(fitted_satellite_model);.cr_m_measure(model,context);cube<-.cr_m_validate_cube(scenario_cube);p<-model$parameters;prep<-model$preprocessing
 if(model$method_id!='macro.fit_satellite_model'||cube$probability_measure!=model$target$probability_measure) .cr_error('TARGET_MISMATCH','Compatible frozen satellite and macro measures required')
 if(!identical(cube$factor_ids,prep$macro_factor_ids)||!identical(cube$units,prep$macro_units)||!identical(cube$frequency,prep$frequency)) .cr_error('FEATURE_MISMATCH','Macro factor order units/frequency differ')
 if(!is.list(initial_target_state)||!identical(initial_target_state$as_of,cube$origin)) .cr_error('INVALID_DATE','Target and macro origins must agree')
 if(cube$origin<model$training$end) .cr_error('LEAKAGE','Satellite training ends after requested macro origin')
 knowledge<-.cr_date(initial_target_state$known_at,'known_at');if(knowledge<cube$information_cutoff||knowledge<model$training$known_at) .cr_error('LEAKAGE','Application precedes fitted model or macro information')
 paths<-.cr_m_cube_values(cube$values,'values');s<-dim(paths)[1];h<-dim(paths)[2];k<-dim(paths)[3];innov<-.cr_m_unwrap(idiosyncratic_innovations)
 if(!is.list(innov)||!identical(innov$scenario_ids,cube$scenario_ids)||!identical(unlist(innov$time_grid),unlist(cube$time_grid))||!identical(innov$kind,'STANDARD_NORMAL')) .cr_error('MACRO_INNOVATIONS','Idiosyncratic draws must name shared scenario/time axes')
 z<-.cr_m_matrix(innov$values,'idiosyncratic_innovations');if(!identical(dim(z),c(s,h))) .cr_error('INCOMPATIBLE_SHAPE','Idiosyncratic draws require scenario by time axes')
 specs<-prep$feature_spec;maxlag<-max(vapply(specs,function(v)as.integer(v$lag),integer(1)));history<-if(maxlag).cr_m_matrix(initial_target_state$macro_history,'macro_history')else matrix(numeric(),0,k)
 if(!identical(dim(history),c(maxlag,k))) .cr_error('INSUFFICIENT_HISTORY','Initial macro history must cover maximum lag ending at cube origin')
 features<-array(0,c(s,h,length(specs)))
 for(i in seq_len(s)){full<-rbind(history,matrix(paths[i,,],nrow=h,ncol=k));for(j in seq_along(specs)){v<-specs[[j]];features[i,,j]<-full[maxlag-v$lag+seq_len(h),match(v$variable_id,unlist(cube$factor_ids))]}}
 coefficients<-.cr_numeric(unlist(p$coefficients),'coefficients',vector_only=TRUE);mode<-initial_target_state$mode;base<-matrix(coefficients[1],s,h);drivers<-features
 if(identical(mode,'baseline_delta')){
  baseline<-.cr_numeric(unlist(initial_target_state$baseline_link_path),'baseline_link_path',vector_only=TRUE);reference<-.cr_m_matrix(initial_target_state$reference_feature_path,'reference_feature_path')
  if(length(baseline)!=h||!identical(dim(reference),c(h,length(specs)))) .cr_error('INCOMPATIBLE_SHAPE','Baseline/reference feature paths must match forecast axes')
  base<-matrix(baseline,s,h,byrow=TRUE);for(i in seq_len(s))drivers[i,,]<-matrix(features[i,,],nrow=h,ncol=length(specs))-reference
 }else if(!identical(mode,'absolute')) .cr_error('SATELLITE_SPEC','Application mode absolute/baseline_delta required')
 macro<-matrix(0,s,h);for(i in seq_len(s))macro[i,]<-as.vector(matrix(drivers[i,,],nrow=h,ncol=length(specs))%*%coefficients[-1L])
 state<-.cr_numeric(initial_target_state$residual_state,'residual_state');if(length(state)!=1L) .cr_error('INCOMPATIBLE_SHAPE','Explicit scalar initial residual required');state<-rep(state,s);idio<-matrix(0,s,h)
 if(p$residual_process=='none'&&any(state!=0)) .cr_error('SATELLITE_SPEC','No residual process implies zero initial residual')
 for(t in seq_len(h)){state<-p$residual_ar*state+p$residual_sigma*z[,t];idio[,t]<-state}
 eta<-base+macro+idio;link<-p$link;out<-switch(link,identity=eta,log=exp(eta),logit=stats::plogis(eta),probit=stats::pnorm(eta),cloglog=-expm1(-exp(eta)))
 if(is.null(out)||any(!is.finite(out))) .cr_error('NUMERICAL_FAILURE','Satellite inverse link invalid')
 provenance<-model$provenance;provenance$data_origin<-if(identical(cube$data_origin,'SYNTHETIC')||identical(model$provenance$data_origin,'SYNTHETIC'))'SYNTHETIC'else 'USER_SUPPLIED'
 result<-cube;result$data_origin<-provenance$data_origin;result$values<-.cr_m_array(array(out,c(s,h,1L)));result$factor_ids<-list(model$target$event);result$units<-list(p$target_unit);result$model_hash<-.cr_hash(model);result$source_macro_hash<-.cr_hash(cube);result$scenario_origin<-'DERIVED_SATELLITE';result$macro_sampling_contract<-cube$sampling_contract
 .cr_result(result,'macro.apply_satellite_model','scenario_cube',context,diagnostics=list(application_mode=mode,baseline_link=.cr_m_rows(base),macro_link=.cr_m_rows(macro),idiosyncratic_link=.cr_m_rows(idio),linear_predictor=.cr_m_rows(eta),link=link,source_scenario_ids=cube$scenario_ids,shared_macro_across_entities=TRUE),provenance=provenance)
}
