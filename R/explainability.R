.cr_ex_contract<-function(contract,context=NULL){
 if(!.cr_object(contract)) .cr_error('EXPLANATION_CONTRACT','Explicit frozen model contract required')
 .cr_data_required(contract,c('model_id','model_version','feature_order','output_scale','target','deterministic_frozen'));features<-.cr_pfs_ids(contract$feature_order,'feature_order')
 if(!contract$output_scale%in%c('probability','log_odds','score','currency')||!identical(contract$deterministic_frozen,TRUE)) .cr_error('EXPLANATION_CONTRACT','Declare output scale and deterministic frozen prediction')
 target<-contract$target;if(!.cr_object(target)) .cr_error('EXPLANATION_CONTRACT','Explicit prediction target required')
 .cr_data_required(target,c('event','horizon','time_unit','probability_measure','conditioning'));ctx<-.cr_result(NULL,'explainability.context',context=context)$context
 for(k in c('horizon','time_unit','probability_measure','conditioning'))if(!is.null(ctx[[k]])&&!isTRUE(ctx[[k]]==target[[k]])) .cr_error('TARGET_MISMATCH','Explanation target differs from context')
 list(features=features,context=ctx)
}
.cr_ex_predict<-function(predictor,x,contract){
 if(!is.function(predictor)) .cr_error('EXPLANATION_PREDICTOR','Frozen local prediction function required')
 raw<-tryCatch(predictor(x),error=function(e){if(inherits(e,'creditratingsystem_error'))stop(e);.cr_error('EXPLANATION_PREDICTOR','Prediction failed')});value<-if(inherits(raw,'CalcResult'))raw$value else raw;y<-.cr_numeric(value,'predictions',vector_only=TRUE)
 if(length(y)!=nrow(x)) .cr_error('EXPLANATION_PREDICTION','One finite prediction per input row required')
 if(contract$output_scale=='probability'&&any(y<0|y>1)) .cr_error('EXPLANATION_PREDICTION','Probability outputs must be in [0,1]');y
}
.cr_ex_groups<-function(groups,features){
 if(!.cr_object(groups)||!length(groups)) .cr_error('EXPLANATION_GROUPS','Explicit nonoverlapping feature groups required')
 ids<-sort(names(groups));indices<-lapply(ids,function(g){members<-.cr_pfs_ids(groups[[g]],'group features');if(any(!members%in%features)) .cr_error('EXPLANATION_GROUPS','Unknown grouped feature');match(members,features)})
 flat<-unlist(indices);if(anyDuplicated(flat)||!setequal(flat,seq_along(features))) .cr_error('EXPLANATION_GROUPS','Every feature belongs to exactly one group');list(ids=ids,indices=indices)
}
explain_linear_credit_prediction<-function(linear_model,transformed_inputs,baseline,feature_groups,offset=NULL,baseline_offset=0,context=NULL){
 checked<-.cr_ex_contract(linear_model,context);features<-checked$features;ctx<-checked$context;.cr_data_required(linear_model,c('coefficients','intercept','link','transformed_feature_lineage'))
 x<-.cr_pfs_matrix(transformed_inputs,'transformed_inputs');base<-.cr_numeric(unlist(baseline),'baseline',vector_only=TRUE);beta<-.cr_numeric(unlist(linear_model$coefficients),'coefficients',vector_only=TRUE);intercept<-.cr_numeric(linear_model$intercept,'intercept')
 if(ncol(x)!=length(features)||length(base)!=length(features)||length(beta)!=length(base)||length(intercept)!=1L) .cr_error('EXPLANATION_SHAPE','Transformed matrix, baseline and coefficients must match feature order')
 lineage<-linear_model$transformed_feature_lineage;if(!.cr_object(lineage)||!setequal(names(lineage),features)||any(!vapply(lineage,function(v)is.list(v)&&length(v)>0,logical(1)))) .cr_error('EXPLANATION_LINEAGE','Every transformed feature needs explicit raw-source lineage')
 off<-if(is.null(offset))rep(0,nrow(x))else .cr_numeric(offset,'offset',vector_only=TRUE);bo<-.cr_numeric(baseline_offset,'baseline_offset');if(length(off)!=nrow(x)||length(bo)!=1L) .cr_error('EXPLANATION_SHAPE','Row offsets and scalar baseline offset required')
 groups<-.cr_ex_groups(feature_groups,features);contributions<-sweep(sweep(x,2,base,'-'),2,beta,'*');grouped<-do.call(cbind,lapply(groups$indices,function(idx)rowSums(contributions[,idx,drop=FALSE])));baseline_eta<-as.numeric(intercept+sum(base*beta)+bo);eta<-as.numeric(intercept+x%*%beta+off);offset_contribution<-off-bo
 if(any(!is.finite(eta))||any(!is.finite(contributions))||!is.finite(baseline_eta)) .cr_error('NUMERICAL_FAILURE','Linear explanation exceeds finite range')
 link<-linear_model$link
 response_fun<-switch(link,identity=function(z)z,logit=stats::plogis,probit=stats::pnorm,cloglog=function(z)-expm1(-exp(pmin(z,709))),NULL)
 if(is.null(response_fun)) .cr_error('EXPLANATION_LINK','Supported identity/logit/probit/cloglog link required')
 response<-response_fun(eta);reference<-response_fun(baseline_eta)
 if(link!='identity'&&linear_model$output_scale!='probability') .cr_error('EXPLANATION_CONTRACT','Binary probability link requires probability output scale')
 if(linear_model$output_scale=='probability'&&(any(response<0|response>1)||reference<0||reference>1)) .cr_error('EXPLANATION_PREDICTION','Probability outputs and baseline must be in [0,1]')
 residual<-eta-(baseline_eta+rowSums(contributions)+offset_contribution)
 .cr_result(list(feature_order=as.list(features),group_ids=as.list(groups$ids),baseline=as.list(base),baseline_linear_predictor=baseline_eta,linear_predictor=as.list(eta),feature_contributions=.cr_pfs_rows(contributions),group_contributions=.cr_pfs_rows(grouped),offset_contributions=as.list(offset_contribution),response=as.list(response),baseline_response=reference,response_difference=as.list(response-reference),reconstruction_residual=as.list(residual),contribution_scale='LINEAR_PREDICTOR',response_scale=linear_model$output_scale,link=link,transformed_feature_lineage=lineage),'explainability.explain_linear_credit_prediction','linear_predictor',ctx,diagnostics=list(model_hash=.cr_hash(linear_model),input_hash=.cr_hash(.cr_pfs_rows(x))),limitations='Contributions are additive on the linear predictor scale. Logit/probit/cloglog contributions are not additive probability-point changes. Baseline and transformed-feature lineage are explicit supplied references, not causal interventions.')
}
estimate_credit_shapley_values<-function(predictor,record,background,model_contract,feature_groups,masking_contract,method='exact',permutations=NULL,max_coalitions=4096,context=NULL){
 checked<-.cr_ex_contract(model_contract,context);features<-checked$features;ctx<-checked$context;x<-.cr_numeric(record,'record',vector_only=TRUE);if(length(x)!=length(features)) .cr_error('EXPLANATION_SHAPE','One record in frozen feature order required')
 grouped<-.cr_ex_groups(feature_groups,features);groups<-grouped$ids;indices<-grouped$indices;m<-length(groups)
 if(!.cr_object(masking_contract)) .cr_error('EXPLANATION_MASK','Explicit masking contract required')
 .cr_data_required(masking_contract,c('masking','reference_id','reference_version'));maskkind<-masking_contract$masking
 if(!maskkind%in%c('INTERVENTIONAL_EMPIRICAL','SUPPLIED_CONDITIONAL_DISTRIBUTIONS')) .cr_error('EXPLANATION_MASK','Interventional and supplied conditional masking must be distinguished')
 if(length(max_coalitions)!=1L||!is.numeric(max_coalitions)||!is.finite(max_coalitions)||max_coalitions<1||max_coalitions!=floor(max_coalitions)) .cr_error('EXPLANATION_BUDGET','Positive explicit coalition budget required')
 cache<-new.env(parent=emptyenv());evaluated<-0L
 game<-function(mask){
  key<-if(any(mask))paste(which(mask)-1L,collapse=',')else'EMPTY'
  if(exists(key,envir=cache,inherits=FALSE))return(get(key,envir=cache,inherits=FALSE))
  if(length(ls(cache))>=max_coalitions) .cr_error('EXPLANATION_BUDGET','Coalition budget exceeded')
  source<-if(maskkind=='INTERVENTIONAL_EMPIRICAL')background else masking_contract$coalition_backgrounds[[key]]
  if(!.cr_object(source)) .cr_error('EXPLANATION_MASK','Each requested reference distribution needs values and weights')
  .cr_data_required(source,c('values','weights'));z<-.cr_pfs_matrix(source$values,'background values');if(ncol(z)!=length(features)) .cr_error('EXPLANATION_SHAPE','Background matrix must match feature order');w<-.cr_pf_weights(unlist(source$weights),nrow(z))
  for(j in which(mask)){idx<-indices[[j]];expected<-matrix(rep(x[idx],each=nrow(z)),nrow=nrow(z));if(maskkind=='SUPPLIED_CONDITIONAL_DISTRIBUTIONS'&&any(z[,idx,drop=FALSE]!=expected)) .cr_error('EXPLANATION_MASK','Conditional background must preserve every observed coalition feature');z[,idx]<-expected}
  value<-.cr_pfs_sum(w*.cr_ex_predict(predictor,z,model_contract));assign(key,value,envir=cache);evaluated<<-evaluated+nrow(z);value
 }
 baseline<-game(rep(FALSE,m));prediction<-.cr_ex_predict(predictor,matrix(x,nrow=1),model_contract)[1];endpoint<-game(rep(TRUE,m))
 if(abs(prediction-endpoint)>max(1e-14,1e-12*max(abs(prediction),abs(endpoint)))) .cr_error('EXPLANATION_PREDICTOR','Frozen predictor gives inconsistent full-coalition prediction')
 se<-draw_contributions<-NULL
 if(method=='exact'){
  if(!is.null(permutations)||m>log2(max_coalitions)) .cr_error('EXPLANATION_BUDGET','Exact enumeration requires every coalition within budget and no permutation plan')
  phi<-numeric(m)
  for(j in seq_len(m)){
   terms<-numeric();others<-setdiff(seq_len(m),j)
   for(code in seq_len(2^length(others))-1){mask<-rep(FALSE,m);if(length(others))mask[others]<-(floor(code/2^(seq_along(others)-1))%%2)==1;nextmask<-mask;nextmask[j]<-TRUE;terms<-c(terms,(game(nextmask)-game(mask))/(m*choose(m-1,sum(mask))))};phi[j]<-.cr_pfs_sum(terms)
  }
 }else if(method=='permutation'){
  if(!is.list(permutations)||!length(permutations)) .cr_error('EXPLANATION_PERMUTATIONS','Explicit shared permutation paths required')
  draws<-matrix(0,nrow=length(permutations),ncol=m)
  for(i in seq_along(permutations)){order<-unlist(permutations[[i]]);if(length(order)!=m||!setequal(order,groups)) .cr_error('EXPLANATION_PERMUTATIONS','Every permutation must contain each group exactly once');mask<-rep(FALSE,m);previous<-baseline
   for(gid in order){j<-match(gid,groups);mask[j]<-TRUE;current<-game(mask);draws[i,j]<-current-previous;previous<-current}
  }
  phi<-colMeans(draws);draw_contributions<-.cr_pfs_rows(draws);design<-masking_contract$permutation_design
  if(is.null(design)||!design%in%c('IID_RANDOM_PERMUTATIONS','FIXED_PERMUTATIONS')) .cr_error('EXPLANATION_PERMUTATIONS','Declare IID random or fixed permutation design')
  if(design=='IID_RANDOM_PERMUTATIONS'&&nrow(draws)>1)se<-as.list(apply(draws,2,stats::sd)/sqrt(nrow(draws)))
 }else .cr_error('EXPLANATION_METHOD','Method must be exact or permutation')
 residual<-prediction-baseline-.cr_pfs_sum(phi);if(abs(residual)>1e-10*max(1,abs(prediction),abs(baseline))) .cr_error('NUMERICAL_FAILURE','Shapley local-accuracy identity failed')
 keys<-sort(ls(cache));.cr_result(list(group_ids=as.list(groups),values=as.list(phi),baseline=baseline,prediction=prediction,local_accuracy_residual=residual,output_scale=model_contract$output_scale,masking=maskkind,method=method,permutation_standard_error=se,permutation_contributions=draw_contributions,coalition_values=lapply(keys,function(k)list(coalition_key=k,value=get(k,envir=cache))),evaluated_coalitions=length(keys),evaluated_background_rows=evaluated),'explainability.estimate_credit_shapley_values',model_contract$output_scale,ctx,diagnostics=list(model_contract_hash=.cr_hash(model_contract),reference_hash=.cr_hash(list(background=background,masking_contract=masking_contract))),limitations='Attributions explain the frozen masking game, not causal changes or credit approval. Interventional background breaks feature dependence; conditional distributions must be supplied and validated independently. Supplied fixed permutations have no IID uncertainty estimate. Grouping changes the game and must preserve transformed-feature meaning.')
}
