.cr_cf_target<-function(target){
 if(!.cr_object(target)) .cr_error('COUNTERFACTUAL_TARGET','Explicit versioned target required');.cr_data_required(target,c('target_id','version','kind'));.cr_pfs_ids(list(target$target_id),'target_id');.cr_pfs_ids(list(target$version),'target version')
 if(target$kind=='MODEL_OUTPUT'){
  .cr_data_required(target,c('operator','value'));v<-.cr_numeric(unlist(target$value),'target value')
  if(target$operator%in%c('<=','>=')){if(length(v)!=1) .cr_error('COUNTERFACTUAL_TARGET','Scalar boundary required')}else if(target$operator=='INTERVAL'){if(length(v)!=2||v[1]>v[2]) .cr_error('COUNTERFACTUAL_TARGET','Ordered closed interval required')}else .cr_error('COUNTERFACTUAL_TARGET','Supported scalar target operator required')
 }else if(target$kind=='POLICY_ACTION'){.cr_data_required(target,'actions');actions<-.cr_pfs_ids(target$actions,'actions');if(any(!actions%in%c('APPROVE','DECLINE','REFER','BLOCKED'))) .cr_error('COUNTERFACTUAL_TARGET','Known policy actions required')}else .cr_error('COUNTERFACTUAL_TARGET','Target is MODEL_OUTPUT or POLICY_ACTION');target
}
.cr_cf_success<-function(prediction,decision,target){
 if(target$kind=='POLICY_ACTION')return(!is.null(decision)&&decision$action%in%unlist(target$actions))
 if(is.null(prediction))return(FALSE)
 if(target$operator=='<=')return(prediction<=target$value)
 if(target$operator=='>=')return(prediction>=target$value)
 v<-unlist(target$value);prediction>=v[1]&&prediction<=v[2]
}
find_feasible_credit_counterfactual<-function(predictor,record,candidates,model_contract,feasibility_contract,cost_contract,target,candidate_ids,max_candidates=10000,policy_evaluator=NULL,policy_contract=NULL,context=NULL){
 checked<-.cr_ex_contract(model_contract,context);features<-checked$features;ctx<-checked$context;base<-.cr_numeric(record,'record',vector_only=TRUE);x<-.cr_pfs_matrix(candidates,'candidates');ids<-.cr_pfs_ids(candidate_ids,'candidate_ids');target<-.cr_cf_target(target)
 if(length(base)!=length(features)||!identical(dim(x),c(length(ids),length(features)))) .cr_error('COUNTERFACTUAL_SHAPE','One candidate row per ID in frozen feature order required')
 if(length(max_candidates)!=1||!is.numeric(max_candidates)||!is.finite(max_candidates)||max_candidates<1||max_candidates!=floor(max_candidates)) .cr_error('COUNTERFACTUAL_BUDGET','Positive integer candidate budget required')
 if(!.cr_object(cost_contract)) .cr_error('COUNTERFACTUAL_COST','Explicit cost contract required')
 .cr_data_required(cost_contract,c('cost_id','version','metric','feature_weights','feature_scales'));for(k in c('cost_id','version')).cr_pfs_ids(list(cost_contract[[k]]),k)
 for(k in c('feature_weights','feature_scales'))if(!.cr_object(cost_contract[[k]])||!setequal(names(cost_contract[[k]]),features)) .cr_error('COUNTERFACTUAL_COST','Cost weights/scales must cover every feature')
 w<-.cr_numeric(unlist(cost_contract$feature_weights[features]),'feature_weights',vector_only=TRUE);scale<-.cr_numeric(unlist(cost_contract$feature_scales[features]),'feature_scales',vector_only=TRUE)
 if(any(w<0)||!any(w>0)||any(scale<=0)) .cr_error('COUNTERFACTUAL_COST','Nonnegative nonzero cost weights and positive feature scales required')
 delta<-sweep(sweep(x,2,base,'-'),2,scale,'/');metric<-cost_contract$metric
 if(metric=='WEIGHTED_L1')costs<-rowSums(sweep(abs(delta),2,w,'*'))else if(metric=='WEIGHTED_L2')costs<-sqrt(rowSums(sweep(delta^2,2,w,'*')))else if(metric=='WEIGHTED_CHANGE_COUNT')costs<-rowSums(sweep(sweep(x,2,base,'!=')*1,2,w,'*'))else .cr_error('COUNTERFACTUAL_COST','Explicit weighted L1, L2 or change-count metric required')
 if(any(!is.finite(costs))) .cr_error('NUMERICAL_FAILURE','Candidate cost exceeds finite representation')
 order<-order(costs,ids);chosen<-head(order,max_candidates);remaining<-if(length(order)>max_candidates)order[seq.int(max_candidates+1,length(order))]else integer()
 if(target$kind=='POLICY_ACTION'&&is.null(policy_evaluator)) .cr_error('COUNTERFACTUAL_TARGET','Policy action target requires full policy replay')
 ice<-compute_credit_ice(predictor,base,x[chosen,,drop=FALSE],model_contract,feasibility_contract,varying_features=features,policy_evaluator=policy_evaluator,policy_contract=policy_contract,context=context)$value
 evaluated<-successful<-list()
 for(j in seq_along(chosen)){i<-chosen[j];feasible<-length(ice$feasibility_findings[[j]])==0;success<-feasible&&.cr_cf_success(ice$predictions[[j]],ice$policy_decisions[[j]],target);row<-list(candidate_id=ids[i],record=as.list(unname(x[i,])),cost=unname(costs[i]),changed_features=as.list(features[x[i,]!=base]),feasible=feasible,findings=ice$feasibility_findings[[j]],prediction=ice$predictions[[j]],policy_decision=ice$policy_decisions[[j]],target_satisfied=success);evaluated[[length(evaluated)+1L]]<-row;if(success)successful[[length(successful)+1L]]<-row}
 status<-if(length(remaining))'SEARCH_LIMIT'else if(length(successful))'FEASIBLE_SOLUTION'else'NO_FEASIBLE_SOLUTION'
 .cr_result(list(status=status,domain_complete=!length(remaining),optimality_scope=if(!length(remaining))'SUPPLIED_FINITE_DOMAIN'else'EVALUATED_SUBSET_ONLY',target=target,cost_contract=cost_contract,baseline_record=as.list(base),baseline_prediction=ice$baseline_prediction,baseline_findings=ice$baseline_findings,evaluated_candidates=evaluated,successful_candidates=successful,best_candidate=if(length(successful))successful[[1]]else NULL,unevaluated_candidate_ids=as.list(ids[remaining]),domain_size=length(ids),evaluated_count=length(chosen),search_budget=max_candidates),'explainability.find_feasible_credit_counterfactual','counterfactual',ctx,diagnostics=list(model_contract_hash=.cr_hash(model_contract),feasibility_hash=.cr_hash(feasibility_contract),domain_hash=.cr_hash(lapply(order(ids),function(i)list(candidate_id=ids[i],record=as.list(unname(x[i,]))))),cost_contract_hash=.cr_hash(cost_contract)),limitations='Optimality is limited to the explicitly supplied finite candidate domain and declared costs. SEARCH_LIMIT remains visible even when a feasible witness was found. Constraints and model or policy replay do not establish causal intervention success or promise credit approval. Every failed evaluated candidate remains in the report.')
}
