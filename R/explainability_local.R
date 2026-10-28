.cr_local_constraints<-function(contract,features){
 if(!.cr_object(contract)) .cr_error('LOCAL_CONSTRAINTS','Versioned feasibility contract required')
 .cr_data_required(contract,c('contract_id','version','bounds','actionable_features','immutable_features','integer_features','one_hot_groups','linear_constraints','derived_relations'))
 for(k in c('contract_id','version')).cr_pfs_ids(list(contract[[k]]),k)
 subset<-function(k){v<-unlist(contract[[k]],use.names=FALSE);if(length(v)&&(!is.character(v)||anyDuplicated(v)||any(!v%in%features))) .cr_error('LOCAL_CONSTRAINTS',paste('Distinct known features required:',k));v}
 actionable<-subset('actionable_features');immutable<-subset('immutable_features');subset('integer_features')
 if(length(intersect(actionable,immutable))||!setequal(c(actionable,immutable),features)) .cr_error('LOCAL_CONSTRAINTS','Actionable and immutable features must partition model features')
 if(!.cr_object(contract$bounds)||!setequal(names(contract$bounds),features)) .cr_error('LOCAL_CONSTRAINTS','Finite declared bounds required for every feature')
 for(f in features){b<-.cr_numeric(unlist(contract$bounds[[f]]),'bounds',vector_only=TRUE);if(length(b)!=2||b[1]>b[2]) .cr_error('LOCAL_CONSTRAINTS','Ordered finite bound pair required')}
 seen<-ids<-character()
 for(g in contract$one_hot_groups){members<-.cr_pfs_ids(g,'one_hot_group');if(any(!members%in%features)||length(intersect(seen,members))) .cr_error('LOCAL_CONSTRAINTS','One-hot groups must be disjoint known features');seen<-c(seen,members)}
 for(rule in contract$linear_constraints){.cr_data_required(rule,c('constraint_id','coefficients','operator','rhs'));rid<-rule$constraint_id;.cr_pfs_ids(list(rid),'constraint_id')
  if(rid%in%ids||!rule$operator%in%c('<=','>=','==')||!.cr_object(rule$coefficients)||!length(rule$coefficients)||any(!names(rule$coefficients)%in%features)) .cr_error('LOCAL_CONSTRAINTS','Distinct identified linear rules on known features required');ids<-c(ids,rid)
  for(v in c(rule$coefficients,list(rule$rhs)))if(length(.cr_numeric(v,'linear scalar'))!=1) .cr_error('LOCAL_CONSTRAINTS','Scalar linear coefficients required')
 }
 for(rule in contract$derived_relations){.cr_data_required(rule,c('constraint_id','output_feature','input_features','operation'));rid<-rule$constraint_id;.cr_pfs_ids(list(rid),'constraint_id');members<-unlist(rule$input_features)
  if(rid%in%ids||!rule$output_feature%in%features||length(members)!=2||any(!members%in%features)||!rule$operation%in%c('product','ratio')) .cr_error('LOCAL_CONSTRAINTS','Distinct two-input product/ratio identity required');ids<-c(ids,rid)
 };contract
}
.cr_local_feasible<-function(x,baseline,features,contract){
 findings<-character();tol<-1e-10
 for(j in seq_along(features)){f<-features[j];b<-unlist(contract$bounds[[f]]);if(x[j]<b[1]||x[j]>b[2])findings<-c(findings,paste0('BOUND:',f));if(f%in%unlist(contract$immutable_features)&&x[j]!=baseline[j])findings<-c(findings,paste0('IMMUTABLE:',f));if(f%in%unlist(contract$integer_features)&&x[j]!=round(x[j]))findings<-c(findings,paste0('INTEGER:',f))}
 for(i in seq_along(contract$one_hot_groups)){v<-x[match(unlist(contract$one_hot_groups[[i]]),features)];if(any(v!=0&v!=1)||sum(v)!=1)findings<-c(findings,paste0('ONE_HOT:',i-1L))}
 for(rule in contract$linear_constraints){lhs<-sum(unlist(rule$coefficients)*x[match(names(rule$coefficients),features)]);rhs<-rule$rhs;eps<-tol*max(1,abs(lhs),abs(rhs));valid<-is.finite(lhs)&&switch(rule$operator,'<='=lhs<=rhs+eps,'>='=lhs>=rhs-eps,'=='=abs(lhs-rhs)<=eps);if(!valid)findings<-c(findings,paste0('LINEAR:',rule$constraint_id))}
 for(rule in contract$derived_relations){v<-x[match(unlist(rule$input_features),features)];expected<-if(rule$operation=='product')v[1]*v[2]else if(v[2]!=0)v[1]/v[2]else NaN;actual<-x[match(rule$output_feature,features)];if(!is.finite(expected)||abs(actual-expected)>tol*max(1,abs(actual),abs(expected)))findings<-c(findings,paste0('DERIVED:',rule$constraint_id))};sort(findings)
}
compute_credit_ice<-function(predictor,record,grid,model_contract,feasibility_contract,varying_features,thresholds=NULL,rating_scale=NULL,policy_evaluator=NULL,policy_contract=NULL,context=NULL){
 checked<-.cr_ex_contract(model_contract,context);features<-checked$features;ctx<-checked$context;base<-.cr_numeric(record,'record',vector_only=TRUE);vary<-.cr_pfs_ids(varying_features,'varying_features');values<-.cr_pfs_matrix(grid,'grid')
 if(length(base)!=length(features)||any(!vary%in%features)||ncol(values)!=length(vary)) .cr_error('LOCAL_SHAPE','Record and grid must match frozen feature and varying-feature orders')
 contract<-.cr_local_constraints(feasibility_contract,features);if(is.null(thresholds))thresholds<-list();seen<-character()
 for(t in thresholds){.cr_data_required(t,c('threshold_id','value','operator'));tid<-t$threshold_id;.cr_pfs_ids(list(tid),'threshold_id');if(tid%in%seen||!t$operator%in%c('<=','>=')||length(.cr_numeric(t$value,'threshold'))!=1) .cr_error('LOCAL_THRESHOLD','Distinct finite scalar prediction boundaries required');seen<-c(seen,tid)}
 rows<-matrix(rep(base,each=nrow(values)),nrow=nrow(values));rows[,match(vary,features)]<-values;findings<-lapply(seq_len(nrow(rows)),function(i).cr_local_feasible(rows[i,],base,features,contract));valid<-which(lengths(findings)==0);pred<-rep(list(NULL),nrow(rows))
 if(length(valid)){v<-.cr_ex_predict(predictor,rows[valid,,drop=FALSE],model_contract);for(j in seq_along(valid))pred[[valid[j]]]<-v[j]}
 base_findings<-.cr_local_feasible(base,base,features,contract);base_prediction<-if(length(base_findings))NULL else .cr_ex_predict(predictor,matrix(base,nrow=1),model_contract)[1]
 if(!is.null(rating_scale)&&model_contract$output_scale!='probability') .cr_error('LOCAL_GRADE','Rating scale requires probability outputs')
 grades<-lapply(pred,function(v)if(is.null(v)||is.null(rating_scale))NULL else map_pd_to_grade(v,rating_scale)$value);decisions<-rep(list(NULL),nrow(rows))
 if(!is.null(policy_evaluator)||!is.null(policy_contract)){
  if(!is.function(policy_evaluator)||!.cr_object(policy_contract)) .cr_error('LOCAL_POLICY','Frozen full-policy evaluator and contract required together')
  .cr_data_required(policy_contract,c('policy_hash','source_mode','fixed_source_hashes'))
  if(policy_contract$source_mode!='REPLAY_FULL_POLICY'||!is.list(policy_contract$fixed_source_hashes)) .cr_error('LOCAL_POLICY','Full policy replay with explicit fixed input hashes required')
  if(any(!names(policy_contract$fixed_source_hashes)%in%c('risk','affordability','eligibility','operations'))) .cr_error('LOCAL_POLICY','Unknown fixed policy source')
  for(i in valid){replayed<-tryCatch(policy_evaluator(as.list(rows[i,]),pred[[i]]),error=function(e){if(inherits(e,'creditratingsystem_error'))stop(e);.cr_error('LOCAL_POLICY','Full policy replay failed')})
   if(!.cr_object(replayed)) .cr_error('LOCAL_POLICY','Replay must return decision and explained_input_hash');.cr_data_required(replayed,c('decision','explained_input_hash'))
   if(!identical(replayed$explained_input_hash,.cr_hash(list(record=as.list(rows[i,]),prediction=pred[[i]])))) .cr_error('LOCAL_POLICY','Replay input binding does not match evaluated record and prediction')
   decision<-.cr_dec_checked(replayed$decision,'DecisionArtifact');if(!identical(decision$policy_hash,policy_contract$policy_hash)||!decision$action%in%c('APPROVE','DECLINE','REFER','BLOCKED')) .cr_error('LOCAL_POLICY','Replay must preserve policy hash and known action')
   for(source in names(policy_contract$fixed_source_hashes))if(!identical(decision$input_hashes[[source]],policy_contract$fixed_source_hashes[[source]])) .cr_error('LOCAL_POLICY','Replay changed an explicitly fixed source');decisions[[i]]<-decision
  }
 }
 states<-lapply(pred,function(v){if(is.null(v)||!length(thresholds))return(NULL);out<-structure(list(),names=character());for(t in thresholds)out[[t$threshold_id]]<-if(t$operator=='<=')v<=t$value else v>=t$value;out})
 segments<-list();directions<-list()
 if(nrow(rows)>1)for(i in 2:nrow(rows)){supported<-!is.null(pred[[i-1]])&&!is.null(pred[[i]]);delta<-if(supported)pred[[i]]-pred[[i-1]]else NULL;directions[length(directions)+1L]<-list(if(is.null(delta))NULL else sign(delta));crossings<-if(!supported)list()else as.list(vapply(Filter(function(t)!identical(states[[i-1]][[t$threshold_id]],states[[i]][[t$threshold_id]]),thresholds),function(t)t$threshold_id,''))
  segments[[length(segments)+1L]]<-list(from_index=i-2L,to_index=i-1L,status=if(supported)'EVALUATED'else'NON_ESTIMABLE',prediction_change=delta,threshold_crossings=crossings,grade_change=if(is.null(grades[[i-1]])||is.null(grades[[i]]))NULL else grades[[i-1]]!=grades[[i]],policy_action_change=if(is.null(decisions[[i-1]])||is.null(decisions[[i]]))NULL else decisions[[i-1]]$action!=decisions[[i]]$action)
 }
 turns<-list();last_sign<-NULL
 for(i in seq_along(directions)){sign<-directions[[i]];if(is.null(sign))last_sign<-NULL else if(sign!=0){if(!is.null(last_sign)&&sign!=last_sign)turns[[length(turns)+1L]]<-i-1L;last_sign<-sign}}
 .cr_result(list(feature_order=as.list(features),varying_features=as.list(vary),baseline_record=as.list(base),baseline_prediction=base_prediction,baseline_findings=as.list(base_findings),grid_values=.cr_pfs_rows(values),evaluated_records=.cr_pfs_rows(rows),predictions=pred,feasibility_findings=lapply(findings,as.list),threshold_states=states,segments=segments,turn_indices=turns,nonmonotone_on_evaluated_path=length(turns)>0,thresholds=thresholds,grades=grades,policy_decisions=decisions),'explainability.compute_credit_ice',model_contract$output_scale,ctx,diagnostics=list(model_contract_hash=.cr_hash(model_contract),feasibility_hash=.cr_hash(contract),rating_scale_hash=if(is.null(rating_scale))NULL else .cr_hash(rating_scale),policy_contract_hash=if(is.null(policy_contract))NULL else .cr_hash(policy_contract)),limitations='The supplied grid is an ordered finite path, not a continuous threshold search. Feasibility uses only declared constraints and is not empirical support or causal intervention validity. Scalar boundaries, explicit rating-scale mapping and sealed full-policy replay are separate. Replay input binding is a caller assertion, not independent authentication. Infeasible records are not evaluated; gaps are never interpolated.')
}
