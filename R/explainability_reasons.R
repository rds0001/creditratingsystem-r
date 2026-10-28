rank_credit_reason_codes<-function(contributions,reference_basis,reason_dictionary,policy_decision=NULL,max_reasons=NULL,context=NULL){
 rows<-.cr_data_rows(contributions,'contributions');basis<-reference_basis;ctx<-.cr_result(NULL,'explainability.context',context=context)$context
 if(!.cr_object(basis)||!.cr_object(reason_dictionary)) .cr_error('REASON_CONTRACT','Versioned reference and dictionary objects required')
 .cr_data_required(basis,c('attribution_id','model_id','model_version','reference_id','explained_quantity','output_scale','baseline','prediction','adverse_direction'));.cr_data_required(reason_dictionary,c('dictionary_id','version','model_reasons','policy_reasons'))
 for(k in c('attribution_id','model_id','model_version','reference_id','explained_quantity','output_scale')).cr_pfs_ids(list(basis[[k]]),k)
 for(k in c('dictionary_id','version')).cr_pfs_ids(list(reason_dictionary[[k]]),k)
 if(!basis$output_scale%in%c('probability','log_odds','score','currency','linear_predictor')||!basis$adverse_direction%in%c('HIGHER','LOWER')) .cr_error('REASON_CONTRACT','Explicit common contribution scale and adverse direction required')
 baseline<-.cr_numeric(basis$baseline,'baseline');prediction<-.cr_numeric(basis$prediction,'prediction');if(length(baseline)!=1||length(prediction)!=1) .cr_error('REASON_CONTRACT','Scalar baseline and prediction required')
 if(basis$output_scale=='probability'&&(baseline<0||baseline>1||prediction<0||prediction>1)) .cr_error('REASON_CONTRACT','Probability reference values must lie in [0,1]')
 values<-list()
 for(row in rows){.cr_data_required(row,c('feature_id','contribution'));fid<-row$feature_id;.cr_pfs_ids(list(fid),'feature_id');v<-.cr_numeric(row$contribution,'contribution');if(fid%in%names(values)||length(v)!=1) .cr_error('REASON_DUPLICATE','One scalar stored contribution per transformed feature required');values[[fid]]<-v}
 residual<-prediction-baseline-.cr_pfs_sum(unlist(values));if(abs(residual)>1e-10*max(1,abs(prediction),abs(baseline))) .cr_error('REASON_RECONSTRUCTION','Stored contributions must reconstruct the declared prediction scale')
 if(!is.null(max_reasons)&&(length(max_reasons)!=1||!is.numeric(max_reasons)||!is.finite(max_reasons)||max_reasons<1||max_reasons!=floor(max_reasons))) .cr_error('REASON_LIMIT','Positive integer reason limit required')
 codes<-assigned<-character();ranked<-suppressed<-list();direction<-if(basis$adverse_direction=='HIGHER')1 else -1
 check_entry<-function(entry){.cr_data_required(entry,c('reason_code','description','priority'));code<-entry$reason_code;.cr_pfs_ids(list(code),'reason_code');.cr_pfs_ids(list(entry$description),'description');p<-entry$priority
 if(code%in%codes||length(p)!=1||!is.numeric(p)||!is.finite(p)||p<0||p!=floor(p)) .cr_error('REASON_DICTIONARY','Distinct codes and nonnegative integer priorities required');codes<<-c(codes,code);code}
 for(entry in .cr_data_rows(reason_dictionary$model_reasons,'model_reasons')){
  code<-check_entry(entry);.cr_data_required(entry,'feature_ids');features<-.cr_pfs_ids(entry$feature_ids,'feature_ids')
  if(any(!features%in%names(values))||any(features%in%assigned)) .cr_error('REASON_GROUPS','Model reason groups must use known features without duplication')
  assigned<-c(assigned,features);net<-.cr_pfs_sum(unlist(values[features]));adverse<-direction*net;features<-sort(features)
  row<-list(reason_code=code,description=entry$description,priority=entry$priority,feature_ids=as.list(features),contribution=net,adverse_contribution=adverse,source_contributions=lapply(features,function(f)list(feature_id=f,contribution=values[[f]])))
  if(adverse>0)ranked[[length(ranked)+1L]]<-row else{row$suppression<-'NON_ADVERSE_GROUP_NET';suppressed[[length(suppressed)+1L]]<-row}
 }
 if(length(ranked))ranked<-ranked[order(-vapply(ranked,function(v)v$adverse_contribution,0.),vapply(ranked,function(v)as.numeric(v$priority),0.),vapply(ranked,function(v)v$reason_code,''))]
 omitted<-list();if(!is.null(max_reasons)&&length(ranked)>max_reasons){omitted<-ranked[seq.int(max_reasons+1,length(ranked))];ranked<-ranked[seq_len(max_reasons)]}
 pdict<-list();for(entry in .cr_data_rows(reason_dictionary$policy_reasons,'policy_reasons')){code<-check_entry(entry);.cr_data_required(entry,'rule_ids');.cr_pfs_ids(entry$rule_ids,'rule_ids');pdict[[code]]<-entry}
 policy<-findings<-list();decision_hash<-action<-NULL
 if(!is.null(policy_decision)){
  decision<-.cr_dec_checked(policy_decision,'DecisionArtifact');decision_hash<-decision$sha256;action<-decision$action;findings<-decision$findings;seen<-character();aggregated<-list()
  for(trace in decision$rule_trace){if(!identical(trace$matched,TRUE))next;code<-trace$reason_code;rule<-trace$rule_id
   if(rule%in%seen) .cr_error('REASON_POLICY','Policy trace contains a duplicate matched rule');seen<-c(seen,rule)
   if(!code%in%names(pdict)||!rule%in%unlist(pdict[[code]]$rule_ids)) .cr_error('REASON_POLICY','Matched rule requires an explicitly bound dictionary reason')
   entry<-pdict[[code]];if(is.null(aggregated[[code]]))aggregated[[code]]<-list(reason_code=code,description=entry$description,priority=entry$priority,rules=list());aggregated[[code]]$rules[[length(aggregated[[code]]$rules)+1L]]<-trace
  }
  policy<-unname(aggregated);if(length(policy))policy<-policy[order(vapply(policy,function(v)as.numeric(v$priority),0.),vapply(policy,function(v)v$reason_code,''))]
  policy<-lapply(policy,function(row){row$rules<-row$rules[order(vapply(row$rules,function(v)v$rule_id,''))];row})
 }
 absolute<-.cr_pfs_sum(abs(unlist(values)));mapped<-.cr_pfs_sum(abs(unlist(values[assigned])));if(length(suppressed))suppressed<-suppressed[order(vapply(suppressed,function(v)v$reason_code,''))]
 unmapped<-sort(setdiff(names(values),assigned));sorted_rows<-rows[order(vapply(rows,function(v)v$feature_id,''))]
 .cr_result(list(reference_basis=basis,model_reasons=ranked,policy_reasons=policy,suppressed_groups=suppressed,omitted_by_limit=omitted,unmapped_contributions=lapply(unmapped,function(f)list(feature_id=f,contribution=values[[f]])),coverage=list(feature_count=length(values),mapped_feature_count=length(assigned),absolute_contribution_mass=absolute,mapped_absolute_mass=mapped,mapped_absolute_fraction=if(absolute==0)NULL else mapped/absolute),reconstruction_residual=residual,policy_decision_hash=decision_hash,policy_action=action,policy_findings=findings),'explainability.rank_credit_reason_codes','reason_codes',ctx,diagnostics=list(dictionary_hash=.cr_hash(reason_dictionary),contribution_hash=.cr_hash(sorted_rows)),limitations='Model reasons consolidate same-scale stored contributions relative to the declared reference. Policy reasons reproduce matched stored rules separately and are never added to model effects. Netting transformed features within a reason can suppress individually adverse terms. Ranking is descriptive, not a causal claim or automatically a legally sufficient adverse-action notice.')
}
