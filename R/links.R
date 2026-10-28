# Canonical identifier order uses UTF-8 radix collation; numeric loss ordering is unchanged.
.cr_link_object<-function(x,name){if(inherits(x,'CalcResult'))x<-x$value;if(!.cr_object(x)) .cr_error('LINK_CONTRACT',paste(name,'must be a named object'));x}
.cr_link_ids<-function(x,name,empty=FALSE){if(is.list(x)&&is.null(names(x))&&all(vapply(x,function(v)is.character(v)&&length(v)==1L,logical(1))))x<-if(length(x))unlist(x,use.names=FALSE) else character();if(!is.character(x)||(!empty&&!length(x))||anyNA(x)||any(!nzchar(x))||anyDuplicated(x)) .cr_error('LINK_CONTRACT',paste(name,'requires distinct nonempty strings'));x}
.cr_link_scalar<-function(x,name,minimum=NULL){a<-.cr_numeric(x,name);if(length(a)!=1L||!is.null(dim(a))||(!is.null(minimum)&&a<minimum)) .cr_error('LINK_CONTRACT',paste(name,'is outside its scalar domain'));as.numeric(a)}
.cr_link_p<-function(x,name){a<-.cr_probability(x,name);if(length(a)!=1L||!is.null(dim(a))) .cr_error('LINK_CONTRACT',paste(name,'must be scalar'));as.numeric(a)}
.cr_link_seal<-function(value,field){value[[field]]<-.cr_hash(value);value}
.cr_link_read<-function(value,field){value<-.cr_link_object(value,field);expected<-value[[field]];value[[field]]<-NULL;if(!identical(expected,.cr_hash(value))) .cr_error('LINK_INTEGRITY','Relationship artifact content or hash differs');value[[field]]<-expected;value}
.cr_link_components<-function(nodes,arcs){
 forward<-reverse<-setNames(lapply(nodes,function(x)character()),nodes)
 for(arc in arcs){forward[[arc[1]]]<-c(forward[[arc[1]]],arc[2]);reverse[[arc[2]]]<-c(reverse[[arc[2]]],arc[1])}
 seen<-order<-character()
 for(start in nodes){if(start%in%seen)next;stack<-list(list(node=start,finished=FALSE))
  while(length(stack)){last<-stack[[length(stack)]];stack[[length(stack)]]<-NULL;node<-last$node
   if(last$finished){order<-c(order,node);next};if(node%in%seen)next;seen<-c(seen,node);stack[[length(stack)+1L]]<-list(node=node,finished=TRUE)
   for(n in rev(sort(enc2utf8(as.character(forward[[node]])),method='radix')))if(!(n%in%seen))stack[[length(stack)+1L]]<-list(node=n,finished=FALSE)
  }
 }
 seen<-character();components<-list()
 for(start in rev(order)){if(start%in%seen)next;stack<-start;members<-character();seen<-c(seen,start)
  while(length(stack)){node<-tail(stack,1);stack<-head(stack,-1);members<-c(members,node);for(neighbor in reverse[[node]])if(!(neighbor%in%seen)){seen<-c(seen,neighbor);stack<-c(stack,neighbor)}}
  components[[length(components)+1L]]<-sort(enc2utf8(as.character(members)),method='radix')
 }
 components[order(enc2utf8(vapply(components,function(x)x[1],character(1))),method='radix')]
}
build_entity_graph<-function(entities,edges,relationship_policy,as_of,known_at,context=NULL){
 rows<-.cr_data_rows(entities,'entities');.cr_data_unique(rows,'entity_id','entities');ids<-sort(enc2utf8(as.character(vapply(rows,function(r)r$entity_id,character(1)))),method='radix');if(!length(ids)) .cr_error('LINK_ENTITIES','At least one declared entity is required')
 policy<-.cr_link_object(relationship_policy,'relationship_policy');.cr_data_required(policy,c('policy_id','version','edge_types'));.cr_string(policy$policy_id,'policy_id','LINK_CONTRACT');.cr_string(policy$version,'version','LINK_CONTRACT');types<-.cr_link_ids(policy$edge_types,'edge_types')
 observation<-.cr_data_date(as_of,'as_of');knowledge<-.cr_data_date(known_at,'known_at');records<-.cr_data_rows(edges,'edges');.cr_data_unique(records,'edge_id','edges');active<-excluded<-list()
 for(row in records){
  .cr_data_required(row,c('source_id','target_id','edge_type','valid_from','known_at','evidence_reference'))
  if(!(row$source_id%in%ids)||!(row$target_id%in%ids)) .cr_error('LINK_ENTITY','Every relationship endpoint must exist')
  if(!(row$edge_type%in%types)) .cr_error('LINK_TYPE','Relationship type not declared by policy')
  .cr_string(row$evidence_reference,'evidence_reference','LINK_CONTRACT');begin<-.cr_data_date(row$valid_from,'valid_from');available<-.cr_data_date(row$known_at,'known_at');end<-if(is.null(row$valid_until))NULL else .cr_data_date(row$valid_until,'valid_until')
  if(!is.null(end)&&end<=begin) .cr_error('LINK_DATE','Relationship validity interval is empty')
  if(row$edge_type=='OWNERSHIP'){.cr_data_required(row,'ownership_fraction');.cr_link_p(row$ownership_fraction,'ownership_fraction')}
  reason<-if(available>knowledge)'NOT_KNOWN' else if(observation<begin||(!is.null(end)&&observation>=end))'NOT_VALID' else NULL
  if(!is.null(reason))excluded[[length(excluded)+1L]]<-list(edge_id=row$edge_id,reason=reason) else active[[length(active)+1L]]<-row
 }
 active<-active[order(enc2utf8(vapply(active,function(r)r$edge_id,character(1))),method='radix')];duplicates<-list();seen<-list()
 for(edge in active){key<-.cr_hash(list(edge$source_id,edge$target_id,edge$edge_type));if(!is.null(seen[[key]]))duplicates[[length(duplicates)+1L]]<-list(first_edge_id=seen[[key]],edge_id=edge$edge_id,endpoints=list(edge$source_id,edge$target_id),edge_type=edge$edge_type) else seen[[key]]<-edge$edge_id}
 cycles<-.cr_empty_object()
 for(kind in types){arcs<-lapply(Filter(function(e)e$edge_type==kind,active),function(e)c(e$source_id,e$target_id));cyclic<-Filter(function(cmp)length(cmp)>1L||any(vapply(arcs,function(a)all(a==cmp[1]),logical(1))),.cr_link_components(ids,arcs));if(length(cyclic))cycles[[kind]]<-lapply(cyclic,as.list)}
 value<-.cr_link_seal(list(as_of=as_of,known_at=known_at,entities=rows[order(enc2utf8(vapply(rows,function(r)r$entity_id,character(1))),method='radix')],edges=active,policy=policy),'graph_hash')
 .cr_result(value,'links.build_entity_graph','entity_graph',context,diagnostics=list(excluded=excluded[order(enc2utf8(vapply(excluded,function(r)r$edge_id,character(1))),method='radix')],duplicate_active_relationships=duplicates,cycles=cycles),limitations=c('Relationship types carry separate meanings. Ownership or control alone does not establish support, enforceability, shared PD or joint default.','Validity is [valid_from,valid_until); known_at may create a retrospective view and must match the historical decision cutoff for backtesting.'))
}
resolve_parent_links<-function(entity_graph,control_policy,context=NULL){
 graph<-.cr_link_read(entity_graph,'graph_hash');policy<-.cr_link_object(control_policy,'control_policy');.cr_data_required(policy,c('policy_id','version','control_edge_types','ownership_threshold','ownership_inclusive'));kinds<-.cr_link_ids(policy$control_edge_types,'control_edge_types',TRUE);threshold<-.cr_link_p(policy$ownership_threshold,'ownership_threshold')
 if(length(setdiff(kinds,unlist(graph$policy$edge_types)))) .cr_error('LINK_POLICY','Control edge types must exist in graph policy')
 if(!is.logical(policy$ownership_inclusive)||length(policy$ownership_inclusive)!=1L||is.na(policy$ownership_inclusive)) .cr_error('LINK_POLICY','ownership_inclusive must be Boolean')
 ids<-vapply(graph$entities,function(r)r$entity_id,character(1));parents<-setNames(lapply(ids,function(n)list()),ids)
 for(edge in graph$edges){eligible<-edge$edge_type%in%kinds;if(edge$edge_type=='OWNERSHIP')eligible<-if(policy$ownership_inclusive)edge$ownership_fraction>=threshold else edge$ownership_fraction>threshold
  if(eligible)parents[[edge$target_id]][[edge$source_id]]<-c(parents[[edge$target_id]][[edge$source_id]],edge$edge_id)
 }
 output<-list()
 for(entity in ids){candidates<-sort(enc2utf8(as.character(names(parents[[entity]]))),method='radix');path<-entity;trace<-character();current<-entity;status<-'RESOLVED';ultimate<-NULL
  repeat{choices<-sort(enc2utf8(as.character(names(parents[[current]]))),method='radix');if(!length(choices)){ultimate<-current;break};if(length(choices)>1L){status<-'AMBIGUOUS_PARENT';break};parent<-choices[1];trace<-c(trace,sort(enc2utf8(as.character(parents[[current]][[parent]])),method='radix'));path<-c(path,parent);if(parent%in%head(path,-1)){status<-'CONTROL_CYCLE';break};current<-parent}
  output[[length(output)+1L]]<-list(entity_id=entity,direct_parent=if(length(candidates)==1L)candidates[1] else NULL,direct_candidates=as.list(candidates),ultimate_parent=if(!is.null(ultimate)&&ultimate!=entity)ultimate else NULL,path=as.list(path),edge_trace=as.list(trace),status=status)
 }
 .cr_result(list(graph_hash=graph$graph_hash,control_policy_hash=.cr_hash(policy),parents=output),'links.resolve_parent_links','parent_mapping',context,limitations='Explicit control edges and the supplied ownership threshold determine eligibility. Indirect share products and support capacity are not inferred. Ambiguous or cyclic chains remain unresolved.')
}
assign_default_groups<-function(entity_graph,default_link_policy,facility_mappings,context=NULL){
 graph<-.cr_link_read(entity_graph,'graph_hash');policy<-.cr_link_object(default_link_policy,'default_link_policy');.cr_data_required(policy,c('policy_id','version','edge_types','direction_policy','transitive'));kinds<-.cr_link_ids(policy$edge_types,'edge_types');direction<-policy$direction_policy
 if(length(setdiff(kinds,unlist(graph$policy$edge_types)))) .cr_error('LINK_POLICY','Default edge types must exist in graph policy')
 if(length(direction)!=1L||!(direction%in%c('mutual_only','undirected_explicit'))||!is.logical(policy$transitive)||length(policy$transitive)!=1L||is.na(policy$transitive)) .cr_error('LINK_POLICY','Declare mutual_only/undirected_explicit and Boolean transitive')
 ids<-vapply(graph$entities,function(r)r$entity_id,character(1));pairs<-list();trace<-list();key<-function(a,b).cr_hash(list(a,b))
 for(edge in graph$edges)if(edge$edge_type%in%kinds){k<-key(edge$source_id,edge$target_id);if(is.null(pairs[[k]]))pairs[[k]]<-list(a=edge$source_id,b=edge$target_id,ids=character());pairs[[k]]$ids<-c(pairs[[k]]$ids,edge$edge_id)}
 neighbors<-setNames(lapply(ids,function(n)character()),ids);ordered<-pairs[order(enc2utf8(vapply(pairs,function(p)p$a,character(1))),enc2utf8(vapply(pairs,function(p)p$b,character(1))),method='radix')]
 for(pair in ordered){a<-pair$a;b<-pair$b;accepted<-direction=='undirected_explicit'||!is.null(pairs[[key(b,a)]]);trace[[length(trace)+1L]]<-list(source_id=a,target_id=b,edge_ids=as.list(sort(enc2utf8(as.character(pair$ids)),method='radix')),accepted=accepted);if(accepted){neighbors[[a]]<-union(neighbors[[a]],b);neighbors[[b]]<-union(neighbors[[b]],a)}}
 arcs<-list();for(a in ids)for(b in sort(enc2utf8(as.character(neighbors[[a]])),method='radix'))arcs[[length(arcs)+1L]]<-c(a,b)
 components<-.cr_link_components(ids,arcs);assignments<-setNames(lapply(ids,function(n)NULL),ids);groups<-conflicts<-list()
 for(members in components){complete<-all(vapply(members,function(a)all(setdiff(members,a)%in%neighbors[[a]]),logical(1)))
  if(!policy$transitive&&!complete){conflicts[[length(conflicts)+1L]]<-list(members=as.list(members),status='NONTRANSITIVE_OVERLAP')
  }else{group_id<-paste0('default-group-',substr(.cr_hash(list(members=as.list(members),policy=policy)),1,20));groups[[length(groups)+1L]]<-list(group_id=group_id,members=as.list(members));for(node in members)assignments[node]<-list(group_id)}
 }
 facilities<-.cr_data_rows(facility_mappings,'facility_mappings');.cr_data_unique(facilities,'facility_id','facility_mappings');mapped<-list();facilities<-facilities[order(enc2utf8(vapply(facilities,function(r)r$facility_id,character(1))),method='radix')]
 for(row in facilities){.cr_data_required(row,'entity_id');if(!(row$entity_id%in%names(assignments))) .cr_error('LINK_ENTITY','Facility owner is absent from the graph');mapped[[length(mapped)+1L]]<-list(facility_id=row$facility_id,entity_id=row$entity_id,group_id=assignments[[row$entity_id]])}
 .cr_result(list(graph_hash=graph$graph_hash,policy_hash=.cr_hash(policy),groups=groups,assignments=lapply(ids,function(n)list(entity_id=n,group_id=assignments[[n]])),facilities=mapped,conflicts=conflicts),'links.assign_default_groups','default_groups',context,diagnostics=list(edge_trace=trace),limitations=c('Default-group equivalence is a supplied policy. A unilateral trigger is not symmetric default equivalence; mutual-only policy requires both directions.','Without transitivity, overlapping non-clique links cannot define disjoint groups and remain unresolved. No member PD is copied or averaged.'))
}
.cr_link_risk<-function(risk,context){
 risk<-.cr_link_object(risk,'risk');.cr_data_required(risk,c('entity_id','pd','stage','target','applied_effects'));.cr_string(risk$entity_id,'entity_id','LINK_CONTRACT');p<-.cr_link_p(risk$pd,'pd');effects<-.cr_link_ids(risk$applied_effects,'applied_effects',TRUE)
 target<-.cr_link_object(risk$target,'target');.cr_data_required(target,c('event','horizon','time_unit','probability_measure','conditioning'))
 if(!identical(target$event,'default')||.cr_link_scalar(target$horizon,'horizon')<=0||!(target$time_unit%in%c('days','months','years'))||!(target$probability_measure%in%c('PHYSICAL','RISK_NEUTRAL'))) .cr_error('LINK_TARGET','A default target with positive horizon, time unit and measure is required')
 .cr_string(target$conditioning,'conditioning','LINK_CONTRACT');ctx<-.cr_result(NULL,'links.context',context=context)$context
 for(key in c('horizon','time_unit','probability_measure','conditioning'))if(!is.null(ctx[[key]])&&!identical(.cr_hash(ctx[[key]]),.cr_hash(target[[key]]))) .cr_error('TARGET_MISMATCH','Risk target conflicts with context')
 list(risk=risk,p=p,effects=effects,target=target,context=ctx)
}
apply_parent_support<-function(standalone_risk,parent_risk,support_contract,joint_default_probability,context=NULL){
 base<-.cr_link_risk(standalone_risk,context);parent<-.cr_link_risk(parent_risk,context);p<-base$p;q<-parent$p;ctx<-base$context
 if(!identical(base$risk$stage,'STANDALONE')||!identical(parent$risk$stage,'STANDALONE')||base$risk$entity_id==parent$risk$entity_id) .cr_error('SUPPORT_STAGE','Distinct standalone child and supporter risks required; recursive support must use an explicit network model')
 if(!identical(.cr_hash(.cr_link_semantic_target(base$target)),.cr_hash(.cr_link_semantic_target(parent$target)))) .cr_error('TARGET_MISMATCH','Parent and child default horizon, measure and conditioning must agree')
 contract<-.cr_link_object(support_contract,'support_contract');.cr_data_required(contract,c('effect_id','assumption_reference','currency','total_capacity','reserved_elsewhere','allocated_capacity','support_required','states'));effect<-contract$effect_id;.cr_string(effect,'effect_id','LINK_CONTRACT');.cr_string(contract$assumption_reference,'assumption_reference','LINK_CONTRACT');currency<-contract$currency;.cr_string(currency,'currency','LINK_CONTRACT')
 if(effect%in%c(base$effects,parent$effects)) .cr_error('LINK_DOUBLE_COUNT','Support effect already applied to an input risk')
 if(!is.null(ctx$currency)&&!identical(ctx$currency,currency)) .cr_error('TARGET_MISMATCH','Support currency conflicts with context')
 total<-.cr_link_scalar(contract$total_capacity,'total_capacity',0);reserved<-.cr_link_scalar(contract$reserved_elsewhere,'reserved_elsewhere',0);capacity<-.cr_link_scalar(contract$allocated_capacity,'allocated_capacity',0);required<-.cr_link_scalar(contract$support_required,'support_required',0)
 if(required<=0||reserved>total||capacity>total-reserved) .cr_error('SUPPORT_CAPACITY','Positive required amount and allocation within remaining total budget required')
 states<-.cr_link_object(contract$states,'states');if(!setequal(names(states),c('parent_alive','joint_default'))||anyDuplicated(names(states))) .cr_error('SUPPORT_STATES','Both parent-alive and joint-default conditional states are required')
 j<-.cr_link_p(joint_default_probability,'joint_default_probability');lower<-max(0,sum(c(p,q,-1)));if(j<lower||j>min(p,q)) .cr_error('INVALID_JOINT_PROBABILITY','Child/supporter intersection outside Frechet bounds')
 details<-list();rescued<-numeric();masses<-c(parent_alive=p-j,joint_default=j)
 for(state in names(masses)){mass<-unname(masses[state]);terms<-.cr_link_object(states[[state]],state);.cr_data_required(terms,c('available_capacity','willingness_given_state','success_given_willing_and_funded','support_permitted','evidence_reference'));available<-.cr_link_scalar(terms$available_capacity,'available_capacity',0)
  if(available>capacity||!is.logical(terms$support_permitted)||length(terms$support_permitted)!=1L||is.na(terms$support_permitted)) .cr_error('SUPPORT_CAPACITY','State capacity cannot exceed earmarked capacity; permission must be Boolean')
  .cr_string(terms$evidence_reference,'evidence_reference','LINK_CONTRACT');willing<-.cr_link_p(terms$willingness_given_state,'willingness');success<-.cr_link_p(terms$success_given_willing_and_funded,'success');funded<-available>=required;conditional<-if(funded&&terms$support_permitted)willing*success else 0;rescue<-mass*conditional;rescued<-c(rescued,rescue)
  details[[length(details)+1L]]<-list(state=state,child_default_mass=mass,available_capacity=available,funding_sufficient=funded,support_permitted=terms$support_permitted,rescue_probability_given_state=conditional,rescued_mass=rescue,remaining_default_mass=mass-rescue)
 }
 supported<-sum(c(p,-rescued));if(supported<0&&supported>= -8*.Machine$double.eps)supported<-0
 if(supported<0||supported>p) .cr_error('NUMERICAL_FAILURE','Support event decomposition has invalid remaining mass')
 .cr_result(list(entity_id=base$risk$entity_id,supporter_id=parent$risk$entity_id,stage='PRE_TRANSFER',pd=supported,standalone_pd=p,parent_pd=q,joint_default_probability=j,target=base$target,applied_effects=as.list(c(base$effects,effect)),support_states=details,capacity_contract=list(currency=currency,total_capacity=total,reserved_elsewhere=reserved,allocated_capacity=capacity,support_required=required),assumption_reference=contract$assumption_reference),'links.apply_parent_support','probability',ctx,limitations=c('Conditional support willingness and effectiveness are declared assumptions. Allocated cash must cover the declared rescue need; no arbitrary capacity ratio is treated as a probability.','Support during supporter default requires explicit permitted, funded, evidenced conditional terms. The resulting PD is before transfer adjustment; no risk contagion or rescue budget across undeclared beneficiaries is inferred.'))
}
allocate_guarantee_coverage<-function(guarantees,claims,allocation_policy,as_of,known_at,currency,context=NULL){
 gs<-.cr_data_rows(guarantees,'guarantees');cs<-.cr_data_rows(claims,'claims');.cr_data_unique(gs,'guarantee_id','guarantees');.cr_data_unique(cs,'claim_id','claims');.cr_string(currency,'currency','LINK_CONTRACT');date<-.cr_data_date(as_of,'as_of');knowledge<-.cr_data_date(known_at,'known_at');policy<-.cr_link_object(allocation_policy,'allocation_policy');.cr_data_required(policy,c('policy_id','version','guarantee_priority','claim_priority','cap_convention'))
 gp<-.cr_link_ids(policy$guarantee_priority,'guarantee_priority',TRUE);cp<-.cr_link_ids(policy$claim_priority,'claim_priority',TRUE)
 if(!setequal(gp,vapply(gs,function(g)g$guarantee_id,character(1)))||!setequal(cp,vapply(cs,function(cl)cl$claim_id,character(1)))||!identical(policy$cap_convention,'non_reinstating_total')) .cr_error('GUARANTEE_POLICY','Complete priority orders and non_reinstating_total cap convention required')
 ctx<-.cr_result(NULL,'links.context',context=context)$context;if(!is.null(ctx$currency)&&!identical(ctx$currency,currency)) .cr_error('TARGET_MISMATCH','Allocation currency conflicts with context')
 claim_map<-remaining<-list()
 for(claim in cs){.cr_data_required(claim,c('entity_id','amount','currency','due_at','known_at'));.cr_string(claim$entity_id,'entity_id','LINK_CONTRACT');amount<-.cr_link_scalar(claim$amount,'claim amount',0);due<-.cr_data_date(claim$due_at,'due_at');available<-.cr_data_date(claim$known_at,'known_at')
  if(claim$currency!=currency||due<date||available>knowledge) .cr_error('GUARANTEE_CLAIM','Claims must be known, in reporting currency, and not past due at the allocation date');claim_map[[claim$claim_id]]<-claim;remaining[[claim$claim_id]]<-amount
 }
 guarantee_map<-unused<-list();exclusions<-list()
 for(guarantee in gs){.cr_data_required(guarantee,c('guarantor_id','currency','cap','cap_used','coverage_fraction','covered_claim_ids','valid_from','valid_until','known_at','legally_effective','evidence_reference'))
  .cr_string(guarantee$guarantor_id,'guarantor_id','LINK_CONTRACT');cap<-.cr_link_scalar(guarantee$cap,'cap',0);used<-.cr_link_scalar(guarantee$cap_used,'cap_used',0);fraction<-.cr_link_p(guarantee$coverage_fraction,'coverage_fraction');covered<-.cr_link_ids(guarantee$covered_claim_ids,'covered_claim_ids',TRUE)
  if(used>cap||length(setdiff(covered,names(claim_map)))) .cr_error('GUARANTEE_CAP','Consumed cap exceeds limit or references unknown claims')
  if(guarantee$currency!=currency) .cr_error('GUARANTEE_CURRENCY','Convert nominal caps under an explicit FX contract before allocation')
  begin<-.cr_data_date(guarantee$valid_from,'valid_from');end<-.cr_data_date(guarantee$valid_until,'valid_until');available<-.cr_data_date(guarantee$known_at,'known_at');.cr_string(guarantee$evidence_reference,'evidence_reference','LINK_CONTRACT')
  if(end<=begin||!is.logical(guarantee$legally_effective)||length(guarantee$legally_effective)!=1L||is.na(guarantee$legally_effective)) .cr_error('GUARANTEE_CONTRACT','Nonempty validity interval and explicit legal-effectiveness flag required')
  reason<-if(available>knowledge)'NOT_KNOWN' else if(!guarantee$legally_effective)'NOT_EFFECTIVE' else if(date>=end)'EXPIRED' else NULL
  guarantee_map[[guarantee$guarantee_id]]<-guarantee;unused[[guarantee$guarantee_id]]<-cap-used;if(!is.null(reason))exclusions[[length(exclusions)+1L]]<-list(guarantee_id=guarantee$guarantee_id,reason=reason)
 }
 excluded<-vapply(exclusions,function(x)x$guarantee_id,character(1));allocations<-list()
 for(gid in gp){if(gid%in%excluded)next;g<-guarantee_map[[gid]]
  for(cid in cp){if(!(cid%in%unlist(g$covered_claim_ids)))next;claim<-claim_map[[cid]];due<-.cr_data_date(claim$due_at,'due_at');if(due<.cr_data_date(g$valid_from,'valid_from')||due>=.cr_data_date(g$valid_until,'valid_until'))next
   amount<-min(remaining[[cid]],g$coverage_fraction*claim$amount,unused[[gid]])
   if(amount>0){allocations[[length(allocations)+1L]]<-list(guarantee_id=gid,guarantor_id=g$guarantor_id,claim_id=cid,entity_id=claim$entity_id,covered_amount=amount,currency=currency,due_at=claim$due_at);remaining[[cid]]<-remaining[[cid]]-amount;unused[[gid]]<-unused[[gid]]-amount}
  }
 }
 value<-.cr_link_seal(list(as_of=as_of,known_at=known_at,currency=currency,policy=policy,claims=lapply(sort(enc2utf8(as.character(names(claim_map))),method='radix'),function(cid)claim_map[[cid]]),allocations=allocations,uncovered_claims=lapply(sort(enc2utf8(as.character(names(remaining))),method='radix'),function(cid)list(claim_id=cid,amount=remaining[[cid]])),remaining_caps=lapply(sort(enc2utf8(as.character(names(unused))),method='radix'),function(gid)list(guarantee_id=gid,amount=unused[[gid]])),excluded=exclusions[order(enc2utf8(vapply(exclusions,function(r)r$guarantee_id,character(1))),method='radix')]),'allocation_hash')
 .cr_result(value,'links.allocate_guarantee_coverage',currency,ctx,limitations=c('Cap allocations are simultaneous, non-reinstating reservations over all supplied claims; priorities determine allocation and are never inferred from input row order.','A due date must fall in the guarantee validity interval. Currency conversion, legal enforceability, existing cap usage and unreported obligations are supplied assumptions.'))
}
.cr_link_semantic_target<-function(x)x[setdiff(names(x),c('entity_id','exposure_id'))]
apply_guarantee_risk<-function(borrower_risk,guarantor_risk,coverage_allocation,claim_id,loss_contract,joint_default_probability,context=NULL){
 b<-.cr_link_risk(borrower_risk,context);g<-.cr_link_risk(guarantor_risk,context);ctx<-b$context;p<-b$p;q<-g$p
 if(!identical(.cr_hash(.cr_link_semantic_target(b$target)),.cr_hash(.cr_link_semantic_target(g$target)))||b$risk$entity_id==g$risk$entity_id) .cr_error('TARGET_MISMATCH','Distinct borrower/guarantor with matching default event contract required')
 allocation<-.cr_link_read(coverage_allocation,'allocation_hash');contract<-.cr_link_object(loss_contract,'loss_contract');.cr_data_required(contract,c('effect_id','currency','assumption_reference','stage','states','valuation_date'));effect<-contract$effect_id;.cr_string(effect,'effect_id','LINK_CONTRACT');.cr_string(contract$assumption_reference,'assumption_reference','LINK_CONTRACT')
 if(b$risk$stage=='GUARANTEE_ADJUSTED'||contract$stage!='PRE_GUARANTEE'||effect%in%c(b$effects,g$effects)) .cr_error('LINK_DOUBLE_COUNT','Only unadjusted conditional losses and a new effect are accepted')
 if(contract$currency!=allocation$currency||(!is.null(ctx$currency)&&ctx$currency!=contract$currency)) .cr_error('TARGET_MISMATCH','Loss and allocation currency must agree')
 .cr_string(claim_id,'claim_id','LINK_CONTRACT');claim<-Filter(function(r)r$claim_id==claim_id,allocation$claims)
 if(length(claim)!=1L||claim[[1]]$entity_id!=b$risk$entity_id) .cr_error('GUARANTEE_CLAIM','Claim must belong to borrower in frozen allocation')
 if(contract$valuation_date!=claim[[1]]$due_at) .cr_error('GUARANTEE_VALUATION','Loss and allocated nominal protection must be measured at claim due date')
 matches<-Filter(function(r)r$claim_id==claim_id,allocation$allocations)
 if(!length(matches)&&!any(vapply(allocation$uncovered_claims,function(r)r$claim_id==claim_id,logical(1)))) .cr_error('GUARANTEE_CLAIM','Claim is absent from frozen allocation')
 if(any(vapply(matches,function(r)r$entity_id!=b$risk$entity_id||r$guarantor_id!=g$risk$entity_id,logical(1)))) .cr_error('GUARANTEE_JOINT_STATES','All allocated protection on claim must belong to borrower and one guarantor; multiple guarantors require joint event model')
 covered<-sum(vapply(matches,function(r)r$covered_amount,numeric(1)));j<-.cr_link_p(joint_default_probability,'joint_default_probability')
 # Accumulate the three terms together, matching the stable parent-support bound.
 # Sequential (p+q)-1 spuriously rejects p=1, q=.1, j=.1.
 if(j<max(0,sum(c(p,q,-1)))||j>min(p,q)) .cr_error('INVALID_JOINT_PROBABILITY','Borrower/guarantor intersection outside Frechet bounds')
 states<-.cr_link_object(contract$states,'states');if(!setequal(names(states),c('borrower_only','joint_default'))) .cr_error('GUARANTEE_STATES','Explicit conditional borrower-only and joint-default loss/payout terms required')
 rows<-list();masses<-c(p-j,j)
 for(i in seq_len(2)){name<-c('borrower_only','joint_default')[i];terms<-.cr_link_object(states[[name]],name);.cr_data_required(terms,c('loss_before_guarantee','payout_fraction','evidence_reference'));.cr_string(terms$evidence_reference,'evidence_reference','LINK_CONTRACT');loss<-.cr_link_scalar(terms$loss_before_guarantee,'loss_before_guarantee');fraction<-.cr_link_p(terms$payout_fraction,'payout_fraction');payment<-min(covered,max(loss,0))*fraction
 rows[[i]]<-list(state=name,probability=masses[i],loss_before_guarantee=loss,guarantee_payment=payment,loss_after_guarantee=loss-payment)}
 before<-sum(vapply(rows,function(r)r$probability*r$loss_before_guarantee,numeric(1)));after<-sum(vapply(rows,function(r)r$probability*r$loss_after_guarantee,numeric(1)))
 if(!is.finite(before)||!is.finite(after)) .cr_error('NUMERICAL_FAILURE','Expected guaranteed loss outside finite range')
 .cr_result(list(entity_id=b$risk$entity_id,guarantor_id=g$risk$entity_id,claim_id=claim_id,pd=p,guarantor_pd=q,target=b$target,stage='GUARANTEE_ADJUSTED',currency=contract$currency,covered_amount=covered,expected_loss_before=before,expected_loss_after=after,states=rows,applied_effects=as.list(c(b$effects,effect)),allocation_hash=allocation$allocation_hash),'links.apply_guarantee_risk',contract$currency,ctx,limitations=c('The guarantee changes conditional monetary loss, never borrower PD. Loss and payout amounts are measured in allocation currency at claim due date. Discount resulting state losses separately under an explicit convention.','Only one guarantor per claim is supported by this two-event calculation. Shared nominal caps must first be allocated across all claims; sequentially applying marginal guarantees is not a joint loss model.'))
}
apply_rating_constraints<-function(base_result,rating_scale,constraints,representative_pd_policy,context=NULL){
 base<-.cr_link_object(base_result,'base_result');.cr_data_required(base,c('pd','grade'));pd<-.cr_link_p(base$pd,'pd');scale<-.cr_scale(rating_scale);grades<-vapply(scale$grades,function(g)g$grade,character(1));mapped<-function(v)as.character(map_pd_to_grade(v,scale)$value)
 if(!(base$grade%in%grades)||mapped(pd)!=base$grade) .cr_error('RATING_CONSISTENCY','Base grade must map from supplied continuous PD and scale')
 if(!(representative_pd_policy%in%c('preserve_pd','representative_on_binding'))) .cr_error('RATING_POLICY','Declare preserve_pd or representative_on_binding')
 rows<-.cr_data_rows(constraints,'constraints');.cr_data_unique(rows,'rule_id','constraints');lo<-1;hi<-length(grades);trace<-list();original<-match(base$grade,grades)
 for(rule in rows){.cr_data_required(rule,c('kind','evidence_reference'));.cr_string(rule$evidence_reference,'evidence_reference','LINK_CONTRACT');kind<-rule$kind
 if(kind=='grade_bounds'){
 .cr_data_required(rule,c('best_allowed_grade','worst_allowed_grade'));if(!(rule$best_allowed_grade%in%grades)||!(rule$worst_allowed_grade%in%grades)) .cr_error('RATING_CONSTRAINT','Unknown bound grade');a<-match(rule$best_allowed_grade,grades);b<-match(rule$worst_allowed_grade,grades)
 }else if(kind=='notch_bounds'){
 .cr_data_required(rule,c('reference_grade','max_improvement','max_deterioration'));if(!(rule$reference_grade%in%grades)) .cr_error('RATING_CONSTRAINT','Unknown reference grade');up<-.cr_link_scalar(rule$max_improvement,'max_improvement',0);down<-.cr_link_scalar(rule$max_deterioration,'max_deterioration',0)
 if(up!=floor(up)||down!=floor(down)) .cr_error('RATING_CONSTRAINT','Notch distances must be nonnegative integers');ref<-match(rule$reference_grade,grades);a<-max(1,ref-up);b<-min(length(grades),ref+down)
 }else .cr_error('RATING_CONSTRAINT','Unknown constraint kind')
 if(a>b) .cr_error('RATING_CONFLICT','A rule has empty permitted grade interval');lo<-max(lo,a);hi<-min(hi,b);trace[[length(trace)+1L]]<-list(rule_id=rule$rule_id,best_allowed_grade=grades[a],worst_allowed_grade=grades[b],excludes_original=!(a<=original&&original<=b))
 }
 if(lo>hi) .cr_error('RATING_CONFLICT','Constraint intersection is empty; rule order cannot resolve contradiction')
 final<-max(lo,min(hi,original));binding<-final!=original;constrained<-if(!binding||representative_pd_policy=='preserve_pd')pd else scale$grades[[final]]$representative_pd
 active<-vapply(Filter(function(r)binding&&((final>original&&r$best_allowed_grade==grades[final])||(final<original&&r$worst_allowed_grade==grades[final])),trace),function(r)r$rule_id,character(1))
 .cr_result(list(original=base,pd=constrained,grade=grades[final],stage='CONSTRAINED',binding=binding,binding_rules=as.list(active),rules=trace,representative_pd_policy=representative_pd_policy,pd_grade_consistent=mapped(constrained)==grades[final],scale_id=scale$scale_id,scale_version=scale$version),'links.apply_rating_constraints','rating',context,limitations=c('Grades are ordered from lower to higher risk. All bounds are intersected, with conflicts rejected. Nonbinding constraints preserve continuous PD exactly.','preserve_pd changes only the grade; pd_grade_consistent exposes any resulting difference. representative_on_binding selects a scale representative only when original grade is excluded.'))
}
apply_override<-function(base_result,override_request,policy,as_of,known_at,context=NULL){
 base<-.cr_link_object(base_result,'base_result');request<-.cr_link_object(override_request,'override_request');policy<-.cr_link_object(policy,'policy');.cr_data_required(policy,c('policy_id','version','targets','require_dual_control'));.cr_string(policy$policy_id,'policy_id','LINK_CONTRACT');.cr_string(policy$version,'version','LINK_CONTRACT')
 if(!is.logical(policy$require_dual_control)||length(policy$require_dual_control)!=1L||is.na(policy$require_dual_control)) .cr_error('OVERRIDE_POLICY','require_dual_control must be Boolean')
 targets<-.cr_link_object(policy$targets,'targets');.cr_data_required(request,c('override_id','target','value','reason','author','approved_by','recorded_at','valid_from','valid_until'))
 for(key in c('override_id','target','reason','author','approved_by')) .cr_string(request[[key]],key,'LINK_CONTRACT')
 if(policy$require_dual_control&&request$author==request$approved_by) .cr_error('OVERRIDE_APPROVAL','Policy requires distinct author and approver')
 date<-.cr_data_date(as_of,'as_of');knowledge<-.cr_data_date(known_at,'known_at');recorded<-.cr_data_date(request$recorded_at,'recorded_at');begin<-.cr_data_date(request$valid_from,'valid_from');end<-.cr_data_date(request$valid_until,'valid_until')
 if(end<=begin) .cr_error('OVERRIDE_DATE','Override validity interval must be nonempty')
 if(!is.null(base$override_hash)){
 prior<-.cr_link_read(base,'override_hash');if(any(vapply(prior$history,function(s)s$as_of!=as_of||s$known_at!=known_at,logical(1)))) .cr_error('OVERRIDE_REPLAY','A different evaluation cutoff requires replaying requests from preserved original')
 original<-prior$original;current<-prior$current;history<-prior$history;previous<-prior$override_hash
 }else{if(identical(base$stage,'OVERRIDDEN')) .cr_error('OVERRIDE_INTEGRITY','An overridden value must retain full audit artifact');original<-base;current<-base;history<-list();previous<-.cr_hash(base)}
 if(any(vapply(history,function(s)s$request$override_id==request$override_id,logical(1)))) .cr_error('OVERRIDE_DUPLICATE','Override ID already recorded in history')
 if(!(request$target%in%names(targets))) .cr_error('OVERRIDE_POLICY','Requested target is not allowed')
 contract<-.cr_link_object(targets[[request$target]],'target contract');.cr_data_required(contract,c('path','kind'));path<-.cr_link_ids(contract$path,'path')
 if(path[1]%in%c('stage','history','original','override_hash','applied_effects','target')) .cr_error('OVERRIDE_POLICY','Audit, effect and event-contract fields cannot be overwritten')
 node<-current
 for(key in head(path,-1)){if(!(key%in%names(node))||!.cr_object(node[[key]])) .cr_error('OVERRIDE_TARGET','Override path does not exist');node<-node[[key]]}
 if(!(tail(path,1)%in%names(node))) .cr_error('OVERRIDE_TARGET','Override field does not exist')
 replacement<-request$value;kind<-contract$kind
 if(kind=='probability'){.cr_link_p(replacement,'override value')
 }else if(kind=='numeric'){
 .cr_data_required(contract,c('minimum','maximum'));a<-.cr_link_scalar(contract$minimum,'minimum');b<-.cr_link_scalar(contract$maximum,'maximum');v<-.cr_link_scalar(replacement,'override value');if(!(a<=v&&v<=b)) .cr_error('OVERRIDE_VALUE','Numeric override outside declared bounds')
 }else if(kind=='grade'){allowed<-.cr_link_ids(contract$allowed_values,'allowed_values');if(!(replacement%in%allowed)) .cr_error('OVERRIDE_VALUE','Grade override outside allowed set')
 }else .cr_error('OVERRIDE_POLICY','Target kind must be probability, numeric or grade')
 status<-if(recorded>knowledge)'NOT_KNOWN' else if(date<begin)'NOT_YET_VALID' else if(date>=end)'EXPIRED' else 'APPLIED';before<-node[[tail(path,1)]]
 if(status=='APPLIED'){current[[path]]<-replacement;current$stage<-'OVERRIDDEN'}
 step<-.cr_link_seal(list(request=request,policy_hash=.cr_hash(policy),as_of=as_of,known_at=known_at,status=status,before=before,after=if(status=='APPLIED')replacement else before,previous_hash=previous),'step_hash');history[[length(history)+1L]]<-step
 value<-.cr_link_seal(list(original=original,current=current,history=history,status=status),'override_hash')
 .cr_result(value,'links.apply_override','override_artifact',context,limitations=c('This local audit artifact records declared author and approval evidence; it does not authenticate people or execute a credit action. Copying only current loses required audit history and must not create a new unadjusted baseline.','Only requested field changes. Grade override does not silently change PD or another parameter; dependent consistency checks remain explicit. Validity is [valid_from,valid_until).'))
}
solve_contagion_network<-function(liability_matrix,external_assets,external_liabilities,entity_ids,currency,model_assumptions,asset_scenarios=NULL,tolerance=1e-8,max_iterations=10000,context=NULL){
 ids<-.cr_link_ids(entity_ids,'entity_ids');n<-length(ids);.cr_string(currency,'currency','LINK_CONTRACT');ctx<-.cr_result(NULL,'links.context',context=context)$context
 if(!is.null(ctx$currency)&&ctx$currency!=currency) .cr_error('TARGET_MISMATCH','Clearing currency conflicts with context')
 required<-list(model='eisenberg_noe',seniority='single_pari_passu',bankruptcy_costs='none',outside_creditor='sink')
 if(!identical(.cr_hash(model_assumptions),.cr_hash(required))) .cr_error('CLEARING_ASSUMPTIONS','Declare Eisenberg-Noe, one pari-passu debt class, no bankruptcy costs and outside creditor sink')
 mat<-.cr_numeric(liability_matrix,'liability_matrix');outside<-.cr_numeric(external_liabilities,'external_liabilities',vector_only=TRUE)
 if(!identical(dim(mat),c(n,n))||length(outside)!=n||any(mat<0)||any(outside<0)||any(diag(mat)!=0)) .cr_error('CLEARING_INPUT','Aligned nonnegative debtor-row liabilities, zero diagonal and outside obligations required')
 tol<-.cr_link_scalar(tolerance,'tolerance');iterations<-.cr_link_scalar(max_iterations,'max_iterations')
 if(tol<1e-12||tol>1e-4||iterations!=floor(iterations)||iterations<1||iterations>1000000) .cr_error('CLEARING_CONTROL','Tolerance in [1e-12,1e-4] and integer iteration cap in [1,1000000] required')
 nominal<-rowSums(mat)+outside;if(any(!is.finite(nominal))) .cr_error('NUMERICAL_FAILURE','Total nominal liabilities outside finite range')
 active<-nominal>0;relative<-mat/ifelse(active,nominal,1)
 solve_one<-function(assets){
 assets<-.cr_numeric(assets,'external_assets',vector_only=TRUE);if(length(assets)!=n||any(assets<0)) .cr_error('CLEARING_INPUT','One nonnegative external asset amount per entity required')
 scale<-max(nominal,assets,1e-300);amounts<-c(nominal[active],assets[assets>0],mat[mat>0],outside[outside>0])
 if(length(amounts)&&min(amounts)/scale<1e-10) .cr_error('CLEARING_SCALE','Amount dynamic range exceeds supported numerical scale; rescale or aggregate with documented economic justification')
 if(!any(active)){payment<-rep(0,n);solver<-'NO_LIABILITIES'}else{
 a<-(diag(nominal,n)-t(mat))/scale;solution<-lpSolve::lp('max',as.numeric(active),rbind(a,diag(n)),rep('<=',2*n),c(assets/scale,as.numeric(active)),scale=0)
 if(solution$status!=0L) .cr_error('CLEARING_SOLVER','Linear clearing optimization failed');payment<-solution$solution*nominal;solver<-'LINEAR_PROGRAM_GREATEST'
 }
 inflow<-as.numeric(t(relative)%*%payment);fixed<-pmin(nominal,assets+inflow);denom<-ifelse(active,nominal,1);residual<-abs(payment-fixed)/denom
 if(any(payment<0)||any(payment>nominal)||max(residual)>tol) .cr_error('CLEARING_RESIDUAL','Greatest solution fails node-relative fixed-point or liability bounds')
 lower<-rep(0,n);steps<-0L
 for(step in seq_len(as.integer(iterations))){updated<-pmin(nominal,assets+as.numeric(t(relative)%*%lower));steps<-step;change<-max(abs(updated-lower)/denom);lower<-updated;if(change<=tol)break}
 lower_residual<-abs(lower-pmin(nominal,assets+as.numeric(t(relative)%*%lower)))/denom;gap<-(payment-lower)/denom
 if(min(gap)< -tol) .cr_error('CLEARING_RESIDUAL','Monotone lower iterate exceeds greatest solution')
 status<-if(max(abs(gap))<=tol)'NUMERICALLY_ENCLOSED' else if(all(lower_residual==0))'MULTIPLE_CLEARINGS' else 'NOT_ESTABLISHED'
 fraction<-ifelse(active,payment/ifelse(active,nominal,1),0);paid<-mat*fraction;paid_outside<-outside*fraction;equity<-assets+inflow-payment
 if(any(!is.finite(equity))) .cr_error('NUMERICAL_FAILURE','Clearing equity outside finite range');defaults<-ids[active&fraction<1-tol]
 list(external_assets=as.list(assets),nominal_payments=as.list(nominal),clearing_payments=as.list(payment),interbank_payments=lapply(seq_len(n),function(i)as.list(paid[i,])),outside_payments=as.list(paid_outside),equity=as.list(equity),default_set=as.list(defaults),diagnostics=list(solver=solver,node_relative_residual=as.list(residual),max_relative_residual=max(residual),lower_iterate=as.list(lower),lower_relative_residual=as.list(lower_residual),greatest_lower_relative_gap=as.list(gap),lower_iterations=steps,uniqueness=status))
 }
 base<-solve_one(external_assets);scenarios<-list()
 if(!is.null(asset_scenarios)){rows<-.cr_data_rows(asset_scenarios,'asset_scenarios');.cr_data_unique(rows,'scenario_id','asset_scenarios');for(row in rows){.cr_data_required(row,'external_assets');result<-solve_one(row$external_assets);scenarios[[length(scenarios)+1L]]<-list(scenario_id=row$scenario_id,result=result,payment_change=as.list(unlist(result$clearing_payments)-unlist(base$clearing_payments)))}}
 .cr_result(list(entity_ids=as.list(ids),currency=currency,model_assumptions=required,base=base,sensitivity=scenarios),'links.solve_contagion_network',currency,ctx,limitations=c('Static single-currency pro-rata clearing with limited liability and absolute priority, no netting, bankruptcy costs or equity cross-holdings. Liabilities are debtor rows and creditor columns; outside creditors do not recycle payments.','Greatest clearing is selected explicitly. A monotone lower iterate bounds every clearing; a small iterate change alone is not a uniqueness certificate. Defaults use declared relative tolerance. Scenario comparisons hold liabilities fixed and do not estimate group PD.'))
}
