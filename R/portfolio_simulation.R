# Canonical identifier order uses UTF-8 radix collation; numeric loss ordering is unchanged.
.cr_pfs_ids<-function(value,name){if(is.list(value)&&is.null(names(value)))value<-unlist(value,use.names=FALSE);if(!is.character(value)||!length(value)||anyNA(value)||any(!nzchar(value))||anyDuplicated(value)) .cr_error('PORTFOLIO_IDS',paste(name,'must be nonempty distinct strings'));value}
.cr_pfs_matrix<-function(value,name){if(is.list(value)&&is.null(dim(value))){if(!length(value)||any(!vapply(value,function(v)is.list(v)||is.numeric(v)||is.logical(v),logical(1)))) .cr_error('PORTFOLIO_SHAPE',paste(name,'requires rows'));sizes<-vapply(value,length,integer(1));if(length(unique(sizes))!=1L) .cr_error('PORTFOLIO_SHAPE',paste(name,'has ragged rows'));value<-do.call(rbind,lapply(value,unlist))};value<-.cr_numeric(value,name);if(!is.matrix(value)) .cr_error('PORTFOLIO_SHAPE',paste(name,'must be a matrix'));value}
.cr_pfs_rows<-function(value)lapply(seq_len(nrow(value)),function(i)as.list(unname(value[i,])))
.cr_pfs_sum<-function(value){out<-sum(value);if(!is.finite(out)) .cr_error('NUMERICAL_FAILURE','Portfolio sum exceeds finite representation');out}
prepare_credit_portfolio<-function(positions,entity_graph,default_link_policy,portfolio_contract,guarantee_allocations=NULL,netting_contracts=NULL,context=NULL){
 rows<-.cr_data_rows(positions,'positions');contract<-portfolio_contract;ctx<-.cr_result(NULL,'portfolio.context',context=context)$context
 if(!.cr_object(contract)) .cr_error('PORTFOLIO_CONTRACT','Versioned portfolio contract required')
 .cr_data_required(contract,c('portfolio_id','version','currency','as_of','known_at','horizon','time_unit','probability_measure','conditioning','event'))
 if(!identical(contract$probability_measure,'PHYSICAL')||!identical(contract$conditioning,'initially_performing')||!identical(contract$event,'first_default')||!contract$time_unit %in% c('days','months','years')||.cr_link_scalar(contract$horizon,'horizon',0)<=0) .cr_error('PORTFOLIO_CONTRACT','Physical initially-performing first-default target required')
 for(k in c('portfolio_id','version','currency')).cr_string(contract[[k]],k,'LINK_CONTRACT')
 for(k in c('as_of','known_at')).cr_data_date(contract[[k]],k)
 for(k in c('currency','horizon','time_unit','probability_measure','conditioning'))if(!is.null(ctx[[k]])&&!isTRUE(ctx[[k]]==contract[[k]])) .cr_error('TARGET_MISMATCH',paste('Portfolio target differs from context',k))
 graph<-.cr_link_read(entity_graph,'graph_hash');if(!identical(graph$as_of,contract$as_of)||!identical(graph$known_at,contract$known_at)) .cr_error('PORTFOLIO_DATE','Graph must use same valuation and information cutoffs')
 netting<-list()
 for(c in .cr_data_rows(if(is.null(netting_contracts))list()else netting_contracts,'netting_contracts')){
  .cr_data_required(c,c('netting_contract_id','facility_ids','currency','legally_effective','evidence_reference','valid_from','valid_until','known_at'));cid<-c$netting_contract_id
  if(cid %in% names(netting)||c$currency!=contract$currency||!identical(c$legally_effective,TRUE)||.cr_data_date(c$valid_from,'valid_from')>.cr_data_date(contract$as_of,'as_of')||.cr_data_date(contract$as_of,'as_of')>=.cr_data_date(c$valid_until,'valid_until')||.cr_data_date(c$known_at,'known_at')>.cr_data_date(contract$known_at,'known_at')) .cr_error('PORTFOLIO_NETTING','Distinct effective dated netting contracts in reporting currency required')
  .cr_pfs_ids(c$facility_ids,'facility_ids');.cr_string(c$evidence_reference,'evidence_reference','LINK_CONTRACT');netting[[cid]]<-c
 }
 position_ids<-character();claims<-facilities<-lots<-list();used_netting<-character();rows<-rows[order(enc2utf8(vapply(rows,function(r)if(is.null(r$position_id))''else r$position_id,character(1))),method='radix')]
 for(row in rows){
  .cr_data_required(row,c('position_id','claim_id','facility_id','entity_id','currency','ead','facility_ead','sector','region','exposure_basis','collateral_treatment'));pid<-row$position_id;fid<-row$facility_id;ead<-.cr_link_scalar(row$ead,'ead',0);total<-.cr_link_scalar(row$facility_ead,'facility_ead',0)
  if(pid %in% position_ids||row$claim_id %in% names(claims)) .cr_error('PORTFOLIO_DUPLICATE','Position and coverage claim IDs must each be unique')
  position_ids<-c(position_ids,pid);claims[[row$claim_id]]<-row
  for(field in c('sector','region')).cr_string(row[[field]],field,'LINK_CONTRACT')
  if(row$currency!=contract$currency||!row$collateral_treatment %in% c('NONE','ALREADY_IN_CONDITIONAL_LGD')) .cr_error('PORTFOLIO_EFFECT','Explicit reporting currency and nonduplicated collateral treatment required')
  if(row$exposure_basis=='NETTED'){
   cid<-row$netting_contract_id;if(is.null(cid)||!cid %in% names(netting)||!fid %in% unlist(netting[[cid]]$facility_ids)) .cr_error('PORTFOLIO_NETTING','Netted exposure needs effective contract covering facility');used_netting<-union(used_netting,cid)
  }else if(row$exposure_basis!='GROSS'||!is.null(row$netting_contract_id)) .cr_error('PORTFOLIO_NETTING','Declare gross or explicitly contracted already-netted EAD')
  if(fid %in% names(facilities)){
   f<-facilities[[fid]];if(!identical(f$entity_id,row$entity_id)||f$total!=total||!identical(f$basis,row$exposure_basis)||!identical(f$netting_id,row$netting_contract_id)) .cr_error('PORTFOLIO_FACILITY','Facility owner total and basis must agree across lots');f$amounts<-c(f$amounts,ead);facilities[[fid]]<-f
  }else facilities[[fid]]<-list(entity_id=row$entity_id,total=total,basis=row$exposure_basis,netting_id=row$netting_contract_id,amounts=ead)
  lots[[length(lots)+1L]]<-row
 }
 if(!length(lots)) .cr_error('PORTFOLIO_EMPTY','Nonempty exposure portfolio required')
 for(f in facilities){amount<-.cr_pfs_sum(f$amounts);if(abs(amount-f$total)>1e-12*max(abs(amount),abs(f$total))) .cr_error('PORTFOLIO_ALLOCATION','Lots must conserve declared facility exposure')}
 if(!setequal(used_netting,names(netting))) .cr_error('PORTFOLIO_NETTING','Unused netting contracts rejected')
 assignment<-assign_default_groups(entity_graph,default_link_policy,lapply(sort(enc2utf8(as.character(names(facilities))),method='radix'),function(fid)list(facility_id=fid,entity_id=facilities[[fid]]$entity_id)),context=ctx)$value
 mapping<-stats::setNames(lapply(assignment$assignments,`[[`,'group_id'),vapply(assignment$assignments,`[[`,character(1),'entity_id'))
 if(any(vapply(lots,function(row)is.null(mapping[[row$entity_id]]),logical(1)))) .cr_error('PORTFOLIO_DEFAULT_GROUP','Unresolved default equivalence cannot be simulated')
 for(i in seq_along(lots))lots[[i]]$default_group_id<-mapping[[lots[[i]]$entity_id]]
 protection<-allocation_hashes<-list();used_guarantees<-character();reserved<-stats::setNames(rep(0,length(position_ids)),position_ids);group_ids<-unique(vapply(lots,`[[`,character(1),'default_group_id'));allocations<-if(is.null(guarantee_allocations))list()else guarantee_allocations
 if(!is.list(allocations)) .cr_error('PORTFOLIO_GUARANTEE','List of sealed simultaneous coverage allocations required')
 if(length(allocations)&&(is.null(contract$guarantee_loss_rule)||!contract$guarantee_loss_rule %in% c('FIRST_LOSS_CAP_IF_GUARANTOR_SURVIVES_HORIZON','PRO_RATA_EAD_CAPPED_IF_GUARANTOR_SURVIVES_HORIZON'))) .cr_error('PORTFOLIO_GUARANTEE','Explicit terminal-horizon guarantee payoff rule required')
 terminal_date<-format(.hy_horizon_end(contract$as_of,contract),'%Y-%m-%d')
 for(raw in allocations){
  allocation<-.cr_link_read(raw,'allocation_hash');if(allocation$currency!=contract$currency||allocation$as_of!=contract$as_of||allocation$known_at!=contract$known_at) .cr_error('PORTFOLIO_GUARANTEE','Allocation currency and cutoffs must match')
  gids<-unique(vapply(allocation$allocations,`[[`,character(1),'guarantee_id'));if(length(intersect(gids,used_guarantees))) .cr_error('PORTFOLIO_GUARANTEE','Guarantee capacity cannot be reused across allocations');used_guarantees<-union(used_guarantees,gids);allocation_hashes[[length(allocation_hashes)+1L]]<-allocation$allocation_hash
  for(coverage in allocation$allocations){
   cid<-coverage$claim_id;if(coverage$due_at!=terminal_date) .cr_error('PORTFOLIO_GUARANTEE','Reserved guarantee must cover declared terminal claim date')
   if(!cid %in% names(claims)) .cr_error('PORTFOLIO_GUARANTEE','Every coverage claim must map to one lot')
   row<-claims[[cid]];guarantor_group<-mapping[[coverage$guarantor_id]]
   if(is.null(guarantor_group)||coverage$entity_id!=row$entity_id) .cr_error('PORTFOLIO_GUARANTEE','Known guarantor group and matching claim owner required')
   reserved[row$position_id]<-reserved[row$position_id]+coverage$covered_amount;group_ids<-union(group_ids,guarantor_group);protection[[length(protection)+1L]]<-list(position_id=row$position_id,guarantee_id=coverage$guarantee_id,guarantor_group_id=guarantor_group,covered_amount=coverage$covered_amount)
  }
 }
 if(any(vapply(lots,function(row)reserved[[row$position_id]]>row$ead+1e-12*max(1,row$ead),logical(1)))) .cr_error('PORTFOLIO_GUARANTEE','Coverage exceeds current lot exposure')
 value<-.cr_link_seal(list(contract=contract,positions=lots,default_group_ids=as.list(sort(enc2utf8(as.character(group_ids)),method='radix')),default_group_assignment=assignment,guarantees=protection,guarantee_allocation_hashes=allocation_hashes,netting_contracts=unname(netting[sort(enc2utf8(as.character(names(netting))),method='radix')]),total_ead=.cr_pfs_sum(vapply(lots,`[[`,numeric(1),'ead'))),'portfolio_hash')
 .cr_result(value,'portfolio.prepare_credit_portfolio','credit_portfolio',ctx,diagnostics=list(position_count=length(lots),facility_count=length(facilities),default_group_count=length(group_ids)),limitations='Default equivalence follows declared graph policy, not PD averaging. Already-netted EAD is evidenced and never netted again. Collateral belongs to conditional LGD. Guarantee allocations are fixed non-reinstating reservations consumed once. Supplied legal evidence and facility totals are not authenticated.')
}
.cr_pfs_defaults<-function(group_ids,pds,dependence_contract,shared_innovations,n){
 .cr_data_required(dependence_contract,c('kind','group_ids'));if(!identical(unlist(dependence_contract$group_ids),group_ids)) .cr_error('PORTFOLIO_ORDER','Dependence group order differs from frozen portfolio')
 if(!.cr_object(pds)||!setequal(names(pds),group_ids)) .cr_error('PORTFOLIO_PD','Exactly one PD per shared default group required')
 pd<-vapply(group_ids,function(g).cr_link_scalar(pds[[g]],'pd',0),numeric(1));if(any(pd>1)) .cr_error('PORTFOLIO_PD','PD exceeds one')
 if(dependence_contract$kind=='SUPPLIED_DEPENDENT_UNIFORMS'){
  u<-.cr_pfs_matrix(shared_innovations$group_uniforms,'group_uniforms');if(!identical(dim(u),c(n,length(group_ids)))||any(u<0|u>1)) .cr_error('PORTFOLIO_INNOVATIONS','Aligned uniforms in [0,1] required')
  defaults<-sweep(u,2,pd,'<')|matrix(pd==1,n,length(pd),byrow=TRUE);latent<-NULL;variance<-NULL
 }else if(dependence_contract$kind=='GAUSSIAN_FACTORS'){
  .cr_data_required(dependence_contract,c('factor_ids','factor_correlation','loadings'));factors<-.cr_pfs_ids(dependence_contract$factor_ids,'factor_ids');corr<-.cr_pfs_matrix(validate_dependence_matrix(.cr_pfs_matrix(dependence_contract$factor_correlation,'factor_correlation'),factors,matrix_kind='correlation',repair_policy='none')$value$matrix,'correlation');loadings<-.cr_pfs_matrix(dependence_contract$loadings,'loadings');z<-.cr_pfs_matrix(shared_innovations$factor_normals,'factor_normals');e<-.cr_pfs_matrix(shared_innovations$group_normals,'group_normals')
  if(!identical(dim(loadings),c(length(group_ids),length(factors)))||!identical(dim(z),c(n,length(factors)))||!identical(dim(e),c(n,length(group_ids)))) .cr_error('PORTFOLIO_INNOVATIONS','Aligned factor loadings and normal matrices required')
  variance<-rowSums((loadings%*%corr)*loadings);if(any(variance< -1e-12|variance>1+1e-12)) .cr_error('PORTFOLIO_VARIANCE','Systematic latent variance must be in [0,1]')
  variance<-pmin(1,pmax(variance,0));systematic<-z%*%t(.cr_psd_cholesky(corr));latent<-systematic%*%t(loadings)+sweep(e,2,sqrt(1-variance),'*');if(any(!is.finite(latent))) .cr_error('NUMERICAL_FAILURE','Latent paths exceed finite range')
  defaults<-sweep(latent,2,stats::qnorm(pd),'<');u<-matrix(stats::pnorm(latent),nrow=n,ncol=length(pd))
 }else .cr_error('PORTFOLIO_DEPENDENCE','Unsupported dependence contract')
 list(defaults=defaults,u=u,latent=latent,variance=variance)
}
simulate_credit_portfolio<-function(prepared_portfolio,group_pds,dependence_contract,shared_innovations,conditional_loss_paths,levels=c(.95,.99),context=NULL){
 portfolio<-.cr_link_read(prepared_portfolio,'portfolio_hash');contract<-portfolio$contract;ctx<-.cr_result(NULL,'portfolio.context',context=context)$context
 for(k in c('currency','horizon','time_unit','probability_measure','conditioning'))if(!is.null(ctx[[k]])&&!isTRUE(ctx[[k]]==contract[[k]])) .cr_error('TARGET_MISMATCH','Portfolio target differs from context')
 if(!.cr_object(shared_innovations)||!.cr_object(conditional_loss_paths)) .cr_error('PORTFOLIO_PATHS','Explicit shared innovations and conditional loss paths required')
 .cr_data_required(shared_innovations,c('scenario_ids','weights','group_ids','sampling_design'));ids<-.cr_pfs_ids(shared_innovations$scenario_ids,'scenario_ids');groups<-unlist(portfolio$default_group_ids);position_ids<-vapply(portfolio$positions,`[[`,character(1),'position_id');n<-length(ids)
 if(!identical(unlist(shared_innovations$group_ids),groups)) .cr_error('PORTFOLIO_ORDER','Innovation default-group order must match portfolio')
 if(!shared_innovations$sampling_design %in% c('IID','FIXED_WEIGHTED_SCENARIOS','DEPENDENT_OR_QMC','EXACT_STATES')) .cr_error('PORTFOLIO_SAMPLING','Explicit supported sampling design required')
 w<-.cr_pf_weights(unlist(shared_innovations$weights),n);drawn<-.cr_pfs_defaults(groups,group_pds,dependence_contract,shared_innovations,n);defaults<-drawn$defaults;driver_hash<-.cr_hash(list(dependence_contract=dependence_contract,shared_innovations=shared_innovations))
 .cr_data_required(conditional_loss_paths,c('scenario_ids','position_ids','driver_hash','ead','lgd','guarantee_treatment'))
 if(!identical(unlist(conditional_loss_paths$scenario_ids),ids)||!identical(unlist(conditional_loss_paths$position_ids),position_ids)||conditional_loss_paths$driver_hash!=driver_hash||!identical(conditional_loss_paths$guarantee_treatment,'NOT_YET_APPLIED')) .cr_error('PORTFOLIO_PATHS','Loss paths must bind same ordered scenarios positions and drivers before guarantees')
 ead<-.cr_pfs_matrix(conditional_loss_paths$ead,'ead');lgd<-.cr_pfs_matrix(conditional_loss_paths$lgd,'lgd');if(!identical(dim(ead),c(n,length(position_ids)))||!identical(dim(lgd),dim(ead))||any(ead<0)) .cr_error('PORTFOLIO_PATHS','Nonnegative EAD and raw LGD require aligned scenario by position arrays')
 group_columns<-match(vapply(portfolio$positions,`[[`,character(1),'default_group_id'),groups);position_defaults<-defaults[,group_columns,drop=FALSE];gross<-matrix(0,n,length(position_ids));gross[position_defaults]<-ead[position_defaults]*lgd[position_defaults]
 if(any(!is.finite(gross))||any(position_defaults&ead!=0&lgd!=0&gross==0)) .cr_error('NUMERICAL_FAILURE','Conditional default losses exceed finite range')
 net<-gross;payments<-list()
 for(coverage in portfolio$guarantees){
  j<-match(coverage$position_id,position_ids);g<-match(coverage$guarantor_group_id,groups);cap<-coverage$covered_amount;entitlement<-rep(cap,n)
  if(contract$guarantee_loss_rule=='PRO_RATA_EAD_CAPPED_IF_GUARANTOR_SURVIVES_HORIZON'){fraction<-rep(0,n);positive<-ead[,j]>0;fraction[positive]<-cap/ead[positive,j];entitlement<-pmin(pmax(gross[,j],0)*pmin(fraction,1),cap)}
  amount<-pmin(pmax(net[,j],0),entitlement)*position_defaults[,j]*(!defaults[,g]);net[,j]<-net[,j]-amount;payments[[length(payments)+1L]]<-list(position_id=coverage$position_id,guarantee_id=coverage$guarantee_id,payments=as.list(amount))
 }
 total<-apply(net,1,.cr_pfs_sum);distribution<-credit_loss_distribution(total,w,scenario_ids=ids,levels=levels,currency=contract$currency,context=ctx)$value
 value<-list(portfolio_hash=portfolio$portfolio_hash,target=contract,scenario_ids=as.list(ids),position_ids=as.list(position_ids),default_group_ids=as.list(groups),group_defaults=.cr_pfs_rows(defaults),position_defaults=.cr_pfs_rows(position_defaults),group_uniforms=.cr_pfs_rows(drawn$u),group_latent_values=if(is.null(drawn$latent))NULL else .cr_pfs_rows(drawn$latent),systematic_asset_variance=if(is.null(drawn$variance))NULL else as.list(drawn$variance),ead_paths=.cr_pfs_rows(ead),lgd_paths=.cr_pfs_rows(lgd),gross_position_losses=.cr_pfs_rows(gross),net_position_losses=.cr_pfs_rows(net),guarantee_payments=payments,portfolio_loss_paths=as.list(total),distribution=distribution,sampling_design=shared_innovations$sampling_design,driver_hash=driver_hash)
 .cr_result(value,'portfolio.simulate_credit_portfolio','portfolio_loss_paths',ctx,diagnostics=list(shared_innovations_hash=.cr_hash(shared_innovations),loss_paths_hash=.cr_hash(conditional_loss_paths)),limitations='Gaussian innovations require independent standard-normal common/residual draws under declared factor law. Covariance describes latent assets, not Bernoulli outcomes. Supplied dependent uniforms may come from copula methods. Shared driver identity does not prove realism. Each default group has one event. Guarantees pay positive remaining loss only when borrower defaults and guarantor survives, within reserved non-reinstating caps. No hidden PD uplift collateral recovery or additional netting is added.')
}
.cr_pfs_cube<-function(value,name){
 if(is.array(value)&&length(dim(value))==3L)return(.cr_numeric(value,name))
 if(!is.list(value)||!length(value)) .cr_error('PORTFOLIO_SHAPE',paste(name,'requires three axes'))
 rows<-lapply(value,function(v).cr_pfs_matrix(v,name));dims<-dim(rows[[1]]);if(any(!vapply(rows,function(v)identical(dim(v),dims),logical(1)))) .cr_error('PORTFOLIO_SHAPE',paste(name,'has ragged planes'))
 out<-array(0,c(length(rows),dims));for(i in seq_along(rows))out[i,,]<-rows[[i]];out
}
.cr_pfs_cube_rows<-function(value)lapply(seq_len(dim(value)[1]),function(i)lapply(seq_len(dim(value)[2]),function(j)as.list(unname(value[i,j,]))))
stress_portfolio_factor_structure<-function(prepared_portfolio,group_pds,dependence_contract,shared_innovations,conditional_loss_paths,stress_cases,levels=c(.95,.99),context=NULL){
 base<-simulate_credit_portfolio(prepared_portfolio,group_pds,dependence_contract,shared_innovations,conditional_loss_paths,levels=levels,context=context)$value;cases<-.cr_data_rows(stress_cases,'stress_cases');seen<-character();out<-list()
 for(case in cases[order(enc2utf8(vapply(cases,function(c)if(is.null(c$case_id))''else c$case_id,character(1))),method='radix')]){
  .cr_data_required(case,c('case_id','dependence_contract','group_pds','conditional_loss_paths','recovery_response'))
  if(case$case_id %in% seen) .cr_error('PORTFOLIO_STRESS','Distinct stress case IDs required');seen<-c(seen,case$case_id)
  if(!case$recovery_response %in% c('FIXED_PATHS','REPLACED_CONDITIONAL_PATHS')) .cr_error('PORTFOLIO_STRESS','Declare recovery/exposure response to stress')
  if(case$recovery_response=='FIXED_PATHS')for(k in c('scenario_ids','position_ids','ead','lgd','guarantee_treatment'))if(!identical(.cr_hash(case$conditional_loss_paths[[k]]),.cr_hash(conditional_loss_paths[[k]]))) .cr_error('PORTFOLIO_STRESS','Fixed recovery requires unchanged EAD/LGD and identities')
  stressed<-simulate_credit_portfolio(prepared_portfolio,case$group_pds,case$dependence_contract,shared_innovations,case$conditional_loss_paths,levels=levels,context=context)$value
  changes<-unlist(stressed$portfolio_loss_paths)-unlist(base$portfolio_loss_paths);comparison<-incremental_credit_risk(unlist(base$portfolio_loss_paths),changes,unlist(shared_innovations$weights),scenario_ids=unlist(base$scenario_ids),levels=levels,currency=base$target$currency,change_id=case$case_id,context=context)$value
  out[[length(out)+1L]]<-list(case_id=case$case_id,recovery_response=case$recovery_response,stressed=stressed,paired_comparison=comparison)
 }
 .cr_result(list(base=base,stress_cases=out,shared_innovations_hash=.cr_hash(shared_innovations)),'portfolio.stress_portfolio_factor_structure','portfolio_stress',context,diagnostics=list(stress_contracts_hash=.cr_hash(cases)),limitations='Every stress case reuses the same supplied random coordinates or group uniforms. Covariance/loadings are validated without substantive repair. Fixed or replaced conditional EAD/LGD is explicit. Comparisons are paired differences; finite-sample default frequencies can change at fixed marginal PDs.')
}
simulate_portfolio_migrations<-function(initial_grades,transition_matrices,shared_uniforms,migration_contract,valuation_contract=NULL,context=NULL){
 contract<-migration_contract
 if(!.cr_object(contract)||!.cr_object(initial_grades)||!.cr_object(shared_uniforms)) .cr_error('PORTFOLIO_MIGRATION','Explicit migration grade and innovation contracts required')
 .cr_data_required(contract,c('contract_id','version','group_ids','states','default_state','allow_cure','probability_measure','time_grid','time_unit','matrix_mode','innovation_law'))
 groups<-.cr_pfs_ids(contract$group_ids,'group_ids');states<-.cr_pfs_ids(contract$states,'states');times<-.cr_numeric(unlist(contract$time_grid),'time_grid',vector_only=TRUE);g<-length(groups);k<-length(states);h<-length(times)
 if(contract$probability_measure!='PHYSICAL'||!contract$default_state %in% states||!is.logical(contract$allow_cure)||length(contract$allow_cure)!=1L||is.na(contract$allow_cure)||!contract$time_unit %in% c('days','months','years')||any(times<=0)||any(diff(times)<=0)||contract$innovation_law!='CONDITIONALLY_UNIFORM_GIVEN_PRIOR_STATES') .cr_error('PORTFOLIO_MIGRATION','Physical positive grid explicit default/cure and sequential uniform law required')
 if(!setequal(names(initial_grades),groups)||any(!vapply(groups,function(x)initial_grades[[x]] %in% states,logical(1)))) .cr_error('PORTFOLIO_MIGRATION','Known initial grade per group required')
 if(contract$matrix_mode=='HOMOGENEOUS'){
  matrix<-.cr_pfs_matrix(transition_matrices,'transition_matrices');if(!identical(dim(matrix),c(k,k))) .cr_error('PORTFOLIO_MIGRATION','Square homogeneous transition matrix required');a<-array(0,c(h,k,k));for(t in seq_len(h))a[t,,]<-matrix
 }else if(contract$matrix_mode=='TIME_VARYING'){
  a<-.cr_pfs_cube(transition_matrices,'transition_matrices');if(!identical(dim(a),c(h,k,k))) .cr_error('PORTFOLIO_MIGRATION','Time by state by state transition tensor required')
 }else .cr_error('PORTFOLIO_MIGRATION','Explicit homogeneous or time-varying matrix mode required')
 if(any(a<0|a>1)) .cr_error('PORTFOLIO_MIGRATION','Transition entries must be probabilities')
 for(t in seq_len(h)){matrix<-matrix(a[t,,],nrow=k,ncol=k);totals<-rowSums(matrix);if(any(abs(totals-1)>1e-12)) .cr_error('PORTFOLIO_MIGRATION','Transition rows must sum to one');a[t,,]<-matrix/totals}
 d<-match(contract$default_state,states)
 if(!contract$allow_cure&&(any(a[,d,d]!=1)||any(a[,d,setdiff(seq_len(k),d),drop=FALSE]!=0))) .cr_error('PORTFOLIO_MIGRATION','Default must be absorbing unless cure explicitly allowed')
 .cr_data_required(shared_uniforms,c('scenario_ids','group_ids','values','weights'));ids<-.cr_pfs_ids(shared_uniforms$scenario_ids,'scenario_ids');u<-.cr_pfs_cube(shared_uniforms$values,'values');n<-length(ids);w<-.cr_pf_weights(unlist(shared_uniforms$weights),n)
 if(!identical(unlist(shared_uniforms$group_ids),groups)||!identical(dim(u),c(n,h,g))||any(u<0|u>=1)) .cr_error('PORTFOLIO_MIGRATION','Aligned scenario/time/group uniforms in [0,1) required')
 ctx<-.cr_result(NULL,'portfolio.context',context=context)$context
 if((!is.null(ctx$probability_measure)&&ctx$probability_measure!='PHYSICAL')||(!is.null(ctx$time_unit)&&ctx$time_unit!=contract$time_unit)||(!is.null(ctx$horizon)&&ctx$horizon!=tail(times,1))) .cr_error('TARGET_MISMATCH','Migration target conflicts with context')
 current<-matrix(vapply(groups,function(x)match(initial_grades[[x]],states),integer(1)),n,g,byrow=TRUE);history<-array(0L,c(n,h+1L,g));history[,1,]<-current;first<-matrix(NA_real_,n,g);first[current==d]<-0
 for(t in seq_len(h)){
  for(s in seq_len(n))for(j in seq_len(g)){cdf<-cumsum(a[t,current[s,j],]);cdf[k]<-1;current[s,j]<-which(u[s,t,j]<cdf)[1];if(current[s,j]==d&&is.na(first[s,j]))first[s,j]<-times[t]}
  history[,t+1L,]<-current
 }
 terminal<-theoretical<-list();product<-diag(k);for(t in seq_len(h))product<-product%*%matrix(a[t,,],nrow=k,ncol=k)
 for(j in seq_along(groups)){
  terminal[[j]]<-list(group_id=groups[j],probabilities=stats::setNames(lapply(seq_along(states),function(i).cr_pfs_sum(w[current[,j]==i])),states));theoretical[[j]]<-list(group_id=groups[j],probabilities=stats::setNames(lapply(seq_along(states),function(i)unname(product[match(initial_grades[[groups[j]]],states),i])),states))
 }
 marked<-NULL
 if(!is.null(valuation_contract)){
  v<-valuation_contract;if(!.cr_object(v)) .cr_error('PORTFOLIO_VALUATION','Explicit fixed value-by-grade contract required')
  .cr_data_required(v,c('valuation_id','version','currency','valuation_basis','group_ids','states','initial_values','terminal_values_by_grade','cash_distributions'))
  if(!identical(unlist(v$group_ids),groups)||!identical(unlist(v$states),states)||v$valuation_basis!='SUPPLIED_FROZEN_MARKED_VALUES') .cr_error('PORTFOLIO_VALUATION','Aligned fixed marked-value table required; physical transitions are not pricing probabilities')
  .cr_string(v$currency,'currency','LINK_CONTRACT');if(!is.null(ctx$currency)&&ctx$currency!=v$currency) .cr_error('TARGET_MISMATCH','Valuation currency conflicts with context')
  initial<-.cr_numeric(unlist(v$initial_values),'initial_values',vector_only=TRUE);terminal_values<-.cr_pfs_matrix(v$terminal_values_by_grade,'terminal_values_by_grade');cash<-.cr_pfs_matrix(v$cash_distributions,'cash_distributions')
  if(length(initial)!=g||!identical(dim(terminal_values),c(g,k))||!identical(dim(cash),c(n,g))) .cr_error('PORTFOLIO_VALUATION','Initial group values terminal group/state table and scenario/group cash required')
  marks<-matrix(0,n,g);for(s in seq_len(n))for(j in seq_len(g))marks[s,j]<-terminal_values[j,current[s,j]]
  loss<-matrix(initial,n,g,byrow=TRUE)-marks-cash;if(any(!is.finite(loss))) .cr_error('NUMERICAL_FAILURE','Marked changes exceed finite range')
  marked<-list(currency=v$currency,terminal_values=.cr_pfs_rows(marks),cash_distributions=.cr_pfs_rows(cash),marked_loss_paths=.cr_pfs_rows(loss),portfolio_marked_loss_paths=as.list(apply(loss,1,.cr_pfs_sum)),valuation_hash=.cr_hash(v))
 }
 grades<-lapply(seq_len(n),function(i)lapply(seq_len(h+1L),function(t)as.list(states[history[i,t,]])));first_values<-lapply(seq_len(n),function(i)lapply(seq_len(g),function(j)if(is.na(first[i,j]))NULL else first[i,j]))
 value<-list(contract=contract,scenario_ids=as.list(ids),group_ids=as.list(groups),states=as.list(states),time_grid=as.list(c(0,times)),grade_paths=grades,first_default_times=first_values,terminal_empirical=terminal,terminal_markov_probabilities=theoretical,marked_values=marked)
 .cr_result(value,'portfolio.simulate_portfolio_migrations','rating_migration_paths',ctx,diagnostics=list(transition_hash=.cr_hash(.cr_pfs_cube_rows(a)),innovations_hash=.cr_hash(shared_uniforms)),limitations='Physical transitions require sequential uniforms conditionally uniform given past states; simultaneous cross-group dependence is retained. Temporally dependent draws without that law do not establish Markov probabilities. First default time remains fixed even after allowed cure. Optional marks/cash are supplied valuations, not prices inferred from physical probabilities.')
}
