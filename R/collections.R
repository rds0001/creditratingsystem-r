# Dated state coverage, operational cure and pre-action information contracts.
.cr_collections_rule <- function(rule) {
 if(!.cr_object(rule)) .cr_error('COLLECTIONS_RULE','Explicit episode rule required')
 .cr_data_required(rule,c('rule_id','version','problem_states','clean_states','terminal_states','default_states','probation_days','separation_days'))
 groups<-list()
 for(field in c('problem_states','clean_states','terminal_states','default_states')) {
  values<-rule[[field]];if(is.list(values)&&is.null(names(values))){if(any(!vapply(values,function(x)is.character(x)&&length(x)==1L,logical(1)))) .cr_error('COLLECTIONS_RULE','State lists require distinct strings');values<-if(length(values))unlist(values,use.names=FALSE) else character()}
  if(!is.character(values)||anyNA(values)||any(!nzchar(values))||anyDuplicated(values)) .cr_error('COLLECTIONS_RULE','State lists require distinct strings')
  groups[[field]]<-values;rule[[field]]<-as.list(values)
 }
 if(!length(groups$problem_states)||!length(groups$clean_states)||length(intersect(groups$problem_states,groups$clean_states))||length(intersect(groups$problem_states,groups$terminal_states))||length(intersect(groups$clean_states,groups$terminal_states))||!all(groups$default_states %in% groups$problem_states)) .cr_error('COLLECTIONS_RULE','Problem, clean and terminal states must partition declared states; defaults are problem states')
 .cr_data_days(rule$probation_days,'probation_days');.cr_data_days(rule$separation_days,'separation_days');rule
}
.cr_collections_history <- function(states,coverage,cutoff,rule) {
 rows<-.cr_data_rows(states,'states');.cr_data_unique(rows,'state_event_id','states');cov<-.cr_data_rows(coverage,'coverage');.cr_data_unique(cov,'account_id','coverage');ends<-list()
 for(row in cov) {
  .cr_data_required(row,c('observed_from','observed_through','known_at'));start<-.cr_data_date(row$observed_from,'observed_from');end<-.cr_data_date(row$observed_through,'observed_through');known<-.cr_data_date(row$known_at,'known_at')
  if(start>end||known<end) .cr_error('COLLECTIONS_DATE','Invalid state-history coverage')
  if(known<=cutoff)ends[[row$account_id]]<-list(start=start,end=min(end,cutoff))
 }
 by_account<-list();allowed<-unlist(c(rule$problem_states,rule$clean_states,rule$terminal_states))
 for(row in rows) {
  .cr_data_required(row,c('account_id','state','occurred_at','known_at'));date<-.cr_data_date(row$occurred_at,'occurred_at');known<-.cr_data_date(row$known_at,'known_at')
  if(known<date||!is.character(row$state)||length(row$state)!=1L||!(row$state %in% allowed)) .cr_error('COLLECTIONS_STATE','Invalid dated state event')
  if(date<=cutoff&&known<=cutoff)by_account[[row$account_id]]<-c(by_account[[row$account_id]],list(row))
 }
 for(aid in names(by_account)) {
  history<-by_account[[aid]];history<-history[order(vapply(history,`[[`,character(1),'occurred_at'),vapply(history,`[[`,character(1),'state_event_id'),method='radix')]
  if(anyDuplicated(vapply(history,`[[`,character(1),'occurred_at'))) .cr_error('COLLECTIONS_AMBIGUITY','Resolve simultaneous states explicitly')
  if(!is.null(ends[[aid]])&&(!length(history)||.cr_data_date(history[[1]]$occurred_at,'date')>ends[[aid]]$start)) .cr_error('COLLECTIONS_COVERAGE','Coverage requires an initial state at or before observed_from')
  by_account[[aid]]<-history
 }
 list(histories=by_account,ends=ends)
}
.cr_collections_actions <- function(actions,cutoff) {
 rows<-.cr_data_rows(actions,'actions');.cr_data_unique(rows,'action_id','actions');output<-list()
 for(row in rows) {
  .cr_data_required(row,c('account_id','action_type','stage','occurred_at','known_at'))
  if(!is.character(row$stage)||length(row$stage)!=1L||!(row$stage %in% c('ASSIGNED','OFFERED','ACCEPTED','PERFORMED','CANCELLED'))) .cr_error('COLLECTIONS_ACTION','Explicit action stage required')
  d<-.cr_data_date(row$occurred_at,'occurred_at');k<-.cr_data_date(row$known_at,'known_at');if(k<d) .cr_error('COLLECTIONS_DATE','Action known before occurrence')
  if(d<=cutoff&&k<=cutoff)output[[length(output)+1L]]<-row
 }
 output
}
build_collections_episodes <- function(states,actions,coverage,episode_rule,evaluation_cutoff,context=NULL) {
 rule<-.cr_collections_rule(episode_rule);cutoff<-.cr_data_date(evaluation_cutoff,'evaluation_cutoff');h<-.cr_collections_history(states,coverage,cutoff,rule);histories<-h$histories;ends<-h$ends;actions<-.cr_collections_actions(actions,cutoff)
 quiet<-max(rule$probation_days,rule$separation_days);episodes<-list()
 for(aid in sort(as.character(names(histories)),method='radix')) {
  history<-histories[[aid]];current<-candidate<-previous_id<-NULL;ever_default<-FALSE;coverage_start<-ends[[aid]]$start;coverage_end<-ends[[aid]]$end
  close_episode<-function(end,status) {current$ended_at<<-.cr_data_iso(end);current$status<<-status;episodes[[length(episodes)+1L]]<<-current;previous_id<<-current$episode_id;current<<-NULL;candidate<<-NULL}
  for(row in history) {
   when<-.cr_data_date(row$occurred_at,'occurred_at');state<-row$state
   if(!is.null(current)&&!is.null(candidate)) {end<-candidate+quiet;if(when>end&&!is.null(coverage_start)&&coverage_start<=candidate&&coverage_end>=end)close_episode(end,'CURED')}
   if(state %in% unlist(rule$default_states))ever_default<-TRUE
   if(is.null(current)&&state %in% unlist(rule$problem_states))current<-list(episode_id=paste0(aid,':',row$state_event_id),account_id=aid,started_at=row$occurred_at,ended_at=NULL,status='OPEN',previous_episode_id=previous_id,timeline=list(),actions=list(),ever_default=ever_default,observation_cutoff=evaluation_cutoff)
   if(!is.null(current)) {
    current$timeline[[length(current$timeline)+1L]]<-row;current$ever_default<-ever_default
    if(state %in% unlist(rule$terminal_states))close_episode(when,'TERMINATED') else if(state %in% unlist(rule$problem_states))candidate<-NULL else if(state %in% unlist(rule$clean_states)&&is.null(candidate))candidate<-when
   }
  }
  if(!is.null(current)) {
   end<-if(is.null(candidate))NULL else candidate+quiet
   if(!is.null(end)&&!is.null(coverage_start)&&coverage_start<=candidate&&coverage_end>=end)close_episode(end,'CURED') else {current['cure_candidate_at']<-list(if(is.null(candidate))NULL else .cr_data_iso(candidate));episodes[[length(episodes)+1L]]<-current}
  }
 }
 assigned<-character()
 for(i in seq_along(episodes)) {
  ep<-episodes[[i]];if(!('cure_candidate_at' %in% names(ep)))ep['cure_candidate_at']<-list(NULL)
  ep$actions<-Filter(function(row)row$account_id==ep$account_id&&row$occurred_at>=ep$started_at&&(is.null(ep$ended_at)||row$occurred_at<=ep$ended_at),actions)
  assigned<-c(assigned,vapply(ep$actions,`[[`,character(1),'action_id'));ep$episode_hash<-.cr_hash(ep);episodes[[i]]<-ep
 }
 state_history<-list();for(aid in sort(as.character(names(histories)),method='radix'))state_history<-c(state_history,histories[[aid]])
 value<-list(episodes=episodes,state_history=state_history,unlinked_actions=Filter(function(row)!(row$action_id %in% assigned),actions),rule=rule,coverage=.cr_data_rows(coverage,'coverage'),evaluation_cutoff=evaluation_cutoff)
 value$history_hash<-.cr_hash(value)
 .cr_result(value,'collections.build_collections_episodes','episodes',context,diagnostics=.cr_empty_object(),limitations=c('Caller supplies complete dated state changes and independent coverage evidence. No regulatory cure or automatic legal contract change is inferred.','Quiet interval is max(probation_days,separation_days); a problem event exactly at its end interrupts cure.'))
}
build_cure_targets <- function(episodes,horizon_days,redefault_horizon_days,evaluation_cutoff,context=NULL) {
 source<-if(inherits(episodes,'CalcResult'))episodes$value else episodes
 if(!.cr_object(source)) .cr_error('COLLECTIONS_INPUT','Episode result required');.cr_data_required(source,c('episodes','state_history','rule','coverage','evaluation_cutoff'))
 sealed<-source;digest<-sealed$history_hash;sealed$history_hash<-NULL;if(!identical(digest,.cr_hash(sealed))) .cr_error('INTEGRITY_ERROR','Episode history or rules changed since construction')
 cutoff<-.cr_data_date(evaluation_cutoff,'evaluation_cutoff');if(cutoff!=.cr_data_date(source$evaluation_cutoff,'episode cutoff')) .cr_error('COLLECTIONS_CUTOFF','Rebuild episodes at the requested evaluation cutoff')
 horizon<-.cr_data_days(horizon_days,'horizon_days',1);red_horizon<-.cr_data_days(redefault_horizon_days,'redefault_horizon_days',1);rule<-.cr_collections_rule(source$rule);by_account<-cov<-list()
 for(row in source$state_history)by_account[[row$account_id]]<-c(by_account[[row$account_id]],list(row))
 for(row in .cr_data_rows(source$coverage,'coverage'))if(.cr_data_date(row$known_at,'known_at')<=cutoff)cov[[row$account_id]]<-list(start=.cr_data_date(row$observed_from,'observed_from'),end=.cr_data_date(row$observed_through,'observed_through'))
 for(ep in source$episodes) {
  payload<-ep;digest<-payload$episode_hash;payload$episode_hash<-NULL;if(!identical(digest,.cr_hash(payload))) .cr_error('INTEGRITY_ERROR','Episode changed since construction')
 }
 output<-list()
 for(ep in source$episodes) {
  start<-.cr_data_date(ep$started_at,'started_at');end<-start+horizon;history<-by_account[[ep$account_id]];history<-history[!duplicated(vapply(history,`[[`,character(1),'state_event_id'),fromLast=TRUE)];history<-history[order(vapply(history,`[[`,character(1),'occurred_at'),method='radix')]
  coverage<-cov[[ep$account_id]];candidate<-cure<-exit_at<-NULL
  for(row in history) {
   when<-.cr_data_date(row$occurred_at,'occurred_at');if(when<start)next
   if(!is.null(candidate)) {proposed<-candidate+rule$probation_days;if(when>proposed&&!is.null(coverage)&&coverage$start<=candidate&&coverage$end>=proposed){cure<-proposed;break}}
   if(row$state %in% unlist(rule$problem_states))candidate<-NULL else if(row$state %in% unlist(rule$clean_states)&&is.null(candidate))candidate<-when else if(row$state %in% unlist(rule$terminal_states)){exit_at<-when;candidate<-NULL;break}
  }
  if(is.null(cure)&&!is.null(candidate)&&!is.null(coverage)){proposed<-candidate+rule$probation_days;if(coverage$start<=candidate&&coverage$end>=proposed&&proposed<=cutoff)cure<-proposed}
  observed_cure<-!is.null(cure)&&cure<=min(end,cutoff);complete<-!is.null(coverage)&&coverage$start<=start&&coverage$end>=end&&cutoff>=end;exited<-!is.null(exit_at)&&exit_at<=min(end,cutoff)
  label<-if(observed_cure)1 else if(complete&&!exited)0 else NULL;status<-if(observed_cure)'SUSTAINED_CURE' else if(exited)'CENSORED_EXIT' else if(complete)'MATURE_NO_CURE' else 'CENSORED'
  redate<-relabel<-NULL;restatus<-'NOT_IN_POST_CURE_RISK_SET'
  if(observed_cure) {
   rend<-cure+red_horizon;later<-Filter(function(row){when<-.cr_data_date(row$occurred_at,'date');cure<when&&when<=min(rend,cutoff)&&row$state %in% unlist(c(rule$default_states,rule$terminal_states))},history)
   if(length(later)&&later[[1]]$state %in% unlist(rule$default_states)){redate<-later[[1]]$occurred_at;relabel<-1;restatus<-'REDEFAULT'} else if(length(later))restatus<-'CENSORED_EXIT' else if(!is.null(coverage)&&coverage$start<=cure&&coverage$end>=rend&&cutoff>=rend){relabel<-0;restatus<-'MATURE_NO_REDEFAULT'} else restatus<-'CENSORED'
  }
  output[[length(output)+1L]]<-list(episode_id=ep$episode_id,account_id=ep$account_id,horizon_end=.cr_data_iso(end),cure_label=label,cure_status=status,cure_at=if(observed_cure).cr_data_iso(cure) else NULL,redefault_label=relabel,redefault_status=restatus,redefault_at=redate,ever_default=ep$ever_default,evaluation_cutoff=evaluation_cutoff)
 }
 .cr_result(output,'collections.build_cure_targets','labels',context,diagnostics=.cr_empty_object(),limitations='Operational sustained cure follows the supplied probation rule; it never erases historical default. Termination censors rather than fabricates cure.')
}
build_collections_action_history <- function(actions,features,outcomes,coverage,index_stage,required_features,horizon_days,maximum_feature_age_days,evaluation_cutoff,context=NULL) {
 cutoff<-.cr_data_date(evaluation_cutoff,'evaluation_cutoff');actions<-.cr_collections_actions(actions,cutoff)
 if(!is.character(index_stage)||length(index_stage)!=1L||!(index_stage %in% c('ASSIGNED','OFFERED'))) .cr_error('COLLECTIONS_ACTION','Index must be assignment or offer')
 if(is.list(required_features)&&is.null(names(required_features)))required_features<-unlist(required_features,use.names=FALSE)
 if(!is.character(required_features)||!length(required_features)||anyNA(required_features)||any(!nzchar(required_features))||anyDuplicated(required_features)) .cr_error('COLLECTIONS_FEATURES','Distinct required feature names needed')
 horizon<-.cr_data_days(horizon_days,'horizon_days',1);age<-.cr_data_days(maximum_feature_age_days,'maximum_feature_age_days');features<-.cr_data_rows(features,'features');outcomes<-.cr_data_rows(outcomes,'outcomes');cov<-.cr_data_rows(coverage,'coverage');.cr_data_unique(features,'feature_record_id','features');.cr_data_unique(outcomes,'outcome_id','outcomes');.cr_data_unique(cov,'account_id','coverage')
 for(f in features) {.cr_data_required(f,c('account_id','feature','observed_at','known_at'));if(!('value' %in% names(f))) .cr_error('COLLECTIONS_FEATURES','Feature value key required, null allowed');if(.cr_data_date(f$known_at,'known_at')<.cr_data_date(f$observed_at,'observed_at')) .cr_error('COLLECTIONS_DATE','Feature known before observation')}
 for(o in outcomes) {.cr_data_required(o,c('account_id','event_type','occurred_at','known_at'));if(.cr_data_date(o$known_at,'known_at')<.cr_data_date(o$occurred_at,'occurred_at')) .cr_error('COLLECTIONS_DATE','Outcome known before occurrence')}
 ends<-list()
 for(c in cov) {.cr_data_required(c,c('observed_from','observed_through','known_at'));start<-.cr_data_date(c$observed_from,'observed_from');end<-.cr_data_date(c$observed_through,'observed_through');known<-.cr_data_date(c$known_at,'known_at');if(start>end||known<end) .cr_error('COLLECTIONS_DATE','Invalid outcome coverage');if(known<=cutoff)ends[[c$account_id]]<-list(start=start,end=end)}
 groups<-list()
 for(action in actions) {.cr_data_required(action,'treatment_id');if(!is.character(action$treatment_id)||length(action$treatment_id)!=1L||!nzchar(action$treatment_id)) .cr_error('COLLECTIONS_ACTION','Treatment ID must be a nonempty string');groups[[action$treatment_id]]<-c(groups[[action$treatment_id]],list(action))}
 rows<-list();unindexed<-character()
 for(tid in sort(as.character(names(groups)),method='radix')) {
  history<-groups[[tid]];identities<-vapply(history,function(a).cr_data_key(a,c('account_id','action_type')),character(1));if(length(unique(identities))!=1L) .cr_error('COLLECTIONS_ACTION','Treatment stages must retain account and action type')
  stages<-vapply(history,`[[`,character(1),'stage');if(anyDuplicated(stages)) .cr_error('COLLECTIONS_ACTION','One record per stage; use separate treatment IDs for repeated offers')
  indexed<-Filter(function(a)a$stage==index_stage,history);if(!length(indexed)){unindexed<-c(unindexed,tid);next}
  index<-indexed[[1]];date<-.cr_data_date(index$occurred_at,'index_at');aid<-index$account_id;end<-date+horizon;stage_dates<-setNames(lapply(history,`[[`,'occurred_at'),stages);rank<-c('ASSIGNED','OFFERED','ACCEPTED','PERFORMED');dated<-unlist(stage_dates[rank[rank %in% names(stage_dates)]],use.names=FALSE);invalid_order<-!identical(dated,sort(dated,method='radix'))
  selected<-source_ids<-.cr_empty_object();missing<-stale<-character()
  for(name in required_features) {
   available<-Filter(function(f)f$account_id==aid&&f$feature==name&&.cr_data_date(f$observed_at,'observed_at')<date&&.cr_data_date(f$known_at,'known_at')<date,features)
   if(length(available))available<-available[order(vapply(available,`[[`,character(1),'observed_at'),vapply(available,`[[`,character(1),'known_at'),vapply(available,`[[`,character(1),'feature_record_id'),method='radix')]
   n<-length(available);if(n>1L&&identical(available[[n]][c('observed_at','known_at')],available[[n-1L]][c('observed_at','known_at')])) .cr_error('COLLECTIONS_AMBIGUITY','Resolve tied pre-action feature revisions')
   record<-if(n)available[[n]] else NULL;selected[name]<-list(if(is.null(record))NULL else record$value);source_ids[name]<-list(if(is.null(record))NULL else record$feature_record_id)
   if(is.null(record)||is.null(record$value))missing<-c(missing,name) else if(as.numeric(date-.cr_data_date(record$observed_at,'date'))>age)stale<-c(stale,name)
  }
  known_outcomes<-Filter(function(o)o$account_id==aid&&.cr_data_date(o$known_at,'known_at')<=cutoff&&.cr_data_date(o$occurred_at,'occurred_at')<=cutoff,outcomes)
  prior<-vapply(Filter(function(o).cr_data_date(o$occurred_at,'occurred_at')<=date,known_outcomes),`[[`,character(1),'outcome_id');subsequent<-Filter(function(o){when<-.cr_data_date(o$occurred_at,'occurred_at');date<when&&when<=end},known_outcomes)
  complete<-!is.null(ends[[aid]])&&ends[[aid]]$start<=date&&ends[[aid]]$end>=end&&cutoff>=end
  row<-list(treatment_id=tid,account_id=aid,action_type=index$action_type,index_stage=index_stage,index_at=index$occurred_at,index_recorded_at=index$known_at,preaction_features=selected,feature_sources=source_ids,missing_features=as.list(missing),stale_features=as.list(stale),descriptive_eligible=!length(missing)&&!length(stale)&&!invalid_order,stage_dates=stage_dates,invalid_stage_order=invalid_order,accepted='ACCEPTED' %in% names(stage_dates),performed='PERFORMED' %in% names(stage_dates),prior_or_same_day_outcome_ids=as.list(prior),outcomes=subsequent,horizon_end=.cr_data_iso(end),outcome_complete=complete,evaluation_cutoff=evaluation_cutoff,causal_effect_identified=FALSE)
  row$row_hash<-.cr_hash(row);rows[[length(rows)+1L]]<-row
 }
 .cr_result(list(rows=rows,unindexed_treatments=as.list(unindexed)),'collections.build_collections_action_history','panel',context,diagnostics=.cr_empty_object(),limitations=c('Post-index stages and outcomes are separate from pre-action features. An offered plan is not a performed treatment.','Same-day ordering is unresolved at date resolution and such outcomes are flagged as prior or same-day, never prospective.','Factual action groups may be selected. No causal effect or treatment eligibility decision is inferred.'))
}
.cr_collections_currency_context <- function(currency,context) {if(!is.character(currency)||length(currency)!=1L||is.na(currency)||!nzchar(currency)) .cr_error('COLLECTIONS_CURRENCY','Currency required');ctx<-.cr_result(NULL,'collections.context',context=context)$context;if(!is.null(ctx$currency)&&!identical(ctx$currency,currency)) .cr_error('TARGET_MISMATCH','Context currency conflicts with collections currency');ctx}
.cr_collections_scalar <- function(value,name,minimum=0) {x<-.cr_numeric(value,name);if(length(x)!=1L||!is.null(dim(x))||x<minimum) .cr_error('COLLECTIONS_AMOUNT',paste(name,'must be a finite scalar in its permitted range'));as.numeric(x)}
.cr_collections_finite <- function(value) {if(any(!is.finite(value))) .cr_error('NUMERICAL_FAILURE','Collections calculation outside finite range');as.numeric(value)}
.cr_collections_dates <- function(period_end_dates) {
 if(is.list(period_end_dates)&&is.null(names(period_end_dates))) {if(any(!vapply(period_end_dates,function(x)is.character(x)&&length(x)==1L,logical(1)))) .cr_error('COLLECTIONS_DATE','Explicit period dates required');period_end_dates<-unlist(period_end_dates,use.names=FALSE)}
 if(!is.character(period_end_dates)||!length(period_end_dates)) .cr_error('COLLECTIONS_DATE','Explicit period dates required')
 dates<-lapply(period_end_dates,.cr_data_date,name='period_end_date');if(any(diff(as.numeric(do.call(c,dates)))<=0)) .cr_error('COLLECTIONS_DATE','Increasing period dates required');list(strings=period_end_dates,dates=dates)
}
simulate_repayment_arrangement <- function(contract,baseline,candidate,affordability_paths,discount_factors,period_end_dates,currency,context=NULL) {
 context<-.cr_collections_currency_context(currency,context)
 if(!.cr_object(contract)) .cr_error('COLLECTIONS_INPUT','Contract object required');.cr_data_required(contract,c('currency','principal','principal_arrears','accrued_interest','fees','as_of'))
 if(!identical(contract$currency,currency)) .cr_error('COLLECTIONS_CURRENCY','Contract currency mismatch');start<-.cr_data_date(contract$as_of,'as_of')
 p0<-.cr_collections_scalar(contract$principal,'principal');arrears<-.cr_collections_scalar(contract$principal_arrears,'principal_arrears');i0<-.cr_collections_scalar(contract$accrued_interest,'accrued_interest');f0<-.cr_collections_scalar(contract$fees,'fees')
 if(arrears>p0) .cr_error('COLLECTIONS_AMOUNT','Arrears principal must be a subset of outstanding principal')
 parsed<-.cr_collections_dates(period_end_dates);dates<-parsed$dates;period_end_dates<-parsed$strings;n<-length(dates);if(dates[[1]]<=start) .cr_error('COLLECTIONS_DATE','Increasing future period ends required')
 discounts<-.cr_numeric(discount_factors,'discount_factors',vector_only=TRUE);if(length(discounts)!=n||any(discounts<0)) .cr_error('COLLECTIONS_DISCOUNT','Nonnegative factor per period required')
 budgets<-.cr_data_rows(affordability_paths,'affordability_paths');.cr_data_unique(budgets,'path_id','affordability_paths')
 for(row in budgets) {.cr_data_required(row,c('currency','available_cash'));cash<-.cr_numeric(row$available_cash,'available_cash',vector_only=TRUE);if(!identical(row$currency,currency)||length(cash)!=n) .cr_error('COLLECTIONS_AFFORDABILITY','Budget path currency/period mismatch')}
 project<-function(spec) {
  if(!.cr_object(spec)) .cr_error('COLLECTIONS_INPUT','Schedule specification required');.cr_data_required(spec,c('plan_id','payments','period_interest_rates','new_fees','servicing_costs','receipt_fractions','capitalize_unpaid_interest'))
  if(!is.logical(spec$capitalize_unpaid_interest)||length(spec$capitalize_unpaid_interest)!=1L||is.na(spec$capitalize_unpaid_interest)) .cr_error('COLLECTIONS_INPUT','Explicit Boolean capitalization choice required')
  arrays<-list();for(name in c('payments','period_interest_rates','new_fees','servicing_costs','receipt_fractions')) {x<-.cr_numeric(spec[[name]],name,vector_only=TRUE);if(length(x)!=n||any(x<0)||(name=='receipt_fractions'&&any(x>1))) .cr_error('COLLECTIONS_SCHEDULE','Invalid schedule amount/rate/fraction vector');arrays[[name]]<-x}
  principal<-p0;interest<-i0;fees<-f0;rows<-households<-list();pv<-numeric()
  for(j in seq_along(period_end_dates)) {
   date<-period_end_dates[j];opening<-principal;accrued<-.cr_collections_finite(principal*arrays$period_interest_rates[j]);interest<-.cr_collections_finite(interest+accrued);fees<-.cr_collections_finite(fees+arrays$new_fees[j]);due<-.cr_collections_finite(principal+interest+fees);scheduled<-min(arrays$payments[j],due);received<-.cr_collections_finite(scheduled*arrays$receipt_fractions[j]);remaining<-received
   paid_fees<-min(remaining,fees);remaining<-remaining-paid_fees;fees<-fees-paid_fees;paid_interest<-min(remaining,interest);remaining<-remaining-paid_interest;interest<-interest-paid_interest;paid_principal<-min(remaining,principal);principal<-principal-paid_principal
   capitalized<-if(spec$capitalize_unpaid_interest)interest else 0;principal<-.cr_collections_finite(principal+capitalized);interest<-interest-capitalized;net<-.cr_collections_finite(received-arrays$servicing_costs[j]);discounted<-.cr_collections_finite(net*discounts[j]);pv<-c(pv,discounted)
   rows[[j]]<-list(period_end=date,opening_principal=opening,interest_accrued=accrued,new_fees=as.numeric(arrays$new_fees[j]),scheduled_payment=scheduled,received_payment=received,unused_payment_capacity=as.numeric(arrays$payments[j])-scheduled,paid_fees=paid_fees,paid_interest=paid_interest,paid_principal=paid_principal,capitalized_interest=capitalized,closing_principal=principal,unpaid_interest=interest,unpaid_fees=fees,servicing_cost=as.numeric(arrays$servicing_costs[j]),net_cashflow=net,discounted_net_cashflow=discounted)
  }
  for(row in budgets) {residual<-.cr_collections_finite(row$available_cash-vapply(rows,`[[`,numeric(1),'scheduled_payment'));households[[length(households)+1L]]<-list(path_id=row$path_id,residual_cash=as.list(residual),shortfall_periods=as.list(period_end_dates[residual<0]))}
  list(plan_id=spec$plan_id,schedule=rows,npv=.cr_collections_finite(sum(pv)),remaining_claim=.cr_collections_finite(principal+interest+fees),affordability=households)
 }
 base<-project(baseline);new<-project(candidate)
 .cr_result(list(baseline=base,candidate=new,npv_change=.cr_collections_finite(new$npv-base$npv),principal_arrears_included=arrears,contract_changed=FALSE,default_status_changed=FALSE),'collections.simulate_repayment_arrangement',currency,context,diagnostics=list(payment_waterfall=list('fees','interest','principal'),interest_convention='effective_period_rate_on_opening_principal'),limitations=c('Explicit receipt fractions are deterministic scenario assumptions, not estimated cure probabilities. Remaining claims carry no assumed terminal recovery value.','Affordability subtracts scheduled obligations, not reduced receipts. This local calculation does not implement the proposal or change legal/default status.'))
}
expected_collections_value <- function(actions,discount_factors,ead,currency,period_end_dates,context=NULL) {
 context<-.cr_collections_currency_context(currency,context)
 exposure<-.cr_collections_scalar(ead,'ead');rows<-.cr_data_rows(actions,'actions');.cr_data_unique(rows,'action_id','actions');if(!length(rows)) .cr_error('COLLECTIONS_INPUT','At least the baseline action is required')
 parsed<-.cr_collections_dates(period_end_dates);dates<-parsed$dates;period_end_dates<-parsed$strings;df<-.cr_numeric(discount_factors,'discount_factors',vector_only=TRUE);if(length(df)!=length(dates)||any(df<0)) .cr_error('COLLECTIONS_DISCOUNT','Nonnegative factor per period required');output<-list()
 for(action in rows) {
  .cr_data_required(action,c('currency','uptake_probability','accepted_scenarios','declined_scenarios','offer_cost','support_status','assumption_reference'));if(!identical(action$currency,currency)) .cr_error('COLLECTIONS_CURRENCY','Action currency mismatch')
  uptake<-.cr_probability(action$uptake_probability);if(length(uptake)!=1L||!is.null(dim(uptake))) .cr_error('COLLECTIONS_INPUT','Scalar uptake required');offer_cost<-.cr_collections_scalar(action$offer_cost,'offer_cost');scenarios<-list()
  for(branch in c('accepted','declined')) {
   mass<-if(branch=='accepted')uptake else 1-uptake;paths<-.cr_data_rows(action[[paste0(branch,'_scenarios')]],paste0(branch,'_scenarios'));.cr_data_unique(paths,'scenario_id','scenarios');if(!length(paths)) .cr_error('COLLECTIONS_SCENARIOS','Supply complete conditional scenarios for both uptake branches');probabilities<-numeric()
   for(path in paths) {
    .cr_data_required(path,c('probability','cashflows'));probability<-.cr_probability(path$probability);if(length(probability)!=1L||!is.null(dim(probability))) .cr_error('COLLECTIONS_SCENARIOS','Scalar conditional probability required');probabilities<-c(probabilities,probability);cashflows<-.cr_data_rows(path$cashflows,'cashflows');.cr_data_unique(cashflows,'cashflow_id','cashflows');recovery<-cost<-numeric()
    for(cash in cashflows) {
     .cr_data_required(cash,c('kind','period_end','amount'));if(!(cash$kind %in% c('RECOVERY','COST'))||!(cash$period_end %in% period_end_dates)) .cr_error('COLLECTIONS_CASHFLOW','Known period and RECOVERY/COST kind required');amount<-.cr_collections_scalar(cash$amount,'cashflow amount');discount<-df[match(cash$period_end,period_end_dates)];v<-.cr_collections_finite(amount*discount);if(cash$kind=='RECOVERY')recovery<-c(recovery,v) else cost<-c(cost,v)
    }
    r<-.cr_collections_finite(sum(recovery));c<-.cr_collections_finite(sum(cost));scenarios[[length(scenarios)+1L]]<-list(branch=branch,scenario_id=path$scenario_id,probability=as.numeric(mass*probability),discounted_recovery=r,discounted_cost=c,net_value=.cr_collections_finite(r-c-offer_cost))
   }
   if(abs(sum(probabilities)-1)>1e-12) .cr_error('COLLECTIONS_SCENARIOS','Conditional scenario masses must sum to one')
  }
  mean<-.cr_collections_finite(sum(vapply(scenarios,function(s)s$probability*s$net_value,numeric(1))));positive<-vapply(Filter(function(s)s$probability>0,scenarios),`[[`,numeric(1),'net_value')
  output[[length(output)+1L]]<-list(action_id=action$action_id,expected_net_recovery=mean,expected_net_recovery_to_ead=if(exposure==0)NULL else .cr_collections_finite(mean/exposure),scenario_min=min(positive),scenario_max=max(positive),scenarios=scenarios,support_status=action$support_status,assumption_reference=action$assumption_reference,causal_effect_identified=FALSE)
 }
 .cr_result(output,'collections.expected_collections_value',currency,context,diagnostics=.cr_empty_object(),limitations=c('Uptake and conditional scenario masses form exclusive branches. Offer cost occurs once at time zero; scenario costs must exclude that same cost.','Scenario ranges are not confidence intervals. Values are conditional projections, not identified treatment effects; action optimization is separate.','Cashflow IDs prevent repeated records within a scenario; callers must assign one ID per economic cashflow.'))
}
