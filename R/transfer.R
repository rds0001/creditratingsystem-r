# Dated payment restrictions and explicit, mutually exclusive default states.
.cr_transfer_object <- function(x,name) {
 if(inherits(x,'CalcResult'))x<-x$value
 if(!.cr_object(x)) .cr_error('TRANSFER_CONTRACT',paste(name,'must be a declared object'))
 x
}
.cr_transfer_names <- function(x,name,empty=FALSE) {
 if(is.list(x)&&is.null(names(x))&&all(vapply(x,function(v)is.character(v)&&length(v)==1L&&!is.na(v),logical(1))))x<-if(length(x))unlist(x,use.names=FALSE) else character()
 if(!is.character(x)||(!empty&&!length(x))||anyNA(x)||any(!nzchar(x))||anyDuplicated(x)) .cr_error('TRANSFER_CONTRACT',paste(name,'must contain distinct named values'))
 x
}
.cr_transfer_p <- function(x,name) {x<-.cr_probability(x,name);if(length(x)!=1L||!is.null(dim(x))) .cr_error('TRANSFER_PROBABILITY',paste(name,'must be scalar'));as.numeric(x)}
.cr_transfer_target <- function(target,context) {
 target<-.cr_transfer_object(target,'target');.cr_data_required(target,c('exposure_id','horizon','time_unit','probability_measure','conditioning'))
 for(field in c('exposure_id','conditioning')) .cr_string(target[[field]],field,'TRANSFER_CONTRACT')
 h<-.cr_numeric(target$horizon,'horizon')
 if(length(h)!=1L||!is.null(dim(h))||h<=0||length(target$time_unit)!=1L||!(target$time_unit %in% c('days','months','years'))||length(target$probability_measure)!=1L||!(target$probability_measure %in% c('PHYSICAL','RISK_NEUTRAL'))) .cr_error('TRANSFER_TARGET','Explicit positive horizon, time unit and probability measure required')
 ctx<-.cr_result(NULL,'transfer.context',context=context)$context
 for(field in c('horizon','time_unit','probability_measure','conditioning'))if(!is.null(ctx[[field]])&&!isTRUE(all.equal(ctx[[field]],target[[field]]))) .cr_error('TARGET_MISMATCH','Context differs from transfer target')
 list(target=target,context=ctx)
}
assess_transfer_applicability <- function(payments,event_policy,as_of,known_at,horizon_end,context=NULL) {
 start<-.cr_data_date(as_of,'as_of');known<-.cr_data_date(known_at,'known_at');end<-.cr_data_date(horizon_end,'horizon_end')
 if(known<start||end<start) .cr_error('TRANSFER_DATE','Require as_of<=known_at and as_of<=horizon_end')
 rows<-.cr_data_rows(payments,'payments');.cr_data_unique(rows,'payment_id','payments');policy<-.cr_transfer_object(event_policy,'event_policy')
 .cr_data_required(policy,c('policy_id','version','events'));.cr_string(policy$policy_id,'policy_id','TRANSFER_CONTRACT');.cr_string(policy$version,'version','TRANSFER_CONTRACT')
 events<-.cr_data_rows(policy$events,'events');.cr_data_unique(events,'event_id','events');findings<-list()
 for(event in events) {
  .cr_data_required(event,c('event_type','jurisdictions','currencies','effective_from','known_at','exemptions'))
  if(length(event$event_type)!=1L||!(event$event_type %in% c('CONVERSION_RESTRICTION','PAYMENT_FREEZE','BORDER_TRANSFER_BLOCK'))) .cr_error('TRANSFER_EVENT','Unsupported explicit event type')
  .cr_transfer_names(event$jurisdictions,'jurisdictions');.cr_transfer_names(event$currencies,'currencies')
  begin<-.cr_data_date(event$effective_from,'effective_from');.cr_data_date(event$known_at,'known_at');finish<-if(is.null(event$effective_until))NULL else .cr_data_date(event$effective_until,'effective_until')
  if(!is.null(finish)&&finish<=begin) .cr_error('TRANSFER_DATE','Empty event applicability interval')
  exemptions<-.cr_data_rows(event$exemptions,'exemptions');.cr_data_unique(exemptions,'payment_id','exemptions')
  for(exemption in exemptions) {.cr_data_required(exemption,c('reason','evidence_reference','known_at'));.cr_string(exemption$reason,'reason','TRANSFER_CONTRACT');.cr_string(exemption$evidence_reference,'evidence_reference','TRANSFER_CONTRACT');.cr_data_date(exemption$known_at,'known_at')}
 }
 for(payment in rows) {
  .cr_data_required(payment,c('due_at','known_at'));due<-.cr_data_date(payment$due_at,'due_at');pk<-.cr_data_date(payment$known_at,'known_at')
  if(pk>known||due<start||due>end) {findings[[length(findings)+1L]]<-list(payment_id=payment$payment_id,status='OUTSIDE_KNOWN_WINDOW',events=list());next}
  fields<-c('currency','source_currency','payer_jurisdiction','payee_jurisdiction','payment_jurisdictions');missing<-fields[vapply(fields,function(f)is.null(payment[[f]]),logical(1))]
  if(length(missing)){findings[[length(findings)+1L]]<-list(payment_id=payment$payment_id,status='INSUFFICIENT_INFORMATION',missing=as.list(missing),events=list());next}
  for(field in fields[-length(fields)]) .cr_string(payment[[field]],field,'TRANSFER_CONTRACT')
  routes<-.cr_transfer_names(payment$payment_jurisdictions,'payment_jurisdictions');jurisdictions<-unique(c(routes,payment$payer_jurisdiction,payment$payee_jurisdiction));applicable<-list()
  for(event in events) {
   begin<-.cr_data_date(event$effective_from,'effective_from');finish<-if(is.null(event$effective_until))NULL else .cr_data_date(event$effective_until,'effective_until')
   if(.cr_data_date(event$known_at,'known_at')>known||due<begin||(!is.null(finish)&&due>=finish))next
   if(!(payment$currency %in% unlist(event$currencies))||!length(intersect(jurisdictions,unlist(event$jurisdictions))))next
   if(event$event_type=='CONVERSION_RESTRICTION'&&payment$currency==payment$source_currency)next
   if(event$event_type=='BORDER_TRANSFER_BLOCK'&&length(jurisdictions)==1L)next
   exemptions<-Filter(function(x)x$payment_id==payment$payment_id&&.cr_data_date(x$known_at,'known_at')<=known,event$exemptions)
   applicable[[length(applicable)+1L]]<-list(event_id=event$event_id,event_type=event$event_type,status=if(length(exemptions))'EXEMPT' else 'APPLICABLE',exemption=if(length(exemptions))exemptions[[1L]] else NULL)
  }
  findings[[length(findings)+1L]]<-list(payment_id=payment$payment_id,status=if(any(vapply(applicable,function(x)x$status=='APPLICABLE',logical(1))))'APPLICABLE' else if(length(applicable))'EXEMPT' else 'NO_MATCHING_EVENT',events=applicable)
 }
 .cr_result(list(policy_id=policy$policy_id,policy_version=policy$version,payments=findings),'transfer.assess_transfer_applicability','applicability',context,diagnostics=list(as_of=as_of,known_at=known_at,horizon_end=horizon_end,policy_hash=.cr_hash(policy)),limitations=c('Rules are supplied economic/legal applicability assumptions, not legal determinations. Equal currencies do not exempt a payment freeze.','Event effective intervals describe policy applicability, not observed transfer-event occurrence or probability.'))
}
transfer_event_default_probability <- function(transfer_event_probability,event_to_default_probability,mitigation_contract,event_id,target,context=NULL) {
 contract<-.cr_transfer_target(target,context);.cr_string(event_id,'event_id','TRANSFER_CONTRACT');p<-.cr_transfer_p(transfer_event_probability,'transfer_event_probability');failure<-.cr_transfer_p(event_to_default_probability,'event_to_default_probability')
 mitigation<-.cr_transfer_object(mitigation_contract,'mitigation_contract');.cr_data_required(mitigation,c('effect_id','success_probability_given_event_and_unmitigated_failure','conditioning','assumption_reference'))
 .cr_string(mitigation$effect_id,'effect_id','TRANSFER_CONTRACT');.cr_string(mitigation$assumption_reference,'assumption_reference','TRANSFER_CONTRACT')
 if(!identical(mitigation$conditioning,'event_and_unmitigated_failure')) .cr_error('TRANSFER_CONDITIONING','Mitigation probability must condition on the event and would-be failure')
 success<-.cr_transfer_p(mitigation$success_probability_given_event_and_unmitigated_failure,'mitigation_success');gross<-p*failure;avoided<-gross*success;probability<-gross*(1-success)
 .cr_result(list(stage='TRANSFER_DEFAULT',probability=probability,target=contract$target,event_id=event_id,event_probability=p,unmitigated_default_probability=gross,mitigated_default_mass=avoided,applied_effects=list(mitigation$effect_id),assumption_reference=mitigation$assumption_reference),'transfer.transfer_event_default_probability','probability',contract$context,limitations='Event-to-default failure and mitigation are declared conditional probabilities. Country ratings or arbitrary liquidity ratios are not silently converted into them.')
}
combine_default_transfer <- function(pre_transfer_risk,transfer_risk,dependence,joint_probability=NULL,context=NULL) {
 base<-.cr_transfer_object(pre_transfer_risk,'pre_transfer_risk');transfer<-.cr_transfer_object(transfer_risk,'transfer_risk');.cr_data_required(base,c('stage','pd','target','applied_effects'));.cr_data_required(transfer,c('stage','probability','target','event_id','applied_effects'))
 if(!identical(base$stage,'PRE_TRANSFER')||!identical(transfer$stage,'TRANSFER_DEFAULT')) .cr_error('TRANSFER_DOUBLE_COUNT','Require unadjusted base PD and separate transfer-default risk')
 contract<-.cr_transfer_target(base$target,context)
 if(!identical(.cr_hash(contract$target),.cr_hash(transfer$target))) .cr_error('TARGET_MISMATCH','Exposure, horizon and conditioning must agree')
 base_effects<-.cr_transfer_names(base$applied_effects,'base applied_effects',TRUE);transfer_effects<-.cr_transfer_names(transfer$applied_effects,'transfer applied_effects',TRUE)
 if(length(intersect(base_effects,transfer_effects))) .cr_error('TRANSFER_DOUBLE_COUNT','An effect cannot be applied at both risk levels')
 p<-.cr_transfer_p(base$pd,'pre_transfer_pd');q<-.cr_transfer_p(transfer$probability,'transfer_default_probability');lower<-max(0,sum(c(p,q,-1)));upper<-min(p,q)
 if(identical(dependence,'explicit')) {if(is.null(joint_probability)) .cr_error('TRANSFER_DEPENDENCE','Explicit intersection probability required');joint<-.cr_transfer_p(joint_probability,'joint_probability')
 } else if(identical(dependence,'independent')) {if(!is.null(joint_probability)) .cr_error('TRANSFER_DEPENDENCE','Do not supply an intersection with an independence assumption');joint<-p*q
 } else .cr_error('TRANSFER_DEPENDENCE','Declare explicit or independent dependence')
 if(joint<lower||joint>upper) .cr_error('INVALID_JOINT_PROBABILITY','Joint probability outside Frechet bounds')
 neither<-sum(c(1,-p,-q,joint));residual<-0
 if(neither<0&&neither>= -8*.Machine$double.eps){residual<-neither;neither<-0}
 if(neither<0) .cr_error('NUMERICAL_FAILURE','Exclusive-state mass is negative')
 states<-list(neither=neither,default_only=p-joint,transfer_only=q-joint,joint=joint)
 .cr_result(list(stage='TRANSFER_ADJUSTED',combined_pd=min(1,sum(c(p,q,-joint))),target=contract$target,pre_transfer_pd=p,transfer_default_probability=q,joint_probability=joint,exclusive_probabilities=states,event_id=transfer$event_id,applied_effects=as.list(c(base_effects,transfer_effects))),'transfer.combine_default_transfer','probability',contract$context,diagnostics=list(dependence=dependence,frechet_lower=lower,frechet_upper=upper,neither_roundoff_adjustment=residual),limitations='Independence is an explicit assumption, not an estimate. An already transfer-adjusted PD is rejected; the union is not a rating-notch adjustment.')
}
transfer_loss_components <- function(combined_risk,conditional_losses,currency,applied_loss_effects,context=NULL) {
 combined<-.cr_transfer_object(combined_risk,'combined_risk');losses<-.cr_transfer_object(conditional_losses,'conditional_losses');.cr_data_required(combined,c('stage','exclusive_probabilities','target','applied_effects'))
 if(!identical(combined$stage,'TRANSFER_ADJUSTED')) .cr_error('TRANSFER_CONTRACT','Combined event-union result required')
 contract<-.cr_transfer_target(combined$target,context);effects<-.cr_transfer_names(applied_loss_effects,'applied_loss_effects',TRUE);previous<-.cr_transfer_names(combined$applied_effects,'applied_effects',TRUE)
 if(length(intersect(effects,previous))) .cr_error('TRANSFER_DOUBLE_COUNT','Risk-level mitigation must not be applied again to state losses')
 keys<-c('neither','transfer_only','default_only','joint')
 if(!setequal(names(losses),keys)||!setequal(names(combined$exclusive_probabilities),keys)||anyDuplicated(names(losses))||anyDuplicated(names(combined$exclusive_probabilities))) .cr_error('TRANSFER_STATES','Explicit losses and probabilities for all four states required')
 states<-lapply(seq_along(keys),function(i)list(state_id=keys[i],events=list(default=i>=3L,transfer=i%%2L==0L),probability=combined$exclusive_probabilities[[keys[i]]],conditional_loss=losses[[keys[i]]]))
 integrated<-event_conditioned_loss(states,c('default','transfer'),currency,contract$target$probability_measure,contract$context)
 contributions<-setNames(lapply(integrated$value$states,function(x)x$contribution),keys)
 .cr_result(list(expected_loss=integrated$value$expected_loss,contributions=contributions,target=contract$target,applied_effects=as.list(c(previous,effects))),'transfer.transfer_loss_components',currency,contract$context,limitations=c('Conditional losses are joint monetary expectations for each exclusive state. Joint-event loss is never inferred as the average of standalone losses.','Effect IDs must identify economic mitigants consistently across PD and recovery layers; unreported duplication cannot be inferred.'))
}
