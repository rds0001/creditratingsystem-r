# Ordered cure and re-default projections reuse frozen multistate estimates.
predict_cure_and_redefault <- function(current_episode,model,action_conditions,horizon_grid,state_recoveries,
                                      target,currency,extrapolation='error',context=NULL) {
 ctx<-.cr_collections_currency_context(currency,context);artifact<-.cr_model(model);parameters<-artifact$parameters
 if(!identical(artifact$method_id,'survival.estimate_multistate_transitions')||!identical(parameters$kind,'aalen_johansen')) .cr_error('COLLECTIONS_MODEL','Frozen Aalen-Johansen multistate model required')
 if(!.cr_object(current_episode)||!.cr_object(action_conditions)) .cr_error('COLLECTIONS_INPUT','Episode and action conditions must be objects')
 .cr_data_required(current_episode,c('episode_id','state','model_time','known_at','ever_default'));.cr_data_required(action_conditions,c('action_id','assumption_reference'))
 for(value in list(current_episode$episode_id,current_episode$state,action_conditions$action_id,action_conditions$assumption_reference))if(!is.character(value)||length(value)!=1L||is.na(value)||!nzchar(value)) .cr_error('COLLECTIONS_INPUT','Nonempty episode/state/action/reference strings required')
 if(.cr_data_date(artifact$training$known_at,'training.known_at')>.cr_data_date(current_episode$known_at,'episode.known_at')) .cr_error('COLLECTIONS_LEAKAGE','Model unavailable at episode knowledge cutoff')
 contract<-artifact$collections_contract;if(!.cr_object(contract)) .cr_error('COLLECTIONS_MODEL','Explicit collections applicability contract required')
 .cr_data_required(contract,c('cure_states','default_states','supported_actions','supported_initial_states','action_interpretation'))
 states<-unlist(parameters$states,use.names=FALSE)
 for(field in c('cure_states','default_states','supported_initial_states')) {
  x<-unlist(contract[[field]],use.names=FALSE)
  if(!is.character(x)||!length(x)||anyNA(x)||anyDuplicated(x)||any(!x %in% states)) .cr_error('COLLECTIONS_MODEL','Invalid collections state contract')
 }
 cure<-unlist(contract$cure_states,use.names=FALSE);default<-unlist(contract$default_states,use.names=FALSE)
 actions<-unlist(contract$supported_actions,use.names=FALSE)
 if(!is.character(actions)||!length(actions)||anyNA(actions)||any(!nzchar(actions))||anyDuplicated(actions)) .cr_error('COLLECTIONS_MODEL','Explicit distinct supported action IDs required')
 if(length(intersect(cure,default))) .cr_error('COLLECTIONS_MODEL','Cure and default states must differ')
 if(!identical(contract$action_interpretation,'observational_conditioning')) .cr_error('COLLECTIONS_MODEL','Declare observational_conditioning; no causal claim supported')
 if(!current_episode$state %in% unlist(contract$supported_initial_states)) .cr_error('COLLECTIONS_MODEL','Initial state outside model applicability')
 if(!is.logical(current_episode$ever_default)||length(current_episode$ever_default)!=1L||is.na(current_episode$ever_default)) .cr_error('COLLECTIONS_INPUT','Explicit historical default flag required')
 supported<-action_conditions$action_id %in% unlist(contract$supported_actions)
 if(!supported&&extrapolation=='error') .cr_error('COLLECTIONS_ACTION_SUPPORT','Action outside declared empirical support')
 start<-.cr_collections_scalar(current_episode$model_time,'model_time')
 prediction<-predict_event_curves(artifact,list(),horizon_grid,feature_order=character(),target=target,
  initial_state=current_episode$state,event_states=cure,start_time=start,extrapolation=extrapolation,context=ctx)
 curve<-prediction$value$curves[[1]];grid<-.cr_numeric(horizon_grid,'horizon_grid',vector_only=TRUE);rewards<-.cr_numeric(state_recoveries,'state_recoveries');n<-length(states)
 if(!is.matrix(rewards)||!identical(as.integer(dim(rewards)),c(length(grid),n))) .cr_error('COLLECTIONS_SHAPE','Recovery amounts require horizon-by-state matrix in model state order')
 initial<-match(current_episode$state,states);phase<-if(states[initial] %in% cure)2L else 1L
 distribution<-matrix(0,3,n);distribution[phase,initial]<-1;times<-unlist(parameters$event_times,use.names=FALSE);pos<-1L;hits<-numeric()
 while(pos<=length(times)&&times[pos]<=start)pos<-pos+1L
 for(h in grid) {
  while(pos<=length(times)&&times[pos]<=start+h) {
   increment<-parameters$hazard_increment_matrices[[pos]]
   if(!is.matrix(increment))increment<-do.call(rbind,lapply(increment,unlist,use.names=FALSE))
   transition<-diag(n)+increment;following<-matrix(0,3,n)
   for(old_phase in seq_len(3))for(i in seq_len(n))for(j in seq_len(n)) {
    next_phase<-if(old_phase==3L||(old_phase==2L&&states[j] %in% default))3L else if(old_phase==2L||states[j] %in% cure)2L else 1L
    following[next_phase,j]<-following[next_phase,j]+distribution[old_phase,i]*transition[i,j]
   }
   distribution<-following;pos<-pos+1L
  }
  hits<-c(hits,sum(distribution[3,]))
 }
 occupancy<-do.call(rbind,lapply(curve$state_occupancy,unlist,use.names=FALSE))
 expected<-.cr_collections_finite(rowSums(occupancy*rewards))
 value<-list(episode_id=current_episode$episode_id,action_id=action_conditions$action_id,
  treatment_support=if(supported)'SUPPORTED' else 'REFERENCE_MODEL_EXTRAPOLATION',horizon_grid=as.list(grid),
  states=as.list(states),state_occupancy=curve$state_occupancy,sustained_cure_probability=curve$first_hit_probability,
  cure_then_redefault_probability=as.list(hits),expected_recovery_by_period=as.list(expected),
  expected_recovery=.cr_collections_finite(sum(expected)),ever_default=current_episode$ever_default,causal_effect_identified=FALSE)
 .cr_result(value,'collections.predict_cure_and_redefault',currency,ctx,
  diagnostics=list(model_id=artifact$model_id,model_version=artifact$model_version,assumption_reference=action_conditions$assumption_reference,
   survival_diagnostics=prediction$diagnostics,recovery_convention='conditional_period_cashflow_given_end_state; undiscounted'),
  limitations=c('Cure states must already encode the specified sustained-cure probation rule in model training. Ordered re-default means a default reached after such cure, not current default occupancy.',
   'Recovery inputs are conditional period cashflow expectations, not cumulative recoveries or state liquidation values; repeated cumulative amounts would double count.',
   'Action support is declared training evidence. Outside-support flat projections reuse the reference model without estimating a treatment effect.'))
}
