# Joint path integration preserves default, recovery, exposure and discount dependence.
.cr_loss_contract <- function(currency,probability_measure,context) {
 .cr_string(currency,'currency','INVALID_CURRENCY')
 if(!is.character(probability_measure)||length(probability_measure)!=1L||is.na(probability_measure)||!(probability_measure %in% c('PHYSICAL','RISK_NEUTRAL'))) .cr_error('INCOMPATIBLE_MEASURE','Declare PHYSICAL or RISK_NEUTRAL')
 ctx<-.cr_result(NULL,'losses.contract',context=context)$context
 for(key in c('currency','probability_measure'))if(!is.null(ctx[[key]])&&!identical(ctx[[key]],get(key))) .cr_error('TARGET_MISMATCH','Context conflicts with declared loss convention')
 ctx
}
.cr_loss_ids <- function(values,name,n=NULL) {
 if(is.list(values)&&is.null(names(values))&&all(vapply(values,function(x)is.character(x)&&length(x)==1L,logical(1)))) values<-unlist(values,use.names=FALSE)
 if(!is.character(values)||!length(values)||anyNA(values)||any(!nzchar(values))||anyDuplicated(values)||(!is.null(n)&&length(values)!=n)) .cr_error('INVALID_IDENTIFIERS',paste(name,'requires unique nonempty strings of matching length'))
 as.character(values)
}
.cr_loss_scalar_id <- function(value,name) {if(!is.character(value)||length(value)!=1L) .cr_error('INVALID_IDENTIFIERS',paste(name,'requires one nonempty string'));.cr_loss_ids(value,name,1L)}
.cr_loss_sum <- function(values) {answer<-sum(values);if(!is.finite(answer)) .cr_error('NUMERICAL_FAILURE','Loss sum outside finite range');as.numeric(answer)}
.cr_loss_weights <- function(weights,n) {
 w<-.cr_numeric(weights,'scenario_weights',vector_only=TRUE)
 if(length(w)!=n||any(w<0)) .cr_error('INVALID_WEIGHTS','Scenario probabilities must match paths and be nonnegative')
 total<-sum(w);if(!is.finite(total)||abs(total-1)>1e-12) .cr_error('INVALID_WEIGHTS','Scenario probabilities must sum to one within 1e-12')
 list(weights=w/total,residual=total-1)
}
.cr_loss_multiply <- function(...) {
 inputs<-list(...);sizes<-vapply(inputs,length,integer(1));n<-max(sizes)
 if(any(sizes!=1L&sizes!=n)) .cr_error('INCOMPATIBLE_SHAPE','Loss factors must have equal lengths or be scalar')
 shape<-NULL;for(x in inputs)if(!is.null(dim(x))&&length(x)==n){shape<-dim(x);break}
 factors<-vapply(inputs,function(x)rep_len(as.numeric(x),n),numeric(n));if(is.null(dim(factors)))factors<-matrix(factors,nrow=n)
 answer<-numeric(n);active<-rowSums(factors==0)==0
 if(any(active)) {
  a<-abs(factors[active,,drop=FALSE]);exponent<-pmin(floor(log2(a)),1023);mantissa<-a/2^exponent
  m<-rep(1,nrow(mantissa));for(j in seq_len(ncol(mantissa)))m<-m*mantissa[,j];e<-rowSums(exponent);adjustment<-floor(log2(m));m<-m/2^adjustment;e<-e+adjustment
  value<-numeric(length(m));normal<-e>= -1022
  value[normal]<-m[normal]*2^e[normal];value[!normal]<-(m[!normal]*2^-1022)*2^(e[!normal]+1022)
  answer[active]<-value*apply(sign(factors[active,,drop=FALSE]),1L,prod)
 }
 if(any(!is.finite(answer))) .cr_error('NUMERICAL_FAILURE','Loss product outside finite range')
 if(!is.null(shape))dim(answer)<-shape
 answer
}
.cr_loss_time <- function(time_grid,n) {
 t<-.cr_numeric(time_grid,'time_grid',vector_only=TRUE)
 if(length(t)!=n||any(t<=0)||any(diff(t)<=0)) .cr_error('INVALID_TIME_GRID','Positive increasing period-end times in years required')
 t
}
.cr_loss_paths <- function(default_indicator,lgd_paths,ead_paths,discount_factors,scenario_weights,scenario_ids,time_grid) {
 d<-.cr_numeric(default_indicator,'default_indicator');l<-.cr_numeric(lgd_paths,'lgd_paths');e<-.cr_numeric(ead_paths,'ead_paths')
 if(length(dim(d))!=2L||!identical(dim(l),dim(d))||!identical(dim(e),dim(d))) .cr_error('INCOMPATIBLE_SHAPE','Default, LGD and EAD require matching paths by periods matrices')
 if(any(!(d %in% c(0,1)))||any(rowSums(d)>1)) .cr_error('INVALID_DEFAULT_PATH','Only one first-default indicator per path is allowed')
 if(any(e<0)) .cr_error('INVALID_EAD','EAD must be nonnegative')
 ids<-.cr_loss_ids(scenario_ids,'scenario_ids',nrow(d));times<-.cr_loss_time(time_grid,ncol(d));weights<-.cr_loss_weights(scenario_weights,nrow(d));df<-.cr_numeric(discount_factors,'discount_factors')
 if(is.null(dim(df))&&length(df)==1L)df<-matrix(df,nrow(d),ncol(d)) else if(is.null(dim(df))&&length(df)==ncol(d))df<-matrix(rep(df,each=nrow(d)),nrow(d),ncol(d)) else if(!identical(dim(df),dim(d))) .cr_error('INCOMPATIBLE_SHAPE','Discounts must be scalar, period vector or matching matrix')
 if(any(df<0)) .cr_error('INVALID_DISCOUNT','Discount factors must be nonnegative')
 list(d=d,l=l,e=e,df=df,w=weights$weights,ids=ids,times=times,residual=weights$residual)
}
.cr_loss_rows <- function(x)lapply(seq_len(nrow(x)),function(i)as.list(as.numeric(x[i,,drop=TRUE])))
expected_loss_from_paths <- function(default_indicator,lgd_paths,ead_paths,scenario_weights,discount_factors,scenario_ids,time_grid,currency,probability_measure,uncertainty='none',context=NULL) {
 ctx<-.cr_loss_contract(currency,probability_measure,context);s<-.cr_loss_paths(default_indicator,lgd_paths,ead_paths,discount_factors,scenario_weights,scenario_ids,time_grid)
 cells<-.cr_loss_multiply(s$d,s$l,s$e,s$df);losses<-apply(cells,1L,.cr_loss_sum)
 period<-vapply(seq_len(ncol(s$d)),function(j).cr_loss_sum(s$w*cells[,j]),numeric(1));pd<-vapply(seq_len(ncol(s$d)),function(j).cr_loss_sum(s$w*s$d[,j]),numeric(1))
 lgd_cond<-ead_df_cond<-vector('list',length(pd));separate<-numeric(length(pd))
 for(j in seq_along(pd))if(pd[j]>0){q<-s$w*s$d[,j]/pd[j];a<-.cr_loss_sum(q*s$l[,j]);b<-.cr_loss_sum(.cr_loss_multiply(q,s$e[,j],s$df[,j]));lgd_cond[[j]]<-a;ead_df_cond[[j]]<-b;separate[j]<-.cr_loss_multiply(pd[j],a,b)}
 mean<-.cr_loss_sum(period);se<-NULL
 if(!is.character(uncertainty)||length(uncertainty)!=1L||is.na(uncertainty)||!(uncertainty %in% c('none','iid'))) .cr_error('INVALID_UNCERTAINTY','Use none or iid')
 if(uncertainty=='iid') {
  if(length(s$w)<2L||!all(s$w==s$w[1])) .cr_error('INCOMPATIBLE_UNCERTAINTY','IID Monte Carlo SE requires at least two equally weighted independent paths')
  scale<-max(abs(losses));se<-if(scale==0)0 else stats::sd(losses/scale)/sqrt(length(s$w))*scale
 }
 .cr_result(list(expected_loss=mean,path_losses=as.list(losses),period_contributions=as.list(period),marginal_default_probability=as.list(pd),default_weighted_lgd=lgd_cond,default_weighted_discounted_ead=ead_df_cond,conditional_independence_loss=.cr_loss_sum(separate),conditional_lgd_exposure_covariance_contribution=.cr_loss_sum(c(mean,-.cr_loss_sum(separate))),standard_error=se),'losses.expected_loss_from_paths',currency,ctx,diagnostics=list(probability_measure=probability_measure,time_unit='years',time_grid=as.list(s$times),scenario_ids=as.list(s$ids),scenario_weight_residual=s$residual,uncertainty=uncertainty,path_fingerprint=.cr_hash(list(default=.cr_loss_rows(s$d),lgd=.cr_loss_rows(s$l),ead=.cr_loss_rows(s$e),discount=.cr_loss_rows(s$df),scenario_ids=as.list(s$ids)))),limitations=c('All matrices share the declared scenario order and first-default periods. Conditional means do not justify unconditional factorization.','IID standard error excludes parameter, discretization and model uncertainty; independence of supplied paths is not verified.'))
}
lifetime_expected_loss <- function(marginal_pd,conditional_loss,discount_factors,time_grid,currency,probability_measure,context=NULL) {
 ctx<-.cr_loss_contract(currency,probability_measure,context);p<-.cr_probability(marginal_pd);l<-.cr_numeric(conditional_loss,'conditional_loss',vector_only=TRUE);df<-.cr_numeric(discount_factors,'discount_factors',vector_only=TRUE)
 if(!is.null(dim(p))||length(p)!=length(l)||length(p)!=length(df)) .cr_error('INCOMPATIBLE_SHAPE','Equal-length period vectors required')
 times<-.cr_loss_time(time_grid,length(p));total<-.cr_loss_sum(p);if(total>1+1e-12) .cr_error('INVALID_PD_MASS','Marginal first-default masses cannot sum above one');if(any(df<0)) .cr_error('INVALID_DISCOUNT','Nonnegative discount factors required');contributions<-.cr_loss_multiply(p,l,df)
 .cr_result(list(lifetime_expected_loss=.cr_loss_sum(contributions),period_contributions=as.list(contributions),total_default_probability=total),'losses.lifetime_expected_loss',currency,ctx,diagnostics=list(time_grid=as.list(times),time_unit='years',probability_measure=probability_measure,conditioning='conditional_loss is E[LGD*EAD | first default in period]'),limitations='Pass marginal, not cumulative PD. Discount factors are fixed per period; stochastic loss-discount dependence needs joint path integration.')
}
event_conditioned_loss <- function(states,event_names,currency,probability_measure,context=NULL) {
 ctx<-.cr_loss_contract(currency,probability_measure,context);names<-.cr_loss_ids(event_names,'event_names')
 if(length(names)>12L) .cr_error('INVALID_STATE_SPACE','Explicit partition supports at most twelve binary events')
 if(!is.list(states)||length(states)!=2^length(names)) .cr_error('INCOMPLETE_PARTITION','Provide all exclusive Boolean event combinations')
 seen<-ids<-character();masses<-contributions<-numeric();details<-list()
 for(row in states) {
  if(!.cr_object(row)||!is.character(row$state_id)||length(row$state_id)!=1L||is.na(row$state_id)||!nzchar(row$state_id)||row$state_id %in% ids) .cr_error('INVALID_STATE_SPACE','Unique state IDs required')
  ids<-c(ids,row$state_id);events<-row$events
  if(!.cr_object(events)||!setequal(names(events),names)||anyDuplicated(names(events))||any(!vapply(events,function(x)is.logical(x)&&length(x)==1L&&!is.na(x),logical(1)))) .cr_error('INVALID_STATE_SPACE','Each state needs one Boolean per event')
  key<-paste(as.integer(unlist(events[names])),collapse='');if(key %in% seen) .cr_error('OVERLAPPING_STATES','Repeated event combination');seen<-c(seen,key)
  p<-.cr_probability(row$probability);loss<-.cr_numeric(row$conditional_loss,'conditional_loss')
  if(length(p)!=1L||length(loss)!=1L||!is.null(dim(p))||!is.null(dim(loss))) .cr_error('INVALID_STATE_SPACE','State probability and joint conditional loss must be scalar')
  contribution<-.cr_loss_multiply(p,loss);masses<-c(masses,p);contributions<-c(contributions,contribution);details[[length(details)+1L]]<-list(state_id=row$state_id,events=events,probability=as.numeric(p),conditional_loss=as.numeric(loss),contribution=contribution)
 }
 total<-.cr_loss_sum(masses);if(abs(total-1)>1e-12) .cr_error('INVALID_PD_MASS','Exclusive state probabilities must sum to one')
 .cr_result(list(expected_loss=.cr_loss_sum(contributions),states=details,probability_sum=total),'losses.event_conditioned_loss',currency,ctx,diagnostics=list(probability_measure=probability_measure,event_names=as.list(names),complete_boolean_partition=TRUE),limitations='State losses are joint conditional monetary expectations, not products of arbitrary conditional means. Event meaning is supplied by the caller.')
}
counterparty_expected_loss <- function(default_indicator,net_exposure_paths,recovery_paths,scenario_weights,discount_factors,counterparty_id,netting_set_id,scenario_ids,time_grid,currency,probability_measure,uncertainty='none',context=NULL) {
 .cr_loss_scalar_id(counterparty_id,'counterparty_id');.cr_loss_scalar_id(netting_set_id,'netting_set_id')
 exposure<-.cr_numeric(net_exposure_paths,'net_exposure_paths');recovery<-.cr_probability(recovery_paths,name='recovery_paths')
 if(!identical(dim(exposure),dim(recovery))||length(exposure)!=length(recovery)) .cr_error('INCOMPATIBLE_SHAPE','Exposure and recovery paths must match')
 result<-expected_loss_from_paths(default_indicator,1-recovery,pmax(exposure,0),scenario_weights,discount_factors,scenario_ids,time_grid,currency,probability_measure,uncertainty,context)
 s<-.cr_loss_paths(default_indicator,1-recovery,pmax(exposure,0),discount_factors,scenario_weights,scenario_ids,time_grid)
 independent<-vapply(seq_len(ncol(s$d)),function(j).cr_loss_multiply(.cr_loss_sum(s$w*s$d[,j]),.cr_loss_sum(.cr_loss_multiply(s$w,s$l[,j],s$e[,j],s$df[,j]))),numeric(1));benchmark<-.cr_loss_sum(independent)
 value<-c(result$value,list(independent_default_exposure_loss=benchmark,default_exposure_dependence_contribution=.cr_loss_sum(c(result$value$expected_loss,-benchmark))))
 .cr_result(value,'losses.counterparty_expected_loss',currency,result$context,diagnostics=c(result$diagnostics,list(counterparty_id=counterparty_id,netting_set_id=netting_set_id,negative_net_values_floored=sum(exposure<0),interpretation=if(probability_measure=='PHYSICAL')'physical_loss_forecast' else 'risk_neutral_discounted_credit_loss')),limitations=c(result$limitations,'One externally established netting set only; legal netting, margin closeout and own-default effects are not inferred. No capital or CVA framework compliance claim.'))
}
aggregate_portfolio_loss <- function(facility_loss_paths,portfolio_mapping,scenario_weights,fx_rates,scenario_ids,reporting_currency,probability_measure,quantile_levels,context=NULL) {
 ctx<-.cr_loss_contract(reporting_currency,probability_measure,context);ids<-.cr_loss_ids(scenario_ids,'scenario_ids');weight<-.cr_loss_weights(scenario_weights,length(ids));w<-weight$weights
 levels<-.cr_numeric(quantile_levels,'quantile_levels',vector_only=TRUE);if(any(levels<=0|levels>=1)||anyDuplicated(levels)) .cr_error('INVALID_QUANTILE','Distinct levels in (0,1) required')
 if(!is.list(facility_loss_paths)||!length(facility_loss_paths)||!is.list(portfolio_mapping)||!length(portfolio_mapping)||!.cr_object(fx_rates)) .cr_error('INVALID_INPUT','Facility rows, allocation rows and explicit FX object required')
 facilities<-allocations<-shares<-list()
 for(row in facility_loss_paths) {
  if(!.cr_object(row)) .cr_error('INVALID_INPUT','Facility loss row must be an object');identifier<-.cr_loss_scalar_id(row$facility_id,'facility_id')
  if(identifier %in% names(facilities)) .cr_error('INVALID_IDENTIFIERS','Repeated facility')
  local_ids<-.cr_loss_ids(row$scenario_ids,'facility scenario ids',length(ids));if(!setequal(local_ids,ids)) .cr_error('SCENARIO_MISMATCH','Every facility must cover the same named scenarios')
  values<-.cr_numeric(row$losses,'facility losses',vector_only=TRUE);if(length(values)!=length(ids)) .cr_error('INCOMPATIBLE_SHAPE','Losses require one value per scenario')
  currency<-row$currency;.cr_string(currency,'currency','INVALID_CURRENCY');if(!(currency %in% names(fx_rates))) .cr_error('MISSING_FX','Explicit rate required for every currency')
  fx<-.cr_numeric(fx_rates[[currency]],'fx_rate');if(is.null(dim(fx))&&length(fx)==1L)fx<-rep(fx,length(ids))
  if(!is.null(dim(fx))||length(fx)!=length(ids)||any(fx<=0)) .cr_error('INVALID_FX','Positive FX scalar or scenario vector required')
  if(currency==reporting_currency&&any(fx!=1)) .cr_error('INVALID_FX','Reporting currency FX must equal one')
  facilities[[identifier]]<-.cr_loss_multiply(values[match(ids,local_ids)],fx)
 }
 for(row in portfolio_mapping) {
  if(!.cr_object(row)) .cr_error('INVALID_INPUT','Allocation row must be an object');fid<-.cr_loss_scalar_id(row$facility_id,'facility_id');pid<-.cr_loss_scalar_id(row$portfolio_id,'portfolio_id');share<-.cr_numeric(row$share,'share')
  if(!(fid %in% names(facilities))||length(share)!=1L||!is.null(dim(share))||share<0||share>1) .cr_error('INVALID_ALLOCATION','Known facility and fraction in [0,1] required')
  if(fid %in% names(allocations[[pid]])) .cr_error('INVALID_ALLOCATION','Duplicate facility/portfolio allocation')
  allocations[[pid]][[fid]]<-as.numeric(share);shares[[fid]]<-(if(is.null(shares[[fid]]))0 else shares[[fid]])+share
 }
 if(any(unlist(shares)>1+1e-12)) .cr_error('DOUBLE_COUNTING','Allocated share exceeds one')
 results<-list()
 for(pid in sort(names(allocations),method='radix')) {
  mapping<-allocations[[pid]];facility_ids<-sort(names(mapping),method='radix');components<-vapply(facility_ids,function(f).cr_loss_multiply(facilities[[f]],mapping[[f]]),numeric(length(ids)));if(is.null(dim(components)))components<-matrix(components,nrow=length(ids))
  paths<-apply(components,1L,.cr_loss_sum);mean<-.cr_loss_sum(w*paths);expected<-vapply(seq_along(facility_ids),function(i).cr_loss_sum(w*components[,i]),numeric(1))
  active<-w>0;ordered<-order(paths[active],method='radix');values<-paths[active][ordered];cum<-cumsum(w[active][ordered]);cum[length(cum)]<-1;tails<-list()
  for(level in levels) {
   var<-as.numeric(values[which(cum>=level)[1]]);above<-paths>var;equal<-paths==var;tail_weights<-ifelse(above,w,0);remaining<-max(0,1-level-.cr_loss_sum(tail_weights));equal_mass<-.cr_loss_sum(w[equal]);if(equal_mass>0)tail_weights[equal]<-w[equal]*(remaining/equal_mass);tail_weights<-tail_weights/(1-level)
   contributions<-vapply(seq_along(facility_ids),function(i).cr_loss_sum(tail_weights*components[,i]),numeric(1));tails[[length(tails)+1L]]<-list(level=as.numeric(level),value_at_risk=var,expected_shortfall=.cr_loss_sum(tail_weights*paths),facility_es_contributions=setNames(as.list(contributions),facility_ids))
  }
  results[[length(results)+1L]]<-list(portfolio_id=pid,path_losses=as.list(paths),expected_loss=mean,facility_expected_loss=setNames(as.list(expected),facility_ids),tail_metrics=tails)
 }
 unallocated<-lapply(sort(names(facilities),method='radix'),function(f)1-(if(is.null(shares[[f]]))0 else shares[[f]]));names(unallocated)<-sort(names(facilities),method='radix')
 .cr_result(list(portfolios=results,unallocated_shares=unallocated),'losses.aggregate_portfolio_loss',reporting_currency,ctx,diagnostics=list(scenario_ids=as.list(ids),scenario_weight_residual=weight$residual,probability_measure=probability_measure,quantile_convention='left_inverse_weighted_cdf',expected_shortfall_convention='upper_(1-alpha)_mass_with_proportional_boundary_ties'),limitations='Loss paths and FX must share the same scenario meaning. Tail contributions allocate portfolio tail losses, not sums of standalone facility VaR.')
}
