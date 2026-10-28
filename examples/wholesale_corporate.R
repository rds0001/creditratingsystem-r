# Synthetic corporate fit, market inversion and independently checked issue recovery.
if(!exists('fit_scorecard'))library(creditratingsystem)
run_wholesale_corporate <- function(data=NULL,verify_reference=TRUE) {
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/wholesale_corporate.json',package='creditratingsystem'),simplifyVector=FALSE)
 rows<-function(x)lapply(seq_len(nrow(x)),function(i)as.list(unname(x[i,])))
 target<-data$target;training<-data$score_training;features<-unlist(training$feature_order)
 ratios<-build_financial_ratios(data$statements,data$ratio_spec)$value
 score<-fit_scorecard(do.call(rbind,lapply(training$X,unlist)),unlist(training$y),feature_order=features,fit_spec=training$fit_spec,target=target,training=training$training,model_id='corporate-fundamental',model_version='1',weights=unlist(training$weights))
 design<-matrix(vapply(ratios,function(r)r$ratios$net_debt_ebitda$value,numeric(1)),ncol=1)
 fundamental<-score_scorecard(score,design,feature_order=features,expected_target=target)$value
 for(key in c('pd','log_odds','reference_contributions'))fundamental[[key]]<-as.list(fundamental[[key]])
 for(key in c('feature_contributions','contribution_differences'))fundamental[[key]]<-rows(fundamental[[key]])
 calibration<-data$calibration;calibrator<-calibrate_logistic(unlist(calibration$values),unlist(calibration$outcomes),unlist(calibration$weights),metadata=calibration$metadata)
 calibrated<-apply_calibrator(calibrator,unlist(fundamental$pd),target=target,base_model_version='1')$value
 routes<-applicability<-negative<-list()
 for(segment in c('SME','large_corporate')) {
  facts<-list(size=segment,financial_statements=TRUE,audited=segment=='large_corporate',listed=segment=='large_corporate')
  snapshot<-list(subject_id=if(segment=='SME')'SME'else'LARGE',subject_type='obligor',as_of=data$as_of,known_at=data$known_at,attributes=lapply(facts,function(v)list(value=v,observed_at='2023-12-31',known_at='2024-01-01')))
  routes[[segment]]<-route_rating_profile(snapshot,data$routing_rules,data$as_of,data$known_at)$value
  applicability[[segment]]<-assess_rating_applicability(snapshot,data$applicability_contracts[[segment]],data$as_of,data$known_at)$value
  bad<-snapshot;key<-if(segment=='SME')'financial_statements'else'audited';bad$attributes[[key]]['value']<-list(NULL)
  negative[[segment]]<-assess_rating_applicability(bad,data$applicability_contracts[[segment]],data$as_of,data$known_at)$value$status
 }
 market<-data$market_inputs;market$asset_bounds<-unlist(market$asset_bounds);market$volatility_bounds<-unlist(market$volatility_bounds)
 inversion<-do.call(infer_merton_assets,market)$value;a<-inversion$asset_value;sigma<-inversion$asset_volatility;barrier<-data$market_inputs$debt_face;mu<-data$physical_drift
 distance<-distance_to_default(a,barrier,sigma,mu,1)$value
 terminal<-merton_default_probability(a,barrier,sigma,mu,1,measure='PHYSICAL')$value
 q<-merton_default_probability(a,barrier,sigma,data$market_inputs$risk_free_rate,1,measure='RISK_NEUTRAL')$value
 passage<-first_passage_pd(a,list(kind='constant',initial_level=barrier),sigma,.5*sigma*sigma,1,measure='PHYSICAL')$value
 arguments<-data$distance_calibration;for(key in c('distances','outcomes','weights'))arguments[[key]]<-unlist(arguments[[key]]);arguments$evaluation_distances<-c(distance$distance_to_default,1)
 mapping<-do.call(calibrate_distance_pd_mapping,arguments)$value
 names_signal<-c('fundamental','observed_distance');ps<-c(calibrated[2],mapping$pd[1])
 signals<-lapply(1:2,function(i)list(signal_id=names_signal[i],model_id=names_signal[i],model_version='1',row_ids=list('LARGE'),pd=list(ps[i]),target=target,as_of=data$as_of,known_at=data$known_at))
 pooled<-list()
 for(scale in c('linear','log_odds','mixture')) {
  contract<-list(interpretation=switch(scale,linear='estimator_pool',log_odds='log_odds_pool',mixture='latent_regime_mixture'),reference='Synthetic declared signal pooling; no TTC assertion')
  if(scale=='mixture'){contract$regime_ids<-as.list(names_signal);contract$conditional_regime_probabilities<-TRUE}
  weights<-if(scale=='mixture')matrix(c(.25,.75),nrow=1)else c(.7,.3)
  pooled[[scale]]<-combine_pd_signals(signals,weights,combination_scale=scale,combination_contract=contract)$value
 }
 standalone<-pooled$linear$pd[[1]]
 risk<-list(entity_id='LARGE',pd=standalone,stage='STANDALONE',target=target,applied_effects=list());supporter<-list(entity_id='SPONSOR',pd=.02,stage='STANDALONE',target=target,applied_effects=list())
 supported<-apply_parent_support(risk,supporter,data$support,joint_default_probability=.01)$value
 ranked<-do.call(simulate_recovery_waterfall,data$recovery)$value;secured<-do.call(simulate_recovery_waterfall,data$secured_recovery)$value
 variants<-list()
 for(seg in c('SME','large_corporate'))variants[[seg]]<-list(evidence_id=paste0('corporate-',seg),profile_id=routes[[seg]]$profile_id,standalone_pd=if(seg=='SME')calibrated[1]else standalone,market_component=if(seg=='SME')'NOT_APPLICABLE_UNLISTED'else'EMPIRICAL_DISTANCE_MAPPING',negative_status=negative[[seg]])
 recoveries<-list(senior=ranked$claim_results[[1]],subordinated=ranked$claim_results[[2]],secured=secured$claim_results[[1]])
 for(name in names(recoveries)){recovery<-recoveries[[name]]
  issue_snapshot<-list(subject_id=name,subject_type='issue',as_of=data$as_of,known_at=data$known_at,attributes=list(rank_security=list(value=name,observed_at=data$as_of,known_at=data$known_at),legal_verified=list(value=TRUE,observed_at=data$as_of,known_at=data$known_at)))
  issue_rule<-list(rule_id=name,profile_id=paste0('issue-',name),profile_version='1',priority=1,effective_from='2023-01-01',effective_to=NULL,known_at='2022-12-01',conditions=list(list(field='rank_security',operator='eq',value=name),list(field='legal_verified',operator='eq',value=TRUE)))
  issue_route<-route_rating_profile(issue_snapshot,list(issue_rule),data$as_of,data$known_at)$value
  issue_snapshot$attributes$legal_verified$value<-FALSE
  issue_negative<-route_rating_profile(issue_snapshot,list(issue_rule),data$as_of,data$known_at)$value
  variants[[name]]<-list(profile_id=issue_route$profile_id,negative_status=issue_negative$status,evidence_id=paste0('corporate-',name),standalone_pd=standalone,lgd=recovery$lgd,ead=recovery$ead_at_default,economic_loss_at_default=recovery$economic_loss,expected_loss=expected_loss(standalone,recovery$lgd,recovery$ead_at_default,context=list(currency='EUR'))$value)}
 missing<-data$statements;missing[[2]]$values['ebitda']<-list(NULL)
 missing_ratio<-build_financial_ratios(missing,data$ratio_spec)$value[[2]]$ratios$net_debt_ebitda
 if(verify_reference) {
  same<-function(x,y,tol=1e-7)stopifnot(is.finite(x),abs(x-y)<=tol+tol*abs(y))
  stopifnot(all(vapply(ratios,function(r)r$ratios$net_debt_ebitda$value,numeric(1))==3))
  expected<-plogis(.5*(qlogis(.1)+qlogis(.3)))
  for(x in fundamental$pd)same(x,expected);for(x in calibrated)same(x,expected)
  same(a,100);same(sigma,.2);same(terminal,pnorm(-distance$distance_to_default));stopifnot(terminal<q)
  same(passage,2*pnorm(-log(100/80)/.2));same(mapping$pd[2],.2)
  p1<-signals[[1]]$pd[[1]];p2<-signals[[2]]$pd[[1]]
  same(pooled$linear$pd[[1]],.7*p1+.3*p2);same(pooled$mixture$pd[[1]],.25*p1+.75*p2);same(pooled$log_odds$pd[[1]],plogis(.7*qlogis(p1)+.3*qlogis(p2)))
  same(supported$pd,standalone-(standalone-.01)*.4)
  stopifnot(identical(vapply(variants[c('senior','subordinated','secured')],function(v)v$lgd,numeric(1)),c(senior=0,subordinated=.5,secured=.125)))
  stopifnot(all(vapply(variants[c('senior','subordinated','secured')],function(v)v$standalone_pd,numeric(1))==standalone),is.null(missing_ratio$value),missing_ratio$status=='MISSING_INPUT',all(unlist(negative)=='REFER'))
 }
 list(product=data$product,data_origin='SYNTHETIC',version=data$version,target=target,ratios=ratios,score_parameters=score$model$parameters,calibration_parameters=calibrator$value$parameters,fundamental=fundamental,calibrated=as.list(calibrated),routing=routes,applicability=applicability,inversion=list(asset_value=a,asset_volatility=sigma),distance=distance,terminal_physical_pd=terminal,terminal_risk_neutral_pd=q,first_passage_pd=passage,empirical_distance_pd=as.list(mapping$pd),pooled=pooled,supported=supported,grade=map_pd_to_grade(supported$pd,data$rating_scale)$value,ranked_recovery=ranked,secured_recovery=secured,variants=variants,missing_ratio=missing_ratio)
}
