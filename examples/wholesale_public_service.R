# Restricted project funds, service cash generation and applicable rating branches.
run_wholesale_public_service <- function(data=NULL,verify_reference=TRUE,return_fitted_models=FALSE) {
  if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/wholesale_public_service.json',package='creditratingsystem'),simplifyVector=FALSE)
  rows<-list();raw<-list()
  for(c in data$cases) {
    sid<-c$case_id;variant<-c$variant;expected<-list(housing_association='RENTAL_SERVICE',education_health='ATTENDANCE_AND_GRANT',utility_service='REGULATED_TARIFF')
    if(c$cashflow_type!=expected[[variant]]||c$restricted_purpose!='CAPITAL_PROJECT')stop('Service variant or restricted-purpose contract is incompatible')
    restricted_used<-min(c$restricted_grant,c$capex);unfunded<-c$capex-restricted_used;cf<-list();flows<-list();oracles<-list()
    for(scenario in c('base','stress')) {
      stress<-scenario=='stress';quantity<-if(stress)c$stress_quantity else c$quantity;price<-if(stress)c$stress_unit_price else c$unit_price;revenue<-quantity*price;cfads<-revenue+c$general_grant-c$operating_cost-unfunded
      cf[[length(cf)+1L]]<-list(scenario_id=scenario,period_start='2024-03-01',period_end='2024-06-01',currency='EUR',base_quantity=quantity,base_unit_price=price,base_unit_variable_cost=0,base_fixed_cost=c$operating_cost,volume_multiplier=1,price_multiplier=1,variable_cost_multiplier=1,fixed_cost_multiplier=1,working_capital_revenue_ratio=0,cash_taxes=0,capex=unfunded,asset_disposals=0,other_operating_cash=c$general_grant)
      flows[[length(flows)+1L]]<-list(scenario_id=scenario,date='2024-06-01',currency='EUR',cash_available=c$opening_unrestricted_cash+revenue+c$general_grant-unfunded,reserve_target=0,amounts_due=list(operating=c$operating_cost,debt=c$debt_service))
      oracles[[length(oracles)+1L]]<-list(scenario_id=scenario,service_revenue=revenue,general_grant=c$general_grant,restricted_capex_funding=restricted_used,restricted_remaining=c$restricted_grant-restricted_used,unrestricted_capex=unfunded,cfads=cfads,debt_service_gap=max(c$debt_service-c$opening_unrestricted_cash-cfads,0))
    }
    operating<-simulate_operating_cashflows(cf,list(contract_id=sid,version='1',currency='EUR',period_unit='quarter',quantity_unit=c$cashflow_type,initial_working_capital=0,include_asset_disposals_in_cfads=FALSE))$value
    waterfall<-apply_cash_waterfall(flows,list(policy_id=sid,version='1',initial_reserve=0,reserve_target=0,reserve_priority=3,excess_reserve_policy='retain',arrears_policy='carry_without_interest',claims=list(list(claim_id='operating',priority=1,reserve_eligible=FALSE),list(claim_id='debt',priority=2,reserve_eligible=FALSE))),currency='EUR')$value
    statement<-list(entity_id=sid,currency='EUR',as_of='2024-03-01',period_start='2023-12-01',period_end='2024-03-01',values=list(debt_service=c$debt_service,unrestricted_cfads=oracles[[1]]$cfads),units=list(debt_service='EUR',unrestricted_cfads='EUR'))
    ratio<-build_financial_ratios(list(statement),list(list(name='debt_service_to_unrestricted_cfads',numerator=list(debt_service=1),denominator=list(unrestricted_cfads=1),denominator_policy='positive',output_unit='dimensionless')))$value[[1]]$ratios$debt_service_to_unrestricted_cfads$value
    t<-c$score_training;z<-2*(ratio-1)
    model<-fit_scorecard(do.call(rbind,lapply(t$X,unlist)),unlist(t$y),feature_order=unlist(t$feature_order),fit_spec=t$fit_spec,target=data$target,training=t$training,model_id=sid,model_version='1',weights=unlist(t$weights))
    pred<-score_scorecard(model,matrix(z,ncol=1),feature_order=unlist(t$feature_order),expected_target=data$target)
    a<-c$calibration;cal<-calibrate_logistic(unlist(a$values),unlist(a$outcomes),unlist(a$weights),metadata=a$metadata,tolerance=a$fit_controls$tolerance,max_iterations=a$fit_controls$max_iterations);calibrated<-apply_calibrator(cal,pred$value$pd,target=data$target,base_model_version='1')
    fundamental<-round(calibrated$value[[1]],data$precision_contract$pd_decimal_places);market<-NULL;raw_market<-NULL;pooled<-NULL;standalone<-fundamental
    if(c$listed) {
      inversion<-do.call(infer_merton_assets,lapply(data$market_inputs,function(x)if(is.list(x))unlist(x)else x))$value;asset<-inversion$asset_value;sigma<-inversion$asset_volatility;barrier<-data$market_inputs$debt_face;mu<-data$physical_drift
      dd<-distance_to_default(asset,barrier,sigma,mu,1)$value$distance_to_default;physical<-merton_default_probability(asset,barrier,sigma,mu,1,measure='PHYSICAL')$value
      d<-data$distance_calibration;mapped<-calibrate_distance_pd_mapping(unlist(d$distances),unlist(d$outcomes),weights=unlist(d$weights),metadata=d$metadata,out_of_bounds=d$out_of_bounds,evaluation_distances=dd)$value
      empirical<-mapped$pd[[1]];raw_market<-list(inversion=inversion,distance=dd,physical_pd=physical,mapping=mapped)
      market<-lapply(list(asset_value=asset,asset_volatility=sigma,distance=dd,physical_pd=physical,empirical_pd=empirical),function(v)round(v,data$precision_contract$market_output_decimal_places))
      signals<-list(list(signal_id='fundamental',model_id='fundamental',model_version='1',row_ids=list(sid),pd=list(fundamental),target=data$target,as_of='2024-03-01',known_at='2024-03-01'),list(signal_id='empirical_distance',model_id='empirical_distance',model_version='1',row_ids=list(sid),pd=list(round(empirical,6)),target=data$target,as_of='2024-03-01',known_at='2024-03-01'))
      pooled<-combine_pd_signals(signals,c(.7,.3),combination_scale='linear',combination_contract=list(interpretation='estimator_pool',reference='Declared synthetic utility estimator weights, not independent event union'))$value;standalone<-pooled$pd[[1]]
      if(verify_reference) {
        oracle_dd<-(log(100/80)+(.1-.5*.2^2))/.2
        stopifnot(abs(asset-100)<1e-7,abs(sigma-.2)<1e-7,abs(dd-oracle_dd)<1e-7,abs(physical-pnorm(-oracle_dd))<1e-7,oracle_dd>1,oracle_dd<2,abs(empirical-.2)<1e-7)
      }
    }
    base<-list(entity_id=sid,pd=standalone,stage='STANDALONE',target=data$target,applied_effects=list());parent<-list(entity_id=paste0(sid,'-sponsor'),pd=.02,stage='STANDALONE',target=data$target,applied_effects=list())
    supported<-apply_parent_support(base,parent,c$support,joint_default_probability=min(.01,standalone*.25))$value
    facts<-list(product='PUBLIC_SERVICE_FINANCE',variant=variant,borrower_class='PUBLIC_SERVICE',legal_claim=c$legal_claim,cashflow_type=c$cashflow_type,listed=c$listed)
    snapshot<-list(subject_id=sid,subject_type='obligor',as_of='2024-03-01',known_at='2024-03-01',attributes=lapply(facts,function(v)list(value=v,observed_at='2024-01-01',known_at='2024-01-01')))
    rule<-list(rule_id=sid,profile_id=sid,profile_version='1',priority=1,effective_from='2023-01-01',effective_to=NULL,known_at='2023-01-01',conditions=lapply(names(facts),function(k)list(field=k,operator='eq',value=facts[[k]])))
    route<-route_rating_profile(snapshot,list(rule),'2024-03-01','2024-03-01')$value;snapshot$attributes$legal_claim<-NULL;missing<-route_rating_profile(snapshot,list(rule),'2024-03-01','2024-03-01')$value
    if(verify_reference) {
      oracle<-plogis((1-z)/2*qlogis(a$values[[1]])+(1+z)/2*qlogis(a$values[[3]]));stopifnot(abs(calibrated$value[[1]]-oracle)<1e-7)
      for(j in 1:2)stopifnot(abs(oracles[[j]]$cfads-c$reference[[if(j==1)'cfads'else'stress_cfads']])<1e-12,abs(oracles[[j]]$debt_service_gap-c$reference[[if(j==1)'base_gap'else'stress_gap']])<1e-12,abs(waterfall$periods[[j]]$payments[[2]]$closing_arrears-oracles[[j]]$debt_service_gap)<1e-12)
      rescue<-if(c$support$states$parent_alive$support_permitted).4 else 0
      stopifnot(missing$status=='NO_ROUTE',abs(supported$pd-(standalone-(standalone-min(.01,standalone*.25))*rescue))<1e-12)
    }
    rows[[length(rows)+1L]]<-list(variant=variant,case_id=sid,cash_oracles=oracles,operating=operating,waterfall=waterfall,ratio=ratio,reported_fundamental_pd=fundamental,market_branch=if(c$listed)'EMPIRICAL_DISTANCE_MAPPING'else'NOT_APPLICABLE_UNLISTED',market=market,pooled=pooled,supported=supported,grade=map_pd_to_grade(supported$pd,data$rating_scale)$value,profile=route$profile_id,missing_claim_status=missing$status)
    if(return_fitted_models)raw[[length(raw)+1L]]<-list(case_id=sid,scorecard=model,calibrator=cal,prediction=pred,calibrated=calibrated,market=raw_market)
  }
  report<-list(data_origin='SYNTHETIC',product='PUBLIC_SERVICE_FINANCE',precision_contract=data$precision_contract,cases=rows)
  if(return_fitted_models)list(report=report,raw_native_fit_details=raw)else report
}
