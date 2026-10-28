# Fund default score, legally distinct borrowing bases and joint asset losses.
run_wholesale_fund_finance <- function(data=NULL,verify_reference=TRUE,return_fitted_models=FALSE) {
  if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata','wholesale_fund_finance.json',package='creditratingsystem'),simplifyVector=FALSE)
  output<-raw<-list()
  for(case in data$cases) {
    expected_basis<-c(NAV_backed='ELIGIBLE_NAV',subscription_line='UNCALLED_COMMITMENTS',levered_fund='ELIGIBLE_NAV')[[case$variant]]
    if(case$borrowing_base_basis!=expected_basis)stop('Variant and borrowing-base contract are incompatible')
    sid<-case$case_id;bases<-funding<-ops<-list()
    for(scenario in data$scenario_ids) {
      stress<-scenario=='stress'
      if(case$borrowing_base_basis=='UNCALLED_COMMITMENTS')eligible<-sum(vapply(Filter(function(p)p$eligible&&(!stress||p$stress_performing),case$commitments),function(p)(p$committed-p$paid_in)*p$haircut,numeric(1))) else eligible<-sum(vapply(Filter(function(p)p$eligible,case$assets),function(p)p$value*(if(stress)p$stress_price else 1)*p$haircut,numeric(1)))
      borrowing<-eligible*case$advance_rate;margin<-max(case$drawn-borrowing,0);redemptions<-if(stress)case$stress_redemptions else 0;cash<-case$cash+case$distributions-case$operating_cost;gap<-max(margin+redemptions-cash,0)
      bases[[length(bases)+1L]]<-list(scenario_id=scenario,basis=case$borrowing_base_basis,eligible_amount=eligible,borrowing_base=borrowing,margin_due=margin,redemptions_due=redemptions,available_cash=cash,liquidity_gap=gap)
      funding[[length(funding)+1L]]<-list(scenario_id=scenario,date='2024-06-01',currency='EUR',cash_available=case$cash+case$distributions,reserve_target=0,amounts_due=list(operating=case$operating_cost,margin=margin,redemptions=redemptions))
      ops[[length(ops)+1L]]<-list(scenario_id=scenario,period_start='2024-03-01',period_end='2024-06-01',currency='EUR',base_quantity=1,base_unit_price=case$distributions,base_unit_variable_cost=0,base_fixed_cost=case$operating_cost,volume_multiplier=1,price_multiplier=1,variable_cost_multiplier=1,fixed_cost_multiplier=1,working_capital_revenue_ratio=0,cash_taxes=0,capex=0,asset_disposals=0,other_operating_cash=0)
    }
    statement<-list(entity_id=sid,currency='EUR',as_of='2024-03-01',period_start='2023-03-01',period_end='2024-03-01',values=list(drawn=case$drawn,borrowing_base=bases[[1]]$borrowing_base),units=list(drawn='EUR',borrowing_base='EUR'))
    ratio<-build_financial_ratios(list(statement),list(list(name='borrowing_base_utilization',numerator=list(drawn=1),denominator=list(borrowing_base=1),denominator_policy='positive',output_unit='dimensionless')))$value[[1]]$ratios$borrowing_base_utilization$value
    tr<-case$score_training;prep<-fit_preprocessor(case$preprocessor_training,list(list(name='borrowing_base_utilization',type='numeric',imputation='error',scaling='standard')),target=data$target,training=tr$training,model_id=paste0(sid,'-preprocessor'),model_version='1')
    design<-transform_features(list(list(borrowing_base_utilization=ratio)),prep)$value
    model<-fit_scorecard(do.call(rbind,lapply(tr$X,unlist)),unlist(tr$y),weights=unlist(tr$weights),feature_order=unlist(tr$feature_order),fit_spec=tr$fit_spec,target=data$target,training=tr$training,model_id=paste0(sid,'-score'),model_version='1')
    score<-score_scorecard(model,design$design_matrix,feature_order=unlist(tr$feature_order),expected_target=data$target)
    cc<-case$calibration;cal<-calibrate_logistic(unlist(cc$values),unlist(cc$outcomes),unlist(cc$weights),metadata=cc$metadata,tolerance=cc$fit_controls$tolerance,max_iterations=cc$fit_controls$max_iterations)
    prediction<-apply_calibrator(cal,score$value$pd,target=data$target,base_model_version='1');pd<-round(prediction$value[[1]],data$precision_contract$pd_decimal_places)
    operating<-simulate_operating_cashflows(ops,list(contract_id=paste0(sid,'-distributions'),version='1',currency='EUR',period_unit='quarter',quantity_unit='portfolio_distribution',initial_working_capital=0,include_asset_disposals_in_cfads=FALSE))$value
    waterfall<-apply_cash_waterfall(funding,list(policy_id=paste0(sid,'-cash-rights'),version='1',initial_reserve=0,reserve_priority=4,excess_reserve_policy='retain',arrears_policy='carry_without_interest',claims=lapply(seq_along(c('operating','margin','redemptions')),function(i)list(claim_id=c('operating','margin','redemptions')[i],priority=i,reserve_eligible=FALSE))),currency='EUR')$value
    assetloss<-lapply(case$assets,function(a)list(facility_id=a$asset_id,currency='EUR',scenario_ids=data$scenario_ids,losses=c(0,a$value*(1-a$stress_price))))
    losses<-aggregate_portfolio_loss(assetloss,lapply(case$assets,function(a)list(facility_id=a$asset_id,portfolio_id=sid,share=1)),unlist(data$scenario_weights),list(EUR=1),scenario_ids=unlist(data$scenario_ids),reporting_currency='EUR',probability_measure='PHYSICAL',quantile_levels=.9)$value
    facts<-list(product='FUND_FINANCE',variant=case$variant,borrower_class='FUND',legal_claim=case$legal_claim,borrowing_base_basis=case$borrowing_base_basis)
    snapshot<-list(subject_id=sid,subject_type='facility',as_of='2024-03-01',known_at='2024-03-01',attributes=lapply(facts,function(v)list(value=v,observed_at='2024-03-01',known_at='2024-03-01')))
    rule<-list(rule_id=sid,profile_id=sid,profile_version='1',priority=1,effective_from='2023-01-01',effective_to=NULL,known_at='2023-01-01',conditions=lapply(names(facts),function(k)list(field=k,operator='eq',value=facts[[k]])))
    route<-route_rating_profile(snapshot,list(rule),'2024-03-01','2024-03-01')$value;snapshot$attributes$borrowing_base_basis$value<-if(case$borrowing_base_basis=='UNCALLED_COMMITMENTS')'ELIGIBLE_NAV' else 'UNCALLED_COMMITMENTS';incompatible<-route_rating_profile(snapshot,list(rule),'2024-03-01','2024-03-01')$value
    z<-2*(ratio-1);p0<-cc$values[[1]];p1<-cc$values[[3]];oracle<-plogis((1-z)/2*qlogis(p0)+(1+z)/2*qlogis(p1))
    if(verify_reference) {
      stopifnot(abs(prediction$value[[1]]-oracle)<data$precision_contract$raw_fit_oracle_absolute_tolerance,abs(bases[[1]]$borrowing_base-case$reference$initial_base)<1e-12,abs(bases[[2]]$borrowing_base-case$reference$stress_base)<1e-12,abs(bases[[2]]$margin_due-case$reference$stress_margin)<1e-12,abs(bases[[2]]$liquidity_gap-case$reference$stress_liquidity_gap)<1e-12)
      portfolio<-losses$portfolios[[1]];stopifnot(abs(portfolio$path_losses[[2]]-case$reference$underlying_stress_loss)<1e-12,abs(portfolio$expected_loss-.2*case$reference$underlying_stress_loss)<1e-12,abs(sum(vapply(waterfall$periods[[2]]$payments,function(p)p$closing_arrears,numeric(1)))-bases[[2]]$liquidity_gap)<1e-12,incompatible$status=='NO_ROUTE')
    }
    output[[length(output)+1L]]<-list(variant=case$variant,case_id=sid,borrowing_bases=bases,ratio=ratio,reported_fund_pd=pd,grade=map_pd_to_grade(pd,data$rating_scale)$value,operating=operating,liquidity=waterfall,underlying_asset_loss=losses,profile=route$profile_id,incompatible_basis_status=incompatible$status)
    if(return_fitted_models)raw[[length(raw)+1L]]<-list(case_id=sid,preprocessor=prep,scorecard=model,calibrator=cal,fundamental=score,calibrated=prediction)
  }
  report<-list(product=data$product,data_origin='SYNTHETIC',precision_contract=data$precision_contract,scenario_law=data$scenario_law,cases=output)
  if(return_fitted_models)list(report=report,raw_native_fit_details=raw) else report
}
