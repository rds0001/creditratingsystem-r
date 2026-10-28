simulate_operating_cashflows<-function(driver_paths,operating_contract,context=NULL){
 rows<-.cr_data_rows(driver_paths,'driver_paths');contract<-operating_contract
 if(!.cr_object(contract)) .cr_error('SIMULATION_OPERATING','Explicit operating contract required')
 .cr_data_required(contract,c('contract_id','version','currency','period_unit','quantity_unit','initial_working_capital','include_asset_disposals_in_cfads'))
 for(key in c('contract_id','version','currency','period_unit','quantity_unit')) .cr_sim_text(contract[[key]],key)
 if(!(contract$period_unit%in%c('day','month','quarter','year'))||!is.logical(contract$include_asset_disposals_in_cfads)||length(contract$include_asset_disposals_in_cfads)!=1L||is.na(contract$include_asset_disposals_in_cfads)) .cr_error('SIMULATION_OPERATING','Declare period unit and Boolean disposal treatment')
 initial<-.cr_sim_number(contract$initial_working_capital,'initial_working_capital',0);ctx<-.cr_result(NULL,'simulation.context',context=context)$context
 if(!is.null(ctx$currency)&&ctx$currency!=contract$currency) .cr_error('TARGET_MISMATCH','Operating currency conflicts with context')
 groups<-list();seen<-new.env(parent=emptyenv())
 for(row in rows){
 .cr_data_required(row,c('scenario_id','period_start','period_end','currency','base_quantity','base_unit_price','base_unit_variable_cost','base_fixed_cost','volume_multiplier','price_multiplier','variable_cost_multiplier','fixed_cost_multiplier','working_capital_revenue_ratio','cash_taxes','capex','asset_disposals','other_operating_cash'))
 start<-.cr_data_date(row$period_start,'period_start');end<-.cr_data_date(row$period_end,'period_end');key<-.cr_hash(list(row$scenario_id,row$period_end))
 if(end<=start||exists(key,seen,inherits=FALSE)) .cr_error('SIMULATION_CALENDAR','Positive unique operating periods required')
 if(row$currency!=contract$currency) .cr_error('TARGET_MISMATCH','Operating records must share reporting currency')
 assign(key,TRUE,seen);groups[[row$scenario_id]]<-c(groups[[row$scenario_id]],list(row))
 }
 outputs<-list()
 for(scenario in sort(names(groups))){
 periods<-groups[[scenario]];periods<-periods[order(vapply(periods,function(r)r$period_end,character(1)))];nwc<-initial;last_end<-NULL
 for(row in periods){
 if(!is.null(last_end)&&row$period_start!=last_end) .cr_error('SIMULATION_CALENDAR','Operating periods must be contiguous within each scenario')
 last_end<-row$period_end;keys<-c('base_quantity','base_unit_price','base_unit_variable_cost','base_fixed_cost','volume_multiplier','price_multiplier','variable_cost_multiplier','fixed_cost_multiplier','working_capital_revenue_ratio','capex','asset_disposals');v<-setNames(lapply(keys,function(k).cr_sim_number(row[[k]],k,0)),keys)
 quantity<-v$base_quantity*v$volume_multiplier;price<-v$base_unit_price*v$price_multiplier;revenue<-quantity*price;variable_cost<-quantity*v$base_unit_variable_cost*v$variable_cost_multiplier;fixed_cost<-v$base_fixed_cost*v$fixed_cost_multiplier;tax<-.cr_sim_number(row$cash_taxes,'cash_taxes');other<-.cr_sim_number(row$other_operating_cash,'other_operating_cash')
 closing_nwc<-revenue*v$working_capital_revenue_ratio;delta_nwc<-closing_nwc-nwc;ebitda<-revenue-variable_cost-fixed_cost;disposal<-if(contract$include_asset_disposals_in_cfads)v$asset_disposals else 0;cfads<-ebitda-tax-v$capex-delta_nwc+other+disposal
 if(any(!is.finite(c(quantity,price,revenue,variable_cost,fixed_cost,closing_nwc,delta_nwc,ebitda,cfads)))) .cr_error('NUMERICAL_FAILURE','Operating bridge exceeds finite representation')
 outputs[[length(outputs)+1L]]<-list(scenario_id=scenario,period_start=row$period_start,period_end=row$period_end,currency=contract$currency,quantity=quantity,unit_price=price,revenue=revenue,variable_cost=variable_cost,fixed_cost=fixed_cost,ebitda=ebitda,cash_taxes=tax,capex=v$capex,opening_working_capital=nwc,closing_working_capital=closing_nwc,change_working_capital=delta_nwc,other_operating_cash=other,asset_disposals=v$asset_disposals,disposals_in_cfads=disposal,cfads=cfads);nwc<-closing_nwc
 }
 }
 .cr_result(list(periods=outputs,contract=contract,contract_hash=.cr_hash(contract)),'simulation.simulate_operating_cashflows','operating_cashflows',ctx,diagnostics=list(input_fingerprint=.cr_hash(rows)),limitations='Explicit quantity-price, variable/fixed-cost and working-capital-to-period-revenue model, not universal sector model. Inputs are totals for declared periods and consistent quantity units. Taxes supplied cash amounts including refunds; no tax-loss or interest-tax shield inferred. Financing, reserves and debt service excluded from operating CFADS. Driver scenarios may be dependent but no independent shocks silently generated.')
}

simulate_asset_values<-function(asset_parameters,market_paths,context=NULL){
 rows<-.cr_data_rows(market_paths,'market_paths');params<-asset_parameters
 if(!.cr_object(params)) .cr_error('SIMULATION_ASSET','Explicit physical asset parameters required')
 .cr_data_required(params,c('asset_id','currency','valuation_date','initial_value','salvage_value','initial_age_years','economic_life_years','depreciation_model','idiosyncratic_volatility','probability_measure','year_basis_days'))
 for(key in c('currency','probability_measure','depreciation_model')) .cr_sim_text(params[[key]],key)
 valuation<-.cr_data_date(params$valuation_date,'valuation_date')
 if(params$probability_measure!='PHYSICAL'||!(params$depreciation_model%in%c('straight_line','geometric_to_salvage'))) .cr_error('SIMULATION_ASSET','Physical P-measure and explicit supported ageing model required')
 initial<-.cr_sim_number(params$initial_value,'initial_value',0);salvage<-.cr_sim_number(params$salvage_value,'salvage_value',0);age<-.cr_sim_number(params$initial_age_years,'initial_age_years',0);life<-.cr_sim_number(params$economic_life_years,'economic_life_years',0);sigma<-.cr_sim_number(params$idiosyncratic_volatility,'idiosyncratic_volatility',0);basis<-.cr_sim_number(params$year_basis_days,'year_basis_days',0)
 if(initial<salvage||life<=age||basis<=0) .cr_error('SIMULATION_ASSET','Initial value >= salvage, remaining economic life and positive day basis required')
 depreciation<-NULL;if(params$depreciation_model=='geometric_to_salvage'){.cr_data_required(params,'annual_depreciation_rate');depreciation<-.cr_sim_number(params$annual_depreciation_rate,'annual_depreciation_rate',0)}
 ctx<-.cr_result(NULL,'simulation.context',context=context)$context
 if(!is.null(ctx$probability_measure)&&ctx$probability_measure!='PHYSICAL') .cr_error('TARGET_MISMATCH','Physical asset paths cannot be labelled risk-neutral')
 if(!is.null(ctx$currency)&&ctx$currency!=params$currency) .cr_error('TARGET_MISMATCH','Asset currency conflicts with context')
 groups<-list();seen<-new.env(parent=emptyenv())
 for(row in rows){
 .cr_data_required(row,c('scenario_id','date','market_index','innovation'));date<-.cr_data_date(row$date,'date');key<-.cr_hash(list(row$scenario_id,row$date))
 if(date<=valuation||exists(key,seen,inherits=FALSE)) .cr_error('SIMULATION_CALENDAR','Unique asset dates strictly after valuation required')
 assign(key,TRUE,seen);groups[[row$scenario_id]]<-c(groups[[row$scenario_id]],list(row))
 }
 outputs<-list()
 for(scenario in sort(names(groups))){
 periods<-groups[[scenario]];periods<-periods[order(vapply(periods,function(r)r$date,character(1)))];last<-valuation;log_idio<-0
 for(row in periods){
 date<-.cr_data_date(row$date,'date');dt<-as.numeric(date-last)/basis;elapsed<-as.numeric(date-valuation)/basis;market<-.cr_sim_number(row$market_index,'market_index',0);innovation<-.cr_sim_number(row$innovation,'innovation');increment<--.5*sigma*sigma*dt+sigma*sqrt(dt)*innovation;log_idio<-log_idio+increment
 if(!is.finite(log_idio)||log_idio<= -709||log_idio>=709) .cr_error('NUMERICAL_FAILURE','Asset innovation path exceeds supported finite log range')
 factor<-exp(log_idio);remaining<-max(life-age-elapsed,0);ageing<-if(params$depreciation_model=='straight_line')remaining/(life-age)else exp(-depreciation*elapsed)
 baseline<-salvage+(initial-salvage)*ageing;market_value<-baseline*market;value<-market_value*factor
 if(!is.finite(value)) .cr_error('NUMERICAL_FAILURE','Asset value exceeds finite representation')
 outputs[[length(outputs)+1L]]<-list(scenario_id=scenario,asset_id=params$asset_id,date=row$date,currency=params$currency,probability_measure='PHYSICAL',age_years=age+elapsed,remaining_life_years=remaining,ageing_baseline=baseline,ageing_contribution=baseline-initial,market_index=market,market_contribution=market_value-baseline,idiosyncratic_factor=factor,idiosyncratic_contribution=value-market_value,asset_value=value,year_fraction=dt,innovation=innovation);last<-date
 }
 }
 .cr_result(list(paths=outputs,parameters=params,parameters_hash=.cr_hash(params)),'simulation.simulate_asset_values','asset_values',ctx,diagnostics=list(input_fingerprint=.cr_hash(rows)),limitations='Physical market values are not collateral realisation proceeds. Salvage is an ageing baseline, not guaranteed price floor: market and idiosyncratic factors affect it. Idiosyncratic innovations must be standard normal with declared joint dependence; exp(-sigma^2 dt/2) gives unit mean under that law, not for arbitrary stress sample. Market indices are levels relative to valuation, not incremental returns. Geometric ageing approaches salvage asymptotically; declared life remains diagnostic.')
}
