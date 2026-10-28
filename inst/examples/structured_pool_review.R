# Forecast collateral cash and independently observed servicing reconcile separately.
run_structured_pool_review<-function(data=NULL,verify_reference=TRUE){
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/structured_pool_review.json',package='creditratingsystem'),simplifyVector=FALSE)
 output<-list()
 for(product in names(data$cases)){
  case<-data$cases[[product]];collateral<-case$retail_collateral;model<-NULL
  if(!is.null(collateral)){
   model<-fit_retail_risk_pools(collateral$training_rows,collateral$pooling_spec,target=collateral$target,training=collateral$training,model_id=case$design$model_id,model_version='1')$model
   fitted_pd<-model$parameters$segments[[1]]$pools[[1]]$statistics$means$pd
   case$path_input$scenario_contract$weights<-list(fitted_pd,1-fitted_pd)
   case$path_input$obligor_paths<-lapply(case$path_input$obligor_paths,function(row){row$conditional_pd<-if(row$date==case$path_input$scenario_contract$dates[[1]])fitted_pd else 0;row})
  }else fitted_pd<-case$path_input$scenario_contract$weights[[1]]
  pool<-do.call(build_credit_asset_pool,case$pool_input)$value
  forecast_inputs<-c(list(asset_pool=pool),case$path_input)
  paths<-do.call(simulate_asset_pool,forecast_inputs)$value
  args<-list(asset_pool=pool,pool_inputs=case$pool_input,forecast_paths=paths,forecast_inputs=forecast_inputs,servicing=case$servicing,allocations=case$allocations,pool_accounts=case$pool_accounts,coverage=case$coverage,design=case$design,cash_contract=case$cash_contract)
  audit<-do.call(asset_pool_backtest,args)$value
  if(!is.null(collateral)){
   asset<-case$pool_input$asset_contracts[[1]];origin<-case$pool_input$as_of;end<-tail(case$path_input$scenario_contract$dates,1)[[1]]
   assignment_inputs<-list(observations=list(collateral$observation),as_of=origin,information_cutoff=origin,expected_target=collateral$target,uncovered_policy='error')
   assignments<-do.call(assign_retail_risk_pool,c(list(fitted_model=model),assignment_inputs))$value
   fitted_means<-model$parameters$segments[[1]]$pools[[1]]$statistics$means
   prediction<-list(prediction_id=asset$asset_id,observation_id=asset$asset_id,entity_id=asset$asset_id,cluster_id=asset$default_group_id,segment=product,as_of=origin,forecast_known_at=origin,horizon_end=end,model_id=case$design$model_id,model_version='1',parameter_version='1',excluded_from_training=TRUE,excluded_from_calibration=TRUE,reference_exposure=100,pd=fitted_pd,lgd=fitted_means$lgd,ead=fitted_means$ead)
   risk_pool<-retail_pool_diagnostics(model,assignments,assignment_inputs,list(prediction),collateral$outcomes,collateral$workouts,collateral$links,collateral$estimation_profile,design=case$design)$value
  }else risk_pool<-list(applicability='NOT_APPLICABLE',reason='Commercial invoices use debtor/invoice contracts; retail household pooling does not apply. Asset-pool servicing, allocation and forecast diagnostics are supplied instead.')
  duplicate<-args;extra<-duplicate$allocations[[1]];extra$allocation_id<-'double-claim';extra$pool_id<-'another-pool';duplicate$allocations<-c(duplicate$allocations,list(extra))
  duplicate_audit<-do.call(asset_pool_backtest,duplicate)$value
  incomplete<-args;incomplete$coverage[[1]]$complete<-FALSE
  incomplete_audit<-do.call(asset_pool_backtest,incomplete)$value
  output[[product]]<-list(pool=pool,forecast_paths=paths,asset_pool_validation=audit,retail_pool_model=model,retail_pool_validation=risk_pool,duplicate_allocation_validation=duplicate_audit,incomplete_coverage_validation=incomplete_audit)
 }
 if(verify_reference){
  for(product in names(output)){
   row<-output[[product]];audit<-row$asset_pool_validation;expected_brier<-if(product=='SECURITIZATION_TRANCHE').64 else .04
   stopifnot(length(audit$violations)==0,abs(audit$metrics$default_probability$brier-expected_brier)<1e-12,all(vapply(audit$cash_residuals,function(r)abs(r$residual)<1e-12,logical(1))),row$duplicate_allocation_validation$violation_counts$CASH_OR_LOSS_DOUBLE_ALLOCATION>=1,is.null(row$incomplete_coverage_validation$metrics$default_probability$brier))
  }
  abs_case<-output$SECURITIZATION_TRANCHE;metrics<-abs_case$asset_pool_validation$metrics;p<-abs_case$retail_pool_validation$metrics$pools[[1]]
  stopifnot(abs(metrics$pool_cash_errors$recovery_gross$bias+22)<1e-12,metrics$observed_owned_flows$recovery_gross==30,abs(p$lgd_error$bias+.1)<1e-12,p$ead_error$bias==0,is.null(p$within_pool_discrimination$auc),output$COVERED_BOND$asset_pool_validation$metrics$prepayment_probability$observed_rate==1,output$TRADE_RECEIVABLES$asset_pool_validation$metrics$observed_owned_flows$dilution==10)
 }
 list(products=output,interpretation='Same collateral IDs connect prior frozen forecasts, observed servicing, ownership allocations and separate default-conditioned LGD/EAD diagnostics. A single realized pool supplies no confidence distribution or empirical homogeneity proof; contractual bond/tranche priority is tested in the product examples, not inferred from cash reconciliation.')
}
