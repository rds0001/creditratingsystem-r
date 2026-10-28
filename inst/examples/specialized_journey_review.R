# Source-bound specialist governance and genuine collateral-kernel convergence.
.specialized_kernel_convergence<-function(case,contract){
 params<-case$asset_parameters;params$idiosyncratic_volatility<-contract$volatility;origin<-as.Date(params$valuation_date);sigma<-params$idiosyncratic_volatility;basis<-params$year_basis_days;fine<-contract$fine_days;T<-contract$horizon_days/basis
 age<-params$initial_age_years;life<-params$economic_life_years;remaining<-max(life-age-T,0);ageing<-if(params$depreciation_model=='straight_line')remaining/(life-age)else exp(-params$annual_depreciation_rate*T);baseline<-params$salvage_value+(params$initial_value-params$salvage_value)*ageing;runs<-list()
 for(rep in names(contract$innovations)){
  innovation<-contract$innovations[[rep]]
  for(size in unlist(contract$sample_sizes)){
   ids<-innovation$path_ids[seq_len(size)];zs<-innovation$values[seq_len(size)]
   for(step in c(fine,2*fine)){
    rows<-list()
    for(i in seq_along(ids)){sid<-ids[[i]];z<-unlist(zs[[i]]);shocks<-if(step==fine)z else c((z[1]+z[2])/sqrt(2),(z[3]+z[4])/sqrt(2));for(k in seq_along(shocks))rows[[length(rows)+1L]]<-list(scenario_id=sid,date=as.character(origin+step*k),market_index=1,innovation=shocks[k])}
    result<-simulate_asset_values(params,rows)$value;terminal_rows<-Filter(function(r)r$date==as.character(origin+contract$horizon_days),result$paths);terminal<-setNames(lapply(terminal_rows,function(r)r$asset_value),vapply(terminal_rows,function(r)r$scenario_id,character(1)));values<-list()
    for(i in seq_along(ids)){sid<-ids[[i]];z<-unlist(zs[[i]]);oracle<-baseline*exp(-.5*sigma*sigma*T+sigma*sqrt(fine/basis)*sum(z));stopifnot(abs(terminal[[sid]]-oracle)<=1e-11+1e-12*abs(oracle));values[[i]]<-round(terminal[[sid]],2)}
    runs[[length(runs)+1L]]<-list(replicate_id=rep,time_step=step,sample_size=size,path_ids=ids,values=values,innovation_fingerprint=content_fingerprint(zs)$value)
   }
  }
 }
 target<-list(event='default',horizon=contract$horizon_days,time_unit='days',probability_measure='PHYSICAL',conditioning='PIT');design<-list(model_id=paste0(case$case_id,'-collateral-kernel'),model_version='1',parameter_version='1',data_fingerprint=content_fingerprint(params)$value,population_id=case$case_id,split_id='prospective-kernel-check',prediction_origin='EXTERNAL',evaluation_cutoff=params$valuation_date,target=target,currency=params$currency)
 sampling<-list(sampling_unit='iid_paths_with_independent_replicates',metric='mean',finest_time_step=fine,time_unit='days',reference=contract$scope,analytic_reference=list(value=baseline,source='Unit-mean lognormal multiplier under explicitly independent standard normals, exact deterministic ageing; cent-rounding expectation error bounded by 0.005 EUR'))
 result<-simulation_convergence(runs,contract$innovations,sampling_contract=sampling,precision=list(confidence_level=.95,mc_half_width=.5,grid_difference_tolerance=.005),design=design)$value
 stopifnot(all(vapply(result$metrics$paired_grid_differences,function(x)x$estimate==0,logical(1))))
 list(result=result,parameters=params,analytic_unrounded_mean=baseline,reporting_convention=contract$reporting_convention,stream=contract$stream,scope=contract$scope)
}
run_specialized_journey_review<-function(data=NULL,verify_reference=TRUE,accounts_path=NULL,releases_path=NULL,governance_data=NULL,model_path=NULL,return_artifacts=FALSE,engine_paths=NULL){
 resource<-function(name)system.file('extdata',name,package='creditratingsystem')
 if(is.null(data))data<-jsonlite::fromJSON(resource('specialized_journey_review.json'),simplifyVector=FALSE)
 if(is.null(governance_data))governance_data<-jsonlite::fromJSON(resource(data$governance_resource),simplifyVector=FALSE)
 helper<-function(name){path<-if(is.null(engine_paths))system.file('examples',paste0(name,'.R'),package='creditratingsystem')else engine_paths[[name]];env<-new.env(parent=environment());sys.source(path,env);env}
 selected<-vapply(data$cases,function(c)c$case_id,character(1));governance_data$journeys<-Filter(function(j)j$case_id%in%selected,governance_data$journeys)
 le<-helper('input_lineage');lineage<-le$prepare_input_snapshot(if(is.null(accounts_path))resource('specialized_positions.csv')else accounts_path,if(is.null(releases_path))resource('specialized_releases.csv')else releases_path,data$lineage_config,data$lineage_config$cutoffs[[1]]);rows<-setNames(lineage$rows,vapply(lineage$rows,function(x)x$entity_id,character(1)))
 for(j in governance_data$journeys){case<-Filter(function(c)c$case_id==j$case_id,data$cases)[[1]];p<-j$frozen_arguments$parameter_snapshot$parameters[[j$case_id]];source<-rows[[j$case_id]];if(source$product_id!=j$product||source$balance!=sum(vapply(p$loans,function(l)l$terms$principal,numeric(1)))||source$x!=p$operating_base[[1]]$base_unit_price||!identical(case$asset_parameters,p$assets[[1]]$parameters))stop('Source must bind actual loan principal, operating input and frozen asset profile')}
 ge<-helper('specialized_governance');governance<-ge$run_specialized_governance(governance_data,verify_reference,model_path,return_audit_artifacts=TRUE);tools<-helper('_wholesale_journey_tools');out<-list();artifacts<-list()
 for(case in data$cases){sid<-case$case_id;audit<-governance$journeys[[sid]];decision<-audit$results[[1]]$value$decision;reason<-setNames(lapply(decision$rule_trace,function(r)paste0('Declared policy rule ',r$rule_id)),vapply(decision$rule_trace,function(r)r$reason_code,character(1)));explanation<-decompose_policy_decision(decision,reason)$value;replay<-tools$.sector_journey_replay(audit,rows[[sid]]);convergence<-.specialized_kernel_convergence(case,data$convergence)
  if(verify_reference)stopifnot(lineage$audit$decision$status=='PASS')
  out[[length(out)+1L]]<-list(case_id=sid,product=case$product,claim_ids=case$claim_ids,source_snapshot_hash=lineage$selected_snapshot_hash,governance=audit$report,policy_explanation=explanation,replay=replay$result,negative_replay=replay$negative,collateral_convergence=convergence);artifacts[[length(artifacts)+1L]]<-list(case_id=sid,governance=audit,replay_inputs=replay$inputs)
 }
 report<-list(data_origin='SYNTHETIC',lineage=lineage$audit,archive_sha256=lineage$archive_sha256,cases=out)
 if(return_artifacts)list(report=report,artifacts=artifacts)else report
}
