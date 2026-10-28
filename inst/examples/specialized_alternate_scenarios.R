# Actual alternative macro fits and immutable economic replay on five product contracts.
run_specialized_alternate_scenarios <- function(data=NULL,verify_reference=TRUE,artifact_paths=NULL,return_audit_artifacts=FALSE,ledger_engine_path=NULL,scenario_methods=NULL){
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/specialized_alternate_scenarios.json',package='creditratingsystem'),simplifyVector=FALSE)
 mapping<-c(vecm='fit_vecm',regime='fit_regime_switching',factor='fit_dynamic_factor',copula='fit_copula');local<-list();models<-list()
 for(key in names(mapping)){
  args<-data$fit_arguments[[key]]
  if(key=='copula'){args$observations<-do.call(rbind,lapply(args$observations,unlist));args$variable_ids<-unlist(args$variable_ids)}
  local[[key]]<-do.call(get(mapping[[key]]),args)
  name<-data$model_resources[[key]];path<-if(is.null(artifact_paths))system.file('extdata',name,package='creditratingsystem')else artifact_paths[[name]];models[[key]]<-load_fitted_model(path)$value
 }
 stopifnot(abs(local$vecm$model$parameters$beta[[2]][[1]]+1)<.025,local$regime$diagnostics$converged,local$factor$diagnostics$converged)
 means<-vapply(local$regime$model$parameters$coefficients,function(v)v[[1]][[1]],numeric(1));stopifnot(all(abs(means-c(-2,2))<.03),local$factor$diagnostics$log_likelihood>local$factor$diagnostics$initial_log_likelihood+10,abs(local$copula$diagnostics$kendall_tau[[1]][[2]]-11/14)<1e-12)
 base<-fit_var(data$baseline_macro_history,lags=1,training=data$baseline_macro_training,target=data$target,model_id='specialized-macro',model_version='1',covariance_estimator='mle')
 scenarios<-data$scenario_contract;ids<-vapply(scenarios,function(s)s$scenario_id,character(1));weights<-vapply(scenarios,function(s)s$weight,numeric(1));n<-length(ids);z<-do.call(rbind,lapply(scenarios,function(s)unlist(s$innovations)))
 draws<-function(k,factors)list(kind='STANDARD_NORMAL',values=lapply(seq_len(n),function(i)lapply(1:2,function(t)as.list(rep(z[i,t],k)))),factor_ids=as.list(factors),scenario_ids=as.list(ids),time_grid=list(1,2),weights=as.list(weights))
 paths<-list();laws<-list();macro_audits<-list();extract<-function(cube)do.call(rbind,lapply(cube$values,function(x)vapply(x,function(v)v[[1]],numeric(1))))
 for(key in c('vecm','regime','factor')){
  model<-models[[key]];p<-model$parameters;factors<-if(key=='factor')unlist(p$state_ids)else unlist(model$feature_order)
  args<-list(fitted_model=model,innovations=draws(length(factors),factors))
  if(key=='regime')args$regime_uniforms<-lapply(seq_len(n),function(i)as.list(rep(if((i-1)%%2==0).05 else .95,3)))
  if(key=='factor'){args$sampling_contract<-list(law='DECLARED_FIXED_STATE',assumption='Conditional sensitivity at the fitted filtered-state mean; posterior initial uncertainty is not integrated');args$initial_state<-unlist(p$filtered_state_end)}
  cube<-do.call(simulate_macro_scenarios,args)$value;anchor<-if(key=='vecm')tail(data$fit_arguments$vecm$level_history$values,1)[[1]][[1]]else 0
  paths[[key]]<-extract(cube)-anchor;laws[[key]]<-cube$sampling_contract;macro_audits[[key]]<-cube
 }
 baseline<-simulate_macro_scenarios(base,draws(1,'market'))$value
 conditional<-condition_macro_paths(base,draws(1,'market'),list(list(time_index=1,factor_id='market',value=-1)))$value
 paths$conditional<-extract(conditional);laws$conditional<-conditional$sampling_contract;macro_audits$conditional<-conditional
 a<-base$model$parameters$lag_coefficients[[1]][[1]][[1]];b<-base$model$parameters$intercept[[1]];q<-base$model$parameters$covariance[[1]][[1]];initial<-tail(base$model$parameters$initial_history,1)[[1]][[1]];m1<-b+a*initial;m2<-b+a*m1
 if(verify_reference){raw<-extract(baseline);expected<-raw[,1]+a/(1+a*a)*(-1-raw[,2]);stopifnot(all(abs(paths$conditional[,1]-expected)<1e-12),all(abs(paths$conditional[,2]+1)<1e-12))}
 reverse<-reverse_stress_scenario(base,function(x)100*(1-exp(.2*x[2,1])),horizon=2,target_loss=30,shock_metric=diag(2),tolerance=1e-8)$value
 target<-log(.7)/.2;vv<-c(a,1)*sqrt(q);optimal<-(target-m2)*vv/sum(vv*vv);reverse_path<-c(m1+sqrt(q)*optimal[1],target)
 stopifnot(reverse$feasible,all(abs(unlist(reverse$shocks)-optimal)<2e-5),abs(reverse$objective-.5*sum(optimal^2))<2e-5)
 paths$reverse<-matrix(rep(reverse_path,each=n),n,2);laws$reverse<-list(law='CONSTRAINED_PRICE_MARKDOWN',conditioning_scope='CONDITIONAL_STRESS',constraint='30percent second-year common collateral price decline',analytic_solution_used_after_solver_verification=TRUE)
 residual<-rbind(c(-.6,-.3),c(.2,.4),c(.8,.6),c(-.4,-.7));uniform<-lapply(seq_len(n),function(i)lapply(1:2,function(t)as.list(rep(pnorm(z[i,t]),2))))
 bootstrap<-block_bootstrap_scenarios(residual,uniform,factor_ids=c('market','capacity'),scenario_ids=ids,time_grid=c(1,2),frequency='annual',block_length=2,scheme='moving',weights=weights)
 if(verify_reference)for(i in seq_len(n)){start<-min(floor(3*uniform[[i]][[1]][[1]]),2);stopifnot(identical(as.numeric(unlist(bootstrap$diagnostics$selected_indices[[i]])),c(start,start+1)),all(abs(do.call(rbind,lapply(bootstrap$value$values[[i]],unlist))-residual[(start+1):(start+2),])<1e-12))}
 paths$bootstrap<-extract(bootstrap$value);laws$bootstrap<-list(law='JOINT_MOVING_BLOCK_BOOTSTRAP',conditioning_scope='PIT_EMPIRICAL_STRESS',block_length=2,cycle_equilibrium_claimed=FALSE);macro_audits$bootstrap<-bootstrap
 dependent<-simulate_copula(models$copula,pnorm(z),variable_ids=c('market','capacity'))$value;normal<-qnorm(if(is.matrix(dependent))dependent else do.call(rbind,lapply(dependent,unlist)));rho<-sin(pi*(11/14)/2)
 if(verify_reference)stopifnot(all(abs(normal[,2]-(rho*z[,1]+sqrt(1-rho*rho)*z[,2]))<1e-11))
 paths$copula<-normal;laws$copula<-list(law='FITTED_GAUSSIAN_COPULA_TWO_DATE_MARKET',conditioning_scope='PIT',time_dependence='Fitted joint two-date ranks, no temporal Markov claim')
 if(is.null(ledger_engine_path))ledger_engine_path<-system.file('examples/specialized_asset_finance.R',package='creditratingsystem')
 e<-new.env(parent=environment());sys.source(ledger_engine_path,envir=e);reports<-list();executions<-list()
 selected<-if(is.null(scenario_methods))names(paths)else unlist(scenario_methods)
 if(!length(selected)||anyDuplicated(selected)||length(setdiff(selected,names(paths))))stop('Select distinct supported scenario methods')
 for(law in selected){
  price<-exp(.2*paths[[law]]);sampling<-list(law='FORECAST_CONDITIONAL',conditioning_scope=if(is.null(laws[[law]]$conditioning_scope))'PIT'else laws[[law]]$conditioning_scope,source_sampling_contract=laws[[law]]);sampling$ensemble<-'Original18explicit weighted innovation/counterparty nodes; no IID confidence interval or unconditional frequency estimate'
  cube<-list(schema_version='1.0',kind='ScenarioCube',axes=list('scenario','time','factor'),scenario_ids=as.list(ids),time_grid=list(1,2),weights=as.list(weights),origin='2023-01-01',frequency='annual',probability_measure='PHYSICAL',sampling_contract=sampling,factor_ids=list(),units=list(),values=lapply(ids,function(i)list(list(),list())))
  for(case in data$cases){
   key<-case$case_id;cube$factor_ids<-c(cube$factor_ids,list(paste0(key,'-revenue'),paste0(key,'-asset')));cube$units<-c(cube$units,list('relative_index','relative_index'))
   for(s in seq_len(n))for(t in 1:2){completion<-if(is.null(case$completion_multiplier))1 else case$completion_multiplier[[t]];cube$values[[s]][[t]]<-c(cube$values[[s]][[t]],list(round(1-case$floating_share[[t]]+case$floating_share[[t]]*price[s,t],12),round(price[s,t]*completion,12)))}
  }
  params<-data$parameter_snapshot;recipes<-lapply(data$cases,function(c)list(transaction_id=c$case_id,version='1',subject_id=c$case_id,parameter_id=c$case_id,calendar=data$calendar,parameter_hash=content_fingerprint(params$parameters[[c$case_id]])$value,cube_hash=content_fingerprint(cube)$value))
  execution<-simulate_transactions(recipes,cube,data$entity_innovations,params)$value;records<-list()
  for(case in data$cases){
   tx<-Filter(function(t)t$transaction_id==case$case_id,execution$transactions)[[1]];checks<-if(verify_reference)lapply(seq_len(n),function(s)e$.specialized_ledger_oracle(case,scenarios[[s]],price[s,],tx))else list()
   if(verify_reference)stopifnot(abs(tx$risk$pd-sum(weights*vapply(checks,function(c)as.numeric(c$defaulted),numeric(1))))<1e-12,abs(tx$risk$expected_loss-sum(weights*vapply(checks,function(c)c$loss,numeric(1))))<1e-10)
   records[[length(records)+1L]]<-list(product=case$product,case_id=case$case_id,risk=tx$risk,ledger_oracles=checks,transaction_hash=content_fingerprint(tx)$value)
  }
  reports[[law]]<-list(sampling_contract=cube$sampling_contract,scenario_cube_hash=execution$scenario_cube_hash,transactions=records);executions[[law]]<-list(cube=cube,execution=execution)
 }
 report<-list(data_origin='SYNTHETIC',reporting_contract=data$reporting_contract,fit_checks=list(vecm_cointegrating_relation=TRUE,two_distinct_hidden_regimes=TRUE,missing_observation_integrated=TRUE,kalman_likelihood_improved=TRUE,copula_kendall_tau=11/14),reverse_oracle=list(price_markdown=.3,shocks=as.list(optimal),objective=.5*sum(optimal^2),path=as.list(reverse_path)),alternatives=reports)
 if(return_audit_artifacts)list(report=report,audits=list(local_fits=local,macro_paths=macro_audits,reverse_solver=reverse,executions=executions))else report
}
