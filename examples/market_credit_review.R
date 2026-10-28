# Quote calibration, identification and a separately observed physical bridge.
run_market_credit_review <- function(data=NULL,verify_reference=TRUE,return_artifacts=FALSE) {
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/market_credit_review.json',package='creditratingsystem'),simplifyVector=FALSE)
 snapshot<-build_credit_quote_snapshot(data$quotes,data$instruments,data$discount,data$snapshot_contract)$value
 cds_rows<-Filter(function(q)isTRUE(q$eligible)&&q$quote_type=='CDS_SPREAD',snapshot$quotes)
 bond_rows<-lapply(Filter(function(q)isTRUE(q$eligible)&&q$quote_type=='BOND_CLEAN_PRICE',snapshot$quotes),function(q){q$price_standard_error<-data$price_standard_error;q})
 cf<-bootstrap_cds_hazard_curve(cds_rows,data$discount,data$cds_calibration)$value
 bf<-calibrate_bond_hazard_curve(bond_rows,data$discount,data$bond_calibration)$value
 priced<-list();reference<-setNames(data$instruments,vapply(data$instruments,function(i)i$instrument_id,character(1)))
 for(key in names(reference)){
  ins<-reference[[key]]
  priced[[key]]<-if(ins$instrument_type=='CDS')cds_expected_cashflow_legs(cf$hazard_curve,data$discount,ins)$value else risky_bond_expected_cashflows(bf$hazard_curve,data$discount,ins)$value
 }
 alternatives<-list(list(scenario_id='recovery20',recovery=.2),list(scenario_id='recovery60',recovery=.6))
 ident_contract<-list(equivalence_tolerance=1e-8,quote_standard_errors=as.list(rep(.0001,length(cds_rows))),recovery_bounds=list(0,.9))
 unidentified<-diagnose_market_credit_identifiability(cf,alternatives,ident_contract)$value
 constrained<-ident_contract;constrained$independent_recovery_observation<-list(value=.4,standard_error=.01,evidence_id='separate-synthetic-recovery-study',known_at='2025-01-01')
 identified<-diagnose_market_credit_identifiability(cf,alternatives,constrained)$value
 basis<-compare_cds_bond_credit_basis(cf,bf,list(recovery=.4,bond_liquidity_spread=0,funding_treatment='NO_ADDITIONAL_FUNDING_WEDGE',maximum_quote_age_days=5,comparison_cds_contract=reference[['cds-3']]))$value
 fitted<-fit_market_to_physical_pd_bridge(data$bridge_observations,'q_pd',data$bridge_contract,data$folds,model_id='observed-market-bridge',model_version='1')
 q<--expm1(-cf$hazard_curve$rates[[1]])
 physical<-as.numeric(predict_pd(fitted,matrix(q,nrow=1),feature_order='q_pd',expected_target=data$bridge_contract$target)$value)[[1]]
 subject<-list(subject_id='REFERENCE-ISSUER',subject_type='obligor');qt<-data$bridge_contract$target;qt$probability_measure<-'RISK_NEUTRAL'
 components<-list(c(list(component_id='market-Q',quantity='PD',value=q),subject,list(variant='market-pricing',definition=qt)),c(list(component_id='historical-P',quantity='PD',value=physical),subject,list(variant='observed-issuer-bridge',definition=data$bridge_contract$target)))
 binding<-list(profile_id='market-review',profile_version='1',model_id=fitted$model$model_id,model_version='1',model_hash=content_fingerprint(fitted$model)$value,input_hash=content_fingerprint(list(snapshot=snapshot$snapshot_hash,q_pd=q))$value)
 assessment<-compose_risk_assessment('market-review',subject,components,binding,'2025-01-01','2025-01-01','2025-01-01')$value
 coeff<-fitted$model$parameters$coefficients
 output<-list(snapshot=snapshot,pricing=priced,cds_hazards=cf$hazard_curve$rates,bond_hazards=bf$hazard_curve$rates,bond_objective=bf$objective,identification=list(rank_without_recovery=unidentified$joint_identifiability$rank,rank_with_recovery=identified$joint_identifiability$rank,equivalent_without_recovery=unidentified$equivalent_scenario_ids,equivalent_with_recovery=identified$equivalent_scenario_ids,alternative_hazards=lapply(unidentified$profiles,function(r)r$hazards)),basis=list(comparable=basis$comparable,basis=basis$basis),bridge=list(coefficients=coeff,physical_pd=physical,q_pd=q,observed_evaluation_count=fitted$diagnostics$observed_evaluation_count,out_of_time_brier=fitted$diagnostics$out_of_time_brier,warmup_count=length(fitted$diagnostics$warmup_indices)),assessment_components=assessment$components,interpretation='Q quote-implied probabilities and historically estimated P issuer probabilities retain distinct measures; the bridge is a selected-population conditional association, not a universal measure conversion. Financial report parity does not imply bitwise-identical fresh optimization artifacts.')
 if(verify_reference){
  near<-function(a,b,tol=1e-9)max(abs(unlist(a)-unlist(b)))<tol
  stopifnot(near(c(cf$hazard_curve$rates,bf$hazard_curve$rates),rep(.02,4)),!Filter(function(r)r$quote_id=='future-spread',snapshot$quotes)[[1]]$eligible)
  for(t in c(1,3)){
   expected<-105*exp(-.05*t)+.8*(-expm1(-.05*t))/.05
   stopifnot(near(priced[[paste0('bond-',t)]]$clean_price_per_100,expected,1e-8),near(priced[[paste0('cds-',t)]]$protection_pv,.012*(-expm1(-.05*t))/.05,1e-10))
  }
  slope<-(log(.1/.9)-log(.02/.98))/.04;intercept<-log(.02/.98)-.01*slope
  stopifnot(unidentified$joint_identifiability$rank==2,identified$joint_identifiability$full_column_rank,length(unidentified$equivalent_scenario_ids)==2,length(identified$equivalent_scenario_ids)==0,basis$comparable,abs(basis$basis)<1e-9,near(coeff[[1]],intercept,1e-8),near(coeff[[2]],slope,1e-7),near(physical,1/(1+exp(-(intercept+slope*q))),1e-9),near(fitted$diagnostics$out_of_time_brier,(.02*.98+.1*.9)/2,1e-12),fitted$diagnostics$observed_evaluation_count==200)
 }
 if(return_artifacts)output$artifacts<-list(cds=cf,bond=bf,bridge=fitted$model,assessment=assessment)
 output
}
