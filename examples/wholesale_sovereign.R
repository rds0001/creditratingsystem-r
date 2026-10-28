# Sovereign vintage, credit default, transfer union and restructuring economics.
run_wholesale_sovereign <- function(data=NULL,verify_reference=TRUE,return_fitted_models=FALSE) {
  if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/wholesale_sovereign.json',package='creditratingsystem'),simplifyVector=FALSE)
  rows<-list();raw<-list()
  for(c in data$cases) {
    sid<-c$case_id;variant<-c$variant;local<-variant=='local_currency'
    if((c$currency=='LOC')!=local)stop('Variant and payment currency are incompatible')
    if(!variant%in%c('local_currency','foreign_currency','restructuring')||(c$legal_claim=='RESTRUCTURED_BOND')!=(variant=='restructuring')||(c$cashflow_type=='EXCHANGED_SCHEDULE')!=(variant=='restructuring'))stop('Variant and restructuring rights are incompatible')
    query<-list(list(entity_id=sid,as_of='2024-01-01',known_at='2024-03-01'))
    vintages<-lapply(c$series,function(x){x$release_id<-if(x$known_at=='2024-04-01')'revision' else x$observed_at;x})
    selected<-join_as_of(query,vintages)$value[[1]]
    lag_spec<-list(list(name='fiscal_change',feature='fiscal_balance',lag=1,operation='difference'))
    lag<-build_distributed_lags(c$series,query,lag_spec,frequency='years',gap_policy='error')$value[[1]]
    latequery<-query;latequery[[1]]$known_at<-'2024-04-02'
    late<-build_distributed_lags(c$series,latequery,lag_spec,frequency='years',gap_policy='error')$value[[1]]
    risk<--lag$features$fiscal_change+12/c$reserves_months+2*(1-c$institution_index)
    p<-c$pretransfer_pd;X<-matrix(c(risk,risk,risk+1,risk+1),ncol=1);weights<-c(100*(1-p),100*p,100*(1-2*p),200*p)
    model<-fit_binary_glm(X,c(0,1,0,1),link='logit',feature_order='sovereign_capacity_stress',fit_intercept=TRUE,target=data$target,training=c$training,model_id=sid,model_version='1',weights=weights)
    pred<-predict_pd(model,matrix(risk,ncol=1),feature_order='sovereign_capacity_stress',expected_target=data$target)
    a<-c$calibration;cal<-calibrate_logistic(unlist(a$values),unlist(a$outcomes),unlist(a$weights),metadata=a$metadata,tolerance=a$fit_controls$tolerance,max_iterations=a$fit_controls$max_iterations)
    calibrated<-apply_calibrator(cal,pred$value,target=data$target,base_model_version='1')
    pd<-round(calibrated$value[[1]],data$precision_contract$pd_decimal_places)
    payment<-list(payment_id=paste0(sid,'-coupon'),due_at='2024-06-01',known_at='2024-01-01',currency=c$currency,source_currency='LOC',payer_jurisdiction='HOME',payee_jurisdiction=if(local)'HOME' else 'FOREIGN',payment_jurisdictions=if(local)list('HOME')else list('HOME','FOREIGN'))
    event<-list(event_id=paste0(sid,'-freeze'),event_type=if(local)'PAYMENT_FREEZE'else'BORDER_TRANSFER_BLOCK',jurisdictions=list('HOME'),currencies=list(c$currency),effective_from='2024-01-01',known_at='2024-01-01',exemptions=list())
    applicability<-assess_transfer_applicability(list(payment),list(policy_id=sid,version='1',events=list(event)),as_of='2024-03-01',known_at='2024-03-01',horizon_end='2025-03-01')$value
    target<-list(exposure_id=sid,horizon=1,time_unit='years',probability_measure='PHYSICAL',conditioning='PIT')
    transfer<-transfer_event_default_probability(c$event_probability,c$failure_given_event,list(effect_id=paste0(sid,'-liquidity-mitigation'),success_probability_given_event_and_unmitigated_failure=c$mitigation_success,conditioning='event_and_unmitigated_failure',assumption_reference='Declared synthetic payment-route stress contract'),event_id=event$event_id,target=target)$value
    base<-list(stage='PRE_TRANSFER',pd=pd,target=target,applied_effects=list())
    combined<-combine_default_transfer(base,transfer,dependence='explicit',joint_probability=c$joint)$value
    loss<-transfer_loss_components(combined,c$losses,currency=c$currency,applied_loss_effects=list())$value
    bad<-base;bad$stage<-'TRANSFER_ADJUSTED'
    rejected<-tryCatch({combine_default_transfer(bad,transfer,dependence='explicit',joint_probability=c$joint);FALSE},error=function(e)TRUE)
    pv<-if(length(c$restructured_cashflows))sum(vapply(c$restructured_cashflows,function(x)x$amount/(1+c$discount_rate)^x$time_years,numeric(1)))else NULL
    restructuring<-list(assessment_date='2024-03-01',exchange_date=if(variant=='restructuring')'2024-06-01'else NULL,observed_exchange=variant=='restructuring',payment_delay_days=0,contractual_default_policy='Distressed exchange counts as default; mere technical delay is a separate event',original_claim=c$original_claim,restructured_pv=pv,economic_loss=if(is.null(pv))NULL else c$original_claim-pv)
    facts<-list(product='SOVEREIGN_DEBT',variant=variant,borrower_class='SOVEREIGN',legal_claim=c$legal_claim,cashflow_type=c$cashflow_type,currency=c$currency)
    snapshot<-list(subject_id=sid,subject_type='issue',as_of='2024-03-01',known_at='2024-03-01',attributes=lapply(facts,function(v)list(value=v,observed_at='2024-01-01',known_at='2024-01-01')))
    rule<-list(rule_id=sid,profile_id=sid,profile_version='1',priority=1,effective_from='2023-01-01',effective_to=NULL,known_at='2023-01-01',conditions=lapply(names(facts),function(k)list(field=k,operator='eq',value=facts[[k]])))
    route<-route_rating_profile(snapshot,list(rule),'2024-03-01','2024-03-01')$value
    snapshot$attributes$legal_claim<-NULL;missing<-route_rating_profile(snapshot,list(rule),'2024-03-01','2024-03-01')$value
    if(verify_reference) {
      stopifnot(abs(calibrated$value[[1]]-p)<1e-7,abs(pred$value[[1]]-p)<1e-7,selected$value==c$fiscal_balance,late$features$fiscal_change!=lag$features$fiscal_change)
      q<-c$event_probability*c$failure_given_event*(1-c$mitigation_success)
      expected<-(p-c$joint)*c$losses$default_only+(q-c$joint)*c$losses$transfer_only+c$joint*c$losses$joint
      stopifnot(abs(combined$combined_pd-(p+q-c$joint))<1e-12,abs(loss$expected_loss-expected)<1e-12,applicability$payments[[1]]$status=='APPLICABLE',rejected,missing$status=='NO_ROUTE')
      if(!is.null(pv))stopifnot(abs(pv-8400/121)<1e-12)
    }
    rows[[length(rows)+1L]]<-list(variant=variant,case_id=sid,selected_release=selected$release_id,lags=lag,later_lags=late,capacity_stress=risk,reported_pretransfer_pd=pd,applicability=applicability,transfer=transfer,combined=combined,loss=loss,grade=map_pd_to_grade(combined$combined_pd,data$rating_scale)$value,restructuring=restructuring,double_count_rejected=rejected,profile=route$profile_id,missing_claim_status=missing$status)
    if(return_fitted_models)raw[[length(raw)+1L]]<-list(case_id=sid,model=model,calibrator=cal,prediction=pred,calibrated=calibrated)
  }
  report<-list(data_origin='SYNTHETIC',product='SOVEREIGN_DEBT',precision_contract=data$precision_contract,cases=rows)
  if(return_fitted_models)list(report=report,raw_native_fit_details=raw)else report
}
