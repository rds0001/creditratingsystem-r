# Native lease credit/residual bridge under explicitly supplied legal claims.
if(!exists("detect_cashflow_default"))library(creditratingsystem)
run_lease_variants<-function(data=NULL,verify_reference=TRUE){
 if(is.null(data))data<-jsonlite::fromJSON(system.file("extdata/lease_variants.json",package="creditratingsystem"),simplifyVector=FALSE)
 out<-list()
 for(name in names(data$cases)){
  c<-data$cases[[name]];claim<-c$credit_claim
  if(startsWith(name,"early_termination")){
   claim<-c$termination_base+if(c$customer_bears_condition)c$condition_charge else 0
   if(claim!=c$claim_receivable)stop("Termination claim differs from declared contractual settlement")
  }
  net<-c$gross_sale-c$realization_cost;if(net<0)stop("Example assumes nonnegative net realization")
  tests<-list(list(scenario_id=name,entity_id="lessee",facility_id="lease",date="2023-01-01",currency="EUR",cfads=c$rent_paid,debt_service_due=c$rent_due,debt_outstanding=claim,asset_value=if(c$asset_proceeds_applied_to_claim)net else 0,liquidity_buffer_available=0,materiality_met=FALSE,material_days_past_due=0,utp_indicators=list()))
  default<-detect_cashflow_default(tests,data$default_policy,currency="EUR")$value
  cash<-apply_cash_waterfall(list(list(scenario_id=name,date="2023-01-01",currency="EUR",cash_available=net,reserve_target=0,amounts_due=list(credit_claim=claim))),data$waterfall_policy,currency="EUR")$value
  row<-cash$periods[[1]];credit_loss<-claim-row$payments[[1]]$total_paid
  residual_loss<-c$residual_carrying_value-row$residual_equity_cash;total<-credit_loss+residual_loss
  stopifnot(abs(total-(claim+c$residual_carrying_value-net))<1e-10)
  defaulted<-default$first_default_events[[1]]$defaulted
  credit<-expected_loss_from_paths(default_indicator=matrix(as.integer(defaulted),1,1),lgd_paths=matrix(if(claim>0)credit_loss/claim else 0,1,1),ead_paths=matrix(claim,1,1),scenario_weights=1,discount_factors=1,scenario_ids=name,time_grid=1,currency="EUR",probability_measure="PHYSICAL")$value
  market<-event_conditioned_loss(list(list(state_id="no_market_loss",events=list(residual_loss=FALSE),probability=0,conditional_loss=0),list(state_id="market_loss",events=list(residual_loss=TRUE),probability=1,conditional_loss=residual_loss)),event_names=list("residual_loss"),currency="EUR",probability_measure="PHYSICAL")$value
  out[[name]]<-list(default_assessment=default,cash_allocation=cash,credit_loss=credit_loss,residual_loss=residual_loss,total_economic_loss=total,credit_path=credit,residual_event=market,cancelled_future_payments=c$cancelled_future_payments,contractual_credit_claim=claim)
 }
 if(verify_reference)stopifnot(!out$operating_lease$default_assessment$first_default_events[[1]]$defaulted,out$operating_lease$credit_loss==0,out$operating_lease$residual_loss==4500,out$finance_lease$credit_loss==4500,out$finance_lease$residual_loss==0,out$early_termination$contractual_credit_claim==2000,out$early_termination_no_condition$contractual_credit_claim==1800,out$early_termination_no_condition$credit_loss==800,out$early_termination$credit_loss==1000,out$early_termination$total_economic_loss==11000)
 list(data_origin=data$data_origin,product=data$product,variants=out)
}
