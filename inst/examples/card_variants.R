# Credit-card minimum dues and charge-card full/deferred payment contracts.
if(!exists('build_revolving_features'))library(creditratingsystem)
run_card_variants <- function(data=NULL,verify_reference=TRUE) {
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/card_variants.json',package='creditratingsystem'),simplifyVector=FALSE)
 revolving<-lapply(data$revolving,function(a)do.call(build_revolving_features,a)$value)
 payments<-lapply(data$payments,function(a)do.call(compute_days_past_due,a)$value)
 exposure<-lapply(data$exposure,function(a){a$annual_interest_rates<-unlist(a$annual_interest_rates);do.call(exposure_schedule,a)$value})
 history<-do.call(build_payment_history_features,data$new_history)$value
 authorizations<-lapply(data$authorizations,function(a)do.call(reconcile_card_authorizations,a)$value)
 if(verify_reference) {
  stopifnot(payments$transactor$dpd==0,payments$revolver$dpd==0,revolving$transactor$full_payer_count==1,revolving$revolver$minimum_without_full_count==1)
  stopifnot(payments$monthly_full_payment$unpaid_due_amount==950,payments$monthly_full_payment$dpd==2,payments$deferred_charge$dpd==0,payments$deferred_charge$unpaid_due_amount==0)
  stopifnot(is.null(history$features$max_dpd),revolving$new_account$latest_snapshot$undrawn_active,revolving$new_account$missing_days==3)
  cut<-revolving$limit_change$balance_changes[[1]]
  stopifnot(cut$balance_change==0,cut$limit_change== -1000,cut$balance_effect_at_new_limit==0,cut$limit_effect_on_previous_balance==.5)
  expected<-c(transactor=0,revolver=950,new_account=200,monthly_full_payment=950,deferred_charge=1000)
  for(name in names(expected))stopifnot(exposure[[name]]$schedule[[1]]$conditional_ead==expected[[name]])
  stopifnot(authorizations$pending$accounts[[1]]$scenario_exposure==200,authorizations$settled$accounts[[1]]$scenario_exposure==200,authorizations$settled$accounts[[1]]$pending_authorization==0,authorizations$expired$accounts[[1]]$scenario_exposure==0)
 }
 list(data_origin=data$data_origin,products=data$products,revolving=revolving,payments=payments,exposure=exposure,new_account_history=history,authorizations=authorizations)
}
