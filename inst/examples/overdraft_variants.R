# Observed overdraft cash bridges with user-supplied contractual demand dates.
if(!exists('build_transaction_features'))library(creditratingsystem)
run_overdraft_variants <- function(data=NULL,verify_reference=TRUE) {
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/overdraft_variants.json',package='creditratingsystem'),simplifyVector=FALSE)
 cases<-lapply(data$cases,function(item) {
  args<-item$transaction_features;cash<-do.call(build_transaction_features,args)$value
  closing<-item$opening_signed_balance+cash$known_external_net_cashflow;snapshots<-args$daily_balances
  if(!length(snapshots)||abs(closing-snapshots[[length(snapshots)]]$closing_balance)>1e-10)stop('Supplied closing balance does not reconcile with booked cash')
  balances<-lapply(snapshots,function(row)list(snapshot_id=row$balance_id,release_seq=row$release_seq,account_id=row$account_id,observed_at=row$observed_at,known_at=row$known_at,balance=max(-row$closing_balance,0),credit_limit=item$credit_limit,activity_status='ACTIVE',currency=row$currency))
  revolving<-build_revolving_features(balances,list(),args$coverage,account_id='account',as_of=args$as_of,information_cutoff=args$information_cutoff,lookback_days=args$lookback_days,currency=args$currency)$value
  payments<-do.call(compute_days_past_due,item$payment_contract)$value
  list(cash=cash,revolving=revolving,payments=payments,closing_signed_balance=closing,drawn_exposure=max(-closing,0),contractual_demand_reference=item$contractual_demand_reference)
 })
 ccf<-lapply(data$ccf_episodes,function(args)do.call(realized_ccf,args)$value)
 if(verify_reference) {
  stopifnot(cases$authorized_line$drawn_exposure==900,cases$authorized_line$payments$dpd==0,cases$unauthorized_excess$drawn_exposure==1200,cases$unauthorized_excess$payments$past_due_amount==200,cases$unauthorized_excess$payments$dpd==1)
  stopifnot(cases$salary_linked_received$drawn_exposure==200,cases$salary_linked_missing$drawn_exposure==1200,cases$salary_linked_received$cash$classified_income==1000,cases$salary_linked_missing$cash$classified_income==0,cases$salary_linked_missing$payments$dpd==0)
  stopifnot(is.null(ccf$no_available_undrawn$ccf),ccf$no_available_undrawn$denominator_status=='NO_POSITIVE_UNDRAWN',abs(ccf$positive_undrawn$ccf-4/7)<1e-14)
 }
 list(data_origin=data$data_origin,product=data$product,cases=cases,separate_historical_ccf_episodes=ccf)
}
