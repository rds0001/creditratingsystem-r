# Explicit local mortgage evidence, legal liabilities and payment history.
if(!exists("build_customer_household_graph"))library(creditratingsystem)
run_mortgage_evidence<-function(data=NULL,verify_reference=TRUE){
 if(is.null(data))data<-jsonlite::fromJSON(system.file("extdata/mortgage_evidence.json",package="creditratingsystem"),simplifyVector=FALSE)
 graph<-do.call(build_customer_household_graph,data$graph)
 household<-do.call(aggregate_household_obligations,c(data$household,list(household_graph=graph)))$value
 bureau<-do.call(build_bureau_features,data$bureau)$value
 income<-do.call(reconcile_income_evidence,data$income)$value
 data$normalized_income$components[[1]]$amount<-income$total_selected_income
 normalized<-do.call(normalize_income_period,data$normalized_income)$value
 expenses<-do.call(estimate_recurring_expenses,data$expenses)$value
 payment<-do.call(build_payment_history_features,data$payment_history)$value
 state<-do.call(assign_delinquency_state,data$delinquency_state)$value
 if(verify_reference){
  stopifnot(household$households[[1]]$total_budget_debt_service==900,all(vapply(household$person_legal_liabilities,function(x)x$legal_debt_stock,numeric(1))==c(100000,100000)),bureau$features$total_open_balance==5000,income$total_selected_income==3000,income$components[[1]]$discrepancy_flag,normalized$total_income==5000,expenses$recurring_expenses==2300,expenses$debt_service==900,payment$features$max_dpd==30,state$state=="DELINQUENT")
 }
 list(data_origin=data$data_origin,household=household,bureau=bureau,income=income,normalized_income=normalized,expenses=expenses,payment_history=payment,delinquency_state=state)
}
