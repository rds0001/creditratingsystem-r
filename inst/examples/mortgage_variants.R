# Source the installed example, then run_mortgage_variants().
# Inputs are replaceable local contracts. Reference checks require the supplied data.
if (!exists("build_debt_schedule")) library(creditratingsystem)
run_mortgage_variants <- function(data=NULL, verify_reference=TRUE) {
  if(is.null(data)) data<-jsonlite::fromJSON(system.file("extdata/mortgage_variants.json",package="creditratingsystem"),simplifyVector=FALSE)
  debt<-lapply(data$debt_cases,function(a)do.call(build_debt_schedule,a)$value)
  shocked<-data$debt_cases$floating_rate
  shocked$base_rate_fixings[[2]]$base_rate<-shocked$base_rate_fixings[[2]]$base_rate+data$rate_reset_shock
  debt$floating_reset<-do.call(build_debt_schedule,shocked)$value
  operating<-lapply(data$operating_cases,function(a)do.call(simulate_operating_cashflows,a)$value)
  joint<-do.call(assemble_joint_application,data$joint_application)$value
  collateral<-do.call(combined_loan_to_value,data$collateral)$value
  recovery<-do.call(simulate_recovery_waterfall,data$recovery)$value
  if(verify_reference){
    same<-function(a,b)stopifnot(is.finite(a),abs(a-b)<=1e-10+1e-12*abs(b))
    expected<-list(fixed_rate=c(53000,51500),floating_rate=c(53000,51500),floating_reset=c(53000,52500),interest_only=c(3000,103000),partially_amortizing_balloon=c(23000,82400))
    for(name in names(expected))for(i in 1:2)same(debt[[name]]$schedule[[i]]$net_payment_due,expected[[name]][i])
    same(debt$interest_only$schedule[[1]]$closing_principal,100000)
    same(debt$partially_amortizing_balloon$schedule[[1]]$closing_principal,80000)
    expected<-c(owner_occupied_base=0,owner_occupied_vacancy=0,buy_to_let_base=800,buy_to_let_vacancy=600)
    for(name in names(expected))same(operating[[name]]$periods[[1]]$cfads,expected[[name]])
    same(joint$monthly_income,5000);same(joint$monthly_debt_service,900);same(collateral$cltv,.8)
    claim<-Filter(function(x)x$claim_id=="mortgage",recovery$claim_results)[[1]]
    same(claim$economic_loss,37000);same(claim$lgd,.37)
  }
  list(data_origin=data$data_origin,version=data$version,product=data$product,debt=debt,operating=operating,joint_application=joint,collateral=collateral,recovery=recovery)
}
