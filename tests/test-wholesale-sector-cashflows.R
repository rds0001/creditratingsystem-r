library(creditratingsystem)
source(system.file('examples/wholesale_sector_cashflows.R',package='creditratingsystem'))
r<-run_wholesale_sector_cashflows();rows<-setNames(r$cases,vapply(r$cases,function(x)x$variant,character(1)))
stopifnot(rows$financial_holding$independent_cash_oracle[[1]]$financial_credit_unpaid==3,rows$financial_holding$balance_stress$restricted_capital_not_cash==200)
stopifnot(identical(vapply(rows[c('life','nonlife','reinsurer')],function(x)x$balance_stress$stressed_capital,numeric(1),USE.NAMES=FALSE),c(10,105,98)),identical(vapply(rows[c('region','municipality','public_agency')],function(x)x$independent_cash_oracle[[2]]$financial_credit_unpaid,numeric(1),USE.NAMES=FALSE),c(25,0,30)))
stopifnot(rows$public_agency$independent_cash_oracle[[2]]$operating_unpaid==45)
stopifnot(rows$reinsurer$independent_cash_oracle[[1]]$counterparty_receipts==50,rows$reinsurer$independent_cash_oracle[[2]]$counterparty_receipts==0)
for(v in c('multilateral_lender','supranational_body')) {
  flows<-rows[[v]]$receivables$contract$cashflows
  stopifnot(sum(vapply(Filter(function(x)x$scenario_id=='normal',flows),function(x)x$received_cash,numeric(1)))==if(v=='multilateral_lender')67 else 35,rows[[v]]$independent_cash_oracle[[1]]$counterparty_receipts==0)
}
