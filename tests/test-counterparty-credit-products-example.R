if(!exists('counterparty_exposure_backtest'))library(creditratingsystem)
ccr_example_file<-system.file('examples/counterparty_credit_products.R',package='creditratingsystem')
ccr_data_file<-system.file('extdata/counterparty_credit_products.json',package='creditratingsystem')
if(!nzchar(ccr_example_file))ccr_example_file<-'creditratingsystem-r/examples/counterparty_credit_products.R'
if(!nzchar(ccr_data_file))ccr_data_file<-'creditratingsystem-r/inst/extdata/counterparty_credit_products.json'
source(ccr_example_file)
ccr_products_result<-run_counterparty_credit_products(jsonlite::fromJSON(ccr_data_file,simplifyVector=FALSE))
stopifnot(setequal(names(ccr_products_result$variants),c('netting_set','collateralized','uncollateralized','wrong_way')))
for(case in ccr_products_result$variants)stopifnot(case$validation$coverage$included_count==1,is.null(case$validation$uncertainty$method))
