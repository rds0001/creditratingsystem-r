if(!exists('simulate_trade_receivables'))library(creditratingsystem)
trade_example_file<-system.file('examples/trade_receivable_products.R',package='creditratingsystem')
trade_data_file<-system.file('extdata/trade_receivable_products.json',package='creditratingsystem')
if(!nzchar(trade_example_file))trade_example_file<-'creditratingsystem-r/examples/trade_receivable_products.R'
if(!nzchar(trade_data_file))trade_data_file<-'creditratingsystem-r/inst/extdata/trade_receivable_products.json'
source(trade_example_file)
trade_products_result<-run_trade_receivable_products(jsonlite::fromJSON(trade_data_file,simplifyVector=FALSE))
stopifnot(setequal(names(trade_products_result$variants),c('recourse','nonrecourse','invoice_pool')))
for(case in trade_products_result$variants)stopifnot(all(vapply(case$paths$ending_states,function(e)abs(e$conservation_residual)<1e-10,logical(1))))
