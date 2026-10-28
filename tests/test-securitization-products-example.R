if(!exists('stress_tranche_structure'))library(creditratingsystem)
securitization_example_file<-system.file('examples/securitization_products.R',package='creditratingsystem')
securitization_data_file<-system.file('extdata/securitization_products.json',package='creditratingsystem')
if(!nzchar(securitization_example_file))securitization_example_file<-'creditratingsystem-r/examples/securitization_products.R'
if(!nzchar(securitization_data_file))securitization_data_file<-'creditratingsystem-r/inst/extdata/securitization_products.json'
source(securitization_example_file)
securitization_products_result<-run_securitization_products(jsonlite::fromJSON(securitization_data_file,simplifyVector=FALSE))
stopifnot(setequal(names(securitization_products_result$variants),c('ABS','RMBS','CMBS','CLO','revolving_pool','static_pool')),length(securitization_products_result$contract_error_codes)==6L)
