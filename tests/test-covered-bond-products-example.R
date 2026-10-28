if(!exists('covered_bond_dual_recourse'))library(creditratingsystem)
covered_example_file<-system.file('examples/covered_bond_products.R',package='creditratingsystem')
covered_data_file<-system.file('extdata/covered_bond_products.json',package='creditratingsystem')
if(!nzchar(covered_example_file))covered_example_file<-'creditratingsystem-r/examples/covered_bond_products.R'
if(!nzchar(covered_data_file))covered_data_file<-'creditratingsystem-r/inst/extdata/covered_bond_products.json'
source(covered_example_file)
covered_products_result<-run_covered_bond_products(jsonlite::fromJSON(covered_data_file,simplifyVector=FALSE))
stopifnot(setequal(names(covered_products_result$variants),c('mortgage_cover_pool','public_sector_cover_pool','maturity_extension')))
for(case in covered_products_result$variants)stopifnot(is.null(case$risk$pd_interval),case$covered_recourse$ending_states[[2]]$estate_payment==0)
