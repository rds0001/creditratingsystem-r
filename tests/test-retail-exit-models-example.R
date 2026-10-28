library(creditratingsystem)
source(system.file('examples/retail_exit_models.R',package='creditratingsystem'))
d<-jsonlite::fromJSON(system.file('extdata/retail_exit_models.json',package='creditratingsystem'),simplifyVector=FALSE)
n<-c('RETAIL_BNPL','RETAIL_RESIDENTIAL_MORTGAGE','RETAIL_EDUCATION_LOAN');d$cases<-d$cases[n]
v<-run_retail_exit_models(d)$products
stopifnot(all(vapply(v,function(x)x$aft$likelihood_oracle_pass,logical(1))),identical(vapply(v,function(x)tail(unlist(x$competing$horizon_grid),1),numeric(1),USE.NAMES=FALSE),c(14,60,180)))
