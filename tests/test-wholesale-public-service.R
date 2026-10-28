library(creditratingsystem)
source(system.file('examples/wholesale_public_service.R',package='creditratingsystem'))
data<-jsonlite::fromJSON(system.file('extdata/wholesale_public_service.json',package='creditratingsystem'),simplifyVector=FALSE)
r<-run_wholesale_public_service(data)$cases
stopifnot(identical(vapply(r,function(x)x$cash_oracles[[1]]$restricted_remaining,numeric(1)),c(10,10,0)),identical(vapply(r,function(x)x$cash_oracles[[1]]$debt_service_gap,numeric(1)),c(5,5,0)),identical(vapply(r,function(x)x$cash_oracles[[2]]$debt_service_gap,numeric(1)),c(25,15,15)))
stopifnot(is.null(r[[1]]$market),is.null(r[[2]]$market),r[[3]]$market$asset_value==100,r[[3]]$market$asset_volatility==.2,r[[3]]$market$physical_pd!=r[[3]]$market$empirical_pd,r[[3]]$market$empirical_pd==.2,r[[2]]$supported$pd==r[[2]]$reported_fundamental_pd)
for(key in c('restricted_purpose','cashflow_type')) {
  bad<-data;bad$cases[[1]][[key]]<-if(key=='restricted_purpose')'GENERAL_DEBT_SERVICE'else'REGULATED_TARIFF'
  error<-tryCatch({run_wholesale_public_service(bad,verify_reference=FALSE);NULL},error=function(e)e)
  stopifnot(inherits(error,'error'),grepl('incompatible',conditionMessage(error)))
}
