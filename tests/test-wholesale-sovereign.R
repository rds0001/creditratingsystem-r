library(creditratingsystem)
source(system.file('examples/wholesale_sovereign.R',package='creditratingsystem'))
data<-jsonlite::fromJSON(system.file('extdata/wholesale_sovereign.json',package='creditratingsystem'),simplifyVector=FALSE)
r<-run_wholesale_sovereign(data)$cases
stopifnot(identical(vapply(r,function(x)x$reported_pretransfer_pd,numeric(1)),c(.04,.08,.12)),max(abs(vapply(r,function(x)x$combined$combined_pd,numeric(1))-c(.07,.14,.26)))<1e-12,max(abs(vapply(r,function(x)x$loss$expected_loss,numeric(1))-c(1.9,5.2,11.8)))<1e-12)
stopifnot(r[[1]]$applicability$payments[[1]]$events[[1]]$event_type=='PAYMENT_FREEZE',r[[3]]$restructuring$payment_delay_days==0,abs(r[[3]]$restructuring$economic_loss-3700/121)<1e-12,all(vapply(r,function(x)x$double_count_rejected,logical(1))))
for(kind in c('currency','legal_claim')) {
  bad<-data;if(kind=='currency')bad$cases[[1]]$currency<-'USD'else bad$cases[[3]]$legal_claim<-'SENIOR_SOVEREIGN_BOND'
  error<-tryCatch({run_wholesale_sovereign(bad,verify_reference=FALSE);NULL},error=function(e)e)
  stopifnot(inherits(error,'error'),grepl('incompatible',conditionMessage(error)))
}
