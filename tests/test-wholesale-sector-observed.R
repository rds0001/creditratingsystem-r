library(creditratingsystem)
source(system.file('examples/wholesale_sector_observed.R',package='creditratingsystem'))
data<-jsonlite::fromJSON(system.file('extdata/wholesale_sector_observed.json',package='creditratingsystem'),simplifyVector=FALSE)
rows<-run_wholesale_sector_observed(data)$journeys
stopifnot(identical(vapply(rows,function(x)x$default_backtest$overall$observed_defaults,numeric(1)),c(1,2,3,2,1,1,2)))
for(row in rows)stopifnot(row$default_backtest$overall$observed_count==20,row$windows[[20]]$drawn_balance_at_origin==0,row$windows[[20]]$eligible,!is.null(row$closed_workout$economic_lgd),is.null(row$incomplete_workout$economic_lgd),row$independent_oracle$discount_days==123)
bad<-data;bad$journeys[[1]]$monitoring$predictions[[1]]$pd<-.9
error<-tryCatch({run_wholesale_sector_observed(bad,verify_reference=FALSE);NULL},error=function(e)e)
stopifnot(inherits(error,'error'),grepl('pinned model',conditionMessage(error)))
