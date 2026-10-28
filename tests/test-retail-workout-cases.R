if(!exists('estimate_open_workout_lgd'))library(creditratingsystem)
rwc_example<-system.file('examples/retail_workout_cases.R',package='creditratingsystem');rwc_data_file<-system.file('extdata/retail_workout_cases.json',package='creditratingsystem')
if(!nzchar(rwc_example))rwc_example<-'creditratingsystem-r/examples/retail_workout_cases.R'
if(!nzchar(rwc_data_file))rwc_data_file<-'creditratingsystem-r/inst/extdata/retail_workout_cases.json'
source(rwc_example);rwc_data<-jsonlite::fromJSON(rwc_data_file,simplifyVector=FALSE);rwc_result<-run_retail_workout_cases(rwc_data)
stopifnot(length(rwc_result$cases)==12,length(unique(vapply(rwc_data$cases,function(c)c$ead,numeric(1))))==12,rwc_result$cure_training$known_at<'2024-02-01')
h<-rwc_result$cases$RETAIL_HOME_EQUITY_LINE$claim_allocation$claim_results;stopifnot(h[[1]]$gross_recovery==40000,h[[2]]$gross_recovery==30000)
b<-rwc_result$cases$RETAIL_BNPL$claim_allocation$claim_results;stopifnot(length(b)==2,sum(vapply(b,function(x)x$gross_recovery,numeric(1)))==150)
f<-rwc_data;f$cases<-f$cases['RETAIL_AUTO_LEASE'];f$cases[[1]]$episode$resolution_known_at<-'2024-06-01';r<-run_retail_workout_cases(f,FALSE)$cases[[1]];stopifnot(is.null(r$closed_workout$economic_lgd),is.null(r$event_conditioned_loss))
rwc_error<-function(expr,code=NULL){e<-tryCatch(force(expr),error=identity);stopifnot(inherits(e,'creditratingsystem_error'));if(!is.null(code))stopifnot(e$code==code)}
f<-rwc_data;f$cases<-f$cases['RETAIL_HOME_EQUITY_LINE'];f$cure_training$training$known_at<-'2024-03-01';rwc_error(run_retail_workout_cases(f),'COLLECTIONS_LEAKAGE')
f<-rwc_data;f$cases<-f$cases['RETAIL_MICRO_LOAN'];f$cases[[1]]$cashflows[[1]]$currency<-'USD';rwc_error(run_retail_workout_cases(f))
f<-rwc_data;f$cases<-f$cases['RETAIL_BNPL'];f$cases[[1]]$claim_allocation$claims[[3]]<-f$cases[[1]]$claim_allocation$claims[[1]];rwc_error(run_retail_workout_cases(f))
cat('Twelve independent retail workout claims, prior-known cure/redefault, open tails and negative evidence PASS\n')
