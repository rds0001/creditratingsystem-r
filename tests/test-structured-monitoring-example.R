if(!exists('lgd_backtest'))library(creditratingsystem)
monitoring_example_file<-system.file('examples/structured_monitoring.R',package='creditratingsystem');monitoring_data_file<-system.file('extdata/structured_monitoring.json',package='creditratingsystem')
if(!nzchar(monitoring_example_file))monitoring_example_file<-'creditratingsystem-r/examples/structured_monitoring.R'
if(!nzchar(monitoring_data_file))monitoring_data_file<-'creditratingsystem-r/inst/extdata/structured_monitoring.json'
source(monitoring_example_file);monitoring_data<-jsonlite::fromJSON(monitoring_data_file,simplifyVector=FALSE);monitoring_result<-run_structured_monitoring(monitoring_data)
expected_bias<-c(SECURITIZATION_TRANCHE=-.1875,COVERED_BOND=.175,TRADE_RECEIVABLES=.2,COUNTERPARTY_EXPOSURE=-.1)
for(product in names(monitoring_result$variants)){r<-monitoring_result$variants[[product]];stopifnot(abs(r$lgd_backtest$metrics$overall$count_weighted$bias-expected_bias[[product]])<1e-12,abs(r$default_counts$overall$observed_expected_ratio-4/3)<1e-12)}
stopifnot(is.null(monitoring_result$variants$COUNTERPARTY_EXPOSURE$ead_backtest))
monitoring_data$variants[[1]]$frozen_forecasts[[2]]$pd<-.6
stopifnot(tryCatch({run_structured_monitoring(monitoring_data);FALSE},creditratingsystem_error=function(e)e$code=='DUPLICATE_UNIT_CONFLICT'))
monitoring_data$variants[[1]]$frozen_forecasts[[2]]$pd<-.5;monitoring_data$variants[[1]]$loss_records[[1]]$forecast_known_at<-'2024-02-02'
stopifnot(tryCatch({run_structured_monitoring(monitoring_data);FALSE},creditratingsystem_error=function(e)TRUE))
