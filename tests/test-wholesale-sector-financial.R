library(creditratingsystem)
source(system.file('examples/wholesale_sector_financial.R',package='creditratingsystem'))
r<-run_wholesale_sector_financial(return_fitted_models=TRUE)
stopifnot(length(r$report$cases)==11,length(r$raw_native_fit_details)==11)
for(i in seq_along(r$report$cases)) {
  case<-r$report$cases[[i]];raw<-r$raw_native_fit_details[[i]]
  stopifnot(raw$scorecard$value$model$model_id==paste0(case$case_id,'-scorecard'))
  if(case$variant %in% c('financial_holding','public_agency','reinsurer'))stopifnot(case$supported$pd==case$reported_standalone_pd)
  if(case$variant=='financial_holding')stopifnot(case$constrained$grade=='C',case$constrained$binding)
  stopifnot(case$override$current$pd==case$constrained$pd)
}
