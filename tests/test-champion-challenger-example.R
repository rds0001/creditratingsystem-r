library(creditratingsystem)
source(system.file('examples/champion_challenger.R',package='creditratingsystem'))
v<-run_champion_challenger();d<-v$comparison$differences
stopifnot(abs(d$auc-.25)<1e-12,abs(d$brier-.1375)<1e-12,abs(d$calibration_error+.225)<1e-12,v$comparison$recommendation=='REQUIRES_MULTIDIMENSIONAL_REVIEW',identical(vapply(v$findings$trace,function(r)r$assessment,character(1)),c('WATCH','WARNING','WARNING')),abs(unlist(v$historical_rows[[1]]$value)-.75)<1e-12,v$replay$status=='REPRODUCED',all(vapply(v$fairness,function(r)r$status=='INSUFFICIENT_INFORMATION'&&is.null(r$metrics$selection_rate),logical(1))),abs(v$linear_explanation$reconstruction_residual[[1]])<1e-15,abs(v$shapley$baseline-6/7)<1e-12,v$sequential$validation_false_alarm$paths==99)
data<-jsonlite::fromJSON(system.file('extdata/champion_challenger.json',package='creditratingsystem'),simplifyVector=FALSE)
incomplete<-build_monitoring_cohorts(v$forecast_inputs[['1']],data$events,list(),target_contract=data$target,evaluation_cutoff=data$evaluation_cutoff)$value
stopifnot(sum(vapply(incomplete$rows,function(r)r$eligible,logical(1)))==6,sum(vapply(incomplete$rows,function(r)r$status=='MISSING_OUTCOME',logical(1)))==6,all(vapply(Filter(function(r)!r$eligible,incomplete$rows),function(r)is.null(r$target),logical(1))))
