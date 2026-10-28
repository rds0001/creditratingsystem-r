if(!exists('estimate_multistate_transitions'))library(creditratingsystem)
source(system.file('examples/mortgage_phase_review.R',package='creditratingsystem'))
r<-run_mortgage_phase_review(return_artifacts=TRUE);raw<-r$raw_artifacts
stopifnot(r$facility_id=='facility_A',r$freshness$status=='STALE',is.null(r$observed_cure_targets[[1]]$cure_label),r$cure_prediction$expected_recovery==.5*.8*95000,r$collections_value[[1]]$expected_net_recovery==.25*(38000-100)+.75*19000-10,r$event_conditioned_realized_loss$expected_loss==.25*(95000-.9*70000),r$default_count$overall$observed_count==1,abs(r$competing_exit_curves$curves[[1]]$cumulative_incidence$exit[[2]]-.5*(1-exp(-.5)))<1e-12,length(raw$open_inputs$cashflows)==0,is.null(raw$open_inputs$episodes[[1]]$resolution_date),is.null(r$unidentified_tail$episodes[[1]]$expected_final_lgd),identical(do.call(estimate_open_workout_lgd,raw$open_inputs)$value,r$open_recovery_scenario),identical(do.call(evaluate_treatment_candidates,raw$treatment_inputs)$value,r$treatment))
ratio<-(1+sqrt(.25*.75)*exp((log(3)/2)^2/2))/3
stopifnot(abs(r$open_recovery_scenario$episodes[[1]]$expected_final_loss-(95000-.95*95000*ratio))<1e-7)
d<-jsonlite::fromJSON(system.file('extdata/mortgage_phase_review.json',package='creditratingsystem'),simplifyVector=FALSE);d$cure_training$training$known_at<-'2025-01-01'
stopifnot(inherits(tryCatch(run_mortgage_phase_review(d),error=identity),'error'))
