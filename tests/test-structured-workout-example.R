if(!exists('estimate_open_workout_lgd'))library(creditratingsystem)
workout_example_file<-system.file('examples/structured_workout.R',package='creditratingsystem')
workout_data_file<-system.file('extdata/structured_workout.json',package='creditratingsystem')
if(!nzchar(workout_example_file))workout_example_file<-'creditratingsystem-r/examples/structured_workout.R'
if(!nzchar(workout_data_file))workout_data_file<-'creditratingsystem-r/inst/extdata/structured_workout.json'
source(workout_example_file)
workout_data<-jsonlite::fromJSON(workout_data_file,simplifyVector=FALSE)
workout_result<-run_structured_workout(workout_data)
stopifnot(identical(vapply(workout_result$variants,function(r)r$closed_workout$economic_loss,numeric(1)),c(SECURITIZATION_TRANCHE=55,COVERED_BOND=32.5,TRADE_RECEIVABLES=30,COUNTERPARTY_EXPOSURE=48)))
for(product in names(workout_data$variants))workout_data$variants[[product]]$episode$resolution_known_at<-'2024-06-01'
delayed_workout<-run_structured_workout(workout_data,verify_reference=FALSE)
stopifnot(all(vapply(delayed_workout$variants,function(r)is.null(r$closed_workout$economic_lgd)&&is.null(r$event_conditioned_loss),logical(1))))
