if(!exists('fit_recovery_hurdle'))library(creditratingsystem)
example_path<-system.file('examples','model_development.R',package='creditratingsystem');if(!nzchar(example_path))example_path<-'creditratingsystem-r/examples/model_development.R';source(example_path);md_upstream()
data_path<-system.file('extdata','model_development.json',package='creditratingsystem');if(!nzchar(data_path))data_path<-'creditratingsystem-r/inst/extdata/model_development.json'
data<-jsonlite::fromJSON(data_path,simplifyVector=FALSE)
loss<-md_loss_development(data);p<-loss$models$fractional$parameters
stopifnot(abs(p$coefficients[[1]])<1e-12,abs(p$covariance[[1]][[1]]-.08)<1e-12)
for(key in c('zero_probability','one_probability','interior_probability','mean_lgd')){wanted<-switch(key,zero_probability=.3,one_probability=.3,interior_probability=.4,mean_lgd=.5);stopifnot(max(abs(unlist(loss$predictions$mixture[[key]])-wanted))<1e-8)}
validation<-md_evaluate_losses(data,loss)$result;metrics<-validation$metrics$overall$count_weighted
stopifnot(validation$coverage$included_count==19,validation$coverage$exclusion_counts$OPEN_WORKOUT==1,abs(metrics$bias-1/38)<1e-10,abs(metrics$mae-8.3/19)<1e-10,abs(metrics$rmse-sqrt(3.75/19))<1e-10)
phi<-uniroot(function(q)digamma(q)-digamma(q/2)+mean(log(c(.1,.2,.8,.9))),c(.01,1000),tol=1e-12)$root
fitted<-loss$models$mixture$parameters$interior_model$parameters
stopifnot(abs(exp(fitted$precision_coefficients[[1]])-phi)<1e-7)
model<-loss$models$hurdle;cutoff<-model$training$known_at
closed<-vapply(loss$hurdle_training_rows,function(row)!is.null(row$resolution_date)&&row$resolution_date<=row$observation_end&&row$resolution_known_at<=cutoff,logical(1))
times<-vapply(seq_along(closed),function(i){row<-loss$hurdle_training_rows[[i]];as.numeric(as.Date(if(closed[i])row$resolution_date else row$observation_end)-as.Date(row$reference_date))},numeric(1));d<-sum(closed);logs<-log(times)
shape<-uniroot(function(k){power<-exp(k*logs);d/k+sum(logs*closed)-d*sum(power*logs)/sum(power)},c(.05,20),tol=1e-12)$root
logscale<-log(sum(times^shape)/d)/shape;timing<-model$parameters$timing_model$parameters
stopifnot(abs(timing$coefficients[[1]]-logscale)<1e-6,abs(timing$log_time_scale-1/shape)<1e-6)
beta<-timing$coefficients[[1]];k<-1/timing$log_time_scale
survival<-function(absolute)exp(-exp(k*(log(absolute)-beta))+exp(k*(log(16)-beta)))
s44<-survival(44);s75<-survival(75);p<-model$parameters;pc<-plogis(p$cure_gate$parameters$coefficients[[1]]);positive<-plogis(p$positive_gate$parameters$coefficients[[1]]);amount<-p$amount_regression;mean_value<-pc+(1-pc)*positive*exp(amount$coefficients[[1]]+amount$residual_variance/2)
expected<-(90-90*mean_value*((1-s44)*.9+(s44-s75)*.8+s75*.7))/100;actual<-loss$open_valuation$episodes[[1]]
stopifnot(actual$known_discounted_net_cashflows==10,abs(actual$expected_final_lgd-expected)<1e-10,abs(actual$unresolved_probability-s75)<1e-12,is.null(loss$open_without_tail$episodes[[1]]$expected_final_lgd))
bad<-data$workouts[[1]];bad$realized_lgd<-.7
stopifnot(inherits(try(md_realized_loss(data,bad,'2023-03-01'),silent=TRUE),'try-error'))
if(identical(Sys.getenv('CREDITRATINGSYSTEM_EXTENDED_TESTS'),'true')){
 v<-run_model_development();stopifnot(v$relational$validation$valid,abs(v$spiegelhalter$value$numerator)<1e-10,abs(v$spiegelhalter$value$variance-15/8)<1e-10,identical(as.numeric(unlist(v$dependent$value$null_counts)),c(20,22,31,21,10,15,26,22)),v$dependent$value$p_value==7/9,v$report$decision$status=='PASS')
 for(name in c('glm','grade','isotonic','scorecard'))stopifnot(abs(v$holdout$benchmark$metrics$by_model[[name]]$brier-3/16)<1e-10)
}
