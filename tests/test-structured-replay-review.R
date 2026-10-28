library(creditratingsystem)
source(system.file('examples/structured_replay_review.R',package='creditratingsystem'))
result<-run_structured_replay_review()
stopifnot(length(result$products)==4)
for(row in result$products){
 before<-row$original_checkpoints[[1]];after<-row$original_checkpoints[[2]]
 stopifnot(identical(before$state,row$actual_replay_checkpoints[[1]]$state),identical(after$state,row$actual_replay_checkpoints[[2]]$state),before$state$assessment$components[[1]]$value==.5,after$state$assessment$components[[1]]$value==.75,!identical(before$snapshot_hashes,after$snapshot_hashes),row$audit$metrics$duplicate_identical_deliveries==1,row$negative_audit$decision$status=='BLOCKED',row$batch_permutation_exact)
 errors<-Filter(function(r)r$status=='ERROR',row$batch$rows)
 stopifnot(length(errors)==1,errors[[1]]$error_code=='RATING_INTEGRITY')
}
