library(creditratingsystem)
source(system.file('examples/transfer_review.R',package='creditratingsystem'))
data<-jsonlite::fromJSON(system.file('extdata/transfer_review.json',package='creditratingsystem'),simplifyVector=FALSE)
result<-run_transfer_review(data)
stopifnot(result$original_hash==result$replay_hash,is.null(result$missing_route$scenarios))
# Integer denominator250 supplies independent state counts, including mitigation.
counts<-list(independent=c(neither=207,default_only=23,transfer_only=18,joint=2),nested=c(neither=225,default_only=5,transfer_only=0,joint=20),disjoint=c(neither=205,default_only=25,transfer_only=20,joint=0))
for(name in names(counts)){
 expected<-counts[[name]]/250;row<-result$original$scenarios[[name]]
 stopifnot(max(abs(unlist(row$risk$exclusive_probabilities)[names(expected)]-expected))<1e-14,abs(row$loss$expected_loss-sum(expected*c(neither=0,default_only=60,transfer_only=20,joint=70)))<1e-12)
}
d<-data;d$pre_transfer_pd<-.7;d$event_probability<-.75;d$event_to_default_probability<-1
stress<-assess_transfer_review(d)$scenarios
stopifnot(abs(stress$disjoint$risk$combined_pd-1)<1e-14,abs(stress$disjoint$loss$expected_loss-51)<1e-12,abs(stress$nested$risk$combined_pd-.7)<1e-14,abs(stress$nested$loss$expected_loss-48)<1e-12)
d<-data;d$event_policy$events[[1]]$event_type<-'CONVERSION_RESTRICTION'
stopifnot(assess_transfer_review(d)$transfer$probability==0)
reject<-function(expr){e<-tryCatch({force(expr);NULL},error=identity);stopifnot(inherits(e,'creditratingsystem_error'),e$code=='TRANSFER_DOUBLE_COUNT')}
risk<-result$original$scenarios$independent$risk;adjusted<-risk;adjusted$pd<-risk$combined_pd
reject(combine_default_transfer(adjusted,result$original$transfer,dependence='independent'))
reject(transfer_loss_components(risk,data$conditional_losses,currency='EUR',applied_loss_effects=list('prearranged-liquidity')))
