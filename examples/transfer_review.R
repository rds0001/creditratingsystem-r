# Dated payment-route assessment, exclusive default union and monetary loss.
assess_transfer_review <- function(data,known_at=data$known_at) {
 applicability<-assess_transfer_applicability(data$payments,data$event_policy,as_of=data$as_of,known_at=known_at,horizon_end=data$horizon_end)$value
 statuses<-vapply(applicability$payments,function(row)row$status,character(1))
 if('INSUFFICIENT_INFORMATION'%in%statuses)return(list(applicability=applicability,scenarios=NULL,status='INSUFFICIENT_INFORMATION'))
 event_p<-if('APPLICABLE'%in%statuses)data$event_probability else 0
 transfer<-transfer_event_default_probability(event_p,data$event_to_default_probability,data$mitigation,event_id=data$event_policy$events[[1]]$event_id,target=data$target)$value
 base<-list(stage='PRE_TRANSFER',pd=data$pre_transfer_pd,target=data$target,applied_effects=list())
 p<-base$pd;q<-transfer$probability;scenarios<-list()
 for(case in data$scenarios){
  rule<-case$joint_rule
  if(!rule%in%c('independent','upper','lower'))stop('Declare independent, upper or lower joint-event assumption')
  j<-if(rule=='independent')p*q else if(rule=='upper')min(p,q)else max(0,sum(c(p,q,-1)))
  joint<-joint_default_probability(p,q,event_order=c('obligor-default','transfer-default'),joint_probability=j)$value
  combined<-combine_default_transfer(base,transfer,dependence='explicit',joint_probability=joint)$value
  loss<-transfer_loss_components(combined,data$conditional_losses,currency=data$currency,applied_loss_effects=list())$value
  grade<-map_pd_to_grade(combined$combined_pd,data$rating_scale)$value
  scenarios[[case$case_id]]<-list(assumption=rule,risk=combined,loss=loss,grade=grade)
 }
 list(applicability=applicability,transfer=transfer,base=base,scenarios=scenarios,status='ASSESSED')
}
run_transfer_review <- function(data=NULL,verify_reference=TRUE){
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/transfer_review.json',package='creditratingsystem'),simplifyVector=FALSE)
 original<-assess_transfer_review(data);later<-assess_transfer_review(data,'2025-03-01')
 missing<-data;missing$payments[[1]]$payment_jurisdictions<-NULL;unknown<-assess_transfer_review(missing)
 replay<-assess_transfer_review(data)
 output<-list(original=original,later_knowledge=later,missing_route=unknown,original_hash=content_fingerprint(original)$value,replay_hash=content_fingerprint(replay)$value,interpretation='Conditional synthetic transfer-default probabilities, explicit dependence sensitivity and four disjoint monetary losses. Route exemptions follow knowledge time. No country-rating notch or legal determination is inferred.')
 if(verify_reference){
  expected<-list(independent=c(.172,7.52),nested=c(.1,6.8),disjoint=c(.18,7.6))
  for(name in names(expected)){row<-original$scenarios[[name]];stopifnot(abs(row$risk$combined_pd-expected[[name]][1])<1e-14,abs(row$loss$expected_loss-expected[[name]][2])<1e-12,row$grade==if(name=='nested')'A'else'B')}
  stopifnot(original$applicability$payments[[1]]$status=='APPLICABLE',later$applicability$payments[[1]]$status=='EXEMPT',all(vapply(later$scenarios,function(v)abs(v$loss$expected_loss-6)<1e-14,logical(1))),is.null(unknown$scenarios),output$original_hash==output$replay_hash)
 }
 output
}
