# Synthetic leveraged loan variants with declared unfunded refinancing.
if(!exists('fit_scorecard'))library(creditratingsystem)
run_wholesale_leveraged <- function(data=NULL,verify_reference=TRUE) {
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/wholesale_leveraged.json',package='creditratingsystem'),simplifyVector=FALSE)
 target<-data$target;tr<-data$score_training;cal<-data$calibration;ec<-data$economic_contract;features<-unlist(tr$feature_order)
 model<-fit_scorecard(do.call(rbind,lapply(tr$X,unlist)),unlist(tr$y),feature_order=features,fit_spec=tr$fit_spec,weights=unlist(tr$weights),target=target,training=tr$training,model_id='leveraged-fundamental',model_version='1')
 calibrator<-calibrate_logistic(unlist(cal$values),unlist(cal$outcomes),unlist(cal$weights),metadata=cal$metadata)
 st<-data$survival_training
 survival<-estimate_survival_curve(unlist(st$entry),unlist(st$exit),unlist(st$event),weight_policy='frequency',target=target,training=st$training,model_id='leveraged-observed-vintage',model_version='1')
 curves<-predict_event_curves(survival,list(),c(0,1,2),feature_order=character(),target=target)$value
 variants<-list()
 for(v in data$variants) {
  identity<-v$id
  statement<-list(entity_id=identity,currency='EUR',as_of='2023-12-31',period_start='2023-01-01',period_end='2023-12-31',values=v[c('debt','cash','ebitda')],units=list(debt='EUR',cash='EUR',ebitda='EUR'))
  ratio<-build_financial_ratios(list(statement),data$ratio_spec)$value[[1]]$ratios$net_debt_ebitda$value
  scored<-score_scorecard(model,matrix(ratio,ncol=1),feature_order=features,expected_target=target)$value$pd[1]
  pd<-apply_calibrator(calibrator,c(scored),target=target,base_model_version='1')$value[1]
  facts<-list(variant=identity,terms_verified=TRUE,sponsor_present=v$sponsor,maintenance_covenant=v$maintenance_covenant)
  snapshot<-list(subject_id=identity,subject_type='facility',as_of=data$as_of,known_at=data$known_at,attributes=lapply(facts,function(x)list(value=x,observed_at=data$as_of,known_at=data$known_at)))
  rule<-list(rule_id=identity,profile_id=paste0('leveraged-',identity),profile_version='1',priority=1,effective_from='2023-01-01',effective_to=NULL,known_at='2023-01-01',conditions=lapply(names(facts),function(k)list(field=k,operator='eq',value=facts[[k]])))
  route<-route_rating_profile(snapshot,list(rule),data$as_of,data$known_at)$value
  snapshot$attributes$terms_verified$value<-FALSE
  negative<-route_rating_profile(snapshot,list(rule),data$as_of,data$known_at)$value
  terms<-data$debt_contract;terms$facility_id<-identity;terms$principal<-v$debt
  debt<-build_debt_schedule(terms,data$calendar)$value
  initial<-list(scenario_id=identity,entity_id=identity,facility_id=identity,date=data$as_of,currency='EUR',cfads=0,debt_service_due=0,debt_outstanding=v$debt,asset_value=v$asset_value,liquidity_buffer_available=0,materiality_met=FALSE,material_days_past_due=0,utp_indicators=list())
  rows<-list(initial)
  for(i in seq_along(debt$schedule)){row<-initial;row$date<-debt$schedule[[i]]$payment_date;row$cfads<-v$cfads[[i]];row$debt_service_due<-debt$schedule[[i]]$debt_service_due;rows[[length(rows)+1]]<-row}
  defaults<-detect_cashflow_default(rows,data$default_policy,currency='EUR')$value
  risk<-list(entity_id=identity,pd=pd,stage='STANDALONE',target=target,applied_effects=list())
  supported<-NULL
  if(v$sponsor)supported<-apply_parent_support(risk,list(entity_id='SPONSOR',pd=.05,stage='STANDALONE',target=target,applied_effects=list()),data$support,joint_default_probability=.01)$value
  covenant_breach<-v$maintenance_covenant&&ratio>ec$maintenance_max_net_debt_ebitda
  shortfall<-debt$schedule[[length(debt$schedule)]]$debt_service_due-tail(unlist(v$cfads),1)
  variants[[identity]]<-list(profile_id=route$profile_id,negative_status=negative$status,net_debt_ebitda=ratio,standalone_pd=pd,supported_pd=if(is.null(supported))NULL else supported$pd,debt_schedule=debt,cashflow_default=defaults,maintenance_covenant_breach=covenant_breach,refinancing_shortfall=shortfall,committed_refinancing=ec$refinancing_committed)
  if(verify_reference){
   stopifnot(route$status=='ROUTED',negative$status=='NO_ROUTE',abs(debt$schedule[[1]]$debt_service_due-v$debt*.1*366/365)<1e-10,abs(shortfall-(v$debt*1.1-tail(unlist(v$cfads),1)))<1e-10,defaults$first_default_events[[1]]$first_default_date=='2026-01-01',all(unlist(v$cfads)>0))
   expected<-plogis(qlogis(.2)+(ratio-4)/4*(qlogis(.4)-qlogis(.2)))
   stopifnot(abs(pd-expected)<1e-7)
   if(!is.null(supported))stopifnot(abs(supported$pd-(pd-.4*(pd-.01)))<1e-7)
  }
 }
 if(verify_reference)stopifnot(max(abs(unlist(curves$curves[[1]]$survival)-c(1,.9,.8)))<1e-12,variants$high_leverage$maintenance_covenant_breach,!variants$covenant_lite$maintenance_covenant_breach,variants$covenant_lite$refinancing_shortfall==variants$sponsor_backed$refinancing_shortfall)
 list(product=data$product,data_origin='SYNTHETIC',target=target,variants=variants,event_curves=curves,economic_contract=ec)
}
