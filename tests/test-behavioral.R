library(creditratingsystem)
error_beh <- function(expr,code) {
  e <- tryCatch(force(expr),error=identity)
  stopifnot(inherits(e,"creditratingsystem_error"),identical(e$code,code))
}
hash_beh <- getFromNamespace(".cr_hash","creditratingsystem")
empty_beh <- structure(list(),names=character())
profile_beh <- list(schema_version="1.0",profile_id="synthetic-states",immaterial_state="CURRENT",
  materiality=list(absolute=1,relative=.01,logic="and"),
  buckets=list(list(state="CURRENT",lower=0,upper=30),list(state="EARLY",lower=30,upper=60),list(state="LATE",lower=60,upper=NULL)),
  event_priority=list(list(event="chargeoff",state="WRITTEN_OFF",at_risk=FALSE,absorbing=TRUE),list(event="utp",state="DEFAULT",at_risk=FALSE,absorbing=TRUE)),
  dpd_default_rule=list(minimum_dpd=90,state="DEFAULT",material_required=TRUE))
obs_beh <- list(list(account_id="a",obligation_id="jan",amount=100,currency="EUR",due_at="2024-01-01",known_at="2023-12-20"),
                list(account_id="a",obligation_id="feb",amount=100,currency="EUR",due_at="2024-02-01",known_at="2023-12-20"))
pay_beh <- list(list(account_id="a",payment_id="p",amount=150,currency="EUR",paid_at="2024-01-15",known_at="2024-01-16",kind="payment"),
                list(account_id="a",payment_id="r",amount=100,currency="EUR",paid_at="2024-02-10",known_at="2024-02-12",kind="reversal",reverses_payment_id="p"))
x_beh <- compute_days_past_due(obs_beh,pay_beh,"2024-02-29","2024-02-29",currency="EUR")$value
stopifnot(x_beh$dpd==59,x_beh$past_due_amount==150,x_beh$obligations[[1]]$unpaid==50)
x_beh <- compute_days_past_due(obs_beh,pay_beh,"2024-01-31","2024-01-31",currency="EUR")$value
stopifnot(x_beh$dpd==0,x_beh$unallocated_payments==50)
stopifnot(compute_days_past_due(obs_beh,pay_beh,"2024-02-11","2024-02-11",currency="EUR")$value$past_due_amount==50)
pay_bad_beh <- pay_beh;pay_bad_beh[[2]]$amount <- 151
error_beh(compute_days_past_due(obs_beh,pay_bad_beh,"2024-02-29","2024-02-29",currency="EUR"),"BEHAVIORAL_REVERSAL")
for(case in list(list(0,"CURRENT"),list(29,"CURRENT"),list(30,"EARLY"),list(59,"EARLY"),list(60,"LATE"),list(90,"DEFAULT")))
  stopifnot(assign_delinquency_state(case[[1]],10,100,empty_beh,profile_beh)$value$state==case[[2]])
stopifnot(assign_delinquency_state(0,0,100,list(utp=TRUE),profile_beh)$value$state=="DEFAULT",
  assign_delinquency_state(100,.5,100,empty_beh,profile_beh)$value$state=="CURRENT",
  assign_delinquency_state(0,0,100,list(utp=TRUE,chargeoff=TRUE),profile_beh)$value$state=="WRITTEN_OFF")

# Dated independent cohort: 100 origins -> 65 active,5 arrears,10 defaults,20 exits.
acc_beh <- list();stat_beh <- list();events_beh <- list();coverage_beh <- list()
for(i in 0:99) {
  aid <- sprintf("a%03d",i)
  acc_beh[[length(acc_beh)+1L]] <- list(account_id=aid,customer_id=sprintf("c%03d",i),product_id="card",
    opened_at="2024-01-01",known_at="2024-01-01",currency="EUR",original_balance=100)
  coverage_beh[[length(coverage_beh)+1L]] <- list(account_id=aid,observed_through="2024-03-31",known_at="2024-03-31")
  for(t in c("2024-01-31","2024-02-29","2024-03-31"))
    stat_beh[[length(stat_beh)+1L]] <- list(account_id=aid,statement_id=paste0(aid,t),observed_at=t,known_at=t,
      balance=if(i==0) 0 else 100,credit_limit=1000,dpd=if(i>=30&&i<35&&t!="2024-01-31") 30 else 0,
      past_due_amount=if(i>=30&&i<35) 10 else 0,currency="EUR")
  if(i<30) events_beh[[length(events_beh)+1L]] <- list(account_id=aid,event_id=paste0(aid,"event"),
    event_type=if(i<10) "DEFAULT" else "CLOSED",occurred_at="2024-02-15",known_at="2024-02-16")
}
panel_beh <- build_account_month_panel(acc_beh,stat_beh,events_beh,c("2024-01-31","2024-02-29","2024-03-31"))$value$accounts
panel_beh <- lapply(panel_beh,function(r) {
  if(r$features$dpd==30) {r$state <- "ARREARS";r$snapshot_hash <- NULL;r$snapshot_hash <- hash_beh(r)}
  r
})
stopifnot(panel_beh[[1]]$features$balance==0,panel_beh[[1]]$at_risk)
feb_beh <- Filter(function(r) r$as_of=="2024-02-29",panel_beh)
stopifnot(sum(vapply(feb_beh,function(r) r$at_risk,logical(1)))==70,
  sum(vapply(feb_beh,function(r) r$lifecycle_state=="CLOSED",logical(1)))==20)
jan_beh <- Filter(function(r) r$as_of=="2024-01-31",panel_beh)
labels_beh <- build_behavioral_targets(jan_beh,events_beh,1,"DEFAULT",list("CLOSED"),"competing","2024-03-31",coverage_beh)$value
stopifnot(sum(vapply(labels_beh,function(r) r$label==1,logical(1)))==10,
  sum(vapply(labels_beh,function(r) r$status=="COMPETING_EVENT",logical(1)))==20)
labels_beh <- build_behavioral_targets(jan_beh,events_beh,1,"DEFAULT",list("CLOSED"),"censor","2024-03-31",coverage_beh)$value
stopifnot(sum(vapply(labels_beh,function(r) is.null(r$label),logical(1)))==20)
labels_beh <- build_behavioral_targets(jan_beh,events_beh,1,"DEFAULT",list("CLOSED"),"censor","2024-03-31",list())$value
stopifnot(sum(vapply(labels_beh,function(r) is.null(r$label),logical(1)))==90,is.null(labels_beh[[100]]$followup_days))
states_beh <- c("ACTIVE","ARREARS","DEFAULT","CLOSED")
roll_beh <- estimate_retail_roll_rates(panel_beh,states_beh,"2024-01-31","2024-02-29",list("product_id"),"count","error")$value$groups[[1]]
stopifnot(identical(unlist(roll_beh$origin_count),c(100L,0L,0L,0L)),
  all(abs(unlist(roll_beh$transition_matrix[[1]])-c(.65,.05,.1,.2))<1e-14),
  all(vapply(roll_beh$transition_matrix[[2]],is.null,logical(1))))
weighted_beh <- estimate_retail_roll_rates(panel_beh,states_beh,"2024-01-31","2024-02-29",list("product_id"),"opening_balance","error")$value$groups[[1]]
stopifnot(weighted_beh$origin_count[[1]]==100,weighted_beh$origin_mass[[1]]==9900,
  abs(weighted_beh$transition_matrix[[1]][[3]]-9/99)<1e-14)
missing_beh <- Filter(function(r) !(r$account_id=="a099"&&r$as_of=="2024-02-29"),panel_beh)
error_beh(estimate_retail_roll_rates(missing_beh,states_beh,"2024-01-31","2024-02-29",list(),"count","error"),"MISSING_SUCCESSOR")
roll_beh <- estimate_retail_roll_rates(missing_beh,states_beh,"2024-01-31","2024-02-29",list(),"count","censor")$value$groups[[1]]
stopifnot(roll_beh$censored_count[[1]]==1,sum(unlist(roll_beh$counts[[1]]))+roll_beh$censored_count[[1]]==100)
mat_beh <- matrix(c(.8,.1,.1,0,1,0,0,0,1),3,3,byrow=TRUE)
proj_beh <- project_retail_roll_rates(c(1,0,0),list(mat_beh,mat_beh),c("P","D","C"),"D","C",matrix(c(100,100,0,0,0,0),2,3,byrow=TRUE))$value
stopifnot(max(abs(unlist(proj_beh$periods[[2]]$state_probabilities)-c(.64,.18,.18)))<1e-14,
  abs(proj_beh$periods[[2]]$marginal_default-.08)<1e-14,proj_beh$periods[[1]]$expected_balance==90,
  proj_beh$periods[[2]]$expected_balance==0)
target_beh <- list(event="first_default",horizon=12,time_unit="months",probability_measure="PHYSICAL",conditioning="PIT")
model_beh <- list(schema_version="1.0",model_id="synthetic-behavioral",model_version="1",method_id="classification.fit_binary_glm",
  feature_order=list("balance"),parameters=list(link="logit",fit_intercept=TRUE,coefficients=list(log(1/9),0),offset_required=FALSE),
  preprocessing=list(transformation="identity"),target=target_beh,
  training=list(data_fingerprint="synthetic-reference",split_id="train-only",known_at="2023-12-01"),diagnostics=empty_beh,
  provenance=list(data_origin="SYNTHETIC",parameter_origin="USER_SUPPLIED"),
  behavioral_contract=list(risk_unit="account",event_policy="first_default",product_ids=list("card"),allowed_states=list("ACTIVE"),maximum_source_age_days=31))
stopifnot(abs(predict_behavioral_risk(panel_beh[[1]],model_beh,target_beh)$value$pd-.1)<1e-14)
bad_snapshot_beh <- panel_beh[[1]];bad_snapshot_beh$features$balance <- 999
error_beh(predict_behavioral_risk(bad_snapshot_beh,model_beh,target_beh),"INTEGRITY_ERROR")
vintage_beh <- build_vintage_performance(acc_beh,panel_beh,events_beh,list(0,1,2),"2024-03-31",coverage_beh,"DEFAULT",list("ARREARS"),list("CLOSED"))$value
stopifnot(vintage_beh[[2]]$cumulative_default_rate==.1,vintage_beh[[2]]$incidence_rate==.1,
  vintage_beh[[2]]$delinquency_stock_rate==.05,vintage_beh[[2]]$exits==20,
  vintage_beh[[3]]$cumulative_default_rate==.1,vintage_beh[[3]]$incidence_rate==0)
cat("Native behavioral payment, 100-account cohort, matrix projection and prediction oracles passed\n")
cure_events_beh <- c(events_beh,list(list(account_id="a000",event_id="cure",event_type="CURED",occurred_at="2024-03-15",known_at="2024-03-16")))
cured_panel_beh <- build_account_month_panel(acc_beh,stat_beh,cure_events_beh,list("2024-03-31"))$value$accounts
stopifnot(cured_panel_beh[[1]]$lifecycle_state=="CURED",cured_panel_beh[[1]]$ever_default,!cured_panel_beh[[1]]$at_risk)
cured_vintage_beh <- build_vintage_performance(acc_beh,cured_panel_beh,cure_events_beh,list(2),"2024-03-31",coverage_beh,"DEFAULT",list("DEFAULT"),list("CLOSED"))$value[[1]]
stopifnot(cured_vintage_beh$cumulative_default_rate==.1,cured_vintage_beh$delinquency_stock_rate==.09)
future_model_beh <- model_beh;future_model_beh$training$known_at <- "2024-02-01"
error_beh(predict_behavioral_risk(panel_beh[[1]],future_model_beh,target_beh),"BEHAVIORAL_LEAKAGE")
ambiguous_beh <- c(events_beh,list(list(account_id="a000",event_id="zzzexit",event_type="CLOSED",occurred_at="2024-02-15",known_at="2024-02-16")))
error_beh(build_vintage_performance(acc_beh,panel_beh,ambiguous_beh,list(1),"2024-03-31",coverage_beh,"DEFAULT",list(),list("CLOSED")),"BEHAVIORAL_AMBIGUITY")
