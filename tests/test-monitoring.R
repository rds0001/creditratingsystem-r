if(!exists("assign_model_role")) library(creditratingsystem)
mon_error <- function(code,expr) {
  e <- tryCatch({force(expr);NULL},error=identity)
  stopifnot(inherits(e,"creditratingsystem_error"),identical(e$code,code))
}
mon_registry <- NULL
for(version in c("1","2")) {
  empty <- structure(list(),names=character())
  model <- list(schema_version="1.0",model_id="glm",model_version=version,method_id="classification.fit_binary_glm",
    feature_order=list("x"),parameters=list(link="logit",fit_intercept=TRUE,coefficients=list(0,1),offset_required=FALSE),
    preprocessing=list(transformation="identity"),target=list(event="default",horizon=1,time_unit="years",probability_measure="PHYSICAL",conditioning="PIT"),
    training=list(data_fingerprint="synthetic-counts",split_id="train",known_at="2020-01-01"),diagnostics=empty,
    provenance=list(data_origin="SYNTHETIC",parameter_origin="USER_SUPPLIED"))
  mon_registry <- register_rating_model_version(mon_registry,list(model=model,dependencies=empty,configuration=empty),
    list(model_id="glm",model_version=version,registered_at="2020-01-01"))
}
mon_event <- function(version="1",event_id="initial",effective="2021-01-01",known="2020-12-01") {
  entry <- Filter(function(e)e$model_version==version,mon_registry$value$entries)[[1]]
  list(event_id=event_id,profile_id="retail",purpose="application",role="champion",slot_id="champion",
    model_id="glm",model_version=version,model_hash=entry$model_hash,effective_from=effective,known_at=known,
    evaluation_reference="synthetic-evidence",reason="explicit local example",required_feature_order=list("x"))
}
first <- assign_model_role(NULL,mon_registry,mon_event())
second <- assign_model_role(first,mon_registry,mon_event("2","promotion","2022-01-01","2021-12-01"))
stopifnot(length(first$value$events)==1L,length(second$value$events)==2L,
  select_effective_model(mon_registry,second,"retail","application","2021-12-31","2021-12-31")$value$selected$model_version=="1",
  select_effective_model(mon_registry,second,"retail","application","2022-01-01","2022-01-01")$value$selected$model_version=="2")
revised <- assign_model_role(second,mon_registry,mon_event("1","later-information","2022-06-01","2022-07-01"))
stopifnot(select_effective_model(mon_registry,revised,"retail","application","2022-06-15","2022-06-15")$value$selected$model_version=="2",
  select_effective_model(mon_registry,revised,"retail","application","2022-06-15","2022-07-01")$value$selected$model_version=="1")
rolled <- rollback_model_assignment(second,mon_registry,"initial",list(event_id="rollback",effective_from="2023-01-01",
  known_at="2022-12-01",reason="synthetic finding",evaluation_reference="re-evaluation",required_feature_order=list("x")))
stopifnot(length(rolled$value$events)==3L,
  select_effective_model(mon_registry,rolled,"retail","application","2023-01-01","2023-01-01")$value$selected$model_version=="1",
  identical(assign_model_role(first,mon_registry,mon_event())$value,first$value))
bad <- mon_event("2");bad$event_id<-"duplicate-boundary"
mon_error("ASSIGNMENT_CONFLICT",assign_model_role(first,mon_registry,bad))
mon_error("FEATURE_MISMATCH",rollback_model_assignment(first,mon_registry,"initial",list(event_id="rollback",
  effective_from="2023-01-01",known_at="2022-12-01",reason="review",evaluation_reference="test",required_feature_order=list("different"))))
mon_policy <- list(schema_version="1.0",policy_id="example-monitor",version="1",periods=list("Q1","Q2","Q3"),series_ids=list("retail"),
  rules=list(list(rule_id="bias",metric="calibration_gap",operator="gt",threshold=.02,consecutive_periods=2,
    severity="WARNING",action_code="REVIEW_CALIBRATION")))
mon_panel <- function(values) lapply(seq_along(values),function(i)list(series_id="retail",metric="calibration_gap",
  period=paste0("Q",i),status="OK",value=values[[i]]))
result <- classify_monitoring_findings(mon_panel(list(.02,.03,.04)),mon_policy)$value
stopifnot(result$status=="WARNING",identical(vapply(result$trace,function(r)r$assessment,character(1)),c("WITHIN_POLICY","WATCH","WARNING")))
panel <- mon_panel(list(.03,NULL,.04));panel[[2]]$status<-"MISSING_OUTCOME"
result <- classify_monitoring_findings(panel,mon_policy)$value
stopifnot(result$status=="WATCH",result$trace[[3]]$streak==1L,result$trace[[2]]$assessment=="MISSING_OUTCOME")
p <- mon_policy;p$series_ids<-list("retail","mortgage")
stopifnot(classify_monitoring_findings(mon_panel(list(0,0,0)),p)$value$status=="INSUFFICIENT_INFORMATION",
  classify_monitoring_findings(list(),mon_policy)$value$status=="INSUFFICIENT_INFORMATION")
panel <- mon_panel(list(0,0,NULL));panel[[3]]$status<-"TECHNICAL_ERROR"
stopifnot(classify_monitoring_findings(panel,mon_policy)$value$status=="TECHNICAL_ERROR")
