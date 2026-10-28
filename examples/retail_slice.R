# Installed offline integration example. Source this file, then run_retail_slice().
# All observations and outcomes are synthetic; no files are written.
if (!exists("fit_binary_glm")) library(creditratingsystem)

run_retail_slice <- function(path=system.file("extdata/retail_slice.json",package="creditratingsystem"), data=NULL, return_artifacts=FALSE) {
  d <- if(is.null(data)) jsonlite::fromJSON(path,simplifyVector=FALSE) else data
  product_ids<-as.list(sort(unique(vapply(d$applications,function(x)x$product_id,character(1)))))
  target <- d$target
  train <- d$training; cal <- d$calibration
  mat <- function(x) matrix(unlist(x,use.names=FALSE),nrow=length(x),byrow=TRUE)
  fit <- fit_binary_glm(mat(train$X),unlist(train$y),weights=unlist(train$weights),
    link="logit",feature_order="recent_arrears",fit_intercept=TRUE,
    target=target,training=train$metadata,model_id="synthetic-retail-glm",model_version="1")
  pred <- predict_pd(fit,mat(cal$X),feature_order="recent_arrears",expected_target=target)
  calibrator <- calibrate_logistic(pred$value,unlist(cal$y),weights=unlist(cal$weights),metadata=cal$metadata)
  model <- fit$model
  model$application_contract <- list(product_ids=product_ids,
    population_ids=list("synthetic_new_customer"),performance_origin="first_draw_at",
    term_stage="requested",term_features=structure(list(),names=character()))
  model$calibration_artifact <- calibrator$value
  registry <- register_rating_model_version(NULL,
    list(model=model,dependencies=list(method="native GLM"),configuration=list(fixture="retail_slice")),
    list(model_id=model$model_id,model_version="1",registered_at="2023-12-31"))
  model_hash <- registry$value$entries[[1]]$model_hash
  route <- route_rating_profile(d$route_snapshot,d$routing_rules,"2024-02-01","2024-02-01")$value
  stopifnot(route$status=="ROUTED")
  assignments <- assign_model_role(NULL,registry,list(event_id="initial",profile_id=route$profile_id,purpose="application",
    role="champion",slot_id="champion",evaluation_reference="synthetic-analytic-calibration",reason="initial example model",
    required_feature_order=list("recent_arrears"),
    model_id=model$model_id,model_version="1",model_hash=model_hash,
    effective_from="2024-01-01",known_at="2023-12-31"))
  selected <- select_effective_model(registry,assignments,route$profile_id,"application","2024-02-01","2024-02-01")$value
  stopifnot(selected$status=="SELECTED")
  snapshots <- build_application_snapshot(d$applications,d$features,required_features="recent_arrears")$value
  requests <- lapply(snapshots,function(s) list(row_id=s$application_id,known_at="2024-02-01",
    model_id=model$model_id,model_version="1",model_hash=selected$selected$model_hash,
    recipe=list(method="application_risk",version="1.0",parameters=list(target=target,calibrator=calibrator$value)),
    input=list(snapshot=s)))
  batch <- rate_subject_batch(requests,registry,"synthetic-applications-2024","2024-02-01")$value
  stopifnot(all(vapply(batch$rows,function(r)r$status=="OK",logical(1))))
  policy <- compile_credit_decision_policy(d$policy)
  actions <- character(); observed_decisions <- list(); pds <- numeric(); decision_artifacts<-list()
  dated <- function(x) c(x,list(as_of="2024-02-01",known_at="2024-02-01"))
  for(i in seq_along(batch$rows)) {
    row <- batch$rows[[i]]; e <- d$economics[[i]]
    residual <- residual_income(e$net_income,e$recurring_expenses,e$existing_debt_service,e$new_debt_service,
      period="monthly",currency="EUR")
    dsti <- debt_service_to_income(e$existing_debt_service+e$new_debt_service,e$net_income,period="monthly",currency="EUR")
    capacity <- assess_affordability(list(residual=residual$value$residual,dsti=dsti$value$ratio),
      d$affordability_profile,list(income_verified=TRUE))
    decision <- apply_credit_decision_policy(policy,
      dated(c(row$value,list(freshness="CURRENT"))),dated(list(status=capacity$value$assessment)),
      dated(list(status=e$eligibility)),dated(list(status=e$operations)),
      as_of="2024-02-01",known_at="2024-02-01")$value
    decision_artifacts[[i]]<-decision
    actions <- c(actions,decision$action); pds <- c(pds,row$value$pd)
    observed <- list(decision_id=paste0("decision_",row$row_id),application_id=row$row_id,
      status=unname(c(APPROVE="approved",DECLINE="rejected",REFER="pending")[decision$action]),
      decided_at="2024-02-01",known_at="2024-02-01")
    if(row$row_id=="A") observed <- c(observed,list(accepted_at="2024-02-02",accepted_known_at="2024-02-02"))
    observed_decisions[[i]] <- observed
  }
  stopifnot(identical(actions,unlist(d$expected_actions,use.names=FALSE)),max(abs(pds-c(.03,.12,.03,.03,.03,.03)))<1e-8)
  outcomes <- build_application_targets(d$applications,observed_decisions,d$booked,d$defaults,list(),365,"2025-03-01")$value
  labels <- lapply(outcomes,function(r)r$target)
  stopifnot(isTRUE(all.equal(labels,d$expected_targets)),outcomes[[6]]$selection_status=="approved_not_taken_up")
  behavior <- d$behavioral; bt <- behavior$training
  bfit <- fit_binary_glm(mat(bt$X),unlist(bt$y),weights=unlist(bt$weights),link="logit",
    feature_order="dpd",fit_intercept=TRUE,target=behavior$target,training=bt$metadata,
    model_id="synthetic-behavioral-glm",model_version="1")
  bmodel <- bfit$model
  bmodel$behavioral_contract <- list(risk_unit="account",event_policy="first_default",
    product_ids=product_ids,allowed_states=list("ACTIVE"),maximum_source_age_days=31)
  registry <- register_rating_model_version(registry,
    list(model=bmodel,dependencies=list(method="native GLM"),configuration=list(fixture="behavioral")),
    list(model_id=bmodel$model_id,model_version="1",registered_at="2023-12-31"))
  bhash <- Filter(function(e)e$model_id==bmodel$model_id,registry$value$entries)[[1]]$model_hash
  panel <- build_account_month_panel(behavior$accounts,behavior$statements,behavior$events,
    observation_dates=unlist(behavior$observation_dates))$value$accounts
  stopifnot(identical(vapply(panel,function(s)s$at_risk,logical(1)),c(TRUE,TRUE,FALSE)))
  breqs <- lapply(panel[1:2],function(s)list(row_id=paste0("behavioral-",s$as_of),known_at=s$known_at,
    model_id=bmodel$model_id,model_version="1",model_hash=bhash,
    recipe=list(method="behavioral_risk",version="1.0",parameters=list(target=behavior$target)),input=list(snapshot=s)))
  bbatch <- rate_subject_batch(breqs,registry,"behavioral-2024","2024-08-31")$value
  stopifnot(all(vapply(bbatch$rows,function(r)r$status=="OK",logical(1))))
  bpds <- vapply(bbatch$rows,function(r)r$value$pd,numeric(1))
  stopifnot(max(abs(bpds-c(.01,.1)))<1e-8)
  blabels <- build_behavioral_targets(panel,behavior$events,horizon_months=12,target_event="DEFAULT",
    competing_events="CLOSED",exit_treatment="censor",evaluation_cutoff="2025-03-01",coverage=behavior$coverage)$value
  stopifnot(isTRUE(all.equal(lapply(blabels,function(r)r$label),list(1,1,NULL))))
  monitoring_target <- list(event="default",default_definition="synthetic-first-default",unit="account",
    horizon_days=365,probability_measure="PHYSICAL",default_grade="D")
  forecasts <- list(list(forecast_id="behavioral-A-Feb",case_id="A-Feb",entity_id="facility_A",cluster_id="customer_A",
    cohort="2024-02",segment=product_ids[[1]],model_id=bmodel$model_id,model_version="1",forecast_at="2024-02-29",
    known_at="2024-02-29",pd=bpds[1],target=monitoring_target,prediction_origin="OUT_OF_TIME",origin_grade="P",override=FALSE))
  mcohort <- build_monitoring_cohorts(forecasts,
    list(list(event_id="default_A",entity_id="facility_A",event="default",event_at="2024-09-01",known_at="2024-09-02",default_definition="synthetic-first-default")),
    list(list(entity_id="facility_A",complete_through="2025-03-01",known_at="2025-03-01")),
    target_contract=monitoring_target,evaluation_cutoff="2025-03-01")
  monitoring <- monitor_rating_cohort(mcohort,group_by="segment",reference_window=list(start="2024-02-01",end="2024-02-29"),
    minimum_count=30,uncertainty="none")$value[[1]]
  stopifnot(monitoring$status=="INSUFFICIENT_INFORMATION",monitoring$metrics$n==1)
  collections <- do.call(build_collections_episodes,c(d$collections,list(evaluation_cutoff="2025-03-01")))
  cure <- build_cure_targets(collections,horizon_days=365,redefault_horizon_days=365,evaluation_cutoff="2025-03-01")$value[[1]]
  stopifnot(collections$value$episodes[[1]]$status=="OPEN",is.null(cure$cure_label),cure$ever_default)
  later_model <- model; later_model$model_version <- "2";later_model$calibration_artifact <- NULL
  later_registry <- register_rating_model_version(registry,
    list(model=later_model,dependencies=list(method="native GLM"),configuration=list(fixture="later-synthetic-model")),
    list(model_id=model$model_id,model_version="2",registered_at="2025-01-01"))
  later_hash <- Filter(function(e)e$model_version=="2",later_registry$value$entries)[[1]]$model_hash
  later_assignments <- assign_model_role(assignments,later_registry,
    list(event_id="later-model",profile_id=route$profile_id,purpose="application",role="champion",slot_id="champion",
      model_id=model$model_id,model_version="2",model_hash=later_hash,effective_from="2025-02-01",known_at="2025-01-01",
      evaluation_reference="synthetic-administration-only",reason="illustrate historical replay",required_feature_order=list("recent_arrears")))
  stopifnot(select_effective_model(later_registry,later_assignments,route$profile_id,"application","2025-02-01","2025-02-01")$value$selected$model_version=="2",
    select_effective_model(later_registry,later_assignments,route$profile_id,"application","2024-02-01","2024-02-01")$value$selected$model_version=="1")
  replay <- replay_assessment_history(requests,later_registry,batch$rows,"2024-02-01")$value
  stopifnot(replay$status=="REPRODUCED")
  breplay <- replay_assessment_history(breqs,later_registry,bbatch$rows,"2024-08-31")$value
  stopifnot(breplay$status=="REPRODUCED")
  summary <- list(data_origin="SYNTHETIC",actions=as.list(actions),calibrated_pd=as.list(pds),
    labels=labels,replay=replay$status,behavioral_pd=as.list(bpds),
    behavioral_labels=lapply(blabels,function(r)r$label),behavioral_replay=breplay$status,
    monitoring_status=monitoring$status,monitoring_observed_rate=monitoring$metrics$observed_rate,
    collections_status=collections$value$episodes[[1]]$status,cure_label=cure$cure_label,
    note="One funded observed outcome; no empirical performance claim or hypothetical reject labels.")
  if(return_artifacts) return(list(summary=summary,model=model,behavioral_model=bmodel,calibrator=calibrator$value,
    registry=later_registry$value,decision_artifacts=decision_artifacts,requests=requests,batch=batch,behavioral_requests=breqs,
    behavioral_batch=bbatch,policy=policy$value,application_targets=outcomes,account_panel=panel,
    behavioral_targets=blabels,observed_decisions=observed_decisions,monitoring=monitoring))
  summary
}
