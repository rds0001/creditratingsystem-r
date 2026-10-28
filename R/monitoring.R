# Local content-bound model-role history and monitoring escalation.
.cr_mon_assignment_fields <- function(event) {
  .cr_data_required(event,c("event_id","profile_id","purpose","role","slot_id","model_id",
    "model_version","model_hash","effective_from","known_at","evaluation_reference","reason"))
  for(key in c("event_id","profile_id","purpose","slot_id","model_id","model_version",
               "model_hash","evaluation_reference","reason")) .cr_rs_string(event[[key]],key)
  if(!event$role %in% c("champion","challenger")) .cr_error("INVALID_MODEL_ROLE","Declare champion or challenger")
  if(event$role=="champion" && event$slot_id!="champion") .cr_error("INVALID_MODEL_ROLE","Only one champion slot per scope")
  .cr_data_date(event$effective_from,"effective_from"); .cr_data_date(event$known_at,"known_at")
  n <- event$required_feature_order
  if(!is.list(n)||!is.null(names(n))||any(!vapply(n,function(x)is.character(x)&&length(x)==1L&&!is.na(x)&&nzchar(x),logical(1)))||anyDuplicated(unlist(n)))
    .cr_error("FEATURE_MISMATCH","Declare a unique ordered feature schema array")
}
.cr_mon_history <- function(history) {
  if(is.null(history)) return(.cr_rs_seal(list(schema_version="1.0",kind="ModelAssignmentHistory",events=list()),"history_hash"))
  h <- .cr_rs_sealed(history,"history_hash","assignment history")
  if(!identical(h$kind,"ModelAssignmentHistory")) .cr_error("INVALID_ASSIGNMENT_HISTORY","Expected model assignment history")
  rows <- .cr_data_rows(h$events,"assignment events"); .cr_data_unique(rows,"event_id","assignment events")
  keys <- character()
  for(row in rows) {
    .cr_mon_assignment_fields(row)
    key <- .cr_data_key(row,c("profile_id","purpose","role","slot_id","effective_from"))
    if(key %in% keys) .cr_error("ASSIGNMENT_CONFLICT","Multiple events at a role/slot effective boundary")
    keys <- c(keys,key)
  }
  h
}
.cr_mon_projection <- function(history,known_at,role="champion") {
  h <- .cr_mon_history(history); cutoff <- .cr_data_date(known_at,"known_at")
  visible <- Filter(function(e).cr_data_date(e$known_at,"event.known_at")<=cutoff&&e$role==role,h$events)
  visible <- .cr_rs_sort(visible,c("profile_id","purpose","role","slot_id","effective_from"))
  output <- list()
  for(i in seq_along(visible)) {
    e <- visible[[i]]
    group <- function(x) .cr_data_key(x,c("profile_id","purpose","role","slot_id"))
    end <- if(i<length(visible)&&group(e)==group(visible[[i+1L]])) visible[[i+1L]]$effective_from else NULL
    output[[length(output)+1L]] <- list(assignment_id=e$event_id,profile_id=e$profile_id,purpose=e$purpose,
      role=e$role,slot_id=e$slot_id,model_id=e$model_id,model_version=e$model_version,model_hash=e$model_hash,
      effective_from=e$effective_from,effective_to=end,known_at=e$known_at)
  }
  output
}
assign_model_role <- function(history,registry,assignment,context=NULL) {
  h <- .cr_mon_history(history); registry <- .cr_rs_registry(registry)
  event <- .cr_rs_object(assignment,"assignment"); .cr_mon_assignment_fields(event)
  previous <- Filter(function(e)e$event_id==event$event_id,h$events)
  if(length(previous)) {
    if(.cr_hash(previous[[1]])!=.cr_hash(event)) .cr_error("ASSIGNMENT_CONFLICT","Event id already binds different content")
    return(.cr_result(h,"monitoring.assign_model_role","assignment_history",context,diagnostics=list(idempotent=TRUE)))
  }
  if(length(h$events)&&event$known_at<max(vapply(h$events,function(e)e$known_at,character(1))))
    .cr_error("ASSIGNMENT_DATE_ORDER","Append in nondecreasing knowledge order")
  model <- Filter(function(e)e$model_id==event$model_id&&e$model_version==event$model_version,registry$entries)
  if(!length(model)||model[[1]]$registered_at>event$known_at) .cr_error("MISSING_MODEL","Assigned model must already be registered and known")
  if(model[[1]]$model_hash!=event$model_hash) .cr_error("RATING_INTEGRITY","Assignment must pin registered content")
  if(!identical(model[[1]]$bundle$model$feature_order,event$required_feature_order)) .cr_error("FEATURE_MISMATCH","Model differs from deployment feature schema")
  h$events[[length(h$events)+1L]] <- event
  h <- .cr_rs_seal(h,"history_hash"); .cr_mon_history(h)
  .cr_result(h,"monitoring.assign_model_role","assignment_history",context,diagnostics=list(idempotent=FALSE),
    limitations="Role and evaluation references are local user assertions; no model deployment or institutional approval occurs.")
}
rollback_model_assignment <- function(history,registry,target_event_id,rollback_event,context=NULL) {
  h <- .cr_mon_history(history); .cr_rs_string(target_event_id,"target_event_id")
  target <- Filter(function(e)e$event_id==target_event_id,h$events)
  if(!length(target)) .cr_error("MISSING_ASSIGNMENT","Explicit historical rollback event required")
  event <- .cr_rs_object(rollback_event,"rollback_event")
  .cr_data_required(event,c("event_id","effective_from","known_at","reason","evaluation_reference"))
  prior <- target[[1]]
  if(.cr_data_date(prior$known_at,"target.known_at")>.cr_data_date(event$known_at,"known_at"))
    .cr_error("ASSIGNMENT_DATE_ORDER","Unknown rollback target")
  if(.cr_data_date(prior$effective_from,"target.effective_from")>=.cr_data_date(event$effective_from,"effective_from"))
    .cr_error("ASSIGNMENT_DATE_ORDER","Rollback must follow original assignment start")
  for(key in c("profile_id","purpose","role","slot_id","model_id","model_version","model_hash")) {
    if(key %in% names(event)&&!identical(event[[key]],prior[[key]])) .cr_error("ASSIGNMENT_CONFLICT","Rollback changes target model or scope")
    event[[key]] <- prior[[key]]
  }
  event$rollback_of <- target_event_id
  result <- assign_model_role(h,registry,event,context)
  .cr_result(result$value,"monitoring.rollback_model_assignment","assignment_history",context,
             diagnostics=result$diagnostics,limitations=result$limitations)
}
classify_monitoring_findings <- function(metric_panel,policy,context=NULL) {
  p <- .cr_rs_object(policy,"monitoring policy")
  .cr_data_required(p,c("schema_version","policy_id","version","periods","series_ids","rules"))
  if(!identical(p$schema_version,"1.0")) .cr_error("UNSUPPORTED_SCHEMA_VERSION","Unsupported monitoring policy")
  .cr_rs_string(p$policy_id,"policy_id"); .cr_rs_string(p$version,"version")
  valid_array <- function(x) is.list(x)&&is.null(names(x))&&length(x)>0L&&
    all(vapply(x,function(z)is.character(z)&&length(z)==1L&&!is.na(z)&&nzchar(z),logical(1)))&&!anyDuplicated(unlist(x))
  if(!valid_array(p$periods)) .cr_error("INVALID_POLICY","Declare distinct ordered periods")
  if(!valid_array(p$series_ids)) .cr_error("INVALID_POLICY","Declare all expected series identifiers")
  periods <- unlist(p$periods,use.names=FALSE); series <- sort(unlist(p$series_ids,use.names=FALSE),method="radix")
  rules <- .cr_data_rows(p$rules,"rules"); .cr_data_unique(rules,"rule_id","rules")
  if(!length(rules)) .cr_error("INVALID_POLICY","Monitoring rules required")
  panel <- .cr_data_rows(metric_panel,"metric_panel"); .cr_data_unique(panel,c("series_id","metric","period"),"metric_panel")
  for(row in panel) {
    .cr_data_required(row,c("series_id","metric","period","status"))
    if(!"value" %in% names(row)) .cr_error("INVALID_INPUT","Value must be present, possibly null")
    if(!row$period %in% periods||!row$status %in% c("OK","MISSING_OUTCOME","INSUFFICIENT_INFORMATION","TECHNICAL_ERROR","NOT_APPLICABLE"))
      .cr_error("INVALID_INPUT","Unknown period or metric status")
    if(!row$series_id %in% series) .cr_error("INVALID_INPUT","Undeclared series")
  }
  traces <- list()
  for(rule in rules) {
    .cr_data_required(rule,c("rule_id","metric","operator","threshold","consecutive_periods","severity","action_code"))
    n <- rule$consecutive_periods
    if(!rule$operator %in% c("gt","ge","lt","le")||!is.numeric(n)||length(n)!=1L||!is.finite(n)||n<1||n!=floor(n)||!rule$severity %in% c("WARNING","CRITICAL"))
      .cr_error("INVALID_POLICY","Invalid threshold direction, persistence or severity")
    threshold <- .cr_numeric(rule$threshold,"threshold")
    if(length(threshold)!=1L) .cr_error("INVALID_POLICY","Scalar threshold required")
    .cr_rs_string(rule$action_code,"action_code")
    for(sid in series) {
      streak <- 0L
      rows <- Filter(function(r)r$series_id==sid&&r$metric==rule$metric,panel)
      for(period in periods) {
        matched <- Filter(function(r)r$period==period,rows)
        row <- if(length(matched))matched[[1]] else NULL
        status <- if(is.null(row))"INSUFFICIENT_INFORMATION" else row$status
        if(status=="OK"&&!is.null(row$value)) {
          value <- .cr_numeric(row$value,"metric value")
          if(length(value)!=1L) .cr_error("INVALID_INPUT","Scalar metric value required")
          breached <- switch(rule$operator,gt=value>threshold,ge=value>=threshold,lt=value<threshold,le=value<=threshold)
          streak <- if(breached)streak+1L else 0L
          assessment <- if(streak>=n)rule$severity else if(breached)"WATCH" else "WITHIN_POLICY"
        } else {
          if(status=="OK")status<-"INSUFFICIENT_INFORMATION"
          breached<-NULL;streak<-0L;assessment<-status
        }
        traces[[length(traces)+1L]] <- list(series_id=sid,metric=rule$metric,period=period,rule_id=rule$rule_id,
          breached=breached,streak=streak,assessment=assessment,
          action_code=if(assessment %in% c("WARNING","CRITICAL"))rule$action_code else NULL)
      }
    }
  }
  states <- vapply(Filter(function(r)r$period==tail(periods,1),traces),function(r)r$assessment,character(1))
  precedence <- c("TECHNICAL_ERROR","CRITICAL","WARNING","MISSING_OUTCOME","INSUFFICIENT_INFORMATION","NOT_APPLICABLE","WATCH","WITHIN_POLICY")
  status <- if(length(states))precedence[which(precedence %in% states)[1]] else "INSUFFICIENT_INFORMATION"
  .cr_result(list(status=status,policy_id=p$policy_id,policy_version=p$version,policy_hash=.cr_hash(p),trace=traces),
    "monitoring.classify_monitoring_findings","monitoring_assessment",context,
    limitations="Thresholds and persistence are supplied policy, not universal significance levels. No automatic recalibration, model promotion or external message occurs.")
}
