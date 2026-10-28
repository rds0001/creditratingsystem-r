.cr_dec_sources <- c("risk", "affordability", "eligibility", "operations")
.cr_dec_number <- function(x) is.numeric(x) && !is.complex(x) && length(x)==1L && is.finite(x)
.cr_dec_text <- function(x, name) .cr_string(x, name, "INVALID_POLICY")
.cr_dec_array <- function(x) is.list(x) && is.null(names(x))
.cr_dec_compare <- function(x, op, y) {
  eq <- (is.character(x) && is.character(y) && identical(x,y)) ||
    (.cr_dec_number(x) && .cr_dec_number(y) && x==y)
  switch(op, eq=eq, ne=!eq, lt=x<y, le=x<=y, gt=x>y, ge=x>=y)
}
# Step one representable binary64 value, including subnormals and signed zero.
.cr_dec_next <- function(x, direction) {
  if(x==0) return(direction * .Machine$double.xmin * .Machine$double.eps)
  bytes <- as.integer(writeBin(as.double(x), raw(), size=8L, endian="big"))
  increment <- (x>0 && direction>0) || (x<0 && direction<0)
  for(i in 8:1) {
    if(increment) { bytes[i] <- bytes[i]+1L; if(bytes[i]<=255L) break; bytes[i] <- 0L }
    else { bytes[i] <- bytes[i]-1L; if(bytes[i]>=0L) break; bytes[i] <- 255L }
  }
  readBin(as.raw(bytes), "double", n=1L, size=8L, endian="big")
}
.cr_dec_possible <- function(predicates, fields) {
  keys <- unique(vapply(predicates, function(p) p$field, character(1)))
  for(key in keys) {
    ps <- Filter(function(p) p$field==key, predicates)
    f <- fields[[key]]
    if(f$type=="number") {
      bounds <- vapply(ps,function(p) p$value,numeric(1))
      candidates <- c(bounds,vapply(bounds,.cr_dec_next,numeric(1),direction=-1),
                      vapply(bounds,.cr_dec_next,numeric(1),direction=1))
      candidates <- candidates[is.finite(candidates)]
    } else candidates <- unlist(f$values,use.names=FALSE)
    if(!any(vapply(candidates,function(x)
      all(vapply(ps,function(p) .cr_dec_compare(x,p$op,p$value),logical(1))),logical(1)))) return(FALSE)
  }
  TRUE
}

.cr_dec_approval_contract <- function(contract,fields) {
  if(!.cr_object(contract) || anyDuplicated(names(contract)) || !setequal(names(contract),c("domain","affordability","required_gates"))) .cr_error("INVALID_POLICY","Explicit domain, affordability applicability and required gates required")
  if(length(contract$domain)!=1L || !is.character(contract$domain) || !contract$domain %in% c("RETAIL_CREDIT","WHOLESALE_CREDIT","INVESTMENT","COUNTERPARTY_LIMIT")) .cr_error("INVALID_POLICY","Unsupported decision domain")
  a<-contract$affordability
  if(!.cr_object(a) || anyDuplicated(names(a)) || length(a$mode)!=1L || !a$mode %in% c("REQUIRED","NOT_APPLICABLE")) .cr_error("INVALID_POLICY","Declare affordability applicability")
  if(a$mode=="REQUIRED") {if(!setequal(names(a),"mode")) .cr_error("INVALID_POLICY","Required affordability has no waiver metadata")}
  else {
    if(!contract$domain %in% c("INVESTMENT","COUNTERPARTY_LIMIT") || !setequal(names(a),c("mode","reason","reference","maximum_age_days"))) .cr_error("INVALID_POLICY","Documented inapplicability restricted to investment/counterparty domains")
    for(k in c("reason","reference")) .cr_dec_text(a[[k]],k)
    age<-a$maximum_age_days;if(!.cr_dec_number(age) || age<0 || age!=floor(age)) .cr_error("INVALID_POLICY","Applicability evidence needs nonnegative whole-day maximum age")
  }
  gates<-contract$required_gates;if(!.cr_dec_array(gates)) .cr_error("INVALID_POLICY","Required gates must be an array")
  seen<-character()
  for(gate in gates) {
    if(!.cr_object(gate) || anyDuplicated(names(gate)) || !setequal(names(gate),c("field","value")) || !is.character(gate$field) || length(gate$field)!=1L || is.na(gate$field)) .cr_error("INVALID_POLICY","Each gate requires field and categorical value")
    key<-gate$field
    if(key %in% seen || !key %in% names(fields) || fields[[key]]$type!="category" || !is.character(gate$value) || length(gate$value)!=1L || is.na(gate$value) || !gate$value %in% unlist(fields[[key]]$values)) .cr_error("INVALID_POLICY","Distinct declared categorical approval gates required")
    seen<-c(seen,key)
  }
}
.cr_dec_applicability_findings <- function(contract,inputs,as_of) {
  a<-contract$affordability;if(a$mode!="NOT_APPLICABLE")return(character());obj<-inputs$affordability;findings<-character()
  if(!identical(obj$applicability_reference,a$reference) || !identical(obj$domain,contract$domain))findings<-c(findings,"affordability.APPLICABILITY_BINDING")
  if(!is.null(obj$as_of) && as.numeric(as.Date(as_of)-as.Date(obj$as_of))>a$maximum_age_days)findings<-c(findings,"affordability.STALE_APPLICABILITY")
  if(is.null(obj$valid_until))findings<-c(findings,"affordability.MISSING_VALIDITY")
  else {.cr_date(obj$valid_until,"valid_until");origin<-if(is.null(obj$as_of))as_of else obj$as_of;if(obj$valid_until<=origin || !(as_of<obj$valid_until))findings<-c(findings,"affordability.EXPIRED_APPLICABILITY")}
  findings
}

compile_credit_decision_policy <- function(specification, context=NULL) {
  if(!.cr_object(specification)) .cr_error("INVALID_POLICY","Policy must be an object")
  s <- specification
  if(length(s$schema_version)!=1L || !s$schema_version %in% c("1.0","1.1")) .cr_error("UNSUPPORTED_SCHEMA_VERSION","Unsupported policy schema")
  if(identical(s$schema_version,"1.0") && "approval_contract" %in% names(s)) .cr_error("INVALID_POLICY","Approval contracts require specification schema 1.1")
  for(k in c("policy_id","version","approval_reference")) .cr_dec_text(s[[k]],k)
  for(k in c("known_at","effective_from","effective_until")) .cr_date(s[[k]],k)
  if(s$effective_from>=s$effective_until) .cr_error("INVALID_POLICY","Effective interval must be nonempty and end-exclusive")
  if(length(s$default_action)!=1L || !s$default_action %in% c("REFER","DECLINE","BLOCKED"))
    .cr_error("INVALID_POLICY","Unmatched cases require a nonapproval action")
  if(length(s$missing_action)!=1L || !s$missing_action %in% c("REFER","BLOCKED"))
    .cr_error("INVALID_POLICY","Missing inputs must refer or block")
  target <- s$target
  if(!.cr_object(target) || !all(c("event","horizon","time_unit","probability_measure","conditioning") %in% names(target)))
    .cr_error("INVALID_POLICY","Declare the risk target")
  if(!.cr_dec_number(target$horizon) || target$horizon<=0 || !identical(target$probability_measure,"PHYSICAL"))
    .cr_error("INVALID_POLICY","Policy requires positive-horizon physical risk")
  for(k in c("event","time_unit","conditioning")) .cr_dec_text(target[[k]],k)
  if(!target$time_unit %in% c("days","months","years")) .cr_error("INVALID_POLICY","Invalid horizon unit")
  fields <- s$fields
  if(!.cr_object(fields) || !length(fields)) .cr_error("INVALID_POLICY","Declare typed source fields")
  for(key in names(fields)) {
    parts <- strsplit(key,".",fixed=TRUE)[[1]]
    if(length(parts)!=2L || !parts[1] %in% .cr_dec_sources || !nzchar(parts[2]) || grepl("\\.$",key))
      .cr_error("INVALID_POLICY","Fields use source.name")
    f <- fields[[key]]
    if(!.cr_object(f) || length(f$type)!=1L || !f$type %in% c("number","category"))
      .cr_error("INVALID_POLICY","Field type must be number or category")
    if(f$type=="category") {
      vs <- f$values
      if(!.cr_dec_array(vs) || !length(vs) ||
         !all(vapply(vs,function(v) is.character(v) && length(v)==1L && !is.na(v) && nzchar(v),logical(1))) || anyDuplicated(unlist(vs)))
        .cr_error("INVALID_POLICY","Categories must enumerate distinct strings")
    }
  }
  if(s$schema_version=="1.1") .cr_dec_approval_contract(s$approval_contract,fields)
  rules <- s$rules
  if(!.cr_dec_array(rules) || !length(rules)) .cr_error("INVALID_POLICY","Policy requires rules")
  ids <- character(); findings <- list()
  for(r in rules) {
    if(!.cr_object(r)) .cr_error("INVALID_POLICY","Rules must be objects")
    for(k in c("rule_id","reason_code")) .cr_dec_text(r[[k]],k)
    if(r$rule_id %in% ids) .cr_error("INVALID_POLICY","Duplicate rule id")
    ids <- c(ids,r$rule_id)
    if(!.cr_dec_number(r$priority) || r$priority<0 || r$priority!=floor(r$priority) ||
       length(r$action)!=1L || !r$action %in% c("APPROVE","REFER","DECLINE","BLOCKED"))
      .cr_error("INVALID_POLICY","Invalid priority or action")
    if(!.cr_dec_array(r$when)) .cr_error("INVALID_POLICY","when must be an array")
    for(p in r$when) {
      if(!.cr_object(p) || length(p$field)!=1L || !p$field %in% names(fields) ||
         length(p$op)!=1L || !p$op %in% c("eq","ne","lt","le","gt","ge"))
        .cr_error("INVALID_POLICY","Invalid field or predicate")
      f <- fields[[p$field]]
      if(f$type=="number") {
        if(!.cr_dec_number(p$value)) .cr_error("INVALID_POLICY","Numeric thresholds must be finite")
      } else if(!p$op %in% c("eq","ne") || !is.character(p$value) || length(p$value)!=1L || !p$value %in% unlist(f$values))
        .cr_error("INVALID_POLICY","Invalid categorical predicate")
    }
    if(!.cr_dec_possible(r$when,fields)) .cr_error("UNREACHABLE_RULE","Contradictory rule",list(rule_id=r$rule_id))
  }
  rules <- rules[order(vapply(rules,function(r) r$priority,numeric(1)),
                       vapply(rules,function(r) r$rule_id,character(1)),method="radix")]
  negate <- c(eq="ne",ne="eq",lt="ge",le="gt",gt="le",ge="lt")
  for(i in seq_along(rules)) for(j in seq_len(i-1L)) {
    r <- rules[[i]]; earlier <- rules[[j]]
    overlap <- .cr_dec_possible(c(earlier$when,r$when),fields)
    if(overlap && earlier$priority==r$priority && earlier$action!=r$action)
      .cr_error("CONFLICTING_RULES","Equal-priority conflicting actions",list(rules=list(earlier$rule_id,r$rule_id)))
    if(overlap && earlier$priority<r$priority) {
      findings[[length(findings)+1L]] <- list(earlier=earlier$rule_id,later=r$rule_id,finding="PRIORITY_OVERLAP")
      implies <- all(vapply(earlier$when,function(p) {
        p$op <- unname(negate[p$op]); !.cr_dec_possible(c(r$when,list(p)),fields)
      },logical(1)))
      if(implies) findings[[length(findings)+1L]] <- list(rule_id=r$rule_id,finding="SHADOWED_RULE",by=earlier$rule_id)
    }
  }
  s$rules <- rules
  artifact <- list(schema_version="1.0",kind="DecisionPolicy",specification=s)
  artifact$sha256 <- .cr_hash(artifact)
  .cr_result(artifact,"decisioning.compile_credit_decision_policy","policy",context,
    diagnostics=list(rule_findings=findings),
    limitations="Approval references are supplied assertions, not institutional authorization.")
}

.cr_dec_checked <- function(artifact,kind) {
  if(inherits(artifact,"CalcResult")) artifact <- artifact$value
  if(!.cr_object(artifact)) .cr_error("INVALID_ARTIFACT","Expected fingerprinted artifact")
  payload <- artifact; digest <- payload$sha256; payload$sha256 <- NULL
  if(!identical(payload$kind,kind) || !identical(digest,.cr_hash(payload)))
    .cr_error("ARTIFACT_HASH_MISMATCH","Artifact differs from fingerprint")
  artifact
}

apply_credit_decision_policy <- function(policy,risk,affordability,eligibility,operations,
                                         as_of,known_at,context=NULL) {
  policy <- .cr_dec_checked(policy,"DecisionPolicy"); s <- policy$specification
  if(!identical(compile_credit_decision_policy(s)$value$sha256,policy$sha256))
    .cr_error("INVALID_ARTIFACT","Policy is not canonical")
  .cr_date(as_of,"as_of"); .cr_date(known_at,"known_at")
  if(known_at<as_of) .cr_error("INVALID_DATE","Knowledge date precedes decision")
  inputs <- lapply(list(risk=risk,affordability=affordability,eligibility=eligibility,operations=operations),
    function(x) if(inherits(x,"CalcResult")) x$value else x)
  if(!all(vapply(inputs,.cr_object,logical(1)))) .cr_error("INVALID_INPUT","Provide four separate source objects")
  findings <- character()
  if(!(s$effective_from<=as_of && as_of<s$effective_until) || s$known_at>known_at)
    findings <- c(findings,"POLICY_NOT_EFFECTIVE")
  for(source in names(inputs)) {
    obj <- inputs[[source]]
    if(!all(c("as_of","known_at") %in% names(obj))) findings <- c(findings,paste0(source,".MISSING_TIME"))
    else {
      .cr_date(obj$as_of,"as_of"); .cr_date(obj$known_at,"known_at")
      if(obj$known_at<obj$as_of) .cr_error("INVALID_DATE","Input knowledge date precedes observation")
      if(obj$as_of>as_of || obj$known_at>known_at) findings <- c(findings,paste0(source,".FUTURE_INPUT"))
    }
  }
  if(!identical(.cr_hash(inputs$risk$target),.cr_hash(s$target))) findings <- c(findings,"risk.TARGET_MISMATCH")
  if(!identical(inputs$risk$freshness,"CURRENT")) findings <- c(findings,"risk.NOT_CURRENT")
  if(s$schema_version=="1.1") findings<-c(findings,.cr_dec_applicability_findings(s$approval_contract,inputs,as_of))
  values <- list()
  for(key in names(s$fields)) {
    f <- s$fields[[key]]; parts <- strsplit(key,".",fixed=TRUE)[[1]]; x <- inputs[[parts[1]]][[parts[2]]]
    if(is.null(x)) findings <- c(findings,paste0(key,".MISSING"))
    else if((f$type=="number" && !.cr_dec_number(x)) ||
            (f$type=="category" && (!is.character(x) || length(x)!=1L || is.na(x) || !x %in% unlist(f$values))))
      findings <- c(findings,paste0(key,".INVALID"))
    else values[[key]] <- x
  }
  trace <- lapply(s$rules,function(r) {
    results <- lapply(r$when,function(p) if(!p$field %in% names(values)) NULL else .cr_dec_compare(values[[p$field]],p$op,p$value))
    matched <- if(any(vapply(results,identical,logical(1),FALSE))) FALSE else if(any(vapply(results,is.null,logical(1)))) NULL else TRUE
    list(rule_id=r$rule_id,priority=r$priority,action=r$action,reason_code=r$reason_code,
      sources=as.list(sort(unique(vapply(r$when,function(p) strsplit(p$field,".",fixed=TRUE)[[1]][1],character(1))),method="radix")),
      predicate_results=results,matched=matched,selected=FALSE)
  })
  matched <- which(vapply(trace,function(r) identical(r$matched,TRUE),logical(1)))
  selected <- if(length(matched)) matched[1] else NULL
  action <- if(length(findings)) s$missing_action else if(length(selected)) trace[[selected]]$action else s$default_action
  if(length(selected) && !length(findings)) trace[[selected]]$selected <- TRUE
  gates <- c(affordability="AFFORDABLE",eligibility="ELIGIBLE",operations="CLEAR");gate_trace<-list()
  if(s$schema_version=="1.1") {
    contract<-s$approval_contract
    if(contract$affordability$mode=="NOT_APPLICABLE")gates["affordability"]<-"NOT_APPLICABLE"
    checks<-c(lapply(names(gates),function(k)list(field=paste0(k,".status"),value=unname(gates[k]))),contract$required_gates)
    gate_trace<-lapply(checks,function(check){parts<-strsplit(check$field,".",fixed=TRUE)[[1]];actual<-inputs[[parts[1]]][[parts[2]]];list(field=check$field,required_value=check$value,actual_value=actual,passed=identical(actual,check$value))})
  }
  if(action=="APPROVE") {
    blocked <- names(gates)[!vapply(names(gates),function(k) identical(inputs[[k]]$status,unname(gates[k])),logical(1))]
    blocked<-if(length(blocked))paste0(blocked,".APPROVAL_GATE")else character()
    if(s$schema_version=="1.1" && length(gate_trace)>3L)for(r in gate_trace[-(1:3)])if(!r$passed)blocked<-c(blocked,paste0(r$field,".APPROVAL_GATE"))
    if(length(blocked)) { findings <- c(findings,blocked); action <- "REFER" }
  }
  result <- list(schema_version="1.0",kind="DecisionArtifact",policy_id=s$policy_id,
    policy_version=s$version,policy_hash=policy$sha256,as_of=as_of,known_at=known_at,action=action,
    local_recommendation=TRUE,input_hashes=lapply(inputs,.cr_hash),
    findings=as.list(sort(unique(findings),method="radix")),rule_trace=trace)
  if(s$schema_version=="1.1"){result$approval_gate_trace<-gate_trace;result$approval_domain<-s$approval_contract$domain}
  result$sha256 <- .cr_hash(result)
  .cr_result(result,"decisioning.apply_credit_decision_policy","decision",context,
    limitations="Local recommendation only; no loan, limit, price or payment is executed.")
}

decompose_policy_decision <- function(decision,reason_dictionary,context=NULL) {
  d <- .cr_dec_checked(decision,"DecisionArtifact")
  if(!identical(d$schema_version,"1.0") || length(d$action)!=1L ||
     !d$action %in% c("APPROVE","REFER","DECLINE","BLOCKED") ||
     !.cr_dec_array(d$rule_trace) || !.cr_dec_array(d$findings) ||
     !all(vapply(d$findings,function(x) is.character(x) && length(x)==1L && !is.na(x),logical(1))))
    .cr_error("INVALID_ARTIFACT","Invalid decision schema")
  scalar_text <- function(x) is.character(x) && length(x)==1L && !is.na(x)
  scalar_bool <- function(x) is.logical(x) && length(x)==1L && !is.na(x)
  for(row in d$rule_trace) {
    if(!.cr_object(row) || !scalar_text(row$rule_id) || !scalar_text(row$reason_code) ||
       !.cr_dec_array(row$sources) || !all(vapply(row$sources,function(x) scalar_text(x) && x %in% .cr_dec_sources,logical(1))) ||
       !scalar_bool(row$selected) || (!is.null(row$matched) && !scalar_bool(row$matched)) ||
       length(row$action)!=1L || !row$action %in% c("APPROVE","REFER","DECLINE","BLOCKED"))
      .cr_error("INVALID_ARTIFACT","Invalid recorded rule evaluation")
  }
  if(!.cr_object(reason_dictionary)) .cr_error("INVALID_INPUT","Reason dictionary must be an object")
  reasons <- list()
  for(row in d$rule_trace) if(identical(row$matched,TRUE)) {
    code <- row$reason_code; description <- reason_dictionary[[code]]
    if(!is.character(description) || length(description)!=1L || is.na(description) || !nzchar(description))
      .cr_error("MISSING_REASON_CODE","Missing reason description",list(reason_code=code))
    reasons[[length(reasons)+1L]] <- list(rule_id=row$rule_id,reason_code=code,
      description=description,sources=row$sources,selected=row$selected,rule_action=row$action)
  }
  .cr_result(list(decision_hash=d$sha256,action=d$action,findings=d$findings,reasons=reasons),
    "decisioning.decompose_policy_decision","explanation",context)
}
