library(creditratingsystem)

policy_spec_dec <- function() list(
  schema_version="1.0",policy_id="retail-example",version="1",
  approval_reference="synthetic-policy-record",known_at="2024-01-01",
  effective_from="2024-01-01",effective_until="2025-01-01",
  default_action="REFER",missing_action="BLOCKED",
  target=list(event="default",horizon=1,time_unit="years",probability_measure="PHYSICAL",conditioning="PIT"),
  fields=list("risk.pd"=list(type="number"),
    "affordability.status"=list(type="category",values=list("AFFORDABLE","NOT_AFFORDABLE","INSUFFICIENT_EVIDENCE")),
    "eligibility.status"=list(type="category",values=list("ELIGIBLE","INELIGIBLE"))),
  rules=list(
    list(rule_id="eligibility",priority=0,action="DECLINE",reason_code="ELIGIBILITY",
      when=list(list(field="eligibility.status",op="eq",value="INELIGIBLE"))),
    list(rule_id="affordability",priority=1,action="DECLINE",reason_code="UNAFFORDABLE",
      when=list(list(field="affordability.status",op="eq",value="NOT_AFFORDABLE"))),
    list(rule_id="high-risk",priority=2,action="DECLINE",reason_code="RISK",
      when=list(list(field="risk.pd",op="gt",value=.1))),
    list(rule_id="low-risk",priority=3,action="APPROVE",reason_code="LOW_RISK",
      when=list(list(field="risk.pd",op="le",value=.02)))))
inputs_dec <- function() {
  timing <- list(as_of="2024-06-01",known_at="2024-06-02")
  list(risk=c(timing,list(pd=.01,target=policy_spec_dec()$target,freshness="CURRENT")),
       affordability=c(timing,list(status="AFFORDABLE")),eligibility=c(timing,list(status="ELIGIBLE")),
       operations=c(timing,list(status="CLEAR")),as_of="2024-06-02",known_at="2024-06-02")
}
apply_dec <- function(spec=policy_spec_dec(),data=inputs_dec())
  do.call(apply_credit_decision_policy,c(list(policy=compile_credit_decision_policy(spec)),data))$value
error_dec <- function(expr,code) {
  e <- tryCatch(force(expr),error=identity)
  stopifnot(inherits(e,"creditratingsystem_error"),identical(e$code,code))
}
next_dec <- getFromNamespace(".cr_dec_next","creditratingsystem")
hash_dec <- getFromNamespace(".cr_hash","creditratingsystem")
# Exact cutoff oracles: .02 included, .1 excluded from decline.
for(case in list(list(0,"APPROVE"),list(.02,"APPROVE"),list(next_dec(.02,1),"REFER"),
                 list(.1,"REFER"),list(next_dec(.1,1),"DECLINE"))) {
  d <- inputs_dec(); d$risk$pd <- case[[1]]
  stopifnot(apply_dec(data=d)$action==case[[2]])
}
for(source in c("affordability","operations")) {
  d <- inputs_dec(); d[[source]]$status <- if(source=="affordability") "INSUFFICIENT_EVIDENCE" else "PENDING"
  result <- apply_dec(data=d)
  stopifnot(result$action=="REFER",paste0(source,".APPROVAL_GATE") %in% unlist(result$findings))
}
for(change in c("missing","stale","future","target","expired")) {
  d <- inputs_dec()
  if(change=="missing") d$risk$pd <- NULL
  if(change=="stale") d$risk$freshness <- "STALE"
  if(change=="future") d$risk$known_at <- "2024-06-03"
  if(change=="target") d$risk$target$conditioning <- "TTC"
  if(change=="expired") {d$as_of <- "2025-01-01";d$known_at <- "2025-01-01"}
  result <- apply_dec(data=d)
  finding <- switch(change,missing="risk.pd.MISSING",stale="risk.NOT_CURRENT",future="risk.FUTURE_INPUT",
                    target="risk.TARGET_MISMATCH",expired="POLICY_NOT_EFFECTIVE")
  stopifnot(result$action=="BLOCKED",finding %in% unlist(result$findings))
}
d <- inputs_dec(); d$affordability$status <- "NOT_AFFORDABLE"; d$eligibility$status <- "INELIGIBLE"
result <- apply_dec(data=d)
stopifnot(result$action=="DECLINE",d$risk$pd==.01)
explained <- decompose_policy_decision(result,list(ELIGIBILITY="Outside eligibility",
  UNAFFORDABLE="Debt service exceeds capacity",LOW_RISK="Below risk cutoff"))$value
stopifnot(identical(explained$reasons[[1]]$sources,list("eligibility")),
          explained$reasons[[1]]$selected,!explained$reasons[[2]]$selected)
error_dec(decompose_policy_decision(result,list(ELIGIBILITY="Outside eligibility")),"MISSING_REASON_CODE")
s <- policy_spec_dec()
s$rules <- list(list(rule_id="a",priority=0,action="APPROVE",reason_code="A",when=list(list(field="risk.pd",op="le",value=.05))),
                list(rule_id="b",priority=0,action="DECLINE",reason_code="B",when=list(list(field="risk.pd",op="ge",value=.05))))
error_dec(compile_credit_decision_policy(s),"CONFLICTING_RULES")
s$rules[[2]]$when[[1]]$op <- "gt"
stopifnot(inherits(compile_credit_decision_policy(s),"CalcResult"))
s$rules[[2]]$when[[2]] <- list(field="risk.pd",op="lt",value=next_dec(.05,1))
error_dec(compile_credit_decision_policy(s),"UNREACHABLE_RULE")
s <- policy_spec_dec()
s$rules[[5]] <- list(rule_id="fallback",priority=4,action="REFER",reason_code="FALLBACK",when=list())
s$rules[[6]] <- list(rule_id="shadow",priority=5,action="REFER",reason_code="SHADOW",when=list())
findings <- compile_credit_decision_policy(s)$diagnostics$rule_findings
stopifnot(any(vapply(findings,function(x) identical(x,list(rule_id="shadow",finding="SHADOWED_RULE",by="fallback")),logical(1))))
p <- compile_credit_decision_policy(policy_spec_dec()); d <- inputs_dec(); before <- d
first <- do.call(apply_credit_decision_policy,c(list(policy=p),d))$value
stopifnot(identical(first,do.call(apply_credit_decision_policy,c(list(policy=p),d))$value),identical(d,before))
p$value$specification$rules[[1]]$action <- "APPROVE"
error_dec(do.call(apply_credit_decision_policy,c(list(policy=p),d)),"ARTIFACT_HASH_MISMATCH")
first$rule_trace <- "invalid"; first$sha256 <- NULL; first$sha256 <- hash_dec(first)
error_dec(decompose_policy_decision(first,structure(list(),names=character())),"INVALID_ARTIFACT")
d <- inputs_dec();d$risk$known_at <- "2024-05-31"
error_dec(apply_dec(data=d),"INVALID_DATE")
s <- policy_spec_dec();s$target$time_unit <- "bananas"
error_dec(compile_credit_decision_policy(s),"INVALID_POLICY")
cat("Native decision policy oracle, conflicts, gates, chronology and replay checks passed\n")
