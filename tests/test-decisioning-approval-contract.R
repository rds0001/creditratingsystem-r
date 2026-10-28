if(!exists('compile_credit_decision_policy'))library(creditratingsystem)
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
specification_dec11<-function(domain='INVESTMENT'){
 s<-policy_spec_dec();s$schema_version<-'1.1';s$policy_id<-'investment';s$fields$affordability.status<-NULL;s$rules<-Filter(function(r)r$rule_id!='affordability',s$rules);s$fields$operations.legal_status<-list(type='category',values=list('VERIFIED','UNVERIFIED'));s$fields$operations.limit_status<-list(type='category',values=list('WITHIN_LIMIT','EXCEEDED'));s$approval_contract<-list(domain=domain,affordability=list(mode='NOT_APPLICABLE',reason='Investment/CSA exposure; household budget is not the target',reference='scope-v1',maximum_age_days=30),required_gates=list(list(field='operations.legal_status',value='VERIFIED'),list(field='operations.limit_status',value='WITHIN_LIMIT')));s
}
data_dec11<-function(domain='INVESTMENT'){
 d<-inputs_dec();d$affordability$status<-'NOT_APPLICABLE';d$affordability$domain<-domain;d$affordability$applicability_reference<-'scope-v1';d$affordability$valid_until<-'2024-07-01';d$operations$legal_status<-'VERIFIED';d$operations$limit_status<-'WITHIN_LIMIT';d
}
run_dec11<-function(s=specification_dec11(),d=data_dec11())apply_dec(s,d)
p<-compile_credit_decision_policy(policy_spec_dec())$value;r<-apply_dec()
stopifnot(p$sha256=='8e6c728e84e847fc1579e7d6720cd1f7748bc927be5bb81334f5200863ccf092',r$sha256=='250aa1df53b889b608f9a6d739061b6216167f4d54a4b66d8e9548af70c9fdbd')
s<-policy_spec_dec();s['approval_contract']<-list(NULL);error_dec(compile_credit_decision_policy(s),'INVALID_POLICY')
for(domain in c('INVESTMENT','COUNTERPARTY_LIMIT')){
 r<-run_dec11(specification_dec11(domain),data_dec11(domain));stopifnot(r$action=='APPROVE',all(vapply(r$approval_gate_trace,function(x)x$passed,logical(1))),r$approval_domain==domain,length(r$approval_gate_trace)==5)
 d<-data_dec11(domain);d$operations$limit_status<-'EXCEEDED';r<-run_dec11(specification_dec11(domain),d);stopifnot(r$action=='REFER','operations.limit_status.APPROVAL_GATE'%in%unlist(r$findings))
}
for(case in list(list('eligibility','status','INELIGIBLE','DECLINE'),list('operations','status','PENDING','REFER'),list('operations','legal_status','UNVERIFIED','REFER'),list('affordability','status','AFFORDABLE','REFER'),list('risk','freshness','STALE','BLOCKED'),list('affordability','known_at','2024-06-03','BLOCKED'),list('affordability','applicability_reference','other','BLOCKED'),list('affordability','domain','RETAIL_CREDIT','BLOCKED'),list('affordability','valid_until','2024-06-02','BLOCKED'),list('affordability','as_of','2024-05-02','BLOCKED'))){
 d<-data_dec11();d[[case[[1]]]][[case[[2]]]]<-case[[3]];stopifnot(run_dec11(d=d)$action==case[[4]])
 if(case[[1]]%in%c('eligibility','operations')){s<-specification_dec11();s$rules<-list(list(rule_id='bypass',priority=0,action='APPROVE',reason_code='TEST',when=list()));stopifnot(run_dec11(s,d)$action=='REFER')}
}
d<-data_dec11();d$affordability$as_of<-'2024-05-03';stopifnot(run_dec11(d=d)$action=='APPROVE');d$affordability$valid_until<-NULL;stopifnot('affordability.MISSING_VALIDITY'%in%unlist(run_dec11(d=d)$findings));d<-data_dec11();d$affordability$applicability_reference<-NULL;stopifnot('affordability.APPLICABILITY_BINDING'%in%unlist(run_dec11(d=d)$findings));d<-data_dec11();d$operations$legal_status<-NULL;stopifnot(run_dec11(d=d)$action=='BLOCKED')
for(change in c('RETAIL_CREDIT','WHOLESALE_CREDIT','negative_age','empty_reason','duplicate','numeric','unknown_value','extra')){
 s<-specification_dec11()
 if(change%in%c('RETAIL_CREDIT','WHOLESALE_CREDIT'))s$approval_contract$domain<-change
 if(change=='negative_age')s$approval_contract$affordability$maximum_age_days<--1
 if(change=='empty_reason')s$approval_contract$affordability$reason<-''
 if(change=='duplicate')s$approval_contract$required_gates<-c(s$approval_contract$required_gates,list(s$approval_contract$required_gates[[1]]))
 if(change=='numeric')s$approval_contract$required_gates[[1]]$field<-'risk.pd'
 if(change=='unknown_value')s$approval_contract$required_gates[[1]]$value<-'UNKNOWN'
 if(change=='extra')s$approval_contract$unknown<-'ignored'
 error_dec(compile_credit_decision_policy(s),'INVALID_POLICY')
}
s<-specification_dec11('RETAIL_CREDIT');s$approval_contract$affordability<-list(mode='REQUIRED');d<-data_dec11('RETAIL_CREDIT');stopifnot(run_dec11(s,d)$action=='REFER');d$affordability$status<-'AFFORDABLE';stopifnot(run_dec11(s,d)$action=='APPROVE');p<-compile_credit_decision_policy(s)$value;p$specification$approval_contract$required_gates<-list();error_dec(do.call(apply_credit_decision_policy,c(list(policy=p),d)),'ARTIFACT_HASH_MISMATCH')
