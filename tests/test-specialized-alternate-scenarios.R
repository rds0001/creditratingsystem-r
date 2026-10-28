library(creditratingsystem)
source(system.file('examples/specialized_alternate_scenarios.R',package='creditratingsystem'))
data<-jsonlite::fromJSON(system.file('extdata/specialized_alternate_scenarios.json',package='creditratingsystem'),simplifyVector=FALSE)
extended<-tolower(Sys.getenv('CREDITRATINGSYSTEM_EXTENDED_TESTS'))=='true'
if(!extended){data$cases<-data$cases[1];sid<-data$cases[[1]]$case_id;data$entity_innovations<-Filter(function(x)startsWith(x$channel_id,paste0(sid,'-')),data$entity_innovations);data$parameter_snapshot$parameters<-data$parameter_snapshot$parameters[sid]}
v<-run_specialized_alternate_scenarios(data,scenario_methods=if(extended)NULL else'conditional',return_audit_artifacts=TRUE)
stopifnot(v$audits$local_fits$factor$diagnostics$missing_entries==1,v$audits$reverse_solver$feasible)
for(name in names(v$audits$executions)){
 audit<-v$audits$executions[[name]];report<-v$report$alternatives[[name]];stopifnot(content_fingerprint(audit$cube)$value==report$scenario_cube_hash)
 for(row in report$transactions){
  tx<-Filter(function(t)t$transaction_id==row$case_id,audit$execution$transactions)[[1]];stopifnot(content_fingerprint(tx)$value==row$transaction_hash,length(row$ledger_oracles)==18)
  changed<-tx;changed$risk$expected_loss<-changed$risk$expected_loss+1;stopifnot(content_fingerprint(changed)$value!=row$transaction_hash)
 }
}
