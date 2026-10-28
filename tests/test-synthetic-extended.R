if (identical(tolower(Sys.getenv("CREDITRATINGSYSTEM_EXTENDED_TESTS")), "true")) {
library(creditratingsystem)
syn_root <- system.file("extdata","syn_t",package="creditratingsystem")
if(nzchar(Sys.getenv("CREDITRATING_SYNTHETIC_REFERENCE"))) syn_root <- Sys.getenv("CREDITRATING_SYNTHETIC_REFERENCE")
stopifnot(nzchar(syn_root),dir.exists(syn_root))
syn_rows <- function(world,layer,table) {
 path<-file.path(world,layer,paste0(table,".jsonl.gz"));if(!file.exists(path)) return(list())
 con<-gzfile(path,"rt");on.exit(close(con));lapply(readLines(con,warn=FALSE),jsonlite::fromJSON,simplifyVector=FALSE)
}
syn_error <- function(expr,code) {
 e<-tryCatch(force(expr),error=identity);stopifnot(inherits(e,"creditratingsystem_error"),identical(e$code,code))
}
syn_spec <- jsonlite::fromJSON(file.path(syn_root,"macro/manifest.json"),simplifyVector=FALSE)$specification
for(name in c("macro","obligors","workouts","retail","pools","events")) {
 audit<-validate_synthetic_world(file.path(syn_root,name));stopifnot(audit$provenance$data_origin=="SYNTHETIC");report<-audit$value
 stopifnot(report$passed,report$rows_checked>0)
}
# Independent financial ledger identities and one-sided exact binomial inversion.
for(r in syn_rows(file.path(syn_root,"obligors"),"observable_inputs","financial_statements")) {
 stopifnot(abs(r$assets-r$equity-r$debt)<1e-7*max(1,r$assets),abs(r$cfads-r$ebitda+r$tax+r$capex)<1e-7*max(1,abs(r$cfads)))
}
cases<-generate_challenge_cases()$value$cases
stopifnot(generate_challenge_cases()$provenance$data_origin=="SYNTHETIC")
names(cases)<-vapply(cases,function(x) x$case_id,character(1))
bound<-cases$zero_default$expected$exact_one_sided_upper_pd
stopifnot(abs((1-bound)^100-.05)<1e-14,cases$cohort_100$expected$denominator==100,is.null(cases$censored_target$expected$binary_target))
syn_error(generate_challenge_cases(list("unknown")),"SYNTHETIC_CASE")
retail<-file.path(syn_root,"retail");panel<-syn_rows(retail,"observable_inputs","account_months")
stopifnot(any(vapply(panel,function(r) r$active_zero_balance,logical(1))))
previous<-new.env(parent=emptyenv())
for(r in panel) {
 stopifnot(abs(r$opening_balance+r$principal_draw+r$interest-r$payment-r$recovery_amount-r$balance)<1e-7)
 if(exists(r$account_id,previous,inherits=FALSE)) stopifnot(abs(get(r$account_id,previous)-r$opening_balance)<1e-7)
 assign(r$account_id,r$balance,previous)
 if(r$months_on_book==0) stopifnot(r$payment==0,r$interest==0,r$principal_draw==0)
 if(r$days_past_due>0&&r$state %in% c("ACTIVE","CURED")) stopifnot(r$payment==0)
}
events<-syn_rows(retail,"observed_outcomes","account_events")
default_ids<-vapply(Filter(function(r) r$event_type=="DEFAULT",events),function(r) r$account_id,character(1))
for(r in syn_rows(retail,"observed_outcomes","application_labels")) {
 if(!r$funded) stopifnot(is.null(r$own_loan_default),is.null(r$account_id))
 else stopifnot(r$own_loan_default==as.numeric(r$account_id %in% default_ids))
}
pool<-file.path(syn_root,"pools");members<-syn_rows(pool,"observable_inputs","pool_memberships")
stopifnot(length(members)==80L,length(unique(vapply(members,function(r) r$source_asset_id,character(1))))==80L,
 setequal(vapply(members,function(r) r$asset_type,character(1)),c("accounts","facilities")))
payment_index<-new.env(parent=emptyenv())
for(r in panel) assign(paste(r$account_id,r$observed_at),r$payment,payment_index)
for(r in syn_rows(pool,"observed_outcomes","servicing_cashflows")) if(r$component=="payment") stopifnot(r$amount==get(paste(r$source_asset_id,r$event_at),payment_index))
checkpoints<-syn_rows(file.path(syn_root,"events"),"latent_truth","independent_checkpoints")[1:4]
stopifnot(identical(vapply(checkpoints,function(r) r$expected_score,numeric(1)),c(110,105,105,105)),
 identical(vapply(checkpoints,function(r) r$expected_support_capacity,numeric(1)),c(100,100,0,0)),
 identical(vapply(checkpoints,function(r) r$expected_model_version,character(1)),c("v1","v1","v1","v2")))
# A caller-selected directory is required: tests use the configured test temp root.
syn_output<-tempfile("synthetic-native-",tmpdir=tempdir());dir.create(syn_output)
syn_draws<-file.path(syn_root,"innovations");worlds<-list()
worlds$macro<-generate_macro_reference_world(syn_spec,syn_draws,file.path(syn_output,"macro"),chunk_rows=7L)
worlds$obligors<-generate_obligor_panel(worlds$macro,syn_spec,syn_draws,file.path(syn_output,"obligors"),chunk_rows=7L)
worlds$workouts<-generate_facility_workouts(worlds$obligors,syn_spec,syn_draws,file.path(syn_output,"workouts"),chunk_rows=7L)
worlds$retail<-generate_retail_reference_world(worlds$macro,syn_spec,syn_draws,file.path(syn_output,"retail"),chunk_rows=7L)
worlds$pools<-generate_asset_pool_reference_world(list(worlds$retail,worlds$workouts),syn_spec,syn_draws,file.path(syn_output,"pools"),chunk_rows=7L)
worlds$events<-generate_system_event_reference_world(list(worlds$obligors,worlds$retail,worlds$workouts,worlds$pools),syn_spec,syn_draws,file.path(syn_output,"events"),chunk_rows=7L)
for(name in names(worlds)) {
 stopifnot(validate_synthetic_world(worlds[[name]])$value$passed)
 for(key in names(worlds[[name]]$value$manifest$tables)) {
  parts<-strsplit(key,"/",fixed=TRUE)[[1L]]
  actual<-syn_rows(file.path(syn_output,name),parts[1L],parts[2L]);expected<-syn_rows(file.path(syn_root,name),parts[1L],parts[2L])
  # Independently evaluated transcendental functions may differ by one ulp;
  # source-content hashes then correctly differ, although numeric DGP parity holds.
  if(name=="events") {
   actual<-lapply(actual,function(r) {r$source_snapshot_hash<-NULL;r$snapshot_hash<-NULL;r})
   expected<-lapply(expected,function(r) {r$source_snapshot_hash<-NULL;r$snapshot_hash<-NULL;r})
  }
  stopifnot(isTRUE(all.equal(actual,expected,tolerance=1e-12,check.attributes=TRUE)))
 }
}
same_source_events<-generate_system_event_reference_world(lapply(c("obligors","retail","workouts","pools"),function(x) file.path(syn_root,x)),syn_spec,syn_draws,file.path(syn_output,"same_sources"))
for(table in c("subjects","system_events")) {
 layer<-if(table=="subjects") "observable_inputs" else "observed_outcomes"
 stopifnot(isTRUE(all.equal(syn_rows(file.path(syn_output,"same_sources"),layer,table),syn_rows(file.path(syn_root,"events"),layer,table),tolerance=1e-12)))
}
syn_error(generate_macro_reference_world(syn_spec,syn_draws,file.path(syn_output,"budget"),memory_budget_mb=32L),"SYNTHETIC_BUDGET")
stopifnot(!dir.exists(file.path(syn_output,"budget")))
bad_spec<-syn_spec;bad_spec$start_date<-"1999-01-01"
syn_error(generate_obligor_panel(worlds$macro,bad_spec,syn_draws,file.path(syn_output,"mismatch")),"SYNTHETIC_LINK")
for(kind in c("table_traversal","cube_traversal","table_symlink","cube_symlink")) {
 target<-file.path(syn_output,kind);dir.create(target)
 invisible(file.copy(list.files(file.path(syn_root,"macro"),full.names=TRUE),target,recursive=TRUE))
 manifest<-jsonlite::fromJSON(file.path(target,"manifest.json"),simplifyVector=FALSE)
 outside<-file.path(syn_output,"outside");writeLines("not a resource",outside)
 relative<-"../outside"
 if(endsWith(kind,"symlink")) {if(!file.symlink(outside,file.path(target,"escape"))) next;relative<-"escape"}
 if(startsWith(kind,"table")) manifest$tables[["observable_inputs/macro_releases"]]$file<-relative
 else manifest$binary_resources$scenario_cube$file<-relative
 writeLines(jsonlite::toJSON(manifest,auto_unbox=TRUE,null="null",digits=17),file.path(target,"manifest.json"))
 syn_error(validate_synthetic_world(target),"SYNTHETIC_WORLD")
}

}
