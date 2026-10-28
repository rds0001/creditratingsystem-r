library(creditratingsystem)
root<-system.file("extdata","syn_t",package="creditratingsystem")
if(nzchar(Sys.getenv("CREDITRATING_SYNTHETIC_REFERENCE")))root<-Sys.getenv("CREDITRATING_SYNTHETIC_REFERENCE")
s<-jsonlite::fromJSON(file.path(root,"macro/manifest.json"),simplifyVector=FALSE)$specification
s$observation_cutoff<-"2000-04-01"
out<-tempfile("fast-synthetic-",tmpdir=tempdir());dir.create(out)
draws<-file.path(root,"innovations");w<-list()
t<-proc.time()
w$macro<-generate_macro_reference_world(s,draws,file.path(out,"macro"))
w$obligors<-generate_obligor_panel(w$macro,s,draws,file.path(out,"obligors"))
w$workouts<-generate_facility_workouts(w$obligors,s,draws,file.path(out,"workouts"))
w$retail<-generate_retail_reference_world(w$macro,s,draws,file.path(out,"retail"))
w$pools<-generate_asset_pool_reference_world(list(w$retail,w$workouts),s,draws,file.path(out,"pools"))
w$events<-generate_system_event_reference_world(list(w$obligors,w$retail,w$workouts,w$pools),s,draws,file.path(out,"events"))
for(n in names(w)){a<-validate_synthetic_world(w[[n]])$value;stopifnot(a$passed);cat(n,a$rows_checked,"\n")}
print(proc.time()-t)
cases<-generate_challenge_cases()$value$cases
names(cases)<-vapply(cases,function(x)x$case_id,character(1))
stopifnot(abs((1-cases$zero_default$expected$exact_one_sided_upper_pd)^100-.05)<1e-14)
con<-gzfile(file.path(out,"retail","observable_inputs","account_months.jsonl.gz"),"rt")
rows<-lapply(readLines(con),jsonlite::fromJSON,simplifyVector=FALSE);close(con)
stopifnot(length(rows)>0)
for(row in rows)stopifnot(abs(row$opening_balance+row$principal_draw+row$interest-row$payment-row$recovery_amount-row$balance)<1e-7)
# Verify resource containment even when an attacker replaces a manifest path.
manifest<-jsonlite::fromJSON(file.path(out,"macro","manifest.json"),simplifyVector=FALSE)
manifest$binary_resources$scenario_cube$file<-"../outside"
writeLines(jsonlite::toJSON(manifest,auto_unbox=TRUE,null="null",digits=17),file.path(out,"macro","manifest.json"))
e<-tryCatch(validate_synthetic_world(file.path(out,"macro")),error=identity)
stopifnot(inherits(e,"creditratingsystem_error"),e$code=="SYNTHETIC_WORLD")

# Resource paths must work with native Windows separators while preserving
# the canonical directory boundary (including drive and UNC roots).
local({
 resource <- getFromNamespace(".cr_syn_resource", "creditratingsystem")
 fixture <- file.path(root, "macro")
 stopifnot(file.exists(resource(fixture, "manifest.json")),
           file.exists(resource(paste0(fixture, "/"), "manifest.json")))
 for (base in c("C:/reference/world", "//server/share/world", "C:/", "/")) {
  for (outside in c(FALSE, TRUE)) {
   sandbox <- new.env(parent = environment(resource))
   sandbox$normalizePath <- function(path, winslash = "\\", mustWork = NA) {
    target <- if (identical(path, fixture)) base else
     if (outside) if (base == "/") "/sibling/manifest.json" else
      paste0(sub("/+$", "", base), "-sibling/manifest.json") else
      paste0(sub("/+$", "", base), "/manifest.json")
    if (winslash == "\\") chartr("/", "\\", target) else target
   }
   probe <- resource
   environment(probe) <- sandbox
   result <- tryCatch(probe(fixture, "manifest.json"), error = identity)
   # Every absolute path is contained in the filesystem root.
   if (outside && base != "/") {
    stopifnot(inherits(result, "creditratingsystem_error"),
              result$code == "SYNTHETIC_WORLD")
   } else stopifnot(identical(result, file.path(fixture, "manifest.json")))
  }
 }
 for (relative in c("../manifest.json", "/manifest.json", "C:/manifest.json", "..\\manifest.json")) {
  result <- tryCatch(resource(fixture, relative), error = identity)
  stopifnot(inherits(result, "creditratingsystem_error"), result$code == "SYNTHETIC_WORLD")
 }
})
unlink(out, recursive = TRUE)
