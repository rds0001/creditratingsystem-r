# Contract-dated revolving and leveraged histories, using the pinned local protocol.
run_wholesale_commitment_governance <- function(data=NULL,verify_reference=TRUE,artifact_paths=NULL,return_audit_artifacts=FALSE,engine_path=NULL) {
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/wholesale_commitment_governance.json',package='creditratingsystem'),simplifyVector=FALSE)
 if(is.null(engine_path))engine_path<-system.file('examples/wholesale_sector_governance.R',package='creditratingsystem')
 e<-new.env(parent=environment());sys.source(engine_path,envir=e)
 e$run_wholesale_sector_governance(data,verify_reference=verify_reference,artifact_paths=artifact_paths,return_audit_artifacts=return_audit_artifacts)
}
