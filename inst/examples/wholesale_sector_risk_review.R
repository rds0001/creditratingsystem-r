# Sector-specific treatment and open claims use shared native fitted-method engines.
run_wholesale_sector_risk_review <- function(data=NULL,verify_reference=TRUE,engine_paths=NULL) {
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/wholesale_sector_risk_review.json',package='creditratingsystem'),simplifyVector=FALSE)
 out<-list(data_origin='SYNTHETIC')
 for(key in c('review','open_workout')) {
  name<-if(key=='review')'specialized_risk_review'else'specialized_open_workout'
  path<-if(is.null(engine_paths))system.file('examples',paste0(name,'.R'),package='creditratingsystem')else engine_paths[[key]]
  engine<-new.env(parent=environment());sys.source(path,envir=engine)
  out[[key]]<-get(paste0('run_',name),envir=engine)(data[[key]],verify_reference=verify_reference)
 }
 out
}
