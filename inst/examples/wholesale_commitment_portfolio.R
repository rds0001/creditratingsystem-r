# Separate contractual-horizon portfolios with exposure timing and observed loss checks.
run_wholesale_commitment_portfolio <- function(data=NULL,verify_reference=TRUE,engine_path=NULL,portfolio_engine_path=NULL){
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/wholesale_commitment_portfolio.json',package='creditratingsystem'),simplifyVector=FALSE)
 if(is.null(engine_path))engine_path<-system.file('examples/wholesale_sector_portfolio.R',package='creditratingsystem')
 e<-new.env(parent=environment());sys.source(engine_path,envir=e)
 list(data_origin='SYNTHETIC',scope=data$scope,portfolios=lapply(data$portfolios,function(p)e$run_wholesale_sector_portfolio(p,verify_reference=verify_reference,engine_path=portfolio_engine_path)))
}
