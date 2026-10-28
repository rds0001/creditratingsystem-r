# Sector exposure timing, closed-loss backtests and explicit event losses.
run_wholesale_sector_portfolio <- function(data=NULL,verify_reference=TRUE,engine_path=NULL) {
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/wholesale_sector_portfolio.json',package='creditratingsystem'),simplifyVector=FALSE)
 if(is.null(engine_path))engine_path<-system.file('examples/specialized_portfolio_review.R',package='creditratingsystem')
 engine<-new.env(parent=environment());sys.source(engine_path,envir=engine);portfolio<-engine$run_specialized_portfolio_review(data,verify_reference=verify_reference)
 losses<-lapply(data$event_losses,function(case){loss<-do.call(event_conditioned_loss,case$arguments)$value;if(verify_reference)stopifnot(abs(loss$expected_loss-case$reference_expected_loss)<1e-12);list(case_id=case$case_id,event_loss=loss)})
 list(data_origin='SYNTHETIC',portfolio_review=portfolio,event_losses=losses)
}
