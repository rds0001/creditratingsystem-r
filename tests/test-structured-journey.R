library(creditratingsystem)
source(system.file('examples/structured_journey.R',package='creditratingsystem'))
out<-run_structured_journey()
stopifnot(out$full_component_reexecution_exact,out$lineage$audit$node_totals$joined$amounts$balance==280)
expected<-list(SECURITIZATION_TRANCHE=55,COVERED_BOND=32.5,TRADE_RECEIVABLES=30)
for(product in names(expected)){
 row<-out$products[[product]]
 stopifnot(row$workout$closed_workout$economic_loss==expected[[product]],row$position_binding$position_id==row$source_row$entity_id,row$pool_validation$asset_pool_validation$decision$status=='NOT_ASSESSED',out$replay_audits[[product]]$decision$status=='PASS')
}
stopifnot(out$products$COVERED_BOND$claim_control$aggregate_recovery==67.5,out$products$TRADE_RECEIVABLES$claim_control$seller_recourse==0)
data<-jsonlite::fromJSON(system.file('extdata/structured_journey.json',package='creditratingsystem'),simplifyVector=FALSE)
data$pools$cases$COVERED_BOND$position_binding$actual_position_recovery<-100
stopifnot(inherits(tryCatch(run_structured_journey(data),error=function(e)e),'error'))
