if(!exists('simulate_recovery_waterfall'))library(creditratingsystem)
srw_fixture<-function(){
 e<-list(list(scenario_id='s',default_event_id='d',subject_id='o',default_date='2024-01-01'))
 c<-lapply(1:2,function(i)list(scenario_id='s',default_event_id='d',subject_id='o',claim_id=c('a','b')[i],pool_id='p',currency='EUR',ead_at_default=c(60,40)[i],legal_balance=c(60,40)[i],priority=i,deficit_cost_share=c(.6,.4)[i]))
 p<-list(list(scenario_id='s',default_event_id='d',pool_id='p',currency='EUR',realization_id='r',collateral_id='asset',date='2025-01-01',gross_value=100,realization_fraction=.8,cost=10,discount_factor=.9,discount_anchor_date='2024-01-01'))
 policy<-list(policy_id='p',version='1',currency='EUR',allocation='LEGAL_CAP',cost_rule='PROCEEDS_FIRST_EXPLICIT_DEFICIT_SHARES',completion='ULTIMATE_COMPLETE')
 list(default_events=e,claims=c,realization_paths=p,recovery_policy=policy)
}
x<-srw_fixture();r<-do.call(simulate_recovery_waterfall,x)$value;a<-r$claim_results[[1]];b<-r$claim_results[[2]];p<-r$pool_results[[1]]
stopifnot(a$gross_recovery==60,b$gross_recovery==10,abs(a$economic_loss-6)<1e-12,abs(b$economic_loss-31)<1e-12,abs(a$lgd-.1)<1e-14,abs(b$lgd-.775)<1e-14,p$realized_proceeds+p$creditor_deficit_funding==p$costs+p$creditor_gross_recovery+p$unallocated_surplus)
x<-srw_fixture();x$realization_paths[[1]]$gross_value<-0;x$realization_paths[[1]]$cost<-20;r<-do.call(simulate_recovery_waterfall,x)$value
stopifnot(max(abs(vapply(r$claim_results,function(c)c$economic_loss,0.)-c(70.8,47.2)))<1e-12,max(abs(vapply(r$claim_results,function(c)c$lgd,0.)-1.18))<1e-14,r$pool_results[[1]]$creditor_deficit_funding==20)
x<-srw_fixture();x$claims[[2]]$priority<-1;p<-x$realization_paths[[1]];p$realization_id<-'r2';p$collateral_id<-'asset2';p$date<-'2026-01-01';p$gross_value<-100;p$realization_fraction<-1;p$cost<-0;p$discount_factor<-.8;x$realization_paths[[2]]<-p;r<-do.call(simulate_recovery_waterfall,x)$value
stopifnot(max(abs(vapply(r$allocation_ledger,function(c)c$gross_allocation,0.)-c(42,28,18,12)))<1e-12,abs(r$pool_results[[1]]$unallocated_surplus-70)<1e-12)
x<-srw_fixture();x$recovery_policy$allocation<-'UNCAPPED_PRO_RATA';x$claims[[2]]$priority<-1;x$claims[[1]]$ead_at_default<-0;x$realization_paths[[1]]$gross_value<-200;x$realization_paths[[1]]$realization_fraction<-1;x$realization_paths[[1]]$cost<-0;x$realization_paths[[1]]$discount_factor<-1;r<-do.call(simulate_recovery_waterfall,x)$value
stopifnot(is.null(r$claim_results[[1]]$lgd),r$claim_results[[2]]$lgd== -1)
x<-srw_fixture();x$realization_paths[[2]]<-x$realization_paths[[1]];x$realization_paths[[2]]$realization_id<-'r2';stopifnot(inherits(try(do.call(simulate_recovery_waterfall,x),silent=TRUE),'try-error'))
x<-srw_fixture();x$recovery_policy$completion<-'OPEN';stopifnot(inherits(try(do.call(simulate_recovery_waterfall,x),silent=TRUE),'try-error'))
