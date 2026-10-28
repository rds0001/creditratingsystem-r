if(!exists('reconcile_card_authorizations')) library(creditratingsystem)
auth_input <- function(amount=200) list(list(authorization_id='a',account_id='card',approved_at='2025-01-01',known_at='2025-01-01',sequence=0,currency='EUR',amount=amount,expires_at='2025-01-10'))
auth_policy <- list(policy_id='p',version='1',known_at='2024-12-01',expiry_boundary='EXCLUSIVE',late_settlement_allowed=FALSE,authorization_conversion_factor=1,minor_unit=.01)
auth_event <- function(id,amount,kind='SETTLEMENT',sequence=1,day='2025-01-02',complete=FALSE,reference=NULL) {
 row <- list(event_id=id,authorization_id='a',account_id='card',occurred_at=day,known_at=day,sequence=sequence,currency='EUR',amount=amount,kind=kind)
 if(kind=='SETTLEMENT') row$authorization_complete <- complete
 if(kind=='REVERSAL') row$reverses_settlement_id <- reference
 row
}
auth_calc <- function(events=list(),amount=200,as_of='2025-01-05',policy=auth_policy) reconcile_card_authorizations(auth_input(amount),events,policy,as_of,as_of,'EUR')$value
auth_error <- function(events,...) {
 caught <- tryCatch({auth_calc(events,...);FALSE},creditratingsystem_error=function(e)TRUE)
 stopifnot(caught)
}
stopifnot(auth_calc()$accounts[[1]]$scenario_exposure==200)
x <- auth_calc(list(auth_event('s',200,complete=TRUE)))$accounts[[1]]
stopifnot(x$booked_exposure==200,x$pending_authorization==0,x$scenario_exposure==200)
x <- auth_calc(list(auth_event('s',80)))$accounts[[1]]
stopifnot(x$booked_exposure==80,x$pending_authorization==120,x$scenario_exposure==200)
stopifnot(auth_calc(list(auth_event('s',80,complete=TRUE)))$accounts[[1]]$scenario_exposure==80)
stopifnot(auth_calc(as_of='2025-01-10')$accounts[[1]]$scenario_exposure==0)
stopifnot(auth_calc(list(auth_event('s',80),auth_event('c',0,'CANCELLATION',2)))$accounts[[1]]$scenario_exposure==80)
x <- auth_calc(list(auth_event('s',80),auth_event('r',40,'REVERSAL',3,reference='s')))$accounts[[1]]
stopifnot(x$booked_exposure==40,x$pending_authorization==120,x$scenario_exposure==160)
x <- auth_calc(list(auth_event('s',.3,complete=TRUE),auth_event('r1',.1,'REVERSAL',2,reference='s'),auth_event('r2',.2,'REVERSAL',3,reference='s')),amount=.3)
stopifnot(x$accounts[[1]]$booked_exposure==0)
p <- auth_policy;p$authorization_conversion_factor <- .25
stopifnot(auth_calc(list(auth_event('s',80)),policy=p)$accounts[[1]]$scenario_exposure==110)
s <- auth_event('s',200,complete=TRUE);s$known_at <- '2025-01-07'
x <- auth_calc(list(s));stopifnot(x$accounts[[1]]$pending_authorization==200,x$excluded[[1]]$reason=='NOT_YET_KNOWN')
stopifnot(auth_calc(list(s),as_of='2025-01-07')$accounts[[1]]$pending_authorization==0)
s <- auth_event('s',200,day='2025-01-10');auth_error(list(s),as_of='2025-01-10');p<-auth_policy;p$late_settlement_allowed<-TRUE
stopifnot(auth_calc(list(s),as_of='2025-01-10',policy=p)$accounts[[1]]$booked_exposure==200)
for(events in list(list(auth_event('s',201)),list(auth_event('s',200),auth_event('s',0,sequence=2)),list(auth_event('s',100),auth_event('x',100)),list(auth_event('r',1,'REVERSAL',reference='absent')),list(auth_event('s',80),auth_event('r',81,'REVERSAL',2,reference='s')),list(auth_event('s',.001)))) auth_error(events)
