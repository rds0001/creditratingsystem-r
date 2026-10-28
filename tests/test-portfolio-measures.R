if(!exists('credit_loss_distribution'))library(creditratingsystem)
r<-credit_loss_distribution(c(0,100),c(.9,.1),c('a','b'),levels=c(0,.8,.95,1),currency='EUR')$value
stopifnot(r$expected_loss==10,max(abs(vapply(r$tails,function(t)t$value_at_risk,0.)-c(0,0,100,100)))<1e-12,max(abs(vapply(r$tails,function(t)t$expected_shortfall,0.)-c(10,50,100,100)))<1e-12,max(abs(unlist(r$tails[[2]]$normalized_tail_weights)-.5))<1e-14,abs(r$standard_deviation-30)<1e-12)
r<-allocate_expected_shortfall(matrix(c(-10,10,120,-20),ncol=2,byrow=TRUE),c(.9,.1),c('a','b'),c('loan','hedge'),.8,'EUR',groups=list(loan='book',hedge='book'))$value
stopifnot(max(abs(vapply(r$contributions,function(p)p$contribution,0.)-c(55,-5)))<1e-12,abs(r$expected_shortfall-50)<1e-12,abs(r$groups[[1]]$contribution-50)<1e-12,abs(r$reconciliation_residual)<1e-12)
r<-incremental_credit_risk(c(0,100),c(0,0),c(.9,.1),c('a','b'),levels=.8,currency='EUR',change_id='none')$value
stopifnot(r$expected_loss_change==0,r$tail_changes[[1]]$expected_shortfall_change==0)
r<-incremental_credit_risk(c(0,100),c(0,100),c(.9,.1),c('a','b'),levels=.8,currency='EUR',change_id='double')$value
stopifnot(r$expected_loss_change==10,abs(r$tail_changes[[1]]$expected_shortfall_change-50)<1e-12)
rows<-lapply(0:3,function(i)list(position_id=as.character(i),ead=1,group=if(i<2)'connected'else as.character(i)))
r<-credit_concentration_measures(rows,'ead','group','EUR')$value
stopifnot(r$dimensions[[1]]$hhi==.25,r$dimensions[[1]]$effective_number==4,r$dimensions[[2]]$hhi==.375,r$dimensions[[1]]$entropy_effective_number==4)
for(i in seq_along(rows))rows[[i]]$ead<-0
r<-credit_concentration_measures(rows,'ead','group','EUR')$value;stopifnot(r$status=='ZERO_WEIGHT_DENOMINATOR',is.null(r$dimensions[[1]]$hhi))
r<-asymptotic_factor_loss(.2,.4,0,c(-2,0,2))$value;stopifnot(max(abs(unlist(r$conditional_loss_rate)-.08))<1e-14)
r<-asymptotic_factor_loss(.5,.4,.5,c(-1,0,1))$value;stopifnot(max(abs(unlist(r$conditional_pd)-stats::pnorm(c(1,0,-1))))<1e-14,r$unconditional_expected_loss_rate==.2)
r<-credit_loss_distribution(c(-100,0,100),c(0,1,0),c('a','b','c'),levels=c(0,.95,1),currency='EUR')$value
stopifnot(all(vapply(r$tails,function(t)t$expected_shortfall==0&&t$value_at_risk==0,logical(1))))
stopifnot(inherits(try(credit_loss_distribution(1,0,'a',currency='EUR'),silent=TRUE),'try-error'))
