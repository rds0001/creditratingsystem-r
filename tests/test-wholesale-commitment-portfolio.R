library(creditratingsystem)
source(system.file('examples/wholesale_commitment_portfolio.R',package='creditratingsystem'))
v<-run_wholesale_commitment_portfolio()
for(i in seq_along(v$portfolios)){p<-v$portfolios[[i]];r<-p$portfolio_review;x<-r$exposures[[1]];stopifnot(abs(r$independent_oracle$hhi-5/9)<1e-12,abs(r$independent_oracle$ead_bias-c(-7.5,0)[i])<1e-12,x$before$schedule[[1]]$conditional_ead==c(0,132)[i],x$after$schedule[[1]]$conditional_ead==c(50,114)[i],length(p$event_losses)==2)}
