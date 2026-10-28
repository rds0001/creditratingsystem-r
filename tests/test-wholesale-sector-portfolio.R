library(creditratingsystem)
source(system.file('examples/wholesale_sector_portfolio.R',package='creditratingsystem'))
r<-run_wholesale_sector_portfolio();portfolio<-r$portfolio_review
stopifnot(length(portfolio$prepared_portfolio$positions)==7)
for(i in seq_along(portfolio$forecast_sources)) {
 s<-portfolio$forecast_sources[[i]];exposure<-portfolio$exposures[[i]];loss<-r$event_losses[[i]];b<-s$observed_ead_bridge;p<-b$principal_at_origin;interest<-p*.05*61/365
 stopifnot(abs(b$observed_ead-(.9*p+interest))<1e-12,abs(exposure$before$schedule[[1]]$conditional_ead-exposure$after$schedule[[1]]$conditional_ead-.2*p)<1e-12,abs(loss$event_loss$expected_loss-s$forecast_pd*(exposure$after$schedule[[1]]$conditional_ead-s$conditional_loss_scenario$net_recovery/1.05^(123/365)))<1e-12)
}
stopifnot(portfolio$forecast_sources[[4]]$currency=='USD',portfolio$forecast_sources[[4]]$fx_rate==.9,portfolio$independent_oracle$ead_bias<0)
