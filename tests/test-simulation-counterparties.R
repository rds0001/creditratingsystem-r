if(!exists('simulate_contract_counterparties'))library(creditratingsystem)
cp_fixture<-function(){
 dates<-c('2021-01-01','2021-02-01','2021-03-01','2021-04-01');paths<-lapply(1:3,function(i)list(scenario_id='s',counterparty_id='tenant',period_start=dates[i],period_end=dates[i+1],conditional_pd=c(0,1,1)[i],uniform=0,probability_measure='PHYSICAL'))
 base<-list(contract_id='a',counterparty_id='tenant',currency='EUR',default_payment_timing='BEFORE_DUE',cure_probability=0,cure_delay_days=2,replacement_enabled=TRUE,replacement_delay_days=10,replacement_payment_fraction=.4,cashflows=lapply(dates,function(d)list(date=d,amount=100)))
 second<-base;second$contract_id<-'b';second$cure_probability<-1;second$cure_delay_days<-5;second$replacement_enabled<-FALSE;second$replacement_delay_days<-0;second$replacement_payment_fraction<-0
 third<-base;third$contract_id<-'c';third$replacement_enabled<-FALSE;third$replacement_delay_days<-0;third$replacement_payment_fraction<-0
 contracts<-list(base,second,third);draws<-lapply(contracts,function(contract)list(scenario_id='s',contract_id=contract$contract_id,uniform=.5))
 list(counterparty_paths=paths,contracts=contracts,cure_uniforms=draws,currency='EUR')
}
r<-do.call(simulate_contract_counterparties,cp_fixture())$value
stopifnot(length(r$counterparty_events)==1L,r$counterparty_events[[1]]$first_default_date=='2021-03-01',identical(vapply(r$cashflows,function(row)row$received_cash,numeric(1)),c(100,100,0,40,100,100,0,100,100,100,0,0)),r$contract_events[[1]]$replacement_date=='2021-03-13',r$contract_events[[2]]$cure_date=='2021-03-06',sum(vapply(r$cashflows,function(row)row$cash_change,numeric(1)))== -460)
f<-cp_fixture();for(key in c('counterparty_paths','contracts','cure_uniforms'))f[[key]]<-rev(f[[key]]);stopifnot(identical(do.call(simulate_contract_counterparties,f)$value,r))
f<-cp_fixture();for(i in seq_along(f$counterparty_paths))f$counterparty_paths[[i]]$conditional_pd<-0;r<-do.call(simulate_contract_counterparties,f)$value;stopifnot(!r$counterparty_events[[1]]$defaulted,all(vapply(r$cashflows,function(row)row$received_cash,numeric(1))==100))
cp_error<-function(code,expr){e<-tryCatch(force(expr),error=identity);stopifnot(inherits(e,'creditratingsystem_error'),e$code==code)}
f<-cp_fixture();f$contracts[[1]]$cashflows[[1]]$date<-'2021-01-15';cp_error('SIMULATION_CALENDAR',do.call(simulate_contract_counterparties,f))
f<-cp_fixture();f$counterparty_paths[[1]]$uniform<-1;cp_error('SIMULATION_PROBABILITY',do.call(simulate_contract_counterparties,f))
f<-cp_fixture();f$cure_uniforms<-f$cure_uniforms[1:2];cp_error('SIMULATION_COUNTERPARTY',do.call(simulate_contract_counterparties,f))
cat('Native shared counterparty cashflow tests PASS\n')

f<-cp_fixture();f$context<-build_calculation_context(currency='EUR',probability_measure='PHYSICAL');stopifnot(do.call(simulate_contract_counterparties,f)$value$counterparty_events[[1]]$defaulted)
f$context<-build_calculation_context(currency='EUR',probability_measure='RISK_NEUTRAL');cp_error('TARGET_MISMATCH',do.call(simulate_contract_counterparties,f))
