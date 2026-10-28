if(!exists('build_debt_schedule'))library(creditratingsystem)
sc_terms<-list(facility_id='f',currency='EUR',principal=100,rate_type='fixed',annual_rate=.1,rate_convention='nominal_simple',day_count='ACT/365F',amortization='bullet',maturity_date='2023-01-01')
sc_calendar<-list(list(start_date='2021-01-01',end_date='2022-01-01',payment_date='2022-01-01'),list(start_date='2022-01-01',end_date='2023-01-01',payment_date='2023-01-01'))
sc_values<-function(x,key)vapply(x,function(r)r[[key]],numeric(1))
sc_error<-function(code,expr){e<-tryCatch(force(expr),error=identity);stopifnot(inherits(e,'creditratingsystem_error'),e$code==code)}
r<-build_debt_schedule(sc_terms,sc_calendar)$value$schedule;stopifnot(max(abs(sc_values(r,'net_payment_due')-c(10,110)))<1e-12)
p<-sc_terms;p$amortization<-'annuity';r<-build_debt_schedule(p,sc_calendar)$value;stopifnot(abs(r$constant_payment-121/2.1)<1e-12,max(abs(sc_values(r$schedule,'net_payment_due')-121/2.1))<1e-12,r$schedule[[2]]$closing_principal==0)
p$annual_rate<-0;stopifnot(max(abs(sc_values(build_debt_schedule(p,sc_calendar)$value$schedule,'net_payment_due')-c(50,50)))<1e-12)
p<-sc_terms;p$amortization<-'equal_principal';stopifnot(max(abs(sc_values(build_debt_schedule(p,sc_calendar)$value$schedule,'net_payment_due')-c(60,55)))<1e-12)
p<-sc_terms;p$rate_type<-'floating';p$spread<-.01;cal<-sc_calendar;for(i in seq_along(cal))cal[[i]]$fixing_date<-cal[[i]]$start_date
r<-build_debt_schedule(p,cal,list(list(fixing_date='2021-01-01',base_rate=-.02),list(fixing_date='2022-01-01',base_rate=.03)))$value$schedule
stopifnot(r[[1]]$debt_service_due==0,abs(r[[1]]$lender_refund_due-1)<1e-12,abs(r[[2]]$net_payment_due-104)<1e-12)
p<-sc_terms;p$amortization<-'custom';cal<-sc_calendar;for(i in seq_along(cal))cal[[i]]$principal_due<-20;sc_error('SIMULATION_AMORTIZATION',build_debt_schedule(p,cal))
sc_policy<-list(policy_id='wf',version='1',initial_reserve=20,reserve_priority=2,excess_reserve_policy='retain',arrears_policy='carry_without_interest',claims=list(list(claim_id='senior',priority=1,reserve_eligible=TRUE),list(claim_id='junior',priority=3,reserve_eligible=FALSE)))
sc_periods<-list(list(scenario_id='s',date='2022-01-01',currency='EUR',cash_available=100,reserve_target=20,amounts_due=list(senior=110,junior=10)),list(scenario_id='s',date='2023-01-01',currency='EUR',cash_available=130,reserve_target=20,amounts_due=list(senior=100,junior=10)))
r<-apply_cash_waterfall(sc_periods,sc_policy,'EUR')$value$periods
stopifnot(r[[1]]$payments[[1]]$payment_from_reserve==10,r[[1]]$payments[[2]]$closing_arrears==10,r[[1]]$closing_reserve==10,r[[2]]$reserve_topup==10,r[[2]]$payments[[2]]$total_due==20,r[[2]]$payments[[2]]$closing_arrears==0,all(sc_values(r,'cash_identity_residual')==0),all(sc_values(r,'residual_equity_cash')==0),identical(apply_cash_waterfall(rev(sc_periods),sc_policy,'EUR')$value$periods,r))
p<-sc_policy;p$claims[[1]]$reserve_eligible<-FALSE;one<-sc_periods[1];one[[1]]$cash_available<-0;one[[1]]$reserve_target<-0;r<-apply_cash_waterfall(one,p,'EUR')$value$periods[[1]];stopifnot(r$closing_reserve==20,r$payments[[1]]$total_paid==0)
p$excess_reserve_policy<-'release_before_claims';r<-apply_cash_waterfall(one,p,'EUR')$value$periods[[1]];stopifnot(r$reserve_release==20,r$payments[[1]]$total_paid==20)
p$reserve_priority<-1;sc_error('SIMULATION_WATERFALL',apply_cash_waterfall(one,p,'EUR'))
cat('Native contractual debt and cash waterfall PASS\n')
debt<-build_debt_schedule(sc_terms,sc_calendar)$value$schedule
p<-list(policy_id='single',version='1',initial_reserve=1,reserve_priority=2,excess_reserve_policy='retain',arrears_policy='carry_without_interest',claims=list(list(claim_id='debt',priority=1,reserve_eligible=TRUE)))
cash<-lapply(seq_along(debt),function(i)list(scenario_id='s',date=debt[[i]]$payment_date,currency='EUR',cash_available=c(9,10)[i],reserve_target=0,amounts_due=list(debt=debt[[i]]$debt_service_due)))
liquidity<-apply_cash_waterfall(cash,p,'EUR')$value$periods
stopifnot(abs(liquidity[[1]]$payments[[1]]$closing_arrears)<1e-12,abs(liquidity[[2]]$payments[[1]]$closing_arrears-100)<1e-12)
p<-list(policy_id='reference',version='1',basis='BASEL_CRE36_REFERENCE',assessment_level='obligor',dscr_threshold=1,ltv_threshold=1,dscr_comparison='lt',ltv_comparison='gt',past_due_days_threshold=90,past_due_comparison='gt',past_due_rule_reference='CRE36.68',materiality_rule_reference='declared',utp_rule_reference='declared-economic-assessment',dual_test_as_utp=TRUE,economic_joint_timing='SAME_DATE',default_state='ABSORBING_FIRST_DEFAULT')
initial<-list(scenario_id='s',entity_id='o',facility_id='f',date='2021-01-01',currency='EUR',cfads=0,debt_service_due=0,debt_outstanding=100,asset_value=80,liquidity_buffer_available=1,materiality_met=FALSE,material_days_past_due=0,utp_indicators=list());rows<-list(initial)
for(i in seq_along(debt)){r<-initial;r$date<-debt[[i]]$payment_date;r$cfads<-cash[[i]]$cash_available;r$debt_service_due<-debt[[i]]$debt_service_due;r$liquidity_buffer_available<-liquidity[[i]]$opening_reserve;rows[[length(rows)+1L]]<-r}
r<-detect_cashflow_default(rows,p,'EUR')$value;stopifnot(r$first_default_events[[1]]$first_default_date=='2023-01-01',r$first_default_events[[1]]$last_nondefault_date=='2022-01-01')
