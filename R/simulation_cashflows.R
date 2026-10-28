build_debt_schedule<-function(loan_terms,calendar,base_rate_fixings=NULL,context=NULL){
 if(!.cr_object(loan_terms)) .cr_error('SIMULATION_TERMS','loan_terms must be a mapping')
 terms<-loan_terms;periods<-.cr_data_rows(calendar,'calendar');.cr_data_required(terms,c('facility_id','currency','principal','rate_type','rate_convention','day_count','amortization','maturity_date'))
 for(key in c('currency','rate_type','rate_convention','day_count','amortization')) .cr_sim_text(terms[[key]],key)
 principal<-.cr_sim_number(terms$principal,'principal',0)
 if(principal<=0||!length(periods)) .cr_error('SIMULATION_TERMS','Positive principal and nonempty calendar required')
 if(!(terms$rate_type%in%c('fixed','floating'))||!(terms$rate_convention%in%c('nominal_simple','effective_annual'))||!(terms$day_count%in%c('ACT/365F','ACT/360'))) .cr_error('SIMULATION_TERMS','Explicit supported rate type/convention and day count required')
 if(!(terms$amortization%in%c('bullet','equal_principal','annuity','custom'))) .cr_error('SIMULATION_TERMS','Unknown amortization')
 if(terms$amortization=='annuity'&&terms$rate_type!='fixed') .cr_error('SIMULATION_TERMS','Constant-payment annuity requires a fixed rate; floating reset schedules require explicit principal amounts')
 ctx<-.cr_result(NULL,'simulation.context',context=context)$context
 if(!is.null(ctx$currency)&&ctx$currency!=terms$currency) .cr_error('TARGET_MISMATCH','Debt currency conflicts with context')
 maturity<-.cr_data_date(terms$maturity_date,'maturity_date');fixings<-list()
 if(terms$rate_type=='floating'){
 .cr_data_required(terms,'spread')
 for(fix in .cr_data_rows(base_rate_fixings,'base_rate_fixings')){
 .cr_data_required(fix,c('fixing_date','base_rate'));.cr_data_date(fix$fixing_date,'fixing_date');if(!is.null(fixings[[fix$fixing_date]])) .cr_error('SIMULATION_DUPLICATE','Duplicate rate fixing');fixings[[fix$fixing_date]]<-.cr_sim_number(fix$base_rate,'base_rate')
 }
 }else if(!is.null(base_rate_fixings)) .cr_error('SIMULATION_TERMS','Fixed-rate schedule must not consume base-rate fixings') else .cr_data_required(terms,'annual_rate')
 validated<-list();last_end<-last_pay<-NULL;used<-character()
 for(period in periods){
 .cr_data_required(period,c('start_date','end_date','payment_date'));start<-.cr_data_date(period$start_date,'start_date');end<-.cr_data_date(period$end_date,'end_date');payment<-.cr_data_date(period$payment_date,'payment_date')
 if(start>=end||payment<end||(!is.null(last_end)&&start!=last_end)||(!is.null(last_pay)&&payment<=last_pay)) .cr_error('SIMULATION_CALENDAR','Contiguous positive accrual periods and strictly increasing payments at/after period end required')
 yf<-as.numeric(end-start)/if(terms$day_count=='ACT/365F')365 else 360
 if(terms$rate_type=='fixed')rate<-.cr_sim_number(terms$annual_rate,'annual_rate')else{
 .cr_data_required(period,'fixing_date');fixdate<-.cr_data_date(period$fixing_date,'fixing_date');if(fixdate>start||is.null(fixings[[period$fixing_date]])) .cr_error('SIMULATION_FIXING','Each floating period needs an explicit fixing dated no later than period start')
 used<-union(used,period$fixing_date);rate<-fixings[[period$fixing_date]]+.cr_sim_number(terms$spread,'spread')
 }
 if(terms$rate_convention=='effective_annual'){
 if(rate<=-1) .cr_error('SIMULATION_RATE','Effective annual rate must exceed -1');exponent<-log1p(rate)*yf;if(exponent>709||exponent< -709) .cr_error('SIMULATION_RATE','Accrual exponent outside supported finite range');growth<-exp(exponent)
 }else growth<-1+rate*yf
 if(!is.finite(growth)||growth<=0) .cr_error('SIMULATION_RATE','Accrual growth factor must be finite and positive')
 validated[[length(validated)+1L]]<-list(period=period,yf=yf,rate=rate,growth=growth);last_end<-end;last_pay<-payment
 }
 if(last_pay!=maturity) .cr_error('SIMULATION_CALENDAR','Final payment must equal contractual maturity')
 if(!setequal(names(fixings),used)) .cr_error('SIMULATION_FIXING','Unused or missing rate fixings not accepted')
 payment_amount<-NULL
 if(terms$amortization=='annuity'){
 logfactor<-0;discounts<-numeric();for(v in validated){logfactor<-logfactor+log(v$growth);discounts<-c(discounts,-logfactor)};m<-max(discounts);logpayment<-log(principal)-(m+log(sum(exp(discounts-m))));if(logpayment<= -709||logpayment>=709) .cr_error('NUMERICAL_FAILURE','Annuity payment outside supported finite range');payment_amount<-exp(logpayment)
 }
 balance<-principal;result<-list();tol<-1e-10*max(1,principal)
 for(index in seq_along(validated)){
 v<-validated[[index]];period<-v$period;opening<-balance;interest<-opening*(if(terms$rate_convention=='nominal_simple')v$rate*v$yf else expm1(log1p(v$rate)*v$yf));last<-index==length(validated)
 repay<-switch(terms$amortization,bullet=if(last)opening else 0,equal_principal=if(last)opening else principal/length(validated),annuity=if(last)opening else payment_amount-interest,custom={.cr_data_required(period,'principal_due');.cr_sim_number(period$principal_due,'principal_due',0)})
 if(repay< -tol||repay>opening+tol) .cr_error('SIMULATION_AMORTIZATION','Principal repayment must be between zero and opening principal')
 repay<-min(opening,max(0,repay));balance<-opening-repay;net<-interest+repay
 if(any(!is.finite(c(interest,repay,balance,net)))) .cr_error('NUMERICAL_FAILURE','Debt schedule exceeds finite representation')
 result[[length(result)+1L]]<-list(facility_id=terms$facility_id,currency=terms$currency,start_date=period$start_date,end_date=period$end_date,payment_date=period$payment_date,year_fraction=v$yf,annual_rate=v$rate,opening_principal=opening,interest_due=interest,principal_due=repay,debt_service_due=max(net,0),lender_refund_due=max(-net,0),net_payment_due=net,closing_principal=balance)
 }
 if(balance>tol) .cr_error('SIMULATION_AMORTIZATION','Contractual maturity must repay all principal; refinancing is not assumed')
 .cr_result(list(schedule=result,terms=terms,terms_hash=.cr_hash(terms),constant_payment=payment_amount),'simulation.build_debt_schedule','debt_schedule',ctx,limitations='Contractual accrual on opening principal, no fees, capitalization, prepayment option or assumed refinancing. Negative interest is retained and net lender refunds reported separately. ACT/360 and ACT/365F only; contractual calendars supplied explicitly.')
}

apply_cash_waterfall<-function(cash_periods,waterfall_policy,currency,context=NULL){
 rows<-.cr_data_rows(cash_periods,'cash_periods');policy<-waterfall_policy;.cr_sim_text(currency,'currency')
 if(!.cr_object(policy)) .cr_error('SIMULATION_WATERFALL','Explicit versioned waterfall policy required')
 .cr_data_required(policy,c('policy_id','version','initial_reserve','reserve_priority','excess_reserve_policy','arrears_policy','claims'))
 for(key in c('policy_id','version','excess_reserve_policy','arrears_policy')) .cr_sim_text(policy[[key]],key)
 reserve_initial<-.cr_sim_number(policy$initial_reserve,'initial_reserve',0);reserve_priority<-.cr_sim_number(policy$reserve_priority,'reserve_priority')
 if(!(policy$excess_reserve_policy%in%c('retain','release_before_claims'))||policy$arrears_policy!='carry_without_interest') .cr_error('SIMULATION_WATERFALL','Declare reserve excess treatment and carry_without_interest arrears')
 claims<-.cr_data_rows(policy$claims,'claims');byid<-list();priorities<-reserve_priority
 for(claim in claims){
 .cr_data_required(claim,c('claim_id','priority','reserve_eligible'));priority<-.cr_sim_number(claim$priority,'priority')
 if(!is.null(byid[[claim$claim_id]])||priority%in%priorities||!is.logical(claim$reserve_eligible)||length(claim$reserve_eligible)!=1L||is.na(claim$reserve_eligible)) .cr_error('SIMULATION_WATERFALL','Unique claim IDs/priorities and Boolean reserve eligibility required')
 byid[[claim$claim_id]]<-claim;priorities<-c(priorities,priority)
 }
 if(!length(claims)) .cr_error('SIMULATION_WATERFALL','At least one claim required')
 ordered<-c(lapply(claims,function(c)list(priority=c$priority,id=c$claim_id)),list(list(priority=reserve_priority,id=NULL)));ordered<-ordered[order(vapply(ordered,function(x)x$priority,numeric(1)))]
 ctx<-.cr_result(NULL,'simulation.context',context=context)$context;if(!is.null(ctx$currency)&&ctx$currency!=currency) .cr_error('TARGET_MISMATCH','Waterfall currency conflicts with context')
 groups<-list();seen<-new.env(parent=emptyenv())
 for(row in rows){
 .cr_data_required(row,c('scenario_id','date','currency','cash_available','reserve_target','amounts_due'));.cr_data_date(row$date,'date')
 if(row$currency!=currency) .cr_error('TARGET_MISMATCH','All waterfall amounts must use reporting currency')
 key<-.cr_hash(list(row$scenario_id,row$date));if(exists(key,seen,inherits=FALSE)) .cr_error('SIMULATION_DUPLICATE','Duplicate scenario/date');assign(key,TRUE,seen)
 if(!.cr_object(row$amounts_due)||!setequal(names(row$amounts_due),names(byid))) .cr_error('SIMULATION_WATERFALL','Every date requires each claim amount, explicitly including zeros')
 groups[[row$scenario_id]]<-c(groups[[row$scenario_id]],list(row))
 }
 outputs<-list()
 for(scenario in sort(names(groups))){
 periods<-groups[[scenario]];periods<-periods[order(vapply(periods,function(r)r$date,character(1)))];reserve<-reserve_initial;arrears<-as.list(setNames(rep(0,length(byid)),names(byid)))
 for(row in periods){
 cash<-.cr_sim_number(row$cash_available,'cash_available',0);target<-.cr_sim_number(row$reserve_target,'reserve_target',0);cash_open<-cash;reserve_open<-reserve;release<-topup<-0;payments<-list()
 if(policy$excess_reserve_policy=='release_before_claims'){release<-max(reserve-target,0);reserve<-reserve-release;cash<-cash+release}
 for(step in ordered){cid<-step$id
 if(is.null(cid)){topup<-min(cash,max(target-reserve,0));cash<-cash-topup;reserve<-reserve+topup;next}
 new_due<-.cr_sim_number(row$amounts_due[[cid]],'amount_due',0);carry<-arrears[[cid]];demand<-carry+new_due;from_cash<-min(cash,demand);cash<-cash-from_cash
 from_reserve<-if(byid[[cid]]$reserve_eligible)min(reserve,demand-from_cash)else 0;reserve<-reserve-from_reserve;paid<-from_cash+from_reserve;arrears[[cid]]<-max(demand-paid,0)
 payments[[length(payments)+1L]]<-list(claim_id=cid,new_amount_due=new_due,opening_arrears=carry,total_due=demand,payment_from_cash=from_cash,payment_from_reserve=from_reserve,total_paid=paid,closing_arrears=arrears[[cid]])
 }
 paid<-sum(vapply(payments,function(p)p$total_paid,numeric(1)));residual<-cash_open+reserve_open-paid-reserve-cash
 if(!is.finite(residual)||abs(residual)>1e-10*max(1,cash_open+reserve_open)) .cr_error('NUMERICAL_FAILURE','Waterfall cash identity failed')
 outputs[[length(outputs)+1L]]<-list(scenario_id=scenario,date=row$date,currency=currency,cash_available=cash_open,opening_reserve=reserve_open,reserve_release=release,reserve_topup=topup,closing_reserve=reserve,reserve_target=target,payments=payments,residual_equity_cash=cash,cash_identity_residual=residual)
 }
 }
 .cr_result(list(periods=outputs,policy=policy,policy_hash=.cr_hash(policy),currency=currency),'simulation.apply_cash_waterfall','cash_waterfall',ctx,limitations='No reserve amount is operating revenue. Due amounts exclude carried arrears, which this function carries once without interest. Negative operating cashflows must be represented by explicit cost claims and available liquidity. Liquidity shortage is not automatically regulatory default; materiality, duration and UTP assessed separately.')
}
