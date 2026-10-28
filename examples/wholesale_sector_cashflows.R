# Sector cash rights, liquidity stress, member calls and legal-priority losses.
.sector_receivable_paths <- function(case) {
  variant<-case$variant;capital<-variant %in% c('multilateral_lender','supranational_body')
  if(!capital&&variant!='reinsurer')return(NULL)
  if(capital)promises<-list(list(promise_id='member-A',amount=if(variant=='multilateral_lender')40 else 20,currency='EUR',fx=1,due='2024-06-01'),list(promise_id='member-B',amount=if(variant=='multilateral_lender')30 else 15,currency=if(variant=='multilateral_lender')'USD' else 'EUR',fx=if(variant=='multilateral_lender').9 else 1,due='2024-07-01')) else promises<-list(list(promise_id='retrocession-A',amount=50,currency='EUR',fx=1,due='2024-05-01'))
  dates<-c('2024-05-01','2024-06-01','2024-07-01');paths<-draws<-contracts<-list()
  for(p in promises)contracts[[length(contracts)+1L]]<-list(contract_id=p$promise_id,counterparty_id=p$promise_id,currency='EUR',default_payment_timing='BEFORE_DUE',cure_probability=0,cure_delay_days=0,replacement_enabled=FALSE,replacement_delay_days=0,replacement_payment_fraction=0,cashflows=lapply(dates,function(d)list(date=d,amount=if(d==p$due)p$amount*p$fx else 0)))
  for(scenario in c('normal','first_member_default','joint_member_default'))for(i in seq_along(promises)) {
    p<-promises[[i]];start<-'2024-04-01'
    for(t in seq_along(dates)) {
      end<-dates[t];default<-t==1&&(scenario=='joint_member_default'||scenario=='first_member_default'&&i==1)
      paths[[length(paths)+1L]]<-list(scenario_id=scenario,counterparty_id=p$promise_id,period_start=start,period_end=end,conditional_pd=if(default)1 else 0,uniform=.5,probability_measure='PHYSICAL');start<-end
    }
    draws[[length(draws)+1L]]<-list(scenario_id=scenario,contract_id=p$promise_id,uniform=.5)
  }
  result<-simulate_contract_counterparties(paths,contracts,draws,currency='EUR')$value;uncalled<-NULL
  if(capital) {
    inactive<-contracts
    for(i in seq_along(inactive))for(j in seq_along(inactive[[i]]$cashflows))inactive[[i]]$cashflows[[j]]$amount<-0
    uncalled<-simulate_contract_counterparties(paths,inactive,draws,currency='EUR')$value
  }
  list(promises=promises,contract=result,untriggered_contract=uncalled,call_terms=if(capital)list(contract_known_at='2024-04-01',scheduled_notice_at='2024-05-01',minimum_notice_days=31,trigger='Explicit synthetic call-authorized stress state; untriggered state schedules no payment',currency_conversion='Declared fixed contractual EUR conversion, not market FX',no_double_credit='Each promise scheduled once; no parallel PD support benefit applied') else NULL,interpretation=if(capital)'Explicit fixed stress states, no probability-weighted loss estimate; capital is financing, not operating revenue' else 'Retrocession receipts depend on the same named counterparty event, not insurer default')
}
run_wholesale_sector_cashflows <- function(data=NULL,verify_reference=TRUE) {
  if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata','wholesale_sector_cashflows.json',package='creditratingsystem'),simplifyVector=FALSE)
  out<-list()
  for(case in data$cases) {
    t<-case$terms;sid<-case$case_id;receivables<-.sector_receivable_paths(case);capital<-case$variant %in% c('multilateral_lender','supranational_body');ops<-water<-expected<-list()
    for(scenario in c('base','stress')) {
      transfers<-t$transfer_receipts*if(scenario=='stress').5 else 1;cp<-0
      if(case$variant=='reinsurer')cp<-sum(vapply(Filter(function(x)x$scenario_id==(if(scenario=='base')'normal' else 'first_member_default')&&x$date=='2024-05-01',receivables$contract$cashflows),function(x)x$received_cash,numeric(1)))
      ops[[length(ops)+1L]]<-list(scenario_id=scenario,period_start='2024-04-01',period_end='2024-05-01',currency='EUR',base_quantity=1,base_unit_price=t$own_receipts,base_unit_variable_cost=0,base_fixed_cost=t$operating_due,volume_multiplier=1,price_multiplier=1,variable_cost_multiplier=1,fixed_cost_multiplier=1,working_capital_revenue_ratio=0,cash_taxes=0,capex=0,asset_disposals=0,other_operating_cash=transfers+cp)
      cash<-t$initial_cash+t$own_receipts+transfers+cp
      water[[length(water)+1L]]<-list(scenario_id=scenario,date='2024-05-01',currency='EUR',cash_available=cash,reserve_target=0,amounts_due=list(operating=t$operating_due,financial_credit=t$financial_due))
      op_paid<-min(cash,t$operating_due);credit_paid<-min(cash-op_paid,t$financial_due)
      expected[[length(expected)+1L]]<-list(scenario_id=scenario,own_receipts=t$own_receipts,transfers=transfers,counterparty_receipts=cp,planned_cfads=t$own_receipts+transfers+cp-t$operating_due,operating_unpaid=t$operating_due-op_paid,financial_credit_unpaid=t$financial_due-credit_paid)
    }
    operating<-simulate_operating_cashflows(ops,list(contract_id=paste0(sid,'-operating'),version='1',currency='EUR',period_unit='month',quantity_unit='explicit_period_cash_receipt',initial_working_capital=0,include_asset_disposals_in_cfads=FALSE))$value
    liquidity<-apply_cash_waterfall(water,list(policy_id=paste0(sid,'-liquidity'),version='1',initial_reserve=0,reserve_priority=3,excess_reserve_policy='retain',arrears_policy='carry_without_interest',claims=list(list(claim_id='operating',priority=1,reserve_eligible=FALSE),list(claim_id='financial_credit',priority=2,reserve_eligible=FALSE))),currency='EUR')$value
    balance<-NULL
    if(!is.null(t$asset_book)) {loss<-t$asset_book*t$asset_haircut;balance<-list(initial_capital=t$asset_book-t$liabilities,market_value_loss=loss,noncash_reserve_strengthening=t$reserve_strengthening,stressed_capital=t$asset_book-loss-t$liabilities-t$reserve_strengthening,restricted_capital_not_cash=t$restricted_capital)}
    recovery<-do.call(simulate_recovery_waterfall,case$recovery)$value
    expected_recovery<-min(max(t$recovery_gross-t$prior_claim,0),t$credit_claim);expected_lgd<-1-expected_recovery/t$credit_claim
    facts<-list(product=case$product,variant=case$variant,borrower_class=t$borrower_class,claim_type=t$claim_type,cashflow_type=t$cashflow_type)
    snapshot<-list(subject_id=sid,subject_type='obligor',as_of='2024-04-01',known_at='2024-04-01',attributes=lapply(facts,function(v)list(value=v,observed_at='2024-04-01',known_at='2024-04-01')))
    rule<-list(rule_id=sid,profile_id=sid,profile_version='1',priority=1,effective_from='2023-01-01',effective_to=NULL,known_at='2023-01-01',conditions=lapply(names(facts),function(k)list(field=k,operator='eq',value=facts[[k]])))
    route<-route_rating_profile(snapshot,list(rule),'2024-04-01','2024-04-01')$value;snapshot$attributes$claim_type['value']<-list(NULL);negative<-route_rating_profile(snapshot,list(rule),'2024-04-01','2024-04-01')$value
    if(verify_reference) {
      for(i in seq_along(expected)) {
        ee<-expected[[i]];op<-operating$periods[[i]];w<-liquidity$periods[[i]];payments<-setNames(w$payments,vapply(w$payments,function(p)p$claim_id,character(1)))
        stopifnot(abs(op$cfads-ee$planned_cfads)<1e-12,payments$operating$closing_arrears==ee$operating_unpaid,payments$financial_credit$closing_arrears==ee$financial_credit_unpaid)
      }
      claim<-Filter(function(x)x$claim_id=='financial_credit',recovery$claim_results)[[1]];stopifnot(abs(claim$lgd-expected_lgd)<1e-12,negative$status=='NO_ROUTE')
      if(capital) {
        stopifnot(sum(vapply(receivables$untriggered_contract$cashflows,function(x)x$received_cash,numeric(1)))==0)
        flows<-receivables$contract$cashflows
        stopifnot(sum(vapply(Filter(function(x)x$date=='2024-05-01',flows),function(x)x$received_cash,numeric(1)))==0,sum(vapply(Filter(function(x)x$scenario_id=='joint_member_default',flows),function(x)x$received_cash,numeric(1)))==0,sum(vapply(Filter(function(x)x$scenario_id=='normal',flows),function(x)x$received_cash,numeric(1)))==sum(vapply(receivables$promises,function(p)p$amount*p$fx,numeric(1))))
      }
    }
    out[[length(out)+1L]]<-list(product=case$product,variant=case$variant,case_id=sid,terms=t,operating=operating,liquidity=liquidity,balance_stress=balance,receivables=receivables,recovery=recovery,independent_cash_oracle=expected,independent_credit_lgd=expected_lgd,profile=route$profile_id,missing_claim_status=negative$status)
  }
  list(data_origin='SYNTHETIC',legal_convention=data$legal_convention,cases=out)
}
