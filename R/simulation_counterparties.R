.cr_sim_probability<-function(value,name,uniform=FALSE){v<-.cr_sim_number(value,name,0);if(v>1||(uniform&&v==1)) .cr_error('SIMULATION_PROBABILITY',paste(name,'outside probability or [0,1) innovation domain'));v}
simulate_contract_counterparties<-function(counterparty_paths,contracts,cure_uniforms,currency,context=NULL){
 paths<-.cr_data_rows(counterparty_paths,'counterparty_paths');contracts<-.cr_data_rows(contracts,'contracts');draws<-.cr_data_rows(cure_uniforms,'cure_uniforms');.cr_sim_text(currency,'currency');ctx<-.cr_result(NULL,'simulation.context',context=context)$context
 if(!is.null(ctx$currency)&&ctx$currency!=currency) .cr_error('TARGET_MISMATCH','Counterparty cashflow currency conflicts with context')
 if(!is.null(ctx$probability_measure)&&ctx$probability_measure!='PHYSICAL') .cr_error('TARGET_MISMATCH','Counterparty paths require physical probabilities')
 groups<-list();seen<-new.env(parent=emptyenv())
 for(row in paths){
 .cr_data_required(row,c('scenario_id','counterparty_id','period_start','period_end','conditional_pd','uniform','probability_measure'))
 start<-.cr_data_date(row$period_start,'period_start');end<-.cr_data_date(row$period_end,'period_end');key<-.cr_hash(list(row$scenario_id,row$counterparty_id,row$period_end))
 .cr_sim_text(row$probability_measure,'probability_measure');if(row$probability_measure!='PHYSICAL') .cr_error('TARGET_MISMATCH','Counterparty conditional PD must be physical P')
 if(end<=start||exists(key,seen,inherits=FALSE)) .cr_error('SIMULATION_CALENDAR','Positive unique counterparty periods required')
 .cr_sim_probability(row$conditional_pd,'conditional_pd');.cr_sim_probability(row$uniform,'uniform',TRUE);assign(key,TRUE,seen);gkey<-.cr_hash(list(row$scenario_id,row$counterparty_id));if(is.null(groups[[gkey]]))groups[[gkey]]<-list(scenario=row$scenario_id,counterparty=row$counterparty_id,records=list());groups[[gkey]]$records<-c(groups[[gkey]]$records,list(row))
 }
 if(!length(groups)||!length(contracts)) .cr_error('SIMULATION_COUNTERPARTY','Nonempty paths and contracts required')
 states<-list();events<-list();groups<-groups[order(vapply(groups,function(g)g$scenario,character(1)),vapply(groups,function(g)g$counterparty,character(1)))]
 for(gkey in names(groups)){
 group<-groups[[gkey]];periods<-group$records;periods<-periods[order(vapply(periods,function(r)r$period_end,character(1)))];last<-first<-NULL;grid<-periods[[1]]$period_start
 for(row in periods){
 if(!is.null(last)&&row$period_start!=last) .cr_error('SIMULATION_CALENDAR','Counterparty periods must be contiguous')
 last<-row$period_end;grid<-union(grid,last);if(is.null(first)&&row$uniform<row$conditional_pd)first<-last
 }
 states[[gkey]]<-list(default=first,grid=grid)
 events[[length(events)+1L]]<-list(scenario_id=group$scenario,counterparty_id=group$counterparty,first_default_date=first,defaulted=!is.null(first),initially_performing=TRUE,observed_through=last,default_time_convention='PERIOD_END_BEFORE_PAYMENT')
 }
 innovations<-list()
 for(row in draws){.cr_data_required(row,c('scenario_id','contract_id','uniform'));key<-.cr_hash(list(row$scenario_id,row$contract_id));if(!is.null(innovations[[key]])) .cr_error('SIMULATION_DUPLICATE','Duplicate contract cure innovation');innovations[[key]]<-.cr_sim_probability(row$uniform,'cure_uniform',TRUE)}
 ids<-character();definitions<-list()
 for(contract in contracts){
 .cr_data_required(contract,c('contract_id','counterparty_id','currency','default_payment_timing','cure_probability','cure_delay_days','replacement_enabled','replacement_delay_days','replacement_payment_fraction','cashflows'))
 if(contract$contract_id%in%ids) .cr_error('SIMULATION_DUPLICATE','Contract IDs must be unique');ids<-c(ids,contract$contract_id);.cr_sim_text(contract$default_payment_timing,'default_payment_timing')
 if(contract$currency!=currency||contract$default_payment_timing!='BEFORE_DUE') .cr_error('SIMULATION_COUNTERPARTY','One reporting currency and explicit BEFORE_DUE default timing required')
 cp<-.cr_sim_probability(contract$cure_probability,'cure_probability');cd<-.cr_sim_number(contract$cure_delay_days,'cure_delay_days',0);rd<-.cr_sim_number(contract$replacement_delay_days,'replacement_delay_days',0);rf<-.cr_sim_number(contract$replacement_payment_fraction,'replacement_payment_fraction',0)
 if(cd!=floor(cd)||rd!=floor(rd)||!is.logical(contract$replacement_enabled)||length(contract$replacement_enabled)!=1L||is.na(contract$replacement_enabled)) .cr_error('SIMULATION_COUNTERPARTY','Integer cure/replacement delays and Boolean replacement flag required')
 if(!contract$replacement_enabled&&(rd!=0||rf!=0)) .cr_error('SIMULATION_COUNTERPARTY','Disabled replacement requires zero delay and zero fraction')
 cash<-.cr_data_rows(contract$cashflows,'cashflows');dates<-character();if(!length(cash)) .cr_error('SIMULATION_COUNTERPARTY','Contract requires explicit cashflow dates')
 for(flow in cash){.cr_data_required(flow,c('date','amount'));.cr_data_date(flow$date,'date');.cr_sim_number(flow$amount,'amount',0);if(flow$date%in%dates) .cr_error('SIMULATION_DUPLICATE','Duplicate contract payment date');dates<-c(dates,flow$date)}
 cash<-cash[order(vapply(cash,function(r)r$date,character(1)))];definitions[[length(definitions)+1L]]<-list(contract=contract,cp=cp,cd=cd,rd=rd,rf=rf,cash=cash)
 }
 definitions<-definitions[order(vapply(definitions,function(d)d$contract$contract_id,character(1)))];scenarios<-sort(unique(vapply(groups,function(g)g$scenario,character(1))));used<-character();outputs<-contract_events<-list()
 for(scenario in scenarios)for(def in definitions){
 contract<-def$contract;key<-.cr_hash(list(scenario,contract$counterparty_id));drawkey<-.cr_hash(list(scenario,contract$contract_id))
 if(is.null(states[[key]])||is.null(innovations[[drawkey]])) .cr_error('SIMULATION_COUNTERPARTY','Every contract requires counterparty path and cure innovation in every scenario')
 state<-states[[key]];used<-union(used,drawkey);first<-state$default;cured<-FALSE;cure_date<-replacement_date<-NULL
 if(!is.null(first)){
 default_date<-.cr_data_date(first,'first_default_date');cured<-innovations[[drawkey]]<def$cp
 if(cured){candidate<-default_date+def$cd;cure_date<-as.character(candidate)}else if(contract$replacement_enabled){candidate<-default_date+def$cd+def$rd;replacement_date<-as.character(candidate)}
 if((!is.null(cure_date)&&(is.na(cure_date)||nchar(cure_date)!=10L))||(!is.null(replacement_date)&&(is.na(replacement_date)||nchar(replacement_date)!=10L))) .cr_error('SIMULATION_CALENDAR','Cure or replacement date outside supported calendar')
 }
 contract_events[[length(contract_events)+1L]]<-list(scenario_id=scenario,contract_id=contract$contract_id,counterparty_id=contract$counterparty_id,counterparty_default_date=first,contract_cured=cured,cure_date=cure_date,replacement_date=replacement_date,cure_uniform=innovations[[drawkey]])
 for(flow in def$cash){date<-flow$date
 if(!(date%in%state$grid)) .cr_error('SIMULATION_CALENDAR','Payment dates must lie on supplied counterparty assessment grid')
 if(is.null(first)||date<first){factor<-1;status<-'ORIGINAL_PERFORMING'}else if(!is.null(cure_date)&&date>=cure_date){factor<-1;status<-'CONTRACT_CURED'}else if(!is.null(replacement_date)&&date>=replacement_date){factor<-def$rf;status<-'REPLACED'}else{factor<-0;status<-'INTERRUPTED'}
 amount<-as.numeric(flow$amount);received<-amount*factor;if(!is.finite(received)) .cr_error('NUMERICAL_FAILURE','Replacement cashflow exceeds finite representation')
 outputs[[length(outputs)+1L]]<-list(scenario_id=scenario,contract_id=contract$contract_id,counterparty_id=contract$counterparty_id,date=date,currency=currency,scheduled_cash=amount,received_cash=received,cash_change=received-amount,payment_fraction=factor,status=status)
 }
 }
 if(!setequal(used,names(innovations))) .cr_error('SIMULATION_COUNTERPARTY','Unused cure innovations rejected')
 .cr_result(list(counterparty_events=events,contract_events=contract_events,cashflows=outputs,currency=currency),'simulation.simulate_contract_counterparties','contract_cashflows',ctx,diagnostics=list(paths_fingerprint=.cr_hash(paths),contracts_fingerprint=.cr_hash(contracts),innovations_fingerprint=.cr_hash(draws)),limitations='All counterparties initially performing. Conditional default draws share a counterparty event across its contracts; dependence supplied, not generated independently per contract. Events occur at assessment-grid endpoints before payments. Cure waits interrupt payments; successful cure restores future payments without invented arrears recoveries. Failed cure precedes replacement delay. Replacement assumed performing for horizon, explicit payment fraction. Counterparty default affects receipts and must not also be counted as borrower default; borrower has its own debt-service and UTP assessment.')
}
