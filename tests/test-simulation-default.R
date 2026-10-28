if(!exists('detect_cashflow_default'))library(creditratingsystem)
sd_policy<-list(policy_id='default-reference',version='1',basis='BASEL_CRE36_REFERENCE',assessment_level='obligor',dscr_threshold=1,ltv_threshold=1,dscr_comparison='lt',ltv_comparison='gt',past_due_days_threshold=90,past_due_comparison='gt',past_due_rule_reference='CRE36.68(2)',materiality_rule_reference='declared-national-policy',utp_rule_reference='approved-financial-assessment',dual_test_as_utp=TRUE,economic_joint_timing='SAME_DATE',default_state='ABSORBING_FIRST_DEFAULT')
sd_row<-function(...){x<-list(scenario_id='s',entity_id='o',facility_id='f',date='2024-01-01',currency='EUR',cfads=110,debt_service_due=100,debt_outstanding=100,asset_value=120,liquidity_buffer_available=0,materiality_met=FALSE,material_days_past_due=0,utp_indicators=list());for(n in names(list(...)))x[[n]]<-list(...)[[n]];x}
sd_error<-function(code,expr){e<-tryCatch(force(expr),error=identity);stopifnot(inherits(e,'creditratingsystem_error'),e$code==code)}
rows<-list(sd_row(scenario_id='0'),sd_row(scenario_id='1',cfads=90),sd_row(scenario_id='2',asset_value=80),sd_row(scenario_id='3',cfads=90,asset_value=80))
r<-detect_cashflow_default(rows,sd_policy,'EUR')$value
stopifnot(identical(vapply(r$facility_tests,function(x)x$dscr_breach,logical(1)),c(FALSE,TRUE,FALSE,TRUE)),identical(vapply(r$facility_tests,function(x)x$ltv_breach,logical(1)),c(FALSE,FALSE,TRUE,TRUE)),identical(vapply(r$first_default_events,function(x)x$defaulted,logical(1)),c(FALSE,FALSE,FALSE,TRUE)))
stopifnot(!detect_cashflow_default(list(sd_row(cfads=100,asset_value=100)),sd_policy,'EUR')$value$first_default_events[[1]]$defaulted)
p<-sd_policy;p$dscr_comparison<-'le';p$ltv_comparison<-'ge';stopifnot(detect_cashflow_default(list(sd_row(cfads=100,asset_value=100)),p,'EUR')$value$facility_tests[[1]]$economic_dual_breach,!detect_cashflow_default(list(sd_row(cfads=100,asset_value=100)),p,'EUR')$value$first_default_events[[1]]$defaulted)
rows<-list(sd_row(scenario_id='utp',asset_value=1000,utp_indicators=list('distressed-restructuring')),sd_row(scenario_id='days',asset_value=1000,materiality_met=TRUE,material_days_past_due=90),sd_row(scenario_id='days',date='2024-01-02',asset_value=1000,materiality_met=TRUE,material_days_past_due=91),sd_row(scenario_id='days',date='2024-01-03',asset_value=1000))
r<-detect_cashflow_default(rows,sd_policy,'EUR')$value;stopifnot(r$first_default_events[[1]]$first_default_date=='2024-01-02',identical(r$first_default_events[[2]]$reasons,list('UNLIKELY_TO_PAY')),r$assessments[[3]]$in_default,!r$assessments[[3]]$regulatory_trigger)
rows<-list(sd_row(utp_indicators=list('bankruptcy')),sd_row(facility_id='f2'));r<-detect_cashflow_default(rows,sd_policy,'EUR')$value;stopifnot(length(r$first_default_events)==1L,r$first_default_events[[1]]$defaulted)
p<-sd_policy;p$assessment_level<-'facility';p$facility_scope_eligibility<-'RETAIL_APPROVED';r<-detect_cashflow_default(rows,p,'EUR')$value;stopifnot(identical(vapply(r$first_default_events,function(x)x$defaulted,logical(1)),c(TRUE,FALSE)),identical(detect_cashflow_default(rev(rows),p,'EUR')$value,r))
stopifnot(!detect_cashflow_default(list(sd_row(cfads=90),sd_row(date='2024-02-01',asset_value=80)),sd_policy,'EUR')$value$first_default_events[[1]]$defaulted)
r<-detect_cashflow_default(list(sd_row(asset_value=0,cfads=90)),sd_policy,'EUR')$value;stopifnot(r$first_default_events[[1]]$defaulted,is.null(r$facility_tests[[1]]$ltv))
p<-sd_policy;p$dual_test_as_utp<-FALSE;r<-detect_cashflow_default(list(sd_row(cfads=90,asset_value=80)),p,'EUR')$value;stopifnot(r$facility_tests[[1]]$economic_dual_breach,!r$first_default_events[[1]]$defaulted)
sd_error('SIMULATION_ARREARS',detect_cashflow_default(list(sd_row(material_days_past_due=91)),sd_policy,'EUR'))
sd_error('SIMULATION_ARREARS',detect_cashflow_default(list(sd_row(materiality_met=TRUE,material_days_past_due=90),sd_row(date='2024-01-02',materiality_met=TRUE,material_days_past_due=80)),sd_policy,'EUR'))
sd_error('SIMULATION_DUPLICATE',detect_cashflow_default(list(sd_row(),sd_row()),sd_policy,'EUR'))
p<-sd_policy;p$past_due_days_threshold<-180;sd_error('SIMULATION_POLICY',detect_cashflow_default(list(sd_row()),p,'EUR'));p$supervisory_exception_eligibility<-'RETAIL_OR_PSE_APPROVED';stopifnot(!detect_cashflow_default(list(sd_row(materiality_met=TRUE,material_days_past_due=180)),p,'EUR')$value$first_default_events[[1]]$defaulted)
cat('Native double-test and Basel-reference event cases PASS\n')

records<-list(sd_row(scenario_id='initial',utp_indicators=list('bankruptcy')),sd_row(scenario_id='later'),sd_row(scenario_id='later',date='2024-02-01',utp_indicators=list('bankruptcy')),sd_row(scenario_id='none'))
e<-detect_cashflow_default(records,sd_policy,'EUR')$value$first_default_events
stopifnot(e[[1]]$initial_default,is.null(e[[1]]$last_nondefault_date),e[[1]]$default_time_status=='LEFT_CENSORED_AT_INITIAL_STATE',!e[[2]]$initial_default,e[[2]]$last_nondefault_date=='2024-01-01',e[[2]]$default_time_status=='DETECTED_AT_GRID_ENDPOINT',e[[2]]$observed_through=='2024-02-01',e[[3]]$default_time_status=='NO_DEFAULT_OBSERVED',e[[3]]$last_nondefault_date=='2024-01-01')

r<-detect_cashflow_default(list(sd_row(cfads=90,asset_value=80,liquidity_buffer_available=10)),sd_policy,'EUR')$value
stopifnot(r$facility_tests[[1]]$economic_dual_breach,!r$facility_tests[[1]]$utp_from_dual_test,!r$first_default_events[[1]]$defaulted)
stopifnot(detect_cashflow_default(list(sd_row(cfads=90,asset_value=80,liquidity_buffer_available=10,utp_indicators=list('bankruptcy'))),sd_policy,'EUR')$value$first_default_events[[1]]$defaulted)
