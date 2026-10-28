.cr_sim_number<-function(x,name,minimum=NULL){v<-.cr_numeric(x,name);if(length(v)!=1L||!is.null(dim(v))||(!is.null(minimum)&&v<minimum)) .cr_error('SIMULATION_INPUT',paste(name,'is outside scalar domain'));as.numeric(v)}
.cr_sim_text<-function(x,name){.cr_string(x,name,'SIMULATION_INPUT');x}
detect_cashflow_default<-function(path_records,default_policy,currency,context=NULL){
 rows<-.cr_data_rows(path_records,'path_records');policy<-default_policy;.cr_sim_text(currency,'currency')
 if(!.cr_object(policy)) .cr_error('SIMULATION_POLICY','A versioned explicit default policy is required')
 .cr_data_required(policy,c('policy_id','version','basis','assessment_level','dscr_threshold','ltv_threshold','dscr_comparison','ltv_comparison','past_due_days_threshold','past_due_comparison','past_due_rule_reference','materiality_rule_reference','utp_rule_reference','dual_test_as_utp','economic_joint_timing','default_state'))
 for(key in c('policy_id','version','basis','assessment_level','dscr_comparison','ltv_comparison','past_due_comparison','economic_joint_timing','default_state','past_due_rule_reference','materiality_rule_reference','utp_rule_reference')) .cr_sim_text(policy[[key]],key)
 if(policy$basis!='BASEL_CRE36_REFERENCE'||!(policy$assessment_level%in%c('obligor','facility'))||policy$economic_joint_timing!='SAME_DATE'||policy$default_state!='ABSORBING_FIRST_DEFAULT') .cr_error('SIMULATION_POLICY','Declare Basel reference, obligor/facility scope, same-date dual test and absorbing first default')
 if(!(policy$dscr_comparison%in%c('lt','le'))||!(policy$ltv_comparison%in%c('gt','ge'))||policy$past_due_comparison!='gt'||!is.logical(policy$dual_test_as_utp)||length(policy$dual_test_as_utp)!=1L||is.na(policy$dual_test_as_utp)) .cr_error('SIMULATION_POLICY','Declare threshold strictness, past-due gt and Boolean dual_test_as_utp')
 dscr_limit<-.cr_sim_number(policy$dscr_threshold,'dscr_threshold',0);ltv_limit<-.cr_sim_number(policy$ltv_threshold,'ltv_threshold',0);dpd_limit<-.cr_sim_number(policy$past_due_days_threshold,'past_due_days_threshold',0)
 if(dpd_limit!=floor(dpd_limit)||dpd_limit<90||dpd_limit>180) .cr_error('SIMULATION_POLICY','Basel-reference day threshold must be integer90..180 with documented applicable rule')
 if(dpd_limit!=90&&!identical(policy$supervisory_exception_eligibility,'RETAIL_OR_PSE_APPROVED')) .cr_error('SIMULATION_POLICY','Non90day threshold requires explicit applicable retail/PSE supervisory exception')
 if(policy$assessment_level=='facility'&&!identical(policy$facility_scope_eligibility,'RETAIL_APPROVED')) .cr_error('SIMULATION_POLICY','Facility-level Basel reference requires explicit retail scope eligibility')
 ctx<-.cr_result(NULL,'simulation.context',context=context)$context;if(!is.null(ctx$currency)&&ctx$currency!=currency) .cr_error('TARGET_MISMATCH','Default path currency conflicts with context')
 seen<-new.env(parent=emptyenv());identities<-new.env(parent=emptyenv());groups<-list();details<-list()
 for(row in rows){
 .cr_data_required(row,c('scenario_id','entity_id','facility_id','date','currency','cfads','debt_service_due','debt_outstanding','asset_value','liquidity_buffer_available','materiality_met','material_days_past_due','utp_indicators'))
 for(key in c('scenario_id','entity_id','facility_id')) .cr_sim_text(row[[key]],key)
 date<-.cr_data_date(row$date,'date');key<-.cr_hash(list(row$scenario_id,row$facility_id,row$date));if(exists(key,seen,inherits=FALSE)) .cr_error('SIMULATION_DUPLICATE','Each scenario/facility/date must be unique');assign(key,row,seen)
 owner_key<-.cr_hash(list(row$scenario_id,row$facility_id));if(exists(owner_key,identities,inherits=FALSE)&&get(owner_key,identities)!=row$entity_id) .cr_error('SIMULATION_IDENTITY','Facility owner must remain fixed within simulation');assign(owner_key,row$entity_id,identities)
 if(row$currency!=currency) .cr_error('TARGET_MISMATCH','Path records must use one reporting currency')
 cfads<-.cr_sim_number(row$cfads,'cfads');due<-.cr_sim_number(row$debt_service_due,'debt_service_due',0);debt<-.cr_sim_number(row$debt_outstanding,'debt_outstanding',0);assets<-.cr_sim_number(row$asset_value,'asset_value',0);dpd<-.cr_sim_number(row$material_days_past_due,'material_days_past_due',0)
 if(dpd!=floor(dpd)||!is.logical(row$materiality_met)||length(row$materiality_met)!=1L||is.na(row$materiality_met)) .cr_error('SIMULATION_ARREARS','Materiality flag and integer material-arrears day counter required')
 if(!row$materiality_met&&dpd!=0) .cr_error('SIMULATION_ARREARS','No material arrears means continuous day counter must be zero')
 indicators<-row$utp_indicators
 if(is.list(indicators)&&is.null(names(indicators))&&all(vapply(indicators,function(x)is.character(x)&&length(x)==1L,logical(1))))indicators<-if(length(indicators))unlist(indicators,use.names=FALSE)else character()
 if(!is.character(indicators)||anyNA(indicators)||any(!nzchar(indicators))||anyDuplicated(indicators)) .cr_error('SIMULATION_UTP','UTP indicators must be distinct explicit evidence IDs')
 dscr<-if(due==0)NULL else cfads/due;dscr_breach<-if(due==0)FALSE else if(policy$dscr_comparison=='lt')dscr<dscr_limit else dscr<=dscr_limit
 ltv<-if(assets==0)NULL else debt/assets;ltv_breach<-if(assets==0)debt>0 else if(policy$ltv_comparison=='gt')ltv>ltv_limit else ltv>=ltv_limit
 if((!is.null(dscr)&&!is.finite(dscr))||(!is.null(ltv)&&!is.finite(ltv))) .cr_error('NUMERICAL_FAILURE','DSCR or LTV exceeds finite representation')
 buffer<-.cr_sim_number(row$liquidity_buffer_available,'liquidity_buffer_available',0);shortfall<-max(due-cfads-buffer,0)
 joint<-dscr_breach&&ltv_breach;dual_utp<-policy$dual_test_as_utp&&joint&&shortfall>0;utp<-length(indicators)>0||dual_utp;past_due<-row$materiality_met&&dpd>dpd_limit
 detail<-list(scenario_id=row$scenario_id,entity_id=row$entity_id,facility_id=row$facility_id,date=row$date,dscr=dscr,dscr_status=if(due==0)'NO_DEBT_SERVICE'else'DEFINED',ltv=ltv,ltv_status=if(debt==0&&assets==0)'NO_DEBT_OR_ASSET'else if(assets==0)'POSITIVE_DEBT_ZERO_ASSET'else'DEFINED',dscr_breach=dscr_breach,ltv_breach=ltv_breach,economic_dual_breach=joint,utp_indicators=as.list(sort(indicators)),liquidity_buffer_available=buffer,unremedied_debt_service_shortfall=shortfall,utp_from_dual_test=dual_utp,unlikely_to_pay=utp,material_past_due_trigger=past_due,regulatory_trigger=utp||past_due)
 details[[length(details)+1L]]<-detail;subject<-if(policy$assessment_level=='obligor')row$entity_id else row$facility_id;gkey<-.cr_hash(list(row$scenario_id,subject));if(is.null(groups[[gkey]]))groups[[gkey]]<-list(scenario=row$scenario_id,subject=subject,records=list());groups[[gkey]]$records[[length(groups[[gkey]]$records)+1L]]<-detail
 }
 assessments<-first_events<-list();groups<-groups[order(vapply(groups,function(g)g$scenario,character(1)),vapply(groups,function(g)g$subject,character(1)))]
 for(group in groups){scenario<-group$scenario;subject<-group$subject;bydate<-split(group$records,vapply(group$records,function(r)r$date,character(1)));sets<-lapply(bydate,function(rows)sort(vapply(rows,function(r)r$facility_id,character(1))))
 if(any(vapply(sets,function(s)!identical(s,sets[[1]]),logical(1)))) .cr_error('SIMULATION_GRID','All facilities of assessed obligor require same explicit date grid')
 first<-NULL;first_reasons<-list();previous<-new.env(parent=emptyenv());last_nondefault<-NULL;default_lower<-NULL;initial_default<-FALSE
 for(date in sort(names(bydate))){dated<-bydate[[date]]
 for(detail in dated){fid<-detail$facility_id;row<-get(.cr_hash(list(scenario,fid,date)),seen)
 if(exists(fid,previous,inherits=FALSE)){last<-get(fid,previous);elapsed<-as.numeric(.cr_data_date(date,'date')-.cr_data_date(last$date,'date'));if(row$material_days_past_due>last$material_days_past_due+elapsed||(row$material_days_past_due>elapsed&&row$material_days_past_due!=last$material_days_past_due+elapsed)) .cr_error('SIMULATION_ARREARS','Continuous material-arrears counter must advance by calendar time or reset within interval')};assign(fid,row,previous)
 }
 utp<-any(vapply(dated,function(r)r$unlikely_to_pay,logical(1)));past_due<-any(vapply(dated,function(r)r$material_past_due_trigger,logical(1)));trigger<-utp||past_due;reasons<-as.list(c(if(utp)'UNLIKELY_TO_PAY',if(past_due)'MATERIAL_PAST_DUE'))
 if(trigger&&is.null(first)){first<-date;first_reasons<-reasons;default_lower<-last_nondefault;initial_default<-is.null(last_nondefault)}
 if(is.null(first))last_nondefault<-date
 assessments[[length(assessments)+1L]]<-list(scenario_id=scenario,subject_id=subject,date=date,assessment_level=policy$assessment_level,unlikely_to_pay=utp,material_past_due_trigger=past_due,regulatory_trigger=trigger,in_default=!is.null(first),first_default_date=first,trigger_reasons=reasons,economic_dual_breach=any(vapply(dated,function(r)r$economic_dual_breach,logical(1))))
 }
 first_events[[length(first_events)+1L]]<-list(scenario_id=scenario,subject_id=subject,defaulted=!is.null(first),first_default_date=first,reasons=first_reasons,initial_default=initial_default,last_nondefault_date=if(!is.null(first))default_lower else last_nondefault,observed_through=max(names(bydate)),default_time_status=if(is.null(first))'NO_DEFAULT_OBSERVED'else if(initial_default)'LEFT_CENSORED_AT_INITIAL_STATE'else'DETECTED_AT_GRID_ENDPOINT')
 }
 details<-details[order(vapply(details,function(r)r$scenario_id,character(1)),vapply(details,function(r)r$facility_id,character(1)),vapply(details,function(r)r$date,character(1)))]
 .cr_result(list(policy=policy,policy_hash=.cr_hash(policy),currency=currency,facility_tests=details,assessments=assessments,first_default_events=first_events),'simulation.detect_cashflow_default','default_events',ctx,diagnostics=list(input_fingerprint=.cr_hash(rows),default_definition='UTP_OR_MATERIAL_PAST_DUE',economic_test='DSCR_BREACH_AND_LTV_BREACH_SAME_DATE'),limitations=c('DSCR/LTV intersection is an economic model indicator. It never vetoes independently established UTP or material-arrears default. Nominal value coverage, security realisation or guarantee do not erase borrower default.','Materiality, valid continuous arrears counters, UTP evidence and applicable supervisory scope are supplied under versioned policy. No universal national implementation or automatic regulatory approval is claimed. First default dates are detection-grid endpoints, with previous nondefault endpoint reported. Initial defaults are left censored and must not enter initially performing risk denominator. Default is absorbing; cure and redefault require separate episode model.'))
}
