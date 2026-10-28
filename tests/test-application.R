if(!exists('build_application_snapshot')) library(creditratingsystem)
app_expect_code<-function(code,expr) { e<-tryCatch({force(expr);NULL},error=identity); stopifnot(inherits(e,'creditratingsystem_error'),identical(e$code,code)) }
app_row<-function(i='A')list(application_id=i,customer_id='C',product_id='consumer',population_id='new_customer',application_at='2024-01-01',information_cutoff='2024-01-01',requested_terms=list(amount=1000,currency='EUR'))
app_target<-list(event='first_default',horizon=30,time_unit='days',probability_measure='PHYSICAL',conditioning='funded',performance_origin='first_draw_at')
app_model<-list(schema_version='1.0',model_id='conditional-logit',model_version='v1',method_id='classification.fit_binary_glm',feature_order=list('income','offer_amount'),parameters=list(link='logit',fit_intercept=TRUE,coefficients=list(0,-1,0.001),offset_required=FALSE),preprocessing=list(transformation='identity'),target=app_target,training=list(data_fingerprint='reference',split_id='train',known_at='2023-12-31'),diagnostics=structure(list(),names=character()),provenance=list(data_origin='REFERENCE',parameter_origin='USER_SUPPLIED'),application_contract=list(product_ids=list('consumer'),population_ids=list('new_customer'),performance_origin='first_draw_at',term_stage='offered',term_features=list(amount='offer_amount')))
features<-list(list(application_id='A',feature='bureau',value=500,observed_at='2023-12-20',known_at='2023-12-21',source_id='v1'),list(application_id='A',feature='bureau',value=800,observed_at='2023-12-20',known_at='2024-01-02',source_id='revision'))
terms<-list(list(application_id='A',stage='booked',terms=list(amount=9999),effective_at='2024-01-02',known_at='2024-01-02'))
s<-build_application_snapshot(list(app_row()),features,terms,c('bureau','income'))$value[[1]]
stopifnot(s$features$bureau==500,is.null(s$terms$booked),s$terms$requested$amount==1000,identical(s$missing_features,list('income')))
features[[1]]$value<-0
stopifnot(s$features$bureau==500)
dup<-features[[1]]; dup$source_id<-'dup'
app_expect_code('APPLICATION_AMBIGUOUS_FEATURE',build_application_snapshot(list(app_row()),list(features[[1]],dup)))
parties<-list(list(party_id='P',role='borrower',liability_share=1,household_id='H'),list(party_id='Q',role='co_borrower',liability_share=1,household_id='H'),list(party_id='G',role='guarantor',liability_share=0.5))
incomes<-list(list(economic_id='salary',party_id='P',amount_monthly=3000,currency='EUR'),list(economic_id='salary',party_id='Q',amount_monthly=3000,currency='EUR'),list(economic_id='guarantor_salary',party_id='G',amount_monthly=10000,currency='EUR'))
debts<-lapply(c('P','Q'),function(p)list(economic_id='mortgage',party_id=p,amount_monthly=900,currency='EUR'))
v<-assemble_joint_application('A',parties,incomes,debts,'EUR')$value
stopifnot(v$monthly_income==3000,v$monthly_debt_service==900,v$dependency_flags$shared_obligation,v$parties[[3]]$role=='guarantor',length(v$excluded_items)==1)
debts[[2]]$amount_monthly<-901
app_expect_code('APPLICATION_AMBIGUOUS_ECONOMIC_ITEM',assemble_joint_application('A',parties,incomes,debts,'EUR'))
ids<-c('R','N','B','D','U','P'); apps<-lapply(ids,app_row)
decisions<-lapply(ids,function(i) { d<-list(decision_id=paste0('d',i),application_id=i,status=if(i=='R')'rejected' else 'approved',decided_at='2024-01-02',known_at='2024-01-02'); if(!(i %in% c('R','N'))) {d$accepted_at<-'2024-01-03';d$accepted_known_at<-'2024-01-03'};d })
bookings<-lapply(c('D','U','P'),function(i) {draw<-if(i=='U')'2024-02-01' else '2024-01-10'; b<-list(application_id=i,facility_id=paste0('F',i),booked_at='2024-01-04',known_at='2024-01-04',first_draw_at=draw,first_draw_known_at=draw); if(i=='P')b$outcome_available_at<-'2024-03-15';b})
events<-list(list(event_id='default',facility_id='FD',default_at='2024-02-09',known_at='2024-02-09'))
v<-build_application_targets(apps,decisions,bookings,events,list(),30,'2024-02-10')$value
stopifnot(identical(vapply(v,function(r)r$selection_status,character(1)),c('rejected','approved_not_taken_up','accepted_not_booked','funded','funded','funded')),v[[4]]$target==1,is.null(v[[5]]$target),is.null(v[[6]]$target),v[[4]]$outcome_end=='2024-02-09',v[[4]]$information_cutoff=='2024-01-01',v[[6]]$label_status=='outcome_pending',!v[[1]]$label_observed)
a<-list(app_row()); d<-list(list(decision_id='D',application_id='A',status='approved',decided_at='2024-01-02',known_at='2024-01-02',accepted_at='2024-01-03',accepted_known_at='2024-01-03')); b<-list(list(application_id='A',facility_id='F',booked_at='2024-01-04',known_at='2024-01-04',first_draw_at='2024-01-05',first_draw_known_at='2024-01-05'))
v<-build_application_targets(a,d,b,list(),list(list(facility_id='F',exit_at='2024-01-20',known_at='2024-01-20')),30,'2024-03-01')$value[[1]]
stopifnot(is.null(v$target),v$label_status=='exit')
d[[1]]$status<-'rejected'
app_expect_code('APPLICATION_SELECTION_CONFLICT',build_application_targets(a,d,b,list(),list(),30,'2024-03-01'))
features<-list(list(application_id='A',feature='income',value=2,observed_at='2023-12-01',known_at='2023-12-02',source_id='payroll'))
terms<-list(list(application_id='A',stage='offered',terms=list(amount=1000),effective_at='2024-01-01',known_at='2024-01-01'))
s<-build_application_snapshot(list(app_row()),features,terms,'income')$value[[1]]
r<-predict_application_risk(s,app_model,app_target)$value
stopifnot(abs(r$pd-1/(1+exp(1)))<1e-14,!r$calibrated)
bad<-app_model;bad$application_contract$product_ids<-list('mortgage')
app_expect_code('APPLICATION_APPLICABILITY',predict_application_risk(s,bad,app_target))
wrong<-app_target;wrong$horizon<-365
app_expect_code('APPLICATION_TARGET',predict_application_risk(s,app_model,wrong))
altered<-s;altered$features$income<-100
app_expect_code('APPLICATION_SNAPSHOT_INTEGRITY',predict_application_risk(altered,app_model,app_target))
empty<-build_application_snapshot(list(app_row()),list(),terms)$value[[1]]
app_expect_code('APPLICATION_MISSING_FEATURES',predict_application_risk(empty,app_model,app_target))
offers<-list(list(offer_id='one',terms=list(amount=1000)),list(offer_id='same',terms=list(amount=1000)),list(offer_id='outside',terms=list(amount=3000)))
v<-compare_offer_risk(s,offers,app_model,app_target,list(amount=c(500,2000)),lgd=0.4)$value
stopifnot(v[[1]]$pd==v[[2]]$pd,abs(v[[1]]$expected_loss-400/(1+exp(1)))<1e-12,v[[3]]$extrapolation,abs(v[[3]]$pd-1/(1+exp(-1)))<1e-14)
app_expect_code('APPLICATION_OFFER_MODEL',compare_offer_risk(s,list(list(offer_id='bad',terms=list(amount=1000,unmodelled_term=30))),app_model,app_target,list(amount=c(500,2000))))
cat('application business tests passed\n')

meta<-list(target=app_target,base_model_version='v1',population_definition='new_customer',outcome_policy='complete funded outcomes',weight_type='frequency',data_origin='REFERENCE',calibration=list(start='2023-01-01',end='2023-06-30',known_at='2023-12-31',split_id='cal',prediction_origin='EXTERNAL_TARGET'))
base<-1/(1+exp(1)); calibrator<-calibrate_intercept(c(base,base),0.4,metadata=meta)
risk<-predict_application_risk(s,app_model,app_target,calibrator)$value
stopifnot(abs(risk$pd-0.4)<1e-11,abs(risk$unadjusted_pd-base)<1e-14,risk$calibrated)
future<-app_row();future$application_at<-'2025-01-01';future$information_cutoff<-'2025-01-01'
app_expect_code('APPLICATION_COHORT_CUTOFF',build_application_targets(list(future),list(),list(),list(),list(),30,'2024-03-01'))
old_decision<-list(decision_id='D',application_id='A',status='rejected',decided_at='2023-12-31',known_at='2023-12-31')
app_expect_code('APPLICATION_DATE_ORDER',build_application_targets(list(app_row()),list(old_decision),list(),list(),list(),30,'2024-03-01'))

a<-app_row();a$information_cutoff<-'2024-04-01'
app_expect_code('APPLICATION_COHORT_CUTOFF',build_application_targets(list(a),list(),list(),list(),list(),30,'2024-03-01'))
d<-list(decision_id='D',application_id='A',status='rejected',decided_at='2024-01-02',known_at='2024-01-02',accepted_at='2024-01-03',accepted_known_at='2024-01-03')
app_expect_code('APPLICATION_SELECTION_CONFLICT',build_application_targets(list(app_row()),list(d),list(),list(),list(),30,'2024-03-01'))
