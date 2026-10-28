library(creditratingsystem)
local({
for(helper in c('.cr_mkt_predict_bridge','.cr_mkt_seal')) if(!exists(helper,mode='function')) assign(helper,getFromNamespace(helper,'creditratingsystem'))
near_market <- function(x,y,tol=1e-8) stopifnot(length(x)==length(y),all(abs(x-y)<=tol))
error_market <- function(expr,code) { e<-tryCatch(force(expr),error=identity); stopifnot(inherits(e,'creditratingsystem_error'),identical(e$code,code)) }
curve_market <- function(rate=.02,hazard=FALSE) { x<-list(as_of='2025-01-01',known_at='2025-01-01',currency='EUR',time_basis='ACT/365F',extrapolation='FLAT_LAST_RATE',knots=list(1,3),rates=list(rate,rate)); if(hazard) x$probability_measure<-'RISK_NEUTRAL'; x }
day_market <- function(t) format(as.Date('2025-01-01')+t,'%Y-%m-%d')
cds_market <- function(days=365) list(as_of=day_market(0),currency='EUR',probability_measure='RISK_NEUTRAL',default_settlement='AT_DEFAULT',recovery=.4,coupon=.012,protection_start=day_market(0),maturity=day_market(days),settlement_date=day_market(0),accrued_premium=0,upfront=0,premium_mode='CONTINUOUS',instrument_type='CDS',issuer_id='A',seniority='SENIOR')
bond_market <- function() list(as_of=day_market(0),currency='EUR',probability_measure='RISK_NEUTRAL',default_settlement='AT_DEFAULT',recovery=.4,liquidity_spread=0,recovery_convention='PAR',cashflow_assumption='CONTRACTUAL_NONOPTIONAL',outstanding_principal=100,accrued_interest=0,settlement_date=day_market(0),schedule=list(list(payment_date=day_market(365),coupon_amount=5,principal_amount=100)),instrument_type='BOND',issuer_id='A',seniority='SENIOR')
flat_market<-cds_expected_cashflow_legs(curve_market(.02,TRUE),curve_market(.03),cds_market())$value
near_market(flat_market$par_spread,.012); near_market(flat_market$protection_pv,.6*.02*(-expm1(-.05))/.05); near_market(flat_market$upfront_residual_pv,0)
stopifnot(cds_expected_cashflow_legs(curve_market(0,TRUE),curve_market(.03),cds_market())$value$protection_pv==0)
h<-curve_market(.02,TRUE); h$knots<-list(.3,1); h$rates<-list(.02,.05); d<-curve_market(.03); c<-cds_market(); c$premium_mode<-'SCHEDULED'; c$day_count<-'ACT/360'; c$accrual_on_default<-TRUE; c$schedule<-list(list(accrual_start=day_market(0),accrual_end=day_market(180),payment_date=day_market(182)),list(accrual_start=day_market(180),accrual_end=day_market(365),payment_date=day_market(367)))
scheduled_market<-cds_expected_cashflow_legs(h,d,c)$value
surv_market<-function(t) exp(-.02*pmin(t,.3)-.05*pmax(0,t-.3))
accrual_oracle<-sum(vapply(list(c(0,.3,0),c(.3,180/365,0),c(180/365,1,180/365)),function(x) integrate(function(t) exp(-.03*t)*surv_market(t)*ifelse(t<.3,.02,.05)*(t-x[3])*365/360,x[1],x[2],abs.tol=1e-13)$value,numeric(1)))
near_market(scheduled_market$accrual_on_default_annuity,accrual_oracle,1e-12)
bonds_market<-list()
for(convention in c('PAR','TREASURY','MARKET_VALUE')) {
 b<-bond_market(); b$recovery_convention<-convention; v<-risky_bond_expected_cashflows(curve_market(.02,TRUE),curve_market(.03),b)$value
 expected<-switch(convention,PAR=105*exp(-.05)+40*.02*(-expm1(-.05))/.05,TREASURY=105*exp(-.03)*(.4+.6*exp(-.02)),MARKET_VALUE=105*exp(-.03-.6*.02))
 near_market(v$dirty_price,expected); near_market(v$dirty_price,v$coupon_pv+v$principal_pv+v$recovery_pv); near_market(risky_bond_expected_cashflows(curve_market(0,TRUE),curve_market(.03),b)$value$dirty_price,105*exp(-.03)); bonds_market[[convention]]<-v
}
h<-curve_market(.02,TRUE); h$rates<-list(.02,.05); cds_rows_market<-bond_rows_market<-list()
for(i in 1:2) {
 days<-c(365,1095)[i]; ins<-cds_market(days); value<-cds_expected_cashflow_legs(h,curve_market(.03),ins)$value$par_spread
 cds_rows_market[[i]]<-list(quote_id=as.character(i-1L),eligible=TRUE,observed_at=day_market(0),known_at=day_market(0),instrument=ins,normalized_unit='FRACTION',quote_type='CDS_SPREAD',value=value)
 ins<-bond_market(); ins$schedule<-list(list(payment_date=day_market(days),coupon_amount=5,principal_amount=100)); value<-risky_bond_expected_cashflows(h,curve_market(.03),ins)$value$clean_price_per_100
 bond_rows_market[[i]]<-list(quote_id=as.character(i-1L),eligible=TRUE,observed_at=day_market(0),known_at=day_market(0),instrument=ins,normalized_unit='PER_100_PAR',quote_type='BOND_CLEAN_PRICE',value=value,price_standard_error=.1)
}
cds_contract_market<-list(recovery=.4,maximum_hazard=2,price_tolerance=1e-11)
cds_fit_market<-bootstrap_cds_hazard_curve(cds_rows_market,curve_market(.03),cds_contract_market)$value
near_market(unlist(cds_fit_market$hazard_curve$rates),c(.02,.05),1e-12)
bad<-cds_rows_market; bad[[2]]$value<-0; stopifnot(!bootstrap_cds_hazard_curve(bad,curve_market(.03),cds_contract_market)$value$feasible)
bond_contract_market<-list(hazard_knots=list(1,3),initial_hazards=list(.03,.03),maximum_hazard=2,recovery=.4,liquidity_spread=0,weight_interpretation='KNOWN_GAUSSIAN_PRICE_STANDARD_ERRORS',max_evaluations=200)
bond_fit_market<-calibrate_bond_hazard_curve(bond_rows_market,curve_market(.03),bond_contract_market)$value
near_market(unlist(bond_fit_market$hazard_curve$rates),c(.02,.05),1e-8); stopifnot(bond_fit_market$identifiability$full_column_rank,length(bond_fit_market$parameter_intervals)==2,bond_fit_market$objective<1e-12)
row<-cds_rows_market[[1]]; row$quote_id<-'q'; row$value<-.012; contract<-cds_contract_market; contract$price_tolerance<-1e-12
one_fit_market<-bootstrap_cds_hazard_curve(list(row),curve_market(.03),contract)$value
alternatives_market<-list(list(scenario_id='low',recovery=.2),list(scenario_id='high',recovery=.6)); diagnostic_contract_market<-list(equivalence_tolerance=1e-8,quote_standard_errors=list(.0001),recovery_bounds=list(0,.9))
identification_market<-diagnose_market_credit_identifiability(one_fit_market,alternatives_market,diagnostic_contract_market)$value
stopifnot(identification_market$joint_identifiability$rank==1,length(identification_market$equivalent_scenario_ids)==2)
near_market(vapply(identification_market$profiles,function(r) r$hazards[[1]],numeric(1)),c(.015,.03),1e-12)
diagnostic_contract_market$independent_recovery_observation<-list(value=.4,standard_error=.01,evidence_id='independent-recovery-study',known_at=day_market(0))
identified_market<-diagnose_market_credit_identifiability(one_fit_market,alternatives_market,diagnostic_contract_market)$value
stopifnot(identified_market$joint_identifiability$full_column_rank,!length(identified_market$equivalent_scenario_ids))
basis_contract_market<-list(recovery=.4,bond_liquidity_spread=0,funding_treatment='NO_ADDITIONAL_FUNDING_WEDGE',maximum_quote_age_days=5,comparison_cds_contract=cds_market(1095))
basis_market<-compare_cds_bond_credit_basis(cds_fit_market,bond_fit_market,basis_contract_market)$value
stopifnot(basis_market$comparable); near_market(basis_market$basis,0,1e-9)
bad<-bond_fit_market; bad$calibration_hash<-NULL; bad$seniority<-'SUBORDINATED'; bad<-.cr_mkt_seal(bad); mismatch<-compare_cds_bond_credit_basis(cds_fit_market,bad,basis_contract_market)$value; stopifnot(!mismatch$comparable,is.null(mismatch$basis),'SENIORITY_MISMATCH' %in% unlist(mismatch$findings))
rows_market<-list()
for(asof in c('2018-01-01','2020-01-01','2022-01-01')) {
 end<-paste0(as.integer(substr(asof,1,4))+1,substr(asof,5,10))
 for(i in 0:19) {
  y<-as.integer(i%%5==0 || i%%7==0); n<-length(rows_market)
  rows_market[[n+1L]]<-list(observation_id=as.character(n),entity_id=paste0('e',n),as_of=asof,label_known_at=end,complete_through=end,market_curve_as_of=asof,market_curve_known_at=asof,features_known_at=asof,market_curve_hash=paste0('historical-',n),outcome_origin='OBSERVED_ISSUER_DEFAULT',initial_state='PERFORMING',outcome=y,default_date=if(y) paste0(substr(asof,1,4),'-06-01') else NULL,features=list(q_pd=i%%4/10+.05))
 }
}
target_market<-list(event='first_default',horizon=1,time_unit='years',probability_measure='PHYSICAL',conditioning='initially_performing')
bridge_contract_market<-list(market_feature_contracts=list(q_pd=list(probability_measure='RISK_NEUTRAL',horizon=1,time_unit='years',definition='One year cumulative Q first-default probability')),target=target_market,feature_probability_measure='RISK_NEUTRAL',outcome_origin='OBSERVED_ISSUER_DEFAULT',target_population='quoted issuers',population_selection='MARKET_QUOTED_ISSUERS',transformation='IDENTITY_PREDECLARED_FEATURES',known_at='2023-01-01',split_id='forward-entity',link='logit',data_origin='SYNTHETIC')
folds_market<-list(list(train=as.list(0:19),validation=as.list(20:39)),list(train=as.list(0:39),validation=as.list(40:59)))
bridge_market<-fit_market_to_physical_pd_bridge(rows_market,'q_pd',bridge_contract_market,folds_market,'bridge','1')
stopifnot(all(vapply(bridge_market$diagnostics$out_of_time_pd[1:20],is.null,logical(1))),bridge_market$diagnostics$observed_evaluation_count==40)
xx<-matrix(vapply(rows_market,function(r) r$features$q_pd,numeric(1)),ncol=1); yy<-vapply(rows_market,function(r) r$outcome,numeric(1))
direct<-fit_binary_glm(xx,yy,link='logit',feature_order='q_pd',fit_intercept=TRUE,target=target_market,training=bridge_market$model$training,model_id='direct',model_version='1')
bridge_predictions_market<-.cr_mkt_predict_bridge(bridge_market$model,matrix(c(.1,.3),ncol=1),'q_pd',target_market)$value
near_market(unlist(bridge_predictions_market),unlist(predict_pd(direct,matrix(c(.1,.3),ncol=1),'q_pd',target_market)$value),1e-12)
bad<-rows_market; bad[[1]]$market_curve_known_at<-'2019-01-01'; error_market(fit_market_to_physical_pd_bridge(bad,'q_pd',bridge_contract_market,folds_market,'bridge','1'),'LOOKAHEAD_BIAS')
bad<-rows_market; bad[[21]]$entity_id<-bad[[1]]$entity_id; error_market(fit_market_to_physical_pd_bridge(bad,'q_pd',bridge_contract_market,folds_market,'bridge','1'),'LEAKAGE')
if(nzchar(Sys.getenv('CREDITRATING_MARKET_REPORT'))) writeLines(jsonlite::toJSON(list(flat=flat_market,scheduled=scheduled_market,bonds=bonds_market,cds=cds_fit_market,bond=bond_fit_market,identification=identification_market,identified=identified_market,basis=basis_market,bridge=bridge_market$model,bridge_predictions=bridge_predictions_market),auto_unbox=TRUE,digits=17,null='null'),Sys.getenv('CREDITRATING_MARKET_REPORT'))

# Full recursive bootstrap derivative against an independently re-solved bumped quote.
bumped<-cds_rows_market; bumped[[1]]$value<-bumped[[1]]$value+1e-7
refit<-bootstrap_cds_hazard_curve(bumped,curve_market(.03),cds_contract_market)$value
difference<-(unlist(refit$hazard_curve$rates)-unlist(cds_fit_market$hazard_curve$rates))/1e-7
near_market(difference,vapply(cds_fit_market$curve_sensitivities$hazard_per_quote,function(r) r[[1]],numeric(1)),2e-6); stopifnot(difference[2]<0)
deficient<-calibrate_bond_hazard_curve(bond_rows_market[1],curve_market(.03),bond_contract_market)$value
stopifnot(!deficient$identifiability$full_column_rank,!length(deficient$parameter_intervals),is.null(deficient$covariance))
row<-cds_rows_market[[1]]; row$value<-0; zero_contract<-cds_contract_market; zero_contract$recovery<-1
zero<-bootstrap_cds_hazard_curve(list(row),curve_market(.03),zero_contract)$value
stopifnot(zero$feasible,zero$curve_sensitivities$identifiability$rank==0,is.null(zero$curve_sensitivities$hazard_per_quote))
b<-bond_market(); b$schedule<-list(list(payment_date=day_market(365),coupon_amount=4,principal_amount=40),list(payment_date=day_market(730),coupon_amount=2.4,principal_amount=60)); b$liquidity_spread<-.01
v<-risky_bond_expected_cashflows(curve_market(.02,TRUE),curve_market(.03),b)$value
i<--expm1(-.06)/.06; expected<-44*exp(-.06)+62.4*exp(-.12)+.4*.02*i*(100+60*exp(-.06))
near_market(v$dirty_price,expected); stopifnot(v$credit_price_effect<0,v$liquidity_price_effect<0)
b$recovery<-0; near_market(risky_bond_expected_cashflows(curve_market(.02,TRUE),curve_market(.03),b)$value$dirty_price,44*exp(-.06)+62.4*exp(-.12))
ins<-cds_market(); ins$instrument_id<-'c'; q<-list(instrument_id='c',quote_type='CDS_SPREAD',unit='BASIS_POINTS',observed_at=day_market(0),known_at=day_market(0))
qc<-list(as_of=day_market(0),known_at=day_market(0),currency='EUR',maximum_age_days=5,stale_policy='EXCLUDE',bid_ask_policy='OPTIONAL')
q1<-c(q,list(quote_id='a',value=100)); q2<-c(q,list(quote_id='b',value=110))
snapshot_market<-build_credit_quote_snapshot(list(q1,q2),list(ins),curve_market(.03),qc)$value
stopifnot(all(vapply(snapshot_market$quotes,function(r) !r$eligible && 'CONFLICTING_SAME_VINTAGE_QUOTES' %in% unlist(r$findings),logical(1))))
d<-curve_market(.03); d$known_at<-'2024-12-31'; fit<-bootstrap_cds_hazard_curve(cds_rows_market[1],d,cds_contract_market)$value; stopifnot(fit$known_at==day_market(0),fit$hazard_curve$known_at==day_market(0))
if(identical(tolower(Sys.getenv('CREDITRATINGSYSTEM_EXTENDED_TESTS')),'true')) for(convention in c('PAR','TREASURY','MARKET_VALUE')) for(recovery in c(0,.2,.7)) for(liquidity in c(0,.005)) {
 rows<-bond_rows_market; cc<-bond_contract_market; cc$recovery<-recovery; cc$liquidity_spread<-liquidity; h<-curve_market(.02,TRUE); h$rates<-list(.02,.05)
 for(j in seq_along(rows)) { rows[[j]]$instrument$recovery<-recovery; rows[[j]]$instrument$liquidity_spread<-liquidity; rows[[j]]$instrument$recovery_convention<-convention; rows[[j]]$value<-risky_bond_expected_cashflows(h,curve_market(.03),rows[[j]]$instrument)$value$clean_price_per_100 }
 fit<-calibrate_bond_hazard_curve(rows,curve_market(.03),cc)$value; near_market(unlist(fit$hazard_curve$rates),c(.02,.05),2e-7)
}
public_market<-predict_pd(bridge_market,matrix(c(.1,.3),ncol=1),'q_pd',target_market)$value
near_market(unlist(public_market),unlist(bridge_predictions_market),1e-15)
path_market<-tempfile('market-bridge-',tmpdir=tempdir(),fileext='.json')
serialize_fitted_model(bridge_market,path_market); frozen_market<-load_fitted_model(path_market)$value
near_market(unlist(predict_pd(frozen_market,matrix(c(.1,.3),ncol=1),'q_pd',target_market)$value),unlist(public_market),1e-15)
error_market(predict_pd(frozen_market,matrix(c(.1,.3),ncol=1),'q_pd',target_market,offset=c(0,0)),'OFFSET_MISMATCH')
b<-bond_market(); b$recovery<-0; b$schedule<-list(list(payment_date=day_market(730),coupon_amount=0,principal_amount=100)); observed<-100*exp(-.123*2)
row<-list(quote_id='closed-form',eligible=TRUE,observed_at=day_market(0),known_at=day_market(0),instrument=b,normalized_unit='PER_100_PAR',quote_type='BOND_CLEAN_PRICE',value=observed,price_standard_error=.1)
cc<-list(hazard_knots=list(2),initial_hazards=list(.03),maximum_hazard=2,recovery=0,liquidity_spread=0,weight_interpretation='KNOWN_GAUSSIAN_PRICE_STANDARD_ERRORS',max_evaluations=100)
fit<-calibrate_bond_hazard_curve(list(row),curve_market(0),cc)$value
near_market(fit$hazard_curve$rates[[1]],-log(observed/100)/2,1e-10)
near_market(fit$covariance[[1]][[1]],1/(2*observed/.1)^2,1e-12)
bad_ctx<-build_calculation_context('2025-01-02','2025-01-02',probability_measure='RISK_NEUTRAL')
error_market(cds_expected_cashflow_legs(curve_market(.02,TRUE),curve_market(.03),cds_market(),context=bad_ctx),'CONTEXT_MISMATCH')
old_ctx<-build_calculation_context('2025-01-01','2024-12-31',probability_measure='RISK_NEUTRAL')
error_market(risky_bond_expected_cashflows(curve_market(.02,TRUE),curve_market(.03),bond_market(),context=old_ctx),'LOOKAHEAD_BIAS')
})
