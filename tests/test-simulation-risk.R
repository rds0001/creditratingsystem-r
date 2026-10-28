if(!exists('estimate_simulation_risk'))library(creditratingsystem)
sr_sampling<-function(design='IID_UNWEIGHTED')list(contract_id='mc',version='1',subject_id='o',currency='EUR',probability_measure='PHYSICAL',horizon=1,time_unit='years',conditioning='initially_performing',sampling_design=design,event='first_default')
sr_trial<-function(i,defaulted=FALSE,ead=0,loss=0,weight=1,initial=FALSE,complete=TRUE)list(scenario_id=as.character(i),subject_id='o',initial_default=initial,defaulted=defaulted,horizon_complete=complete,weight=weight,ead_at_default=ead,economic_loss=loss)
rows<-c(lapply(0:9,sr_trial),list(sr_trial('old',TRUE,100,50,initial=TRUE)));r<-estimate_simulation_risk(rows,sr_sampling())$value
stopifnot(r$pd==0,r$risk_set_trials==10,max(abs(unlist(r$pd_interval)-c(0,1-.025^.1)))<1e-13,r$pd_mc_standard_error==0,r$pd_boundary_sample,is.null(r$conditional_mean_lgd),identical(r$excluded,list(list(scenario_id='old',reason='INITIAL_DEFAULT'))))
rows<-list(sr_trial('a',TRUE,100,20),sr_trial('b',TRUE,200,160),sr_trial('c'));r<-estimate_simulation_risk(rows,sr_sampling())$value
stopifnot(abs(3*r$pd_interval[[1]]^2-2*r$pd_interval[[1]]^3-.025)<1e-13,abs(r$pd_interval[[2]]-.975^(1/3))<1e-13)
stopifnot(abs(r$pd-2/3)<1e-14,abs(r$expected_loss-60)<1e-12,r$conditional_mean_ead==150,r$conditional_mean_lgd==.5,r$ead_weighted_lgd==.6,abs(r$pd*r$conditional_mean_ead*r$conditional_mean_lgd-50)<1e-12,abs(r$pd*r$conditional_mean_ead*r$ead_weighted_lgd-r$expected_loss)<1e-12)
rows<-list(sr_trial('a',TRUE,100,20,.1),sr_trial('b',TRUE,200,160,.2),sr_trial('c',weight=.7))
for(design in c('FIXED_WEIGHTED_SCENARIOS','DEPENDENT_OR_QMC')){r<-estimate_simulation_risk(rows,sr_sampling(design))$value;stopifnot(abs(r$pd-.3)<1e-14,abs(r$expected_loss-34)<1e-12,is.null(r$pd_interval),is.null(r$loss_mc_standard_error),abs(r$effective_weight_count-1/.54)<1e-13)}
sr_error<-function(code,expr){e<-tryCatch(force(expr),error=identity);stopifnot(inherits(e,'creditratingsystem_error'),e$code==code)}
sr_error('SIMULATION_SAMPLING',estimate_simulation_risk(rows,sr_sampling()))
r<-estimate_simulation_risk(list(sr_trial('a',TRUE,0,10)),sr_sampling())$value;stopifnot(r$expected_loss==10,is.null(r$conditional_mean_lgd),is.null(r$ead_weighted_lgd))
r<-estimate_simulation_risk(list(sr_trial('a',TRUE,100,150)),sr_sampling())$value;stopifnot(r$conditional_mean_lgd==1.5)
r<-estimate_simulation_risk(list(),sr_sampling())$value;stopifnot(r$status=='EMPTY_RISK_SET',is.null(r$pd))
sr_error('SIMULATION_CENSORING',estimate_simulation_risk(list(sr_trial('a',complete=FALSE)),sr_sampling()))
sr_error('SIMULATION_DUPLICATE',estimate_simulation_risk(list(sr_trial('a'),sr_trial('a')),sr_sampling()))
cat('Native sampling-aware simulation risk tests PASS\n')
