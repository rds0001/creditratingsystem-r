estimate_simulation_risk<-function(path_records,sampling_contract,confidence_level=.95,context=NULL){
 rows<-.cr_data_rows(path_records,'path_records');contract<-sampling_contract
 if(!.cr_object(contract)) .cr_error('SIMULATION_SAMPLING','Explicit sampling contract required')
 .cr_data_required(contract,c('contract_id','version','subject_id','currency','probability_measure','horizon','time_unit','conditioning','sampling_design','event'))
 for(key in c('contract_id','version','subject_id','currency','probability_measure','time_unit','conditioning','sampling_design','event')) .cr_sim_text(contract[[key]],key)
 horizon<-.cr_sim_number(contract$horizon,'horizon',0);level<-.cr_sim_number(confidence_level,'confidence_level',0)
 if(horizon<=0||level<=0||level>=1||!(contract$time_unit%in%c('days','months','years'))||!(contract$probability_measure%in%c('PHYSICAL','RISK_NEUTRAL'))||contract$conditioning!='initially_performing'||contract$event!='first_default') .cr_error('SIMULATION_SAMPLING','Declare positive horizon, common probability measure and initially_performing target')
 design<-contract$sampling_design;if(!(design%in%c('IID_UNWEIGHTED','FIXED_WEIGHTED_SCENARIOS','DEPENDENT_OR_QMC'))) .cr_error('SIMULATION_SAMPLING','Unsupported sampling design; importance sampling requires its own density-ratio estimator')
 ctx<-.cr_result(NULL,'simulation.context',context=context)$context
 for(key in c('currency','probability_measure','horizon','time_unit','conditioning'))if(!is.null(ctx[[key]])&&ctx[[key]]!=contract[[key]]) .cr_error('TARGET_MISMATCH',paste('Sampling target conflicts with context',key))
 seen<-character();active<-excluded<-list()
 for(row in rows){
 .cr_data_required(row,c('scenario_id','subject_id','initial_default','defaulted','horizon_complete','weight','ead_at_default','economic_loss'))
 if(row$scenario_id%in%seen) .cr_error('SIMULATION_DUPLICATE','One independent sampling unit per unique scenario required');seen<-c(seen,row$scenario_id)
 if(row$subject_id!=contract$subject_id) .cr_error('SIMULATION_SAMPLING','One subject per risk estimate; correlated obligors are not extra Monte Carlo trials')
 for(key in c('initial_default','defaulted','horizon_complete'))if(!is.logical(row[[key]])||length(row[[key]])!=1L||is.na(row[[key]])) .cr_error('SIMULATION_SAMPLING',paste(key,'must be Boolean'))
 weight<-.cr_sim_number(row$weight,'weight',0);ead<-.cr_sim_number(row$ead_at_default,'ead_at_default',0);loss<-.cr_sim_number(row$economic_loss,'economic_loss')
 if(design=='IID_UNWEIGHTED'&&weight!=1) .cr_error('SIMULATION_SAMPLING','IID exact-binomial calculations require unit trial weights')
 if(row$initial_default&&!row$defaulted) .cr_error('SIMULATION_SAMPLING','Initial default cannot become a nondefault first-event path')
 if(!row$defaulted&&(ead!=0||loss!=0)) .cr_error('SIMULATION_SAMPLING','Nondefault paths have zero default-conditioned EAD and loss')
 if(row$initial_default||weight==0){excluded[[length(excluded)+1L]]<-list(scenario_id=row$scenario_id,reason=if(row$initial_default)'INITIAL_DEFAULT'else'ZERO_WEIGHT');next}
 if(!row$horizon_complete) .cr_error('SIMULATION_CENSORING','Positive-weight performing trials require complete target horizon; do not discard informative censoring')
 active[[length(active)+1L]]<-c(weight,as.integer(row$defaulted),ead,loss)
 }
 value<-list(subject_id=contract$subject_id,currency=contract$currency,target=contract,sampling_design=design,input_trials=length(rows),risk_set_trials=length(active),excluded=excluded,confidence_level=level)
 if(!length(active)){
 value<-c(value,list(status='EMPTY_RISK_SET',pd=NULL,pd_interval=NULL,expected_loss=NULL,conditional_mean_loss=NULL,conditional_mean_ead=NULL,conditional_mean_lgd=NULL,ead_weighted_lgd=NULL,effective_weight_count=NULL,pd_mc_standard_error=NULL,loss_mc_standard_error=NULL))
 }else{
 a<-do.call(rbind,active);w<-a[,1];w<-w/max(w);w<-w/sum(w);if(any(w==0)) .cr_error('NUMERICAL_FAILURE','Positive scenario weight underflowed during normalization')
 default<-a[,2];ead<-a[,3];loss<-a[,4];pd<-sum(w[default==1])/sum(w);el<-sum(w*loss);k<-sum(default);n<-length(active);mass<-w*default;dm<-sum(mass)
 if(!is.finite(el)) .cr_error('NUMERICAL_FAILURE','Weighted monetary loss exceeds finite representation')
 conditional_loss<-if(dm==0)NULL else sum((mass/dm)*loss);conditional_ead<-if(dm==0)NULL else sum((mass/dm)*ead);individual_lgd<-NULL
 if(dm>0&&all(ead[default==1]>0)){
 ratios<-loss[default==1]/ead[default==1];if(any(!is.finite(ratios))) .cr_error('NUMERICAL_FAILURE','Individual LGD exceeds finite representation');individual_lgd<-sum((mass[default==1]/dm)*ratios)
 }
 weighted_lgd<-if(is.null(conditional_ead)||conditional_ead==0)NULL else conditional_loss/conditional_ead
 if(!is.null(weighted_lgd)&&!is.finite(weighted_lgd)) .cr_error('NUMERICAL_FAILURE','EAD-weighted LGD exceeds finite representation')
 interval<-pdse<-lossse<-NULL
 if(design=='IID_UNWEIGHTED'){
 alpha<-1-level;interval<-as.list(c(if(k==0)0 else stats::qbeta(alpha/2,k,n-k+1),if(k==n)1 else stats::qbeta(alpha/2,k+1,n-k,lower.tail=FALSE)));pdse<-sqrt(pd*(1-pd)/n)
 if(n>1){scale<-max(abs(loss));lossse<-if(scale==0)0 else scale*(stats::sd(loss/scale)/sqrt(n))}
 }
 value<-c(value,list(status='ESTIMABLE',pd=pd,pd_interval=interval,pd_interval_method=if(!is.null(interval))'CLOPPER_PEARSON_TWO_SIDED'else'NOT_AVAILABLE_FOR_SAMPLING_DESIGN',observed_default_trials=k,expected_loss=el,conditional_mean_loss=conditional_loss,conditional_mean_ead=conditional_ead,conditional_mean_lgd=individual_lgd,ead_weighted_lgd=weighted_lgd,effective_weight_count=1/sum(w*w),pd_mc_standard_error=pdse,loss_mc_standard_error=lossse,uncertainty_status=if(design=='IID_UNWEIGHTED')'IID_EXACT_PD_AND_SAMPLE_LOSS_MOMENTS'else'NO_INDEPENDENT_MONTE_CARLO_INTERVAL',pd_boundary_sample=k%in%c(0,n),zero_ead_default_trials=sum(default==1&ead==0)))
 }
 .cr_result(value,'simulation.estimate_simulation_risk','simulation_risk',ctx,diagnostics=list(input_fingerprint=.cr_hash(rows),contract_hash=.cr_hash(contract)),limitations='Independence and representative sampling are caller assumptions, not proven by path IDs. Exact binomial intervals apply only to IID unit-weight trials; Kish effective weight count is descriptive, never substituted into binomial calculations. Zero-default plug-in standard error zero is not certainty: exact interval has positive upper bound. Weighted scenarios and dependent/QMC points have no IID interval here. Expected loss is joint mean of default loss; separate mean EAD and mean LGD generally do not factor. Raw losses/LGD not capped. Initially defaulted paths excluded; incomplete positive-weight initially-performing paths rejected.')
}

importance_sample_defaults<-function(standard_normal_innovations,proposal_shift,evaluator,importance_contract,context=NULL){
 z<-standard_normal_innovations;shift<-unlist(proposal_shift);contract<-importance_contract
 if(!is.matrix(z)||!is.numeric(z)||any(!is.finite(z))||!nrow(z)||!ncol(z)||!is.numeric(shift)||length(shift)!=ncol(z)||any(!is.finite(shift))) .cr_error('SIMULATION_IMPORTANCE','Nonempty finite trial/coordinate matrix and complete shift vector required')
 if(!is.function(evaluator)||!.cr_object(contract)) .cr_error('SIMULATION_IMPORTANCE','Callable evaluator and explicit importance contract required')
 .cr_data_required(contract,c('contract_id','version','evaluator_id','evaluator_version','base_law','proposal','all_randomness_in_innovations','deterministic_given_innovations','sampling_design','subject_id','currency','probability_measure','event','conditioning','horizon','time_unit'))
 for(key in c('contract_id','version','evaluator_id','evaluator_version','base_law','proposal','sampling_design','subject_id','currency','probability_measure','event','conditioning','time_unit')) .cr_sim_text(contract[[key]],key)
 if(contract$base_law!='INDEPENDENT_STANDARD_NORMAL'||contract$proposal!='MEAN_SHIFT'||!identical(contract$all_randomness_in_innovations,TRUE)||!identical(contract$deterministic_given_innovations,TRUE)) .cr_error('SIMULATION_IMPORTANCE','All random coordinates must have the declared base law and be supplied to a deterministic evaluator')
 if(!(contract$sampling_design%in%c('IID','DEPENDENT_OR_QMC'))||contract$event!='first_default'||contract$conditioning!='initially_performing'||!(contract$probability_measure%in%c('PHYSICAL','RISK_NEUTRAL'))||!(contract$time_unit%in%c('days','months','years'))||.cr_sim_number(contract$horizon,'horizon',0)<=0) .cr_error('SIMULATION_IMPORTANCE','Explicit initially-performing first-default target and sampling design required')
 ctx<-.cr_result(NULL,'simulation.context',context=context)$context
 for(key in c('currency','probability_measure','horizon','time_unit','conditioning'))if(!is.null(ctx[[key]])&&ctx[[key]]!=contract[[key]]) .cr_error('TARGET_MISMATCH',paste('Importance target conflicts with context',key))
 x<-sweep(z,2,shift,'+');norm2<-sum(shift*shift);logw<-as.numeric(-z%*%shift)-.5*norm2;weights<-exp(logw)
 if(any(!is.finite(x))||!is.finite(norm2)||any(!is.finite(weights))||any(weights<=0)) .cr_error('SIMULATION_IMPORTANCE_RANGE','Proposal density ratios must remain finite and positive; no clipping or silent weight truncation is performed')
 raw<-tryCatch(evaluator(x),error=function(e) .cr_error('SIMULATION_EVALUATOR',paste('Target evaluator failed:',conditionMessage(e))))
 result<-if(inherits(raw,'CalcResult'))raw$value else raw
 if(!.cr_object(result)) .cr_error('SIMULATION_EVALUATOR','Evaluator must return defaulted and economic_loss vectors')
 .cr_data_required(result,c('defaulted','economic_loss'));default<-unlist(result$defaulted);loss<-unlist(result$economic_loss)
 if(!(is.numeric(default)||is.logical(default))||length(default)!=nrow(z)||any(!is.finite(default))||any(!(default%in%c(0,1)))) .cr_error('SIMULATION_EVALUATOR','One binary default outcome per proposal trial required')
 default<-as.numeric(default)
 if(!is.numeric(loss)||length(loss)!=length(default)||any(!is.finite(loss))||any(loss[default==0]!=0)) .cr_error('SIMULATION_EVALUATOR','One finite monetary default loss per trial; nondefaults have zero default loss')
 pc<-weights*default;lc<-weights*loss
 if(any(!is.finite(lc))||any(loss!=0&lc==0)) .cr_error('SIMULATION_IMPORTANCE_RANGE','Weighted losses exceed finite representation; weights are never silently truncated')
 n<-nrow(z)
 moments<-function(v){scale<-max(abs(v));se<-NULL;if(scale==0)return(list(mean=0,se=if(n>1&&contract$sampling_design=='IID')0 else NULL));u<-v/scale;if(n>1&&contract$sampling_design=='IID')se<-scale*(stats::sd(u)/sqrt(n));list(mean=scale*(sum(u)/n),se=se)}
 pd<-moments(pc);el<-moments(lc);mw<-moments(weights);relative<-weights/max(weights);sr<-sum(relative);normalized<-relative/sr;ess<-sr*sr/sum(relative*relative)
 value<-list(contract=contract,proposal_shift=as.list(shift),proposal_innovations=lapply(seq_len(n),function(i)as.list(x[i,])),log_likelihood_ratios=as.list(logw),likelihood_ratios=as.list(weights),defaulted=as.list(as.logical(default)),economic_loss=as.list(loss),weighted_default_contributions=as.list(pc),weighted_loss_contributions=as.list(lc),pd_estimate=pd$mean,expected_loss_estimate=el$mean,pd_mc_standard_error=pd$se,loss_mc_standard_error=el$se,mean_likelihood_ratio=mw$mean,self_normalized_pd=sum(normalized*default),effective_weight_count=ess,max_normalized_weight=max(normalized),proposal_default_count=sum(default),trials=n,pd_estimate_in_unit_interval=pd$mean>=0&&pd$mean<=1,zero_proposal_defaults=!any(default!=0),target_zero_default_bound=NULL,uncertainty_status=if(contract$sampling_design=='IID')'IID_SAMPLE_CONTRIBUTION_MOMENTS'else'NO_IID_MONTE_CARLO_INTERVAL')
 .cr_result(value,'simulation.importance_sample_defaults','importance_sampling',ctx,diagnostics=list(innovation_fingerprint=.cr_hash(lapply(seq_len(n),function(i)as.list(z[i,]))),contract_hash=.cr_hash(contract)),limitations='Density ratio includes every shifted standard-normal coordinate. Ordinary importance mean is unbiased only for IID proposal draws with complete support. Dependent/QMC inputs have no IID uncertainty. Finite-sample PD may exceed one and is not clipped. Self-normalized PD is separately labelled and generally biased. Zero proposal defaults give no exact target-PD upper bound; zero sample standard error is not certainty. Effective weight count is concentration, not rare-event precision. Local evaluator ID/version is caller-declared; all randomness must be supplied in innovations.')
}
