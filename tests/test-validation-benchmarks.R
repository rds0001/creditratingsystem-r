if(!exists('benchmark_ratings',mode='function'))library(creditratingsystem)
target<-list(event='default',horizon=30,time_unit='days',probability_measure='PHYSICAL',conditioning='performing')
d<-list(model_id='reference',model_version='1',parameter_version='1',data_fingerprint='paired-cohorts',population_id='performing',split_id='holdout',prediction_origin='HOLDOUT',evaluation_cutoff='2020-04-01',target=target)
dimensions<-list(rating_type='issuer',support_concept='standalone',currency='EUR',rating_philosophy='TTC')
rows<-lapply(0:3,function(i)c(list(prediction_id=paste0('p',i),entity_id=paste0('e',i),cluster_id=paste0('g',i),segment='all',as_of='2020-02-01',forecast_known_at='2020-02-01',horizon_end='2020-03-02',model_id='reference',model_version='1',parameter_version='1',rating_unit_id=paste0('u',i),scale_id='internal',scale_version='1',rating_status='ACTIVE',grade=c('A','B','C','A')[i+1]),dimensions))
external<-lapply(0:3,function(i)c(list(benchmark_id=paste0('b',i),entity_id=paste0('e',i),rating_unit_id=paste0('u',i),provider_id='provider',scale_id='external',scale_version='1',status='ACTIVE',grade=c('x','x','w','y')[i+1],rating_at='2020-01-01',known_at='2020-01-01',target=target),dimensions))
mapping<-list(mapping_id='m',version='1',reference='frozen',known_at='2019-01-01',common_grades=c('1','2','3'),internal=list(scale_id='internal',scale_version='1',grades=c('A','B','C')),benchmark=list(scale_id='external',scale_version='1',grades=c('x','y','z','w')),internal_to_common=list(A='1',B='2',C='3'),benchmark_to_common=list(x='1',y='2',z='2',w='3'))
policy<-list(benchmark_origin='EXTERNAL_JUDGEMENT',method='latest_known',watch_policy='include',kappa_weighting='quadratic',provider_id='provider',max_age_days=40)
run<-function(ext=external,p=policy,uncertainty='none',draws=NULL)benchmark_ratings(rows,ext,scale_mapping=mapping,matching_policy=p,design=d,uncertainty=uncertainty,cluster_resamples=draws)$value
m<-run()$metrics
stopifnot(m$exact_agreement==.5,m$within_one_notch==1,m$mean_signed_notches==0,m$mean_absolute_notches==.5,m$observed_weighted_agreement==.875,m$expected_weighted_agreement==.65625,abs(m$kappa-7/11)<1e-14,abs(m$spearman_rank_correlation-.5)<1e-14)
r<-run(uncertainty='cluster_bootstrap',draws=list(rep('g0',4),rep('g3',4)))
stopifnot(r$uncertainty$intervals$kappa$reason=='NON_ESTIMABLE_RESAMPLE')
w<-external[[1]];w$benchmark_id<-'withdraw';w$status<-'WITHDRAWN';w['grade']<-list(NULL);w$rating_at<-w$known_at<-'2020-01-15';r<-run(c(external,list(w)));stopifnot(r$coverage$excluded_count==1,r$coverage$exclusion_counts$WITHDRAWN_BENCHMARK==1)
w$known_at<-'2020-03-01';stopifnot(run(c(external,list(w)))$coverage$included_count==4)
p<-policy;p$max_age_days<-20;stopifnot(run(p=p)$applicability=='NOT_APPLICABLE')
w<-external[[1]];w$benchmark_id<-'duplicate';stopifnot(inherits(try(run(c(external,list(w))),silent=TRUE),'try-error'))
# Real public cohort construction and independently known paired score differences.
mt<-list(event='default',probability_measure='PHYSICAL',default_definition='contract-default-v1',unit='obligor',horizon_days=30,default_grade='D')
events<-lapply(c(1,3),function(i)list(event_id=paste0('d',i),entity_id=paste0('e',i),event='default',event_at='2020-01-15',known_at='2020-01-16',default_definition=mt$default_definition))
coverage<-lapply(0:3,function(i)list(entity_id=paste0('e',i),complete_through='2020-01-31',known_at='2020-02-01'))
entries<-list()
for(model in c('reference','candidate')){pd<-if(model=='reference')c(.1,.8,.3,.6)else c(.2,.7,.4,.5);predictions<-lapply(0:3,function(i)list(forecast_id=paste0(model,i),case_id=paste0('c',i),entity_id=paste0('e',i),cluster_id=paste0('g',i),cohort='2020-01',segment='all',model_id=model,model_version='1',forecast_at='2020-01-01',known_at='2020-01-01',pd=pd[i+1],weight=1,target=mt,prediction_origin='HOLDOUT'));entries[[model]]<-list(cohort=build_monitoring_cohorts(predictions,events,coverage,target_contract=mt,evaluation_cutoff='2020-03-01')$value,model_version='1',parameter_version='1',model_fingerprint=paste0('frozen-',model),training_known_at='2019-01-01')}
d$evaluation_cutoff<-'2020-03-01';r<-benchmark_predictions(entries,reference_model_id='reference',metrics=c('brier','auc'),matching_policy='strict',multiple_comparisons='bonferroni',design=d,uncertainty='cluster_bootstrap',cluster_resamples=list(rep('g0',4),rep('g3',4)),confidence_level=.8)$value
stopifnot(abs(r$metrics$paired_differences$candidate$brier$difference-.06)<1e-14,r$metrics$paired_differences$candidate$auc$difference==0,abs(r$uncertainty$intervals[['candidate/brier']]$lower-.033)<1e-14,abs(r$uncertainty$intervals[['candidate/brier']]$upper-.087)<1e-14,r$uncertainty$intervals[['candidate/auc']]$reason=='NON_ESTIMABLE_RESAMPLE')
