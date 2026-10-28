library(creditratingsystem)
T<-list(event='default',horizon=1,time_unit='years',probability_measure='PHYSICAL',conditioning='performing');C<-list(population_id='retail',reference='documented synthetic elicitation',known_at='2024-01-01',expert_ids=list('e1'),evidence_ids=list('prior-evidence'),target=T);O<-list(population_id='retail',known_at='2025-01-01',evidence_ids=list('cohort-2024'),target=T,conditional_iid=TRUE,outcome_complete=TRUE)
close<-function(x,y,tol=1e-8)stopifnot(max(abs(unlist(x)-unlist(y)))<tol)
reject<-function(expr,code) {e<-tryCatch(force(expr),error=identity);stopifnot(inherits(e,'error'),grepl(code,e$code,fixed=TRUE))}
p<-fit_elicited_beta_prior(list(method='moments',mean=.1,concentration=20),C);close(c(p$value$alpha,p$value$beta),c(2,18))
q<-update_expert_beta_binomial(p,3,100,O,predictive_exposures=2)$value;close(c(q$posterior$alpha,q$posterior$beta),c(5,115));close(q$predictive_probabilities,c(115*116,2*5*115,5*6)/(120*121));close(q$prior_weight,1/6)
reject(update_expert_beta_binomial(q$posterior,3,100,O),'DOUBLE_COUNT');reject(fit_elicited_beta_prior(list(method='moments',mean=.1,variance=.1),C),'EXPERT_ELICITATION')
x<-c(.03,.1,.2);prob<-1-(1-x)^18*(1+18*x);p<-fit_elicited_beta_prior(list(method='quantiles',quantiles=x,probabilities=prob,weights=c(1,2,1)),C,max_cdf_error=1e-6)$value;close(c(p$alpha,p$beta),c(2,18),1e-5)
for(rule in c('linear','logarithmic')) {a<-rbind(c(.5,.25),c(.2,.4));result<-pool_expert_probability_distributions(a,c(1,1),list(kind='histogram',breaks=list(0,1,3)),rule)$value;expected<-if(rule=='linear')colMeans(a)else sqrt(a[1,]*a[2,]);expected<-expected/sum(expected*c(1,2));close(result$density_or_mass,expected);close(tail(unlist(result$cumulative_probabilities),1),1)
 same<-pool_expert_probability_distributions(rbind(c(.2,.8),c(.2,.8)),c(2,3),list(kind='discrete',values=list('low','high')),rule)$value;close(same$density_or_mass,c(.2,.8))}
reject(pool_expert_probability_distributions(diag(2),c(1,1),list(kind='discrete',values=list('a','b')),'logarithmic'),'DISJOINT_SUPPORT')
g<-list(group_id='g',evidence_ids=list('warning1','warning2'),joint_likelihood_ratio=2,reference='joint study');dep<-list(reference='single joint LR');close(update_odds_from_evidence(.1,list(g),dep)$value$posterior_pd,2/11);close(update_odds_from_evidence(0,list(g),dep)$value$posterior_pd,0);reject(update_odds_from_evidence(.1,list(g),dep,previous_evidence_ids='warning1'),'DOUBLE_COUNT')
w<-c(1,2,4);result<-check_expert_preference_consistency(outer(w,w,'/'),letters[1:3],'error')$value;close(result$components[[1]]$priorities,w/sum(w));close(result$triad_residuals[[1]]$log_residual,0)
cycle<-check_expert_preference_consistency(rbind(c(1,2,.5),c(.5,1,2),c(2,.5,1)),letters[1:3],'error')$value;stopifnot(length(cycle$cycle_witnesses)>0,!cycle$consistent_with_tolerance)
missing<-check_expert_preference_consistency(rbind(c(1,NA),c(NA,1)),letters[1:2],'connected_components')$value;stopifnot(!missing$globally_identified,length(missing$components)==2)
a<-c('A','A','A','B','B','B',NA);b<-c('A','A','B','A','B','B','A');kw<-list(ordered_grades=c('A','B'),rater_pair=c('one','two'),weighting='unweighted',missing_policy='pairwise_complete');result<-do.call(assess_expert_rating_agreement,c(list(a,b),kw))$value;close(result$kappa,1/3);stopifnot(result$complete_cases==6L)
degenerate<-do.call(assess_expert_rating_agreement,c(list(a,b,bootstrap_indices=rbind(rep(0,6),0:5)),kw))$value;stopifnot(is.null(degenerate$bootstrap_percentile_interval),degenerate$bootstrap_degenerate==1L)

x<-c(.1,.3,.6);prob<-Reduce(`+`,lapply(3:9,function(k)choose(9,k)*x^k*(1-x)^(9-k)));q<-fit_elicited_beta_prior(list(method="quantiles",quantiles=x,probabilities=prob,weights=c(1,2,1)),C,max_cdf_error=1e-6)$value;close(c(q$alpha,q$beta),c(3,7),1e-5)
