if(!exists('content_fingerprint'))library(creditratingsystem)
vtd<-if(exists('.vb_design'))get('.vb_design')else getFromNamespace('.vb_design','creditratingsystem');etd<-if(exists('.ex_target'))get('.ex_target')else getFromNamespace('.ex_target','creditratingsystem')
d<-list(model_id='fixed',model_version='1',parameter_version='1',data_fingerprint='cohort',population_id='funded',split_id='heldout',prediction_origin='HOLDOUT',evaluation_cutoff='2025-01-02',target=list(event='default',horizon=365,time_unit='days',probability_measure='PHYSICAL',conditioning='given_funding'))
stopifnot(identical(vtd(d),d),identical(etd(d$target),d$target));first<-d;first$target$event<-'first_default';stopifnot(identical(vtd(first),first),content_fingerprint(vtd(first))$value!=content_fingerprint(d)$value,inherits(try(etd(first$target),silent=TRUE),'try-error'))
for(bad in c('delinquency','prepayment','unknown')){z<-d;z$target$event<-bad;stopifnot(inherits(try(vtd(z),silent=TRUE),'try-error'))}
for(pair in list(list('probability_measure','RISK_NEUTRAL'),list('horizon',0),list('conditioning',''))){z<-first;z$target[[pair[[1]]]]<-pair[[2]];stopifnot(inherits(try(vtd(z),silent=TRUE),'try-error'))}
stopifnot(inherits(try(vtd(d,context=list(horizon=30)),silent=TRUE),'try-error'))
