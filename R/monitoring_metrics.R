# Mature-outcome metrics with explicit independent-cluster and null-path designs.
.mm_rows <- function(x,empty=FALSE) {
  if(!is.list(x)||!is.null(names(x))||(!empty&&!length(x))||any(!vapply(x,is.list,logical(1)))) .cr_error('INVALID_TABLE','List of records required')
  x
}
.mm_string <- function(x,name) {if(!is.character(x)||length(x)!=1L||is.na(x)||!nzchar(trimws(x))) .cr_error('INVALID_INPUT',paste(name,'must be a nonempty string'));x}
.mm_scalar <- function(x,name,low=-Inf,high=Inf) {v<-.cr_numeric(x,name);if(length(v)!=1L||!is.null(dim(v))||v<low||v>high).cr_error('INVALID_INPUT',paste(name,'outside scalar domain'));as.numeric(v)}
.mm_day <- function(x) {if(!is.character(x)||length(x)!=1L||is.na(x)||!grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$',x)).cr_error('INVALID_DATE','ISO calendar date required');d<-tryCatch(as.Date(x),error=function(e)NA);if(is.na(d)||format(d,'%Y-%m-%d')!=x).cr_error('INVALID_DATE','ISO calendar date required');d}
.mm_conf <- function(x) {c<-.mm_scalar(x,'confidence_level',0,1);if(c %in% c(0,1)).cr_error('INVALID_INPUT','Confidence level must be interior');c}
.mm_interval <- function(k,n,c) {if(!n)return(list(lower=NULL,upper=NULL));a<-(1-c)/2;list(lower=if(!k)0 else stats::qbeta(a,k,n-k+1),upper=if(k==n)1 else stats::qbeta(1-a,k+1,n-k))}
.mm_get <- function(x,k,default=NULL) {v<-x[[k]];if(is.null(v))default else v}
build_monitoring_cohorts <- function(predictions,outcome_events,coverage,target_contract,evaluation_cutoff,context=NULL) {
  rows<-.mm_rows(predictions);events<-.mm_rows(outcome_events,TRUE);cover<-.mm_rows(coverage,TRUE);cutoff<-.mm_day(evaluation_cutoff)
  if(!is.list(target_contract)||!identical(target_contract$probability_measure,'PHYSICAL')||!identical(target_contract$event,'default')).cr_error('INCOMPATIBLE_TARGET','Explicit physical default target required')
  .mm_string(target_contract$default_definition,'default_definition');.mm_string(target_contract$unit,'unit');horizon<-.mm_scalar(target_contract$horizon_days,'horizon_days',1)
  if(horizon!=floor(horizon)).cr_error('INCOMPATIBLE_TARGET','Positive integer horizon days required')
  known<-list();rating<-list();cov<-list();ids<-character();ignored<-0
  for(e in events) {
    id<-.mm_string(e$event_id,'event_id');entity<-.mm_string(e$entity_id,'entity_id');if(id %in% ids).cr_error('DUPLICATE_EVENT','Event IDs must be unique');ids<-c(ids,id)
    at<-.mm_day(e$event_at);k<-.mm_day(e$known_at);if(k<at).cr_error('INVALID_CHRONOLOGY','Outcome cannot be known before occurrence')
    if(is.null(e$event)||!e$event %in% c('default','exit','rating')).cr_error('INVALID_OUTCOME','Only default, rating or censoring exit events supported')
    if(e$event=='rating').mm_string(e$grade,'grade')
    if(!identical(e$default_definition,target_contract$default_definition)).cr_error('INCOMPATIBLE_TARGET','Outcome definition differs')
    if(k>cutoff){ignored<-ignored+1;next}
    if(e$event=='rating')rating[[entity]]<-c(rating[[entity]],list(e)) else known[[entity]]<-c(known[[entity]],list(e))
  }
  for(e in cover) {
    entity<-.mm_string(e$entity_id,'entity_id');through<-.mm_day(e$complete_through);k<-.mm_day(e$known_at)
    if(k<through).cr_error('INVALID_CHRONOLOGY','Complete-through declaration cannot precede completion')
    if(k>cutoff){ignored<-ignored+1;next};old<-cov[[entity]]
    if(is.null(old)||e$complete_through>old$complete_through||(e$complete_through==old$complete_through&&e$known_at>old$known_at))cov[[entity]]<-e
  }
  output<-list();ids<-character()
  for(r in rows) {
    id<-.mm_string(r$forecast_id,'forecast_id');if(id %in% ids).cr_error('DUPLICATE_FORECAST','Unique forecast IDs required');ids<-c(ids,id)
    for(key in c('case_id','entity_id','cluster_id','cohort','segment','model_id','model_version')).mm_string(r[[key]],key)
    if(.cr_hash(r$target)!=.cr_hash(target_contract)).cr_error('INCOMPATIBLE_TARGET','Prediction target differs from cohort contract')
    at<-.mm_day(r$forecast_at);k<-.mm_day(r$known_at);if(k>at).cr_error('FORECAST_LEAKAGE','Frozen forecast must be known by forecast date')
    end<-at+horizon;p<-.mm_scalar(r$pd,'pd',0,1);weight<-.mm_scalar(.mm_get(r,'weight',1),'weight',0);history<-known[[r$entity_id]]
    prior<-any(vapply(history,function(e).mm_day(e$event_at)<=at&&e$event=='default',logical(1)))
    exited<-any(vapply(history,function(e).mm_day(e$event_at)<=at&&e$event=='exit',logical(1)))
    exits<-Filter(function(e)e$event=='exit'&&.mm_day(e$event_at)>at,history);exit_at<-if(length(exits))min(vapply(exits,function(e)as.numeric(.mm_day(e$event_at)),numeric(1))) else Inf
    defaults<-Filter(function(e)e$event=='default'&&.mm_day(e$event_at)>at&&.mm_day(e$event_at)<=end&&as.numeric(.mm_day(e$event_at))<=exit_at,history)
    default<-if(length(defaults))defaults[[order(vapply(defaults,`[[`,'','event_at'),vapply(defaults,`[[`,'','known_at'))[1]]] else NULL
    mature<-end<=cutoff;complete<-cov[[r$entity_id]];covered<-!is.null(complete)&&.mm_day(complete$complete_through)>=end;censored<-exit_at<as.numeric(end)&&is.null(default)
    target<-if(!is.null(default))1 else if(mature&&covered&&!censored&&!prior&&!exited)0 else NULL
    status<-if(at>cutoff)'FUTURE_FORECAST' else if(prior)'PRIOR_DEFAULT' else if(exited)'EXITED_BEFORE_FORECAST' else if(!mature)'IMMATURE' else if(censored)'CENSORED' else if(is.null(target))'MISSING_OUTCOME' else 'ELIGIBLE'
    ratings<-Filter(function(e).mm_day(e$event_at)>at&&.mm_day(e$event_at)<=end,rating[[r$entity_id]])
    grade<-NULL
    if(mature&&covered&&!censored&&!prior&&!exited) {
      if(!is.null(target)&&target==1)grade<-target_contract$default_grade else if(length(ratings))grade<-ratings[[tail(order(vapply(ratings,`[[`,'','event_at'),vapply(ratings,`[[`,'','known_at'),vapply(ratings,`[[`,'','event_id')),1)]]$grade else grade<-r$origin_grade
    }
    out<-r[c('forecast_id','case_id','entity_id','cluster_id','cohort','segment','model_id','model_version','forecast_at')]
    out<-c(out,list(pd=p,weight=weight,target=target,outcome_end=format(end,'%Y-%m-%d'),outcome_known_at=if(!is.null(default))default$known_at else if(!is.null(target)&&target==0)complete$known_at else NULL,mature=mature,complete_through=if(is.null(complete))NULL else complete$complete_through,eligible=status=='ELIGIBLE',status=status,prior_default=prior,origin_grade=r$origin_grade,outcome_grade=grade,override=r$override,decision=r$decision,prediction_origin=r$prediction_origin))
    if(!is.null(out$override)&&(!is.logical(out$override)||length(out$override)!=1L||is.na(out$override))).cr_error('INVALID_INPUT','override must be boolean or null')
    if(!is.null(out$decision)&&!out$decision %in% c('approve','decline')).cr_error('INVALID_INPUT','Decision must be approve, decline or null')
    output[[length(output)+1L]]<-out
  }
  bundle<-list(rows=output,target_contract=target_contract,evaluation_cutoff=evaluation_cutoff);bundle$cohort_hash<-.cr_hash(bundle);clusters<-vapply(output,`[[`,'','cluster_id');entities<-vapply(output,`[[`,'','entity_id')
  .cr_result(bundle,'monitoring.build_monitoring_cohorts','cohort_bundle',context,diagnostics=list(eligible_count=sum(vapply(output,`[[`,logical(1),'eligible')),ignored_not_yet_known_records=ignored,repeated_cluster_rows=length(clusters)-length(unique(clusters)),repeated_entity_rows=length(entities)-length(unique(entities)),negative_labels_require_complete_through=TRUE),limitations='Declared coverage must include complete default-event ascertainment. An early default is retained but excluded from metrics until horizon maturity.')
}
.mm_cohort <- function(cohort) {
  b<-if(inherits(cohort,'CalcResult'))cohort$value else cohort
  if(!is.list(b)||!is.list(b$rows)).cr_error('INVALID_COHORT','Hashed monitoring cohort bundle required')
  payload<-b;payload$cohort_hash<-NULL;if(!identical(b$cohort_hash,.cr_hash(payload))).cr_error('COHORT_HASH_MISMATCH','Cohort content changed')
  for(r in b$rows)if(isTRUE(r$eligible)&&(!isTRUE(r$mature)||is.null(r$target)||!r$target %in% c(0,1))).cr_error('INVALID_COHORT','Eligible outcomes must be mature and observed')
  b
}
.mm_metrics <- function(rows,d=NULL) {
  active<-Filter(function(r)r$eligible&&r$weight>0&&(is.null(d)||d[[r$cluster_id]]>0),rows)
  if(!length(active))return(list(n=0,weight_sum=0,observed_rate=NULL,mean_pd=NULL,calibration_error=NULL,brier=NULL,auc=NULL,gini=NULL,log_loss=NULL,log_loss_status='UNIDENTIFIED'))
  y<-vapply(active,`[[`,0,'target');p<-vapply(active,`[[`,0,'pd');w<-vapply(active,function(r)r$weight*(if(is.null(d))1 else d[[r$cluster_id]]),0);total<-sum(w);rate<-sum(w*y)/total;mean<-sum(w*p)/total;a<-auc_gini(y,p,w,score_direction='higher_is_risk')$value;ll<-log_loss(y,p,w)$value
  list(n=length(active),weight_sum=total,observed_rate=rate,mean_pd=mean,calibration_error=rate-mean,brier=brier_score(y,p,w)$value$brier,auc=a$auc,gini=a$gini,log_loss=ll$log_loss,log_loss_status=ll$loss_status)
}
.mm_draws <- function(draws,rows) {
  clusters<-sort(unique(vapply(rows,`[[`,'','cluster_id')))
  if(!is.list(draws)||length(draws)<2L).cr_error('INVALID_RESAMPLES','At least two supplied cluster-resample paths required')
  lapply(draws,function(d){d<-unlist(d,use.names=FALSE);if(length(d)!=length(clusters)||any(!d %in% clusters)).cr_error('INVALID_RESAMPLES','Each draw must sample original number of known clusters');as.list(setNames(vapply(clusters,function(c)sum(d==c),0),clusters))})
}
.mm_bands <- function(samples,keys,c) {
  setNames(lapply(keys,function(k){v<-unlist(lapply(samples,`[[`,k),use.names=FALSE);list(lower=if(length(v)>=2)as.numeric(stats::quantile(v,(1-c)/2,type=7)) else NULL,upper=if(length(v)>=2)as.numeric(stats::quantile(v,(1+c)/2,type=7)) else NULL,identified_resamples=length(v),degenerate=length(v)>=2&&min(v)==max(v))}),keys)
}
monitor_rating_cohort <- function(cohort,group_by,reference_window,minimum_count,uncertainty,cluster_resamples=NULL,confidence_level=.95,context=NULL) {
  b<-.mm_cohort(cohort);rows<-b$rows;c<-.mm_conf(confidence_level);minimum<-.mm_scalar(minimum_count,'minimum_count',1);group_by<-unlist(group_by,use.names=FALSE)
  if(minimum!=floor(minimum)||!length(group_by)||anyDuplicated(group_by)).cr_error('INVALID_INPUT','Integer minimum and unique grouping fields required')
  if(!is.list(reference_window)).cr_error('INVALID_REFERENCE','Fixed reference window required');start<-.mm_day(reference_window$start);end<-.mm_day(reference_window$end);if(start>end).cr_error('INVALID_REFERENCE','Reference start must precede end')
  if(!uncertainty %in% c('none','iid_homogeneous_binomial','independent_cluster_bootstrap')).cr_error('INVALID_UNCERTAINTY','Explicit uncertainty assumptions required')
  draws<-if(uncertainty=='independent_cluster_bootstrap').mm_draws(cluster_resamples,rows) else NULL
  if(uncertainty!='independent_cluster_bootstrap'&&!is.null(cluster_resamples)).cr_error('INVALID_RESAMPLES','Resamples require bootstrap mode')
  groups<-list()
  for(r in rows){if(any(!group_by %in% names(r))).cr_error('MISSING_FEATURE','Missing grouping field');g<-r[group_by];key<-.cr_hash(g);if(is.null(groups[[key]]))groups[[key]]<-list(group=g,rows=list());groups[[key]]$rows<-c(groups[[key]]$rows,list(r))}
  out<-lapply(sort(names(groups)),function(key){g<-groups[[key]];part<-g$rows;m<-.mm_metrics(part);eligible<-Filter(function(r)r$eligible&&r$weight>0,part);intervals<-structure(list(),names=character())
    if(uncertainty=='iid_homogeneous_binomial'&&length(eligible)) {
      if(any(vapply(eligible,`[[`,0,'weight')!=1)||anyDuplicated(vapply(eligible,`[[`,'','cluster_id'))).cr_error('INCOMPATIBLE_ASSUMPTIONS','IID homogeneous intervals require unit weights and unique clusters')
      intervals$observed_rate<-.mm_interval(sum(vapply(eligible,`[[`,0,'target')),length(eligible),c)
    } else if(!is.null(draws))intervals<-.mm_bands(lapply(draws,function(d).mm_metrics(part,d)),c('observed_rate','calibration_error','brier','auc'),c)
    migration<-list();pairs<-list();over<-Filter(function(r)!is.null(r$override),eligible)
    for(r in eligible)if(!is.null(r$origin_grade)&&!is.null(r$outcome_grade)){k<-.cr_hash(list(as.character(r$origin_grade),as.character(r$outcome_grade)));if(is.null(pairs[[k]]))pairs[[k]]<-list(from=as.character(r$origin_grade),to=as.character(r$outcome_grade),weight=0);pairs[[k]]$weight<-pairs[[k]]$weight+r$weight}
    if(length(pairs)){migration<-unname(pairs);migration<-migration[order(vapply(migration,`[[`,'','from'),vapply(migration,`[[`,'','to'))]}
    reference<-Filter(function(r).mm_day(r$forecast_at)>=start&&.mm_day(r$forecast_at)<=end,part)
    list(group_id=key,group=g$group,status=if(!m$n)'MISSING_OUTCOME' else if(m$n<minimum)'INSUFFICIENT_INFORMATION' else 'DESCRIPTIVE_AVAILABLE',metrics=m,intervals=intervals,reference_metrics=.mm_metrics(reference),migration=migration,override_rate=if(length(over))sum(vapply(over,function(r)r$weight*r$override,0))/sum(vapply(over,`[[`,0,'weight')) else NULL,excluded_count=sum(!vapply(part,`[[`,logical(1),'eligible')),repeated_cluster_rows=length(eligible)-length(unique(vapply(eligible,`[[`,'','cluster_id'))))
  })
  .cr_result(out,'monitoring.monitor_rating_cohort','monitoring_panel',context,diagnostics=list(cohort_hash=b$cohort_hash,reference_window=reference_window,uncertainty=uncertainty,confidence_level=c,minimum_count=minimum),limitations='Exact rates assume independent homogeneous Bernoulli outcomes. Bootstrap assumes independent clusters; degenerate boundary bands do not establish precision. No universal threshold or automatic recalibration.')
}
.mm_weights <- function(w,n){if(is.null(w))return(rep(1,n));w<-.cr_numeric(w,'weights',vector_only=TRUE);if(length(w)!=n||any(w<0)||!is.finite(sum(w))||sum(w)<=0).cr_error('INVALID_WEIGHTS','Finite nonnegative weights with positive total required');w}
detect_credit_feature_drift <- function(reference,current,feature_order,method,permutations,multiple_testing,dependence_assumption,reference_weights=NULL,current_weights=NULL,bandwidth=NULL,frozen_bins=NULL,smoothing=.5,permutation_units=NULL,context=NULL) {
  ref<-.mm_rows(reference);cur<-.mm_rows(current);features<-unlist(feature_order,use.names=FALSE);if(!length(features)||anyDuplicated(features)).cr_error('INVALID_FEATURE_ORDER','Unique feature names required')
  rows<-c(ref,cur);if(any(vapply(rows,function(r)any(!features %in% names(r)),logical(1)))).cr_error('MISSING_FEATURE','Feature missing from sample')
  n<-length(ref);N<-length(rows);wr<-.mm_weights(reference_weights,n);wc<-.mm_weights(current_weights,length(cur));weights<-c(wr,wc);out<-list()
  if(!method %in% c('ks','wasserstein','mmd_rbf','psi')).cr_error('INVALID_METHOD','Declare supported drift method')
  missing<-function(v)is.null(v)||(is.numeric(v)&&length(v)==1L&&is.na(v))
  if(method=='psi') {
    if(!is.null(permutations)||multiple_testing!='not_applicable').cr_error('INVALID_RESAMPLING','PSI is descriptive')
    if(!is.list(frozen_bins)).cr_error('INVALID_BINS','Frozen numeric or categorical bins required');alpha<-.mm_scalar(smoothing,'smoothing',0);if(!alpha).cr_error('INVALID_INPUT','Positive PSI smoothing required')
    for(f in features) {
      s<-frozen_bins[[f]];if(!is.list(s)).cr_error('INVALID_BINS','Each feature requires frozen bins')
      if(identical(s$kind,'numeric')) {
        cuts<-.cr_numeric(s$cutpoints,'cutpoints',allow_empty=TRUE,vector_only=TRUE);if(any(diff(cuts)<=0)).cr_error('INVALID_BINS','Increasing finite cutpoints required');support<-.cr_numeric(s$support,'support',vector_only=TRUE);if(length(support)!=2||support[1]>=support[2]).cr_error('INVALID_BINS','Explicit support bounds required');size<-length(cuts)+2L
        encode<-function(v)if(missing(v))size else sum(cuts<.mm_scalar(v,f))+1L
        new<-sum(vapply(cur,function(r)!missing(r[[f]])&&(.mm_scalar(r[[f]],f)<support[1]||.mm_scalar(r[[f]],f)>support[2]),logical(1)));label<-'out_of_reference_support'
      } else if(identical(s$kind,'categorical')) {
        levels<-unlist(s$levels,use.names=FALSE);if((length(levels)&&!is.character(levels))||anyDuplicated(levels)).cr_error('INVALID_BINS','Unique fixed string levels required');size<-length(levels)+2L
        encode<-function(v){if(missing(v))return(size-1L);if(!is.character(v)||length(v)!=1L).cr_error('INVALID_FEATURE_TYPE','Categorical value must be string');j<-match(v,levels);if(is.na(j))size else j}
        new<-sum(vapply(cur,function(r)!missing(r[[f]])&&!r[[f]] %in% levels,logical(1)));label<-'new_category_rows'
      } else .cr_error('INVALID_BINS','Bin kind numeric/categorical required')
      a<-b<-numeric(size);for(i in seq_along(ref)){j<-encode(ref[[i]][[f]]);a[j]<-a[j]+wr[i]};for(i in seq_along(cur)){j<-encode(cur[[i]][[f]]);b[j]<-b[j]+wc[i]}
      pa<-(a+alpha)/(sum(a)+alpha*size);pb<-(b+alpha)/(sum(b)+alpha*size);r<-list(feature=f,statistic=sum((pb-pa)*log(pb/pa)),p_value=NULL,adjusted_p_value=NULL,reference_missing=sum(vapply(ref,function(r)missing(r[[f]]),logical(1))),current_missing=sum(vapply(cur,function(r)missing(r[[f]]),logical(1))),reference_shares=as.list(pa),current_shares=as.list(pb));r[[label]]<-new;out[[length(out)+1L]]<-r
    }
  } else {
    if(!multiple_testing %in% c('none','holm','bonferroni')).cr_error('INVALID_MULTIPLE_TESTING','Declare none, holm or bonferroni')
    if(!dependence_assumption %in% c('exchangeable_rows','exchangeable_clusters')).cr_error('INCOMPATIBLE_ASSUMPTIONS','Explicit permutation exchangeability required')
    draws<-.cr_numeric(permutations,'permutations');if(length(dim(draws))!=2L||nrow(draws)<2||ncol(draws)!=N||any(draws!=floor(draws))||any(apply(draws,1,function(d)!identical(sort(as.integer(d)),seq.int(0L,N-1L))))).cr_error('INVALID_PERMUTATIONS','At least two complete zero-based pooled-row permutations required');draws<-draws+1L
    if(dependence_assumption=='exchangeable_clusters') {
      units<-unlist(permutation_units,use.names=FALSE);if(!is.character(units)||length(units)!=N||anyNA(units)||any(!nzchar(units))||length(intersect(units[seq_len(n)],units[seq.int(n+1L,N)]))).cr_error('INVALID_CLUSTERS','Pooled complete cluster IDs required')
      positions<-lapply(sort(unique(units)),function(u)which(units==u))
      for(i in seq_len(nrow(draws)))for(ix in positions){source<-draws[i,ix];if(length(unique(units[source]))!=1L||!identical(as.integer(source),which(units==units[source[1]]))).cr_error('INVALID_PERMUTATIONS','Move complete clusters preserving internal order')}
    } else if(!is.null(permutation_units)).cr_error('INVALID_CLUSTERS','Cluster labels require cluster mode')
    data<-do.call(rbind,lapply(rows,function(r)vapply(features,function(f)if(missing(r[[f]]))NA_real_ else .mm_scalar(r[[f]],f),0)))
    if(method=='mmd_rbf'){bw<-.mm_scalar(bandwidth,'bandwidth',0);if(!bw).cr_error('INVALID_KERNEL','Positive prespecified bandwidth required')}
    statistic<-function(indices,cols) {
      left<-indices[seq_len(n)];right<-indices[seq.int(n+1L,N)];left<-left[rowSums(!is.finite(data[left,cols,drop=FALSE]))==0&weights[left]>0];right<-right[rowSums(!is.finite(data[right,cols,drop=FALSE]))==0&weights[right]>0]
      if(!length(left)||!length(right))return(NULL);x<-data[left,cols,drop=FALSE];y<-data[right,cols,drop=FALSE];a<-weights[left]/sum(weights[left]);b<-weights[right]/sum(weights[right])
      if(method %in% c('ks','wasserstein')){grid<-sort(unique(c(x[,1],y[,1])));delta<-vapply(grid,function(z)sum(a[x[,1]<=z])-sum(b[y[,1]<=z]),0);return(if(method=='ks')max(abs(delta)) else sum(abs(head(delta,-1))*diff(grid)))}
      kernel<-function(v,u){k<-matrix(0,nrow(v),nrow(u));for(i in seq_len(nrow(v)))for(j in seq_len(nrow(u)))k[i,j]<-exp(-.5*sum(((v[i,]-u[j,])/bw)^2));k}
      max(0,as.numeric(t(a)%*%kernel(x,x)%*%a+t(b)%*%kernel(y,y)%*%b-2*t(a)%*%kernel(x,y)%*%b))
    }
    columns<-if(method=='mmd_rbf')list(seq_along(features)) else as.list(seq_along(features))
    for(cols in columns){obs<-statistic(seq_len(N),cols);null<-lapply(seq_len(nrow(draws)),function(i)statistic(draws[i,],cols));if(is.null(obs)||any(vapply(null,is.null,logical(1)))).cr_error('INSUFFICIENT_COVERAGE','All permutations require positive observed support');null<-unlist(null);exceedances<-sum(null>=obs-1e-14*max(1,abs(obs)));p<-(1+exceedances)/(length(null)+1)
      out[[length(out)+1L]]<-list(feature=if(method=='mmd_rbf')'joint' else features[cols],statistic=obs,p_value=p,adjusted_p_value=p,permutation_standard_error=sqrt(p*(1-p)/(length(null)+1)),permutation_tail_interval=.mm_interval(exceedances,length(null),.95),null_statistics=as.list(null),reference_missing=sum(rowSums(!is.finite(data[seq_len(n),cols,drop=FALSE]))>0),current_missing=sum(rowSums(!is.finite(data[seq.int(n+1L,N),cols,drop=FALSE]))>0))
    }
    adjusted<-stats::p.adjust(vapply(out,`[[`,0,'p_value'),method=multiple_testing);for(i in seq_along(out))out[[i]]$adjusted_p_value<-adjusted[i]
  }
  .cr_result(out,'monitoring.detect_credit_feature_drift','distribution_distance',context,diagnostics=list(method=method,feature_order=as.list(features),multiple_testing=multiple_testing,dependence_assumption=dependence_assumption,kernel_bandwidth=bandwidth,mmd_variant=if(method=='mmd_rbf')'weighted_biased_empirical_embedding_squared' else NULL,permutation_index_base=0,monte_carlo_confidence_level=.95,missing_policy='separate_PSI_bin_otherwise_complete_cases',no_concept_drift_claim=TRUE),limitations='Permutation p-values require exchangeability of declared rows or intact clusters, including weights. Finite shared permutations limit resolution. PSI has no universal significance threshold; input drift does not establish concept drift.')
}
detect_pd_calibration_drift <- function(residuals,method,parameters,null_paths,null_spec,alpha,validation_null_paths=NULL,confidence_level=.95,context=NULL) {
  rows<-.mm_rows(residuals);times<-vapply(rows,function(r)as.numeric(.mm_day(r$time)),0);values<-vapply(rows,function(r).mm_scalar(r$value,'residual'),0);c<-.mm_conf(confidence_level);a<-.mm_conf(alpha)
  if(any(!vapply(rows,function(r)isTRUE(r$mature),logical(1)))||any(diff(times)<=0)).cr_error('INVALID_RESIDUALS','Strictly ordered mature residual aggregates required')
  if(!method %in% c('cusum','ewma')||!is.list(parameters)||!is.list(null_spec)).cr_error('INVALID_NULL','Explicit method, parameters and null specification required')
  for(k in c('model_id','model_version','calibration_id','residual_scale')).mm_string(null_spec[[k]],k)
  if(is.null(null_spec$dependence)||!null_spec$dependence %in% c('independent','prespecified_dependent')||!identical(null_spec$probability_measure,'PHYSICAL')||as.numeric(.mm_day(null_spec$calibrated_before))>times[1]).cr_error('INVALID_NULL','Prespecified physical null and dependence required before monitoring')
  null<-.cr_numeric(null_paths,'null_paths');if(length(dim(null))!=2L||nrow(null)<2||ncol(null)!=length(rows)).cr_error('INVALID_NULL','Null paths must share complete horizon')
  if(method=='cusum'){
    allowance<-.mm_scalar(parameters$allowance,'allowance',0)
    path<-function(x){up<-down<-0;out<-numeric(length(x));for(i in seq_along(x)){up<-max(0,up+x[i]-allowance);down<-max(0,down-x[i]-allowance);out[i]<-max(up,down)};out}
  } else {
    smoothing<-.mm_scalar(parameters$smoothing,'smoothing',0,1);if(!smoothing).cr_error('INVALID_INPUT','EWMA smoothing must be positive')
    path<-function(x){z<-0;out<-numeric(length(x));for(i in seq_along(x)){z<-(1-smoothing)*z+smoothing*x[i];out[i]<-abs(z)};out}
  }
  maxima<-apply(null,1,function(x)max(path(x)));rank<-ceiling((nrow(null)+1)*(1-a));if(rank>nrow(null)).cr_error('INSUFFICIENT_NULL_SIMULATIONS','More null paths required for requested level')
  threshold<-sort(maxima)[rank];observed<-path(values);cross<-which(observed>threshold);validation<-NULL
  if(!is.null(validation_null_paths)) {
    independent<-.cr_numeric(validation_null_paths,'validation_null_paths');if(length(dim(independent))!=2L||nrow(independent)<2||ncol(independent)!=length(rows)||!isTRUE(null_spec$validation_paths_independent)).cr_error('INVALID_NULL','Declare independent validation paths with same horizon')
    count<-sum(apply(independent,1,function(x)max(path(x))>threshold));validation<-list(exceedances=count,paths=nrow(independent),estimate=count/nrow(independent),interval=.mm_interval(count,nrow(independent),c))
  }
  .cr_result(list(statistic_path=as.list(observed),threshold=threshold,first_crossing_index=if(length(cross))cross[1]-1L else NULL,first_crossing_time=if(length(cross))rows[[cross[1]]]$time else NULL,validation_false_alarm=validation),'monitoring.detect_pd_calibration_drift','residual_scale',context,diagnostics=list(method=method,parameters=parameters,null_spec=null_spec,alpha=a,null_paths=nrow(null),order_statistic_rank=rank,strict_crossing=TRUE,horizon=length(rows),null_paths_hash=.cr_hash(lapply(seq_len(nrow(null)),function(i)as.list(null[i,]))),calibration_exceedance_fraction=mean(maxima>threshold),validation_performed=!is.null(validation),index_base=0),limitations='Finite-horizon calibration under the declared null requires independent complete-path simulations, including within-path dependence. Calibration paths do not independently validate false alarms. Horizon extension requires new calibration.')
}
compare_challenger_cohorts <- function(champion,challenger,cluster_resamples,selection_protocol,confidence_level=.95,context=NULL) {
  a<-.mm_cohort(champion);b<-.mm_cohort(challenger);c<-.mm_conf(confidence_level)
  if(.cr_hash(a$target_contract)!=.cr_hash(b$target_contract)||a$evaluation_cutoff!=b$evaluation_cutoff).cr_error('INCOMPATIBLE_COHORT','Target and evaluation cutoff must match')
  if(!is.list(selection_protocol)||!identical(selection_protocol$evaluation_role,'OUT_OF_TIME_TEST')||is.null(selection_protocol$multiple_challenger_policy)||!selection_protocol$multiple_challenger_policy %in% c('single_prespecified','separate_selection_and_final_test')).cr_error('INVALID_SELECTION_PROTOCOL','Declare untouched final test and selection policy')
  .mm_string(selection_protocol$protocol_id,'protocol_id')
  keyed<-function(rows){ids<-vapply(rows,`[[`,'','case_id');if(anyDuplicated(ids)).cr_error('DUPLICATE_CASE','One forecast per matched case required');setNames(rows,ids)}
  aa<-keyed(a$rows);bb<-keyed(b$rows);left<-right<-list();disagreements<-character()
  for(key in sort(intersect(names(aa),names(bb)))) {
    x<-aa[[key]];y<-bb[[key]]
    if(any(vapply(c('entity_id','cluster_id','forecast_at','outcome_end','weight'),function(k)!identical(x[[k]],y[[k]]),logical(1)))||(!is.null(x$target)&&!is.null(y$target)&&x$target!=y$target)).cr_error('PAIR_MISMATCH','Paired identity, window, weights and observed labels must agree')
    if(x$eligible!=y$eligible)disagreements<-c(disagreements,key)
    if(x$eligible&&y$eligible){if(!identical(x$prediction_origin,'OUT_OF_TIME')||!identical(y$prediction_origin,'OUT_OF_TIME')).cr_error('INVALID_SELECTION_PROTOCOL','Predictions must be declared out of time');left[[length(left)+1L]]<-x;right[[length(right)+1L]]<-y}
  }
  if(!length(left)).cr_error('INSUFFICIENT_INFORMATION','No jointly eligible paired cases')
  draws<-.mm_draws(cluster_resamples,left);keys<-c('auc','brier','log_loss','calibration_error','mean_pd')
  difference<-function(x,y)setNames(lapply(keys,function(k)if(is.null(x[[k]])||is.null(y[[k]]))NULL else y[[k]]-x[[k]]),keys)
  m1<-.mm_metrics(left);m2<-.mm_metrics(right);samples<-lapply(draws,function(d)difference(.mm_metrics(left,d),.mm_metrics(right,d)));decisions<-Filter(function(i)!is.null(left[[i]]$decision)&&!is.null(right[[i]]$decision),as.list(seq_along(left)))
  value<-list(champion_metrics=m1,challenger_metrics=m2,differences=difference(m1,m2),intervals=.mm_bands(samples,keys,c),paired_count=length(left),champion_only=as.list(sort(setdiff(names(aa),names(bb)))),challenger_only=as.list(sort(setdiff(names(bb),names(aa)))),eligibility_disagreements=as.list(disagreements),decision_flips=sum(vapply(decisions,function(i)left[[i]]$decision!=right[[i]]$decision,logical(1))),observed_decision_pairs=length(decisions),recommendation='REQUIRES_MULTIDIMENSIONAL_REVIEW')
  .cr_result(value,'monitoring.compare_challenger_cohorts','paired_metric_difference',context,diagnostics=list(champion_hash=a$cohort_hash,challenger_hash=b$cohort_hash,selection_protocol=selection_protocol,resamples=length(draws),resampling_unit='independent_cluster',confidence_level=c,difference_direction='challenger_minus_champion',automatic_deployment=FALSE),limitations='Paired percentile bands assume independent clusters. One-class resamples cannot identify AUC; infinite exact loss has no finite difference. A declared protocol cannot establish absence of undisclosed model search.')
}
diagnose_fairness_slices <- function(cohort,group_labels,reference_group,use_basis,minimum_count,risk_threshold,cluster_resamples,confidence_level=.95,context=NULL) {
  b<-.mm_cohort(cohort);.mm_string(use_basis,'use_basis');.mm_string(reference_group,'reference_group');c<-.mm_conf(confidence_level);threshold<-.mm_scalar(risk_threshold,'risk_threshold',0,1);minimum<-.mm_scalar(minimum_count,'minimum_count',1)
  if(minimum!=floor(minimum)).cr_error('INVALID_INPUT','Integer minimum count required')
  labels<-list();for(r in .mm_rows(group_labels)){case<-.mm_string(r$case_id,'case_id');g<-.mm_string(r$group,'group');if(!is.null(labels[[case]])).cr_error('DUPLICATE_CASE','Unique case labels required');labels[[case]]<-g}
  used<-Filter(function(r)r$case_id %in% names(labels),b$rows);if(anyDuplicated(vapply(used,`[[`,'','case_id'))).cr_error('DUPLICATE_CASE','One forecast per labeled case required')
  groups<-sort(unique(vapply(used,function(r)labels[[r$case_id]],'')));if(!reference_group %in% groups).cr_error('INVALID_REFERENCE','Reference group must occur in cohort')
  draws<-.mm_draws(cluster_resamples,used);parts<-setNames(lapply(groups,function(g)Filter(function(r)labels[[r$case_id]]==g,used)),groups);keys<-c('selection_rate','true_positive_rate','false_positive_rate','error_rate','observed_rate','mean_pd','calibration_error','brier')
  metrics<-function(rows,d=NULL) {
    active<-Filter(function(r)r$weight>0&&(is.null(d)||d[[r$cluster_id]]>0),rows);base<-.mm_metrics(rows,d)
    average<-function(sub,fn){w<-vapply(sub,function(r)r$weight*(if(is.null(d))1 else d[[r$cluster_id]]),0);if(!sum(w))return(NULL);sum(w*vapply(sub,function(r)as.numeric(fn(r)),0))/sum(w)}
    eligible<-Filter(function(r)r$eligible,active);positive<-Filter(function(r)r$target==1,eligible);negative<-Filter(function(r)r$target==0,eligible)
    c(list(selection_rate=average(Filter(function(r)!is.null(r$decision),active),function(r)r$decision=='approve'),true_positive_rate=average(positive,function(r)r$pd>=threshold),false_positive_rate=average(negative,function(r)r$pd>=threshold),error_rate=average(eligible,function(r)(r$pd>=threshold)!=as.logical(r$target))),base[c('observed_rate','mean_pd','calibration_error','brier')])
  }
  point<-lapply(parts,metrics);replicas<-lapply(parts,function(p)lapply(draws,function(d)metrics(p,d)));gap<-function(x,y)setNames(lapply(keys,function(k)if(is.null(x[[k]])||is.null(y[[k]]))NULL else x[[k]]-y[[k]]),keys)
  out<-lapply(groups,function(g){eligible<-Filter(function(r)r$eligible&&r$weight>0,parts[[g]]);defaults<-sum(vapply(eligible,`[[`,0,'target'));list(group=g,status=if(length(eligible)<minimum)'INSUFFICIENT_INFORMATION' else 'DESCRIPTIVE_AVAILABLE',case_count=length(parts[[g]]),mature_observed_count=length(eligible),default_count=defaults,boundary_outcome_sample=!length(eligible)||defaults %in% c(0,length(eligible)),metrics=point[[g]],intervals=.mm_bands(replicas[[g]],keys,c),reference_gaps=gap(point[[g]],point[[reference_group]]),gap_intervals=.mm_bands(Map(gap,replicas[[g]],replicas[[reference_group]]),keys,c),selection_denominator='all_known_decisions',outcome_denominator='mature_observed_outcomes')})
  .cr_result(out,'monitoring.diagnose_fairness_slices','group_diagnostic',context,diagnostics=list(cohort_hash=b$cohort_hash,use_basis=use_basis,reference_group=reference_group,risk_threshold=threshold,positive_prediction='pd_at_least_threshold',resampling_unit='independent_cluster',confidence_level=c,unlabeled_case_count=length(b$rows)-length(used),compliance_certification=FALSE),limitations='Group labels are supplied, never inferred. Selective outcomes can distort comparisons. Pointwise bootstrap assumes independent clusters; degenerate boundary bands do not establish precision. Differing base rates can make fairness criteria incompatible. This is not legal or ethical certification.')
}
