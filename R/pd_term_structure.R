#' Cumulative first-default probabilities from conditional period probabilities.
cumulative_pd_from_conditional <- function(conditional_pd, context = NULL) {
  q <- .cr_probability(conditional_pd, "conditional_pd", vector_only = TRUE)
  value <- -expm1(cumsum(log1p(-q)))
  .cr_result(value, "pd_term_structure.cumulative_pd_from_conditional", "probability", context,
             diagnostics = list(conditioning = "survival_to_period_start"))
}

#' Unconditional first-default probability mass in each period.
marginal_pd_from_conditional <- function(conditional_pd, context = NULL) {
  q <- .cr_probability(conditional_pd, "conditional_pd", vector_only = TRUE)
  value <- if (length(q)) q * exp(c(0, head(cumsum(log1p(-q)), -1L))) else numeric()
  .cr_result(value, "pd_term_structure.marginal_pd_from_conditional", "probability", context,
             diagnostics = list(conditioning = "unconditional_first_default_mass"))
}

conditional_pd_from_cumulative <- function(cumulative_pd,context=NULL) {
  f<-.cr_probability(cumulative_pd,"cumulative_pd",vector_only=TRUE)
  if(any(diff(f)<0)).cr_error("NONMONOTONE_CURVE","Cumulative default probabilities must not decrease")
  previous<-if(length(f))c(0,head(f,-1L)) else numeric();undefined<-previous==1
  values<-lapply(seq_along(f),function(i)if(undefined[i])NULL else (f[i]-previous[i])/(1-previous[i]))
  .cr_result(list(conditional_pd=values,undefined_after_full_default=as.list(undefined)),"pd_term_structure.conditional_pd_from_cumulative","probability",context,
    diagnostics=list(conditioning="survival_to_period_start",default_absorbing=TRUE))
}
.cr_term_context <- function(context,horizon=NULL) {
  ctx<-.cr_result(NULL,"context",context=context)$context
  if(!is.null(ctx$time_unit)&&!identical(ctx$time_unit,"years")).cr_error("INCOMPATIBLE_HORIZON","Rates are annual and horizons in years")
  if(!is.null(horizon)&&!is.null(ctx$horizon)&&!identical(.cr_hash(ctx$horizon),.cr_hash(horizon))).cr_error("TARGET_MISMATCH","Context horizon differs")
  ctx
}
hazard_to_pd <- function(hazard,interval_lengths,context=NULL) {
  h<-.cr_numeric(hazard,"hazard",vector_only=TRUE);dt<-.cr_numeric(interval_lengths,"interval_lengths",vector_only=TRUE)
  if(length(h)!=length(dt)).cr_error("INCOMPATIBLE_SHAPE","One intensity and length per interval required")
  if(any(h<0)||any(dt<=0)).cr_error("INVALID_HAZARD","Nonnegative rates and positive lengths required")
  integrated<-h*dt;cumulative<-cumsum(integrated)
  if(any(!is.finite(cumulative))).cr_error("NUMERICAL_RANGE","Integrated hazard exceeds finite range")
  q<- -expm1(-integrated);f<- -expm1(-cumulative);survival<-exp(-cumulative)
  prior<-if(length(h))c(1,head(survival,-1L)) else numeric()
  .cr_result(list(conditional_pd=q,cumulative_pd=f,survival=survival,marginal_pd=prior*q),"pd_term_structure.hazard_to_pd","probability",.cr_term_context(context,sum(dt)),
    diagnostics=list(time_unit="years",rate_unit="per_year",integrated_hazard=as.list(integrated),default_absorbing=TRUE))
}
.cr_term_states <- function(states,default_state) {
  if(is.list(states))states<-unlist(states)
  if(!is.character(states)||!length(states)||anyNA(states)||any(!nzchar(trimws(states)))||anyDuplicated(states)).cr_error("INVALID_STATES","Unique ordered labels required")
  if(!is.character(default_state)||length(default_state)!=1L||is.na(default_state)||!default_state %in% states).cr_error("INVALID_STATES","Default must be a declared state")
  list(states=states,default=match(default_state,states))
}
project_transition_matrix <- function(matrix,horizon,matrix_kind,states,default_state,step_length=NULL,time_unit="years",context=NULL) {
  labels<-.cr_term_states(states,default_state);states<-labels$states;default<-labels$default;n<-length(states)
  m<-.cr_numeric(matrix,"matrix");t<-.cr_numeric(horizon,"horizon",vector_only=TRUE)
  if(length(t)!=1L||t<0||!identical(time_unit,"years")).cr_error("INCOMPATIBLE_HORIZON","Nonnegative horizon in years required")
  if(!is.matrix(m)||!identical(dim(m),c(n,n))).cr_error("INCOMPATIBLE_SHAPE","Square matrix matching states required")
  tol<-1e-12
  if(identical(matrix_kind,"transition")) {
    step<-.cr_numeric(step_length,"step_length",vector_only=TRUE)
    if(length(step)!=1L||step<=0).cr_error("INCOMPATIBLE_HORIZON","Positive step length required")
    if(any(m<0)||any(m>1)||any(abs(rowSums(m)-1)>tol)).cr_error("INVALID_TRANSITION_MATRIX","Stochastic rows required")
    if(any(m[default,]!=diag(n)[default,])).cr_error("DEFAULT_NOT_ABSORBING","Default must be exactly absorbing")
    periods<-t/step
    if(!is.finite(periods)||abs(periods-round(periods))>1e-9||periods>2^31-1).cr_error("INCOMPATIBLE_HORIZON","Integer number of declared periods required")
    projected<-diag(n);base<-m;k<-round(periods)
    while(k>0) {if(k%%2==1)projected<-projected%*%base;k<-floor(k/2);if(k>0)base<-base%*%base}
    method<-"integer_matrix_power"
  } else if(identical(matrix_kind,"generator")) {
    if(!is.null(step_length)).cr_error("INCOMPATIBLE_HORIZON","Generator is per year, without discrete step")
    off<-m;diag(off)<-0
    if(any(off<0)||any(diag(m)>0)||any(abs(rowSums(m))>tol*pmax(1,rowSums(abs(m))))).cr_error("INVALID_GENERATOR","Nonnegative off-diagonals and zero row sums required")
    if(any(m[default,]!=0)).cr_error("DEFAULT_NOT_ABSORBING","Default generator row must be zero")
    scaled<-m*t
    if(any(!is.finite(scaled))).cr_error("NUMERICAL_RANGE","Generator horizon product overflow")
    projected<-as.matrix(Matrix::expm(Matrix::Matrix(scaled,sparse=FALSE)));method<-"matrix_exponential"
  } else .cr_error("INVALID_INPUT","matrix_kind must be transition or generator")
  if(any(!is.finite(projected))||min(projected)< -tol||max(abs(rowSums(projected)-1))>1e-10).cr_error("NUMERICAL_PROJECTION_FAILURE","Projected matrix failed stochastic checks")
  correction<-max(abs(pmin(projected,0)));projected<-pmax(projected,0);projected<-projected/rowSums(projected)
  .cr_result(list(matrix=projected,pd_by_origin=projected[,default],survival_by_origin=1-projected[,default]),"pd_term_structure.project_transition_matrix","transition_probabilities",.cr_term_context(context,t),
    diagnostics=list(states=as.list(states),default_state=default_state,horizon=t,time_unit="years",matrix_kind=matrix_kind,algorithm=method,roundoff_negative_correction=correction,orientation="origin_rows_destination_columns",default_absorbing=TRUE),
    limitations="Projection assumes time-homogeneous Markov dynamics. State occupation of absorbing default equals cumulative default probability.")
}
estimate_transition_matrix <- function(ratings,observation_windows,states,default_state,estimator,knowledge_date,observation_scheme,exits=NULL,cohort_period_days=NULL,days_per_year=365.25,empty_row_policy="error",context=NULL) {
  labels<-.cr_term_states(states,default_state);states<-labels$states;default<-labels$default;n<-length(states)
  date<-function(value,name)as.Date(.cr_date(value,name))
  cutoff<-date(knowledge_date,"knowledge_date");basis<-.cr_numeric(days_per_year,"days_per_year",vector_only=TRUE)
  if(length(basis)!=1L||basis<=0).cr_error("INVALID_INPUT","Positive days_per_year required")
  choice<-function(x,allowed)is.character(x)&&length(x)==1L&&!is.na(x)&&x %in% allowed
  if(!choice(estimator,c("cohort","duration"))||!choice(empty_row_policy,c("error","identity"))).cr_error("INVALID_INPUT","Declare estimator and empty row policy")
  if(!choice(observation_scheme,c("exact_event_times","state_carried_until_change"))||(estimator=="duration"&&observation_scheme!="exact_event_times")).cr_error("INVALID_OBSERVATION_SCHEME","Duration needs exact transition times")
  period<-NULL
  if(estimator=="cohort") {
    period<-.cr_numeric(cohort_period_days,"cohort_period_days",vector_only=TRUE)
    if(length(period)!=1L||period<=0||period!=floor(period)).cr_error("INCOMPATIBLE_HORIZON","Positive integer cohort period days required")
  }
  records<-function(x)if(is.data.frame(x))lapply(seq_len(nrow(x)),function(i)as.list(x[i,,drop=FALSE])) else x
  ratings<-records(ratings);observation_windows<-records(observation_windows);exits<-records(exits)
  if(!is.list(ratings)||!is.list(observation_windows)||!length(observation_windows)).cr_error("INVALID_INPUT","Rating records and windows required")
  entity<-function(row){if(!.cr_object(row)).cr_error("INVALID_ENTITY","Record object required");.cr_string(row$entity_id,"entity_id","INVALID_ENTITY")}
  windows<-groups<-exit_map<-list();ignored<-0L
  for(row in observation_windows) {
    eid<-entity(row)
    if(eid %in% names(windows)).cr_error("DUPLICATE_WINDOW","One window per entity required")
    start<-date(row$start,"start");end<-date(row$end,"end");complete<-date(row$complete_until,"complete_until");known<-date(row$known_at,"known_at")
    if(start>=end||complete<start||known<complete||known>cutoff).cr_error("INVALID_WINDOW","Window completeness must be available by knowledge date")
    if(estimator=="cohort"&&as.numeric(end-start)!=period).cr_error("INCOMPATIBLE_HORIZON","Common cohort duration required")
    windows[[eid]]<-list(start=start,end=end,complete=min(end,complete,cutoff));groups[[eid]]<-list()
  }
  for(row in ratings) {
    eid<-entity(row);at<-date(row$at,"at");known<-date(row$known_at,"known_at")
    if(known<at).cr_error("INVALID_CHRONOLOGY","Rating cannot be known before occurrence")
    if(!choice(row$state,states)).cr_error("INVALID_STATES","Unknown rating state")
    if(!eid %in% names(windows)).cr_error("UNKNOWN_ENTITY","Rating entity lacks window")
    if(known>cutoff){ignored<-ignored+1L;next}
    groups[[eid]][[length(groups[[eid]])+1L]]<-list(at=at,state=match(row$state,states))
  }
  if(!is.null(exits)) {
    if(!is.list(exits)).cr_error("INVALID_INPUT","Exit records required")
    for(row in exits) {
      eid<-entity(row);at<-date(row$at,"at");known<-date(row$known_at,"known_at")
      if(known<at).cr_error("INVALID_CHRONOLOGY","Exit cannot be known before occurrence")
      if(!eid %in% names(windows)).cr_error("UNKNOWN_ENTITY","Exit entity lacks window")
      if(known>cutoff){ignored<-ignored+1L;next}
      if(eid %in% names(exit_map)).cr_error("DUPLICATE_EXIT","One exit per entity required")
      exit_map[[eid]]<-at
    }
  }
  counts<-matrix(0,n,n);exposure<-numeric(n);excluded<-list();used<-character()
  exclude<-function(eid,reason){excluded[[length(excluded)+1L]]<<-list(entity_id=eid,reason=reason)}
  for(eid in names(windows)) {
    w<-windows[[eid]];start<-w$start;end<-w$end;complete<-w$complete
    events<-groups[[eid]]
    if(length(events))events<-events[order(vapply(events,function(e)as.numeric(e$at),numeric(1)))]
    dates<-vapply(events,function(e)as.numeric(e$at),numeric(1))
    if(anyDuplicated(dates)).cr_error("AMBIGUOUS_EVENT","Multiple ratings at same date need resolution")
    seen_default<-FALSE
    for(event in events){if(seen_default&&event$state!=default).cr_error("DEFAULT_NOT_ABSORBING","Observed rating leaves default");if(event$state==default)seen_default<-TRUE}
    before<-events[dates<=as.numeric(start)]
    if(!length(before)){exclude(eid,"missing_initial_state");next}
    origin<-before[[length(before)]]$state;stop<-if(is.null(exit_map[[eid]]))complete else min(complete,exit_map[[eid]])
    if(stop<start){exclude(eid,"exited_before_start");next}
    transitions<-events[dates>as.numeric(start)&dates<=as.numeric(stop)]
    if(estimator=="cohort") {
      terminal<-if(length(transitions))transitions[[length(transitions)]]$state else origin
      if(stop<end&&terminal!=default){exclude(eid,"censored_before_endpoint");next}
      counts[origin,terminal]<-counts[origin,terminal]+1;used<-c(used,eid)
    } else {
      current<-origin;previous<-start
      for(event in transitions) {
        exposure[current]<-exposure[current]+as.numeric(event$at-previous)/basis
        if(event$state!=current)counts[current,event$state]<-counts[current,event$state]+1
        previous<-event$at;current<-event$state
      }
      exposure[current]<-exposure[current]+as.numeric(stop-previous)/basis;used<-c(used,eid)
    }
  }
  denominator<-if(estimator=="cohort")rowSums(counts) else exposure
  empty<-states[seq_len(n)!=default&denominator==0]
  if(length(empty)&&empty_row_policy=="error").cr_error("EMPTY_TRANSITION_ROW","Unobserved nondefault origins",list(states=as.list(empty)))
  matrix<-matrix(0,n,n)
  for(i in seq_len(n)) {
    if(i==default){if(estimator=="cohort")matrix[i,i]<-1}
    else if(denominator[i]>0){matrix[i,]<-counts[i,]/denominator[i];if(estimator=="duration")matrix[i,i]<- -sum(matrix[i,])}
    else if(estimator=="cohort")matrix[i,i]<-1
  }
  .cr_result(list(matrix=matrix,matrix_kind=if(estimator=="cohort")"transition" else "generator",counts=counts,exposure_years=exposure),"pd_term_structure.estimate_transition_matrix",if(estimator=="cohort")"transition_probabilities" else "per_year",.cr_term_context(context),
    diagnostics=list(states=as.list(states),default_state=default_state,estimator=estimator,knowledge_date=knowledge_date,observation_scheme=observation_scheme,days_per_year=basis,step_length_years=if(is.null(period))NULL else period/basis,empty_row_policy=empty_row_policy,unidentified_rows=as.list(empty),excluded_entities=excluded,used_entities=as.list(used),ignored_not_yet_known_records=ignored,default_absorbing=TRUE,orientation="origin_rows_destination_columns"),
    limitations=c("Cohort estimates use complete cases and can be selection-biased by informative censoring. Duration MLE assumes all transition times observed and homogeneous Markov intensities.","Identity/zero-generator completion of unidentified rows is an explicit assumption, not an estimated zero migration rate. Default has no cure transition in this contract."))
}
