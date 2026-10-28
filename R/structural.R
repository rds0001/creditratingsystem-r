.cr_struct_scalar <- function(x, name, lower = NULL, strict = FALSE) {
  .cr_numeric(x, name, vector_only = TRUE)
  if (length(x) != 1L || (!is.null(lower) && if (strict) x <= lower else x < lower)) .cr_error("INVALID_INPUT", paste(name, "must be a scalar in its declared domain"))
  as.numeric(x)
}
.cr_struct_measure <- function(measure, horizon, context) {
  if (!is.character(measure) || length(measure) != 1L || is.na(measure) || !measure %in% c("PHYSICAL", "RISK_NEUTRAL")) .cr_error("INCOMPATIBLE_MEASURE", "Declare PHYSICAL or RISK_NEUTRAL")
  ctx <- .cr_result(NULL, "context", context = context)$context
  expected <- list(probability_measure=measure,time_unit="years",horizon=horizon)
  for (key in names(expected)) if (!is.null(ctx[[key]]) && !identical(.cr_hash(ctx[[key]]), .cr_hash(expected[[key]]))) .cr_error("TARGET_MISMATCH", paste("Context",key,"conflicts with structural contract"))
  ctx
}
.cr_struct_args <- function(a,b,s,mu,t,q) {
  values <- c(.cr_struct_scalar(a,"asset_value",0,TRUE),.cr_struct_scalar(b,"barrier",0),.cr_struct_scalar(s,"volatility",0),.cr_struct_scalar(mu,"drift"),.cr_struct_scalar(t,"horizon",0),.cr_struct_scalar(q,"payout"))
  if(t>0 && (any(!is.finite(c(s*s*t,mu*t,q*t))) || (s>0 && s*s==0))) .cr_error("NUMERICAL_RANGE","Derived variance or drift outside supported range")
  values
}
.cr_struct_equity <- function(a,d,s,r,t,q) {
  ad <- a*exp(-q*t); dd <- d*exp(-r*t)
  if (!is.finite(ad) || !is.finite(dd) || ad==0 || (d>0 && dd==0)) .cr_error("NUMERICAL_OVERFLOW", "Discounted values outside representable range")
  if (d==0) return(list(equity_value=ad,delta=exp(-q*t),d1=NULL,d2=NULL))
  if (t==0 || s==0) return(list(equity_value=max(ad-dd,0),delta=exp(-q*t)*if (ad>dd) 1 else if (ad<dd) 0 else .5,d1=NULL,d2=NULL))
  v <- s*sqrt(t); d1 <- (log(a)-log(d)+(r-q+.5*s*s)*t)/v; d2 <- d1-v
  l1 <- log(ad)+stats::pnorm(d1,log.p=TRUE); l2 <- log(dd)+stats::pnorm(d2,log.p=TRUE)
  list(equity_value=exp(l1)*(-expm1(min(0,l2-l1))),delta=exp(-q*t)*stats::pnorm(d1),d1=d1,d2=d2)
}
merton_equity_value <- function(asset_value,debt_face,asset_volatility,risk_free_rate,maturity,payout=0,currency,context=NULL) {
  x <- .cr_struct_args(asset_value,debt_face,asset_volatility,risk_free_rate,maturity,payout)
  .cr_string(currency,"currency","INVALID_UNIT"); ctx <- .cr_struct_measure("RISK_NEUTRAL",x[5],context)
  value <- do.call(.cr_struct_equity,as.list(x))
  .cr_result(value,"structural.merton_equity_value",currency,ctx,
    diagnostics=list(probability_measure="RISK_NEUTRAL",time_unit="years",maturity=x[5],degenerate_delta_convention="one_half_at_kink"),
    limitations="Single terminal debt face and constant geometric Brownian asset dynamics; payout is a continuous yield.")
}
infer_merton_assets <- function(equity_value,equity_volatility,debt_face,risk_free_rate,maturity,asset_bounds,volatility_bounds,currency,payout=0,tolerance=1e-9,max_iterations=1000,context=NULL) {
  e <- .cr_struct_scalar(equity_value,"equity_value",0,TRUE); se <- .cr_struct_scalar(equity_volatility,"equity_volatility",0,TRUE)
  d <- .cr_struct_scalar(debt_face,"debt_face",0); r <- .cr_struct_scalar(risk_free_rate,"risk_free_rate"); t <- .cr_struct_scalar(maturity,"maturity",0,TRUE); q <- .cr_struct_scalar(payout,"payout")
  tol <- .cr_struct_scalar(tolerance,"tolerance",0,TRUE); limit <- .cr_struct_scalar(max_iterations,"max_iterations",1)
  if (tol>=.01 || limit!=floor(limit)) .cr_error("INVALID_INPUT","Invalid solver controls")
  for (b in list(asset_bounds,volatility_bounds)) {
    .cr_numeric(b,"bounds",vector_only=TRUE)
    if (length(b)!=2L || b[1]<=0 || b[2]<=b[1]) .cr_error("INVALID_BOUNDS","Strict positive increasing bounds required")
  }
  .cr_string(currency,"currency","INVALID_UNIT"); ctx <- .cr_struct_measure("RISK_NEUTRAL",t,context)
  lo <- log(c(asset_bounds[1],volatility_bounds[1])); hi <- log(c(asset_bounds[2],volatility_bounds[2]))
  residual <- function(x) {
    a <- exp(x[1]); s <- exp(x[2]); model <- .cr_struct_equity(a,d,s,r,t,q)
    c((model$equity_value-e)/e,(a*model$delta*s-e*se)/(e*se))
  }
  start <- pmin(pmax(log(c(e+d*exp(-r*t),se*e/(e+d*exp(-r*t)))),lo+1e-8*(hi-lo)),hi-1e-8*(hi-lo))
  # Native bounded nonlinear least squares followed by numerical-Jacobian Newton refinement.
  fitted <- stats::nlminb(start,function(x)sum(residual(x)^2),lower=lo,upper=hi,control=list(iter.max=limit,eval.max=3*limit,rel.tol=max(tol^2,1e-14),x.tol=1e-12))
  x <- fitted$par
  for (iteration in seq_len(30)) {
    err <- residual(x)
    if (max(abs(err))<=tol) break
    h <- 1e-5
    jac <- cbind((residual(x+c(h,0))-residual(x-c(h,0)))/(2*h),(residual(x+c(0,h))-residual(x-c(0,h)))/(2*h))
    step <- tryCatch(solve(jac,err),error=function(e)NULL)
    if (is.null(step)) break
    improved <- FALSE
    for (scale in 2^-(0:20)) {
      candidate <- pmin(pmax(x-scale*step,lo),hi)
      if (sum(residual(candidate)^2)<sum(err^2)) {x<-candidate;improved<-TRUE;break}
    }
    if (!improved) break
  }
  err <- residual(x)
  if (max(abs(err))>tol) .cr_error("NONCONVERGENCE","No in-bounds solution satisfies both equations",list(scaled_residuals=as.list(err),solver_message=fitted$message))
  .cr_result(list(asset_value=exp(x[1]),asset_volatility=exp(x[2])),"structural.infer_merton_assets","asset_parameters",ctx,
    diagnostics=list(scaled_residuals=as.list(err),converged=TRUE,evaluations=unname(fitted$evaluations[1]),asset_bounds=as.list(asset_bounds),volatility_bounds=as.list(volatility_bounds),currency=currency,probability_measure="RISK_NEUTRAL",time_unit="years",maturity=t),
    limitations="Numerical inversion of a specified equity option model does not identify the physical asset drift or validate its debt proxy.")
}
distance_to_default <- function(asset_value,default_barrier,asset_volatility,physical_drift,horizon,payout=0,context=NULL) {
  x <- .cr_struct_args(asset_value,default_barrier,asset_volatility,physical_drift,horizon,payout)
  a<-x[1];b<-x[2];s<-x[3];mu<-x[4];t<-x[5];q<-x[6];ctx<-.cr_struct_measure("PHYSICAL",t,context)
  ld <- if (b==0) NULL else log(a)-log(b)+(mu-q-.5*s*s)*t
  sd <- s*sqrt(t); dd <- if (b>0 && sd>0) ld/sd else NULL
  status <- if (!is.null(dd)) "FINITE" else if (b==0) "NO_POSITIVE_BARRIER" else if (ld==0) "DETERMINISTIC_AT_BARRIER" else if (ld>0) "DETERMINISTIC_ABOVE" else "DETERMINISTIC_BELOW"
  .cr_result(list(distance_to_default=dd,log_distance=ld,log_standard_deviation=sd,boundary_status=status),"structural.distance_to_default","standard_deviations",ctx,
    diagnostics=list(probability_measure="PHYSICAL",time_unit="years",horizon=t,event="terminal_asset_at_or_below_barrier"),limitations="No empirical default-frequency mapping or automatic TTC interpretation.")
}
merton_default_probability <- function(asset_value,debt_face,asset_volatility,drift_or_rate,horizon,measure,payout=0,context=NULL) {
  x <- .cr_struct_args(asset_value,debt_face,asset_volatility,drift_or_rate,horizon,payout)
  a<-x[1];d<-x[2];s<-x[3];mu<-x[4];t<-x[5];q<-x[6];ctx<-.cr_struct_measure(measure,t,context);dd<-NULL
  if (d==0) pd<-0 else {
    ld <- log(a)-log(d)+(mu-q-.5*s*s)*t
    if (t==0 || s==0) pd<-as.numeric(ld<=0) else {dd<-ld/(s*sqrt(t));pd<-stats::pnorm(-dd)}
  }
  .cr_result(pd,"structural.merton_default_probability","probability",ctx,
    diagnostics=list(distance_statistic=dd,probability_measure=measure,time_unit="years",horizon=t,event="terminal_asset_at_or_below_debt",drift_interpretation=if(measure=="PHYSICAL")"physical_expected_asset_return" else "risk_free_rate"),
    limitations="Risk-neutral PD is a pricing-model quantity, not a historical default forecast.")
}
first_passage_pd <- function(asset_value,barrier_spec,volatility,drift,horizon,measure,payout=0,context=NULL) {
  if (!.cr_object(barrier_spec) || !is.character(barrier_spec$kind) || length(barrier_spec$kind)!=1L || is.na(barrier_spec$kind) || !barrier_spec$kind %in% c("constant","exponential")) .cr_error("INVALID_BARRIER","Declare constant or exponential barrier")
  growth <- if (barrier_spec$kind=="constant") 0 else .cr_struct_scalar(barrier_spec$growth_rate,"growth_rate")
  if (barrier_spec$kind=="constant" && !is.null(barrier_spec$growth_rate) && !identical(as.numeric(barrier_spec$growth_rate),0)) .cr_error("INVALID_BARRIER","Constant barrier cannot grow")
  v <- .cr_struct_args(asset_value,barrier_spec$initial_level,volatility,drift,horizon,payout)
  a<-v[1];b<-v[2];s<-v[3];mu<-v[4];t<-v[5];q<-v[6];ctx<-.cr_struct_measure(measure,t,context)
  if (b==0) pd<-0 else if (a<=b) pd<-1 else if (t==0) pd<-0 else {
    x<-log(a)-log(b);m<-mu-q-.5*s*s-growth
    if (s==0) pd<-as.numeric(x+min(0,m*t)<=0) else {
      v<-s*sqrt(t);la<-stats::pnorm((-x-m*t)/v,log.p=TRUE);lb<- -2*m*x/(s*s)+stats::pnorm((-x+m*t)/v,log.p=TRUE)
      if(any(is.nan(c(la,lb)))) .cr_error("NUMERICAL_RANGE","First-passage tail outside numerical domain")
      largest<-max(la,lb);pd<-if(is.infinite(largest)&&largest<0)0 else min(1,exp(largest)*(exp(la-largest)+exp(lb-largest)))
    }
  }
  .cr_result(pd,"structural.first_passage_pd","probability",ctx,
    diagnostics=list(barrier_spec=barrier_spec,probability_measure=measure,time_unit="years",horizon=t,monitoring="continuous",event="asset_at_or_below_barrier_any_time_including_start"),
    limitations="Constant GBM dynamics and a constant or exponential barrier are required; this is not terminal-only default.")
}
simulate_structural_paths <- function(asset_dynamics,barrier_schedule,time_grid,innovations,measure,jump_counts=NULL,jump_innovations=NULL,bridge_uniforms=NULL,context=NULL) {
  if (!.cr_object(asset_dynamics)) .cr_error("INVALID_INPUT","asset_dynamics object required")
  times<-.cr_numeric(time_grid,"time_grid",vector_only=TRUE)
  if(length(times)<2L || times[1]!=0 || any(diff(times)<=0)) .cr_error("INVALID_TIME_GRID","Increasing years starting at zero required")
  a<-.cr_struct_scalar(asset_dynamics$asset_value,"asset_value",0,TRUE);s<-.cr_struct_scalar(asset_dynamics$volatility,"volatility",0);mu<-.cr_struct_scalar(asset_dynamics$drift,"drift")
  optional<-function(name)if(is.null(asset_dynamics[[name]]))0 else asset_dynamics[[name]]
  q<-.cr_struct_scalar(optional("payout"),"payout");intensity<-.cr_struct_scalar(optional("jump_intensity"),"jump_intensity",0)
  jm<-.cr_struct_scalar(optional("jump_log_mean"),"jump_log_mean");js<-.cr_struct_scalar(optional("jump_log_sd"),"jump_log_sd",0)
  ctx<-.cr_struct_measure(measure,tail(times,1),context);z<-.cr_numeric(innovations,"innovations")
  if(!is.matrix(z)||nrow(z)<1L||ncol(z)!=length(times)-1L).cr_error("INCOMPATIBLE_SHAPE","Innovations require paths by intervals matrix")
  b<-.cr_numeric(barrier_schedule,"barrier_schedule",vector_only=TRUE)
  if(length(b)!=length(times)||any(b<0)).cr_error("INVALID_BARRIER","Nonnegative barrier at every grid point required")
  if(xor(is.null(jump_counts),is.null(jump_innovations))||(intensity>0&&is.null(jump_counts))).cr_error("MISSING_INPUT","Explicit jump counts and innovations required")
  counts<-if(is.null(jump_counts))matrix(0,nrow(z),ncol(z)) else .cr_numeric(jump_counts,"jump_counts")
  jz<-if(is.null(jump_innovations))matrix(0,nrow(z),ncol(z)) else .cr_numeric(jump_innovations,"jump_innovations")
  if(!identical(dim(counts),dim(z))||!identical(dim(jz),dim(z))).cr_error("INCOMPATIBLE_SHAPE","Jump draws must match diffusion matrix")
  if(any(counts<0)|any(counts!=floor(counts))||(intensity==0&&any(counts!=0))).cr_error("INVALID_JUMPS","Invalid jump counts")
  bridge<-NULL
  if(!is.null(bridge_uniforms)) {
    bridge<-.cr_numeric(bridge_uniforms,"bridge_uniforms")
    if(!identical(dim(bridge),dim(z))||any(bridge<=0|bridge>=1)).cr_error("INVALID_BRIDGE","Bridge draws must match shape and lie in (0,1)")
    if(intensity>0||any(b<=0)).cr_error("INVALID_BRIDGE","Bridge supports positive barriers and pure diffusion only")
  }
  dt<-diff(times);compensation<-if(intensity==0)0 else intensity*expm1(jm+.5*js*js)
  if(!is.finite(compensation)).cr_error("NUMERICAL_OVERFLOW","Jump compensation overflow")
  increments<-sweep(z,2,s*sqrt(dt),"*")+matrix(rep((mu-q-compensation-.5*s*s)*dt,each=nrow(z)),nrow(z))+counts*jm+sqrt(counts)*js*jz
  logs<-matrix(log(a),nrow(z),length(times))
  for(j in seq_len(ncol(z)))logs[,j+1]<-logs[,j]+increments[,j]
  paths<-exp(logs);paths[,1]<-a
  if(any(!is.finite(paths))||any(paths==0)).cr_error("NUMERICAL_OVERFLOW","Asset path outside representable positive range")
  hits<-sweep(paths,2,b,"<=")
  if(!is.null(bridge)&&s>0) {
    x<-sweep(logs[,-ncol(logs),drop=FALSE],2,log(b[-length(b)]),"-")
    y<-sweep(logs[,-1,drop=FALSE],2,log(b[-1]),"-")
    probability<-exp(sweep(-2*pmax(x,0)*pmax(y,0),2,s*s*dt,"/"))
    hits[,-1]<-hits[,-1,drop=FALSE]|bridge<probability
  }
  defaulted<-hits
  for(j in seq_len(ncol(z)))defaulted[,j+1]<-defaulted[,j+1]|defaulted[,j]
  defaults<-lapply(seq_len(nrow(z)),function(i)if(any(defaulted[i,]))times[which(defaulted[i,])[1]] else NULL)
  survival<-colMeans(!defaulted);se<-sqrt(survival*(1-survival)/nrow(z))
  rows<-function(m)lapply(seq_len(nrow(m)),function(i)as.list(m[i,]))
  .cr_result(list(asset_paths=paths,default_time_upper_bounds=defaults,survival_curve=survival,survival_standard_error=se),"structural.simulate_structural_paths","paths",ctx,
    diagnostics=list(probability_measure=measure,time_unit="years",time_grid=as.list(times),n_paths=nrow(z),monitoring=if(is.null(bridge))"grid_only" else "continuous_diffusion_bridge",jump_law="compound_poisson_lognormal",jump_drift_compensation=compensation,
      innovations_fingerprint=.cr_hash(list(normal=rows(z),counts=rows(counts),jump_normal=rows(jz),bridge_uniform=if(is.null(bridge))NULL else rows(bridge))),
      barrier_miss_between_grid_points=is.null(bridge),default_times_are_interval_upper_bounds=TRUE,iid_mc_assumption_verified=FALSE),
    limitations=c("Supplied diffusion, jump-count, jump-normal and bridge draws must have the declared independent laws. Standard errors assume independent paths.","Jump path crossings between observation times are not observed; bridge correction is only supported without jumps. Exact within-interval hitting times are not returned."))
}
calibrate_distance_pd_mapping <- function(distances,outcomes,metadata,weights=NULL,evaluation_distances=NULL,bootstrap_indices=NULL,confidence_level=.95,out_of_bounds="error",context=NULL) {
  if(!.cr_object(metadata)||!identical(metadata$target$probability_measure,"PHYSICAL")).cr_error("INCOMPATIBLE_MEASURE","Observed default mapping requires PHYSICAL target")
  confidence<-.cr_struct_scalar(confidence_level,"confidence_level",0,TRUE)
  if(confidence>=1).cr_error("INVALID_INPUT","confidence_level must lie in (0,1)")
  fitted<-calibrate_isotonic(distances,outcomes,weights,metadata=metadata,increasing=FALSE,out_of_bounds=out_of_bounds,context=context)
  x<-.cr_numeric(distances,"distances",vector_only=TRUE);y<-.cr_numeric(outcomes,"outcomes",vector_only=TRUE)
  evaluation<-if(is.null(evaluation_distances))x else .cr_numeric(evaluation_distances,"evaluation_distances",vector_only=TRUE)
  predicted<-apply_calibrator(fitted,evaluation,target=metadata$target,base_model_version=metadata$base_model_version)$value
  lower<-upper<-NULL;count<-0L
  if(!is.null(bootstrap_indices)) {
    if(!is.null(weights)&&any(.cr_numeric(weights,"weights")!=1)).cr_error("INVALID_BOOTSTRAP","Bootstrap requires unit observation weights")
    indices<-.cr_numeric(bootstrap_indices,"bootstrap_indices")
    if(!is.matrix(indices)||nrow(indices)<2L||ncol(indices)!=length(x)||any(indices!=floor(indices))||any(indices<0|indices>=length(x))).cr_error("INVALID_BOOTSTRAP","At least two complete zero-based row resamples required")
    samples<-matrix(0,nrow(indices),length(evaluation))
    for(i in seq_len(nrow(indices))) {
      index<-indices[i,]+1L
      boot<-calibrate_isotonic(x[index],y[index],metadata=metadata,increasing=FALSE,out_of_bounds="clip",context=context)
      samples[i,]<-apply_calibrator(boot,evaluation,target=metadata$target,base_model_version=metadata$base_model_version)$value
    }
    lower<-apply(samples,2,stats::quantile,probs=(1-confidence)/2,type=7,names=FALSE)
    upper<-apply(samples,2,stats::quantile,probs=(1+confidence)/2,type=7,names=FALSE);count<-nrow(samples)
  }
  .cr_result(list(calibration_artifact=fitted$value,evaluation_distances=evaluation,pd=predicted,lower=lower,upper=upper),"structural.calibrate_distance_pd_mapping","empirical_pd_mapping",fitted$context,
    diagnostics=list(monotonicity="decreasing",bootstrap_samples=count,confidence_level=confidence,bootstrap_interpolation="linear",bootstrap_boundary_policy="clip",out_of_sample_validation_performed=FALSE,uncertainty=if(count==0)"none" else "pointwise_iid_percentile_bootstrap"),provenance=fitted$provenance,
    limitations=c(fitted$limitations,"Bootstrap quantile bands do not guarantee coverage for data-adaptive isotonic blocks. An independent time-separated test set remains necessary."))
}
