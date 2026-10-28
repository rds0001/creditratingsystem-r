# Release-aware macro histories and shared scenario dynamics, implemented natively.
.cr_m_unwrap <- function(value) if(inherits(value,'CalcResult')) value$value else value
.cr_m_integer <- function(value,name,minimum=0) {
 x<-.cr_numeric(value,name);if(length(x)!=1L||x!=floor(x)||x<minimum) .cr_error('MACRO_INPUT',paste(name,'must be an integer at least',minimum));as.integer(x)
}
.cr_m_ids <- function(value,name) {
 v<-unlist(value,use.names=FALSE)
 if(!is.character(v)||!length(v)||anyNA(v)||any(!nzchar(v))||anyDuplicated(v)) .cr_error('INVALID_IDENTIFIERS',paste(name,'requires unique nonempty strings'))
 v
}
.cr_m_dates <- function(values,frequency) {
 if(length(frequency)!=1L||!frequency %in% c('monthly','quarterly','annual')) .cr_error('MACRO_FREQUENCY','Declare monthly, quarterly or annual')
 ds<-vapply(values,function(v).cr_date(v,'date'),character(1))
 if(!length(ds)||anyDuplicated(ds)||!identical(ds,sort(ds))) .cr_error('INVALID_TIME_GRID','Unique increasing dates required')
 step<-c(monthly=1,quarterly=3,annual=12)[frequency];months<-as.integer(substr(ds,1,4))*12+as.integer(substr(ds,6,7))
 if(any(diff(months)!=step)) .cr_error('MACRO_FREQUENCY','Complete regular calendar grid required; no interpolation')
 ds
}
.cr_m_window <- function(window) {
 if(!.cr_object(window)) .cr_error('MACRO_WINDOW','Explicit start/end/known_at window required')
 w<-lapply(c('start','end','known_at'),function(k).cr_date(window[[k]],k));names(w)<-c('start','end','known_at')
 if(!(w$start<=w$end&&w$end<=w$known_at)) .cr_error('MACRO_WINDOW','Require start <= end <= known_at')
 w
}
.cr_m_rows <- function(x) lapply(seq_len(nrow(x)),function(i)as.list(unname(x[i,])))
.cr_m_array <- function(x) lapply(seq_len(dim(x)[1]),function(i).cr_m_rows(matrix(x[i,,],nrow=dim(x)[2],ncol=dim(x)[3])))
.cr_m_matrix <- function(x,name) {
 if(is.list(x)) x<-do.call(rbind,lapply(x,function(v)unlist(v,use.names=FALSE)))
 value<-.cr_numeric(x,name);if(length(dim(value))!=2L) .cr_error('INCOMPATIBLE_SHAPE',paste(name,'must be a numeric matrix'));value
}
.cr_m_cube_values <- function(x,name) {
 if(is.list(x)) {
  if(!length(x)) .cr_error('EMPTY_INPUT','Cube cannot be empty')
  mats<-lapply(x,function(v).cr_m_matrix(v,name));shape<-dim(mats[[1]])
  if(any(!vapply(mats,function(v)identical(dim(v),shape),logical(1)))) .cr_error('INCOMPATIBLE_SHAPE','Cube matrices must have equal shape')
  a<-array(0,c(length(mats),shape));for(i in seq_along(mats))a[i,,]<-mats[[i]];x<-a
 }
 value<-.cr_numeric(x,name);if(length(dim(value))!=3L) .cr_error('INCOMPATIBLE_SHAPE','Cube must have scenario,time,factor axes');value
}
transform_macro_history <- function(history,variable_transforms,frequency,fit_window,information_cutoff,context=NULL) {
 cutoff<-.cr_date(information_cutoff,'information_cutoff');window<-.cr_m_window(fit_window)
 if(window$known_at>cutoff) .cr_error('LEAKAGE','Fit cutoff exceeds information cutoff')
 if(!is.list(variable_transforms)||!length(variable_transforms)) .cr_error('MACRO_TRANSFORM','Per-variable transformation metadata required')
 variables<-.cr_m_ids(lapply(variable_transforms,`[[`,'variable_id'),'variable_ids');specs<-variable_transforms
 for(spec in specs) if(length(spec$transform)!=1L||!spec$transform %in% c('identity','log','difference','log_difference')||!is.logical(spec$standardize)||length(spec$standardize)!=1L||is.na(spec$standardize)||!is.character(spec$unit)||length(spec$unit)!=1L||!nzchar(spec$unit)) .cr_error('MACRO_TRANSFORM','Declare transform, boolean standardize and original unit')
 if(!is.list(history)||!length(history)) .cr_error('MACRO_HISTORY','Release-dated records required')
 selected<-list();seen<-character();excluded<-0L
 for(row in history) {
  if(!.cr_object(row)||length(row$variable_id)!=1L||!row$variable_id %in% variables) .cr_error('MACRO_HISTORY','History record needs declared variable_id')
  obs<-.cr_date(row$date,'date');released<-.cr_date(row$release_date,'release_date');key<-paste(row$variable_id,obs,released,sep='|');value<-.cr_numeric(row$value,'value')
  if(length(value)!=1L||released<obs) .cr_error('MACRO_HISTORY','Scalar value and release no earlier than observation required')
  if(key %in% seen) .cr_error('DUPLICATE_ID','Duplicate variable/date/release vintage');seen<-c(seen,key)
  if(released>cutoff){excluded<-excluded+1L;next}
  cell<-paste(row$variable_id,obs,sep='|');if(is.null(selected[[cell]])||selected[[cell]]$release_date<released)selected[[cell]]<-row
 }
 dates<-.cr_m_dates(sort(unique(vapply(selected,`[[`,character(1),'date'))),frequency)
 if(any(vapply(dates,function(t)any(vapply(variables,function(v)is.null(selected[[paste(v,t,sep='|')]]),logical(1))),logical(1)))) .cr_error('MACRO_MISSING','Aligned complete variables required; no imputation')
 raw<-matrix(0,length(dates),length(variables));releases<-list()
 for(i in seq_along(dates)) {releases[[i]]<-list();for(j in seq_along(variables)){r<-selected[[paste(variables[j],dates[i],sep='|')]];raw[i,j]<-r$value;releases[[i]][[j]]<-r$release_date}}
 drop<-as.integer(any(vapply(specs,function(s)s$transform %in% c('difference','log_difference'),logical(1))));output_dates<-dates[seq_len(length(dates)-drop)+drop];x<-matrix(0,length(output_dates),length(variables));state<-list()
 fit_rows<-which(output_dates>=window$start&output_dates<=window$end)
 if(length(fit_rows)<2L) .cr_error('INSUFFICIENT_OBSERVATIONS','At least two transformation fit observations required')
 fit_values<-matrix(NA_real_,length(dates),length(variables))
 for(i in seq_along(dates))for(j in seq_along(variables)) {
  eligible<-Filter(function(r)r$variable_id==variables[j]&&r$date==dates[i]&&r$release_date<=window$known_at,history)
  if(length(eligible)){index<-which.max(as.numeric(as.Date(vapply(eligible,`[[`,character(1),'release_date'))));fit_values[i,j]<-eligible[[index]]$value}
 }
 for(j in seq_along(specs)) {
  spec<-specs[[j]];kind<-spec$transform;level<-raw[,j]
  if(kind %in% c('log','log_difference')&&any(level<=0)) .cr_error('MACRO_LOG_DOMAIN','Log original values must be positive')
  base<-if(kind %in% c('log','log_difference'))log(level)else level
  transformed<-if(kind %in% c('difference','log_difference'))diff(base)else base[seq_len(length(base)-drop)+drop]
  fitting<-numeric(length(fit_rows))
  for(qq in seq_along(fit_rows)) {
   original<-fit_rows[qq]+drop;needed<-if(kind %in% c('difference','log_difference'))c(original-1L,original)else original;vals<-fit_values[needed,j]
   if(anyNA(vals)) .cr_error('LEAKAGE','Transformation fit data not released by cutoff')
   if(kind %in% c('log','log_difference')){if(any(vals<=0)) .cr_error('MACRO_LOG_DOMAIN','Training log values must be positive');vals<-log(vals)}
   fitting[qq]<-if(length(vals)==2L)vals[2]-vals[1]else vals[1]
  }
  center<-if(spec$standardize)mean(fitting)else 0;scale<-if(spec$standardize)sqrt(mean((fitting-mean(fitting))^2))else 1
  if(scale<=0) .cr_error('MACRO_ZERO_VARIANCE','Constant training series cannot be standardized')
  x[,j]<-(transformed-center)/scale
  state[[j]]<-list(variable_id=variables[j],transform=kind,unit=spec$unit,standardize=spec$standardize,center=center,scale=scale,initial_original_value=level[1],last_original_value=tail(level,1L),training_last_original_value=fit_values[tail(fit_rows,1L)+drop,j],standard_deviation_ddof=0)
 }
 train_x<-matrix(0,length(fit_rows),length(variables))
 for(i in seq_along(fit_rows))for(j in seq_along(state)) {
  spec<-state[[j]];original<-fit_rows[i]+drop;kind<-spec$transform;val<-fit_values[original,j]
  if(kind %in% c('log','log_difference'))val<-log(val)
  if(kind %in% c('difference','log_difference')){previous<-fit_values[original-1L,j];val<-val-if(kind=='log_difference')log(previous)else previous}
  train_x[i,j]<-(val-spec$center)/spec$scale
 }
 units<-lapply(specs,function(s)if(s$standardize)'standard_deviation'else if(s$transform=='log_difference')'log_ratio'else if(s$transform=='log')paste0('log(',s$unit,')')else if(s$transform=='difference')paste0('change(',s$unit,')')else s$unit)
 result<-list(schema_version='1.0',kind='MacroHistory',dates=as.list(output_dates),values=.cr_m_rows(x),variable_ids=as.list(variables),frequency=frequency,release_dates=releases[seq_len(length(releases)-drop)+drop],units=units,information_cutoff=cutoff,fit_window=window,inverse_state=state,training_dates=as.list(output_dates[fit_rows]),training_values=.cr_m_rows(train_x),training_vintage=window$known_at,dropped_initial_dates=as.list(if(drop)dates[1]else character()),data_origin='USER_SUPPLIED')
 .cr_result(result,'macro.transform_macro_history','macro_history',context,diagnostics=list(excluded_future_releases=excluded,training_vintage_separated=TRUE,data_fingerprint=.cr_hash(history)),limitations='No seasonal adjustment, frequency conversion or missing-data imputation is inferred.')
}
.cr_m_history <- function(history,training,missing=FALSE) {
 h<-.cr_m_unwrap(history);if(!.cr_object(h)) .cr_error('MACRO_HISTORY','Explicit history object required')
 variables<-.cr_m_ids(h$variable_ids,'variable_ids');dates<-.cr_m_dates(h$dates,h$frequency);window<-.cr_m_window(training);source<-h$values;use_dates<-dates
 if(!is.null(h$training_vintage)) {
  if(h$training_vintage!=window$known_at||.cr_hash(h$fit_window)!=.cr_hash(window)) .cr_error('LEAKAGE','Fit requires frozen transformation window/vintage')
  source<-h$training_values;use_dates<-unlist(h$training_dates)
 }
 if(missing) {
  if(is.list(source))x<-do.call(rbind,lapply(source,function(row)vapply(row,function(v)if(is.null(v))NA_real_ else as.numeric(v),numeric(1)))) else x<-source
  if(!is.numeric(x)||any(is.infinite(x))) .cr_error('MACRO_HISTORY','Numeric/missing observation matrix required')
 } else x<-.cr_m_matrix(source,'history_values')
 if(!identical(dim(x),c(length(use_dates),length(variables)))) .cr_error('INCOMPATIBLE_SHAPE','History matrix must match dates and variables')
 ix<-which(use_dates>=window$start&use_dates<=window$end);if(!length(ix)) .cr_error('INSUFFICIENT_OBSERVATIONS','No observations in training window')
 if(is.null(h$training_vintage)) {
  releases<-h$release_dates;if(!is.list(releases)||length(releases)!=length(dates)) .cr_error('MACRO_HISTORY','Per-observation release dates required')
  for(i in ix) {r<-vapply(releases[[i]],function(v).cr_date(v,'release_date'),character(1));if(length(r)!=length(variables)||any(r>window$known_at|r<dates[i])) .cr_error('LEAKAGE','Future or invalid training release')}
 }
 units<-unlist(h$units);if(!is.character(units)||length(units)!=length(variables)||any(!nzchar(units))) .cr_error('MACRO_UNITS','One explicit unit per variable required')
 fit_dates<-use_dates[ix];.cr_m_dates(as.list(fit_dates),h$frequency)
 list(x=x[ix,,drop=FALSE],dates=fit_dates,variables=variables,h=h,window=window)
}
.cr_m_sqrt_cov <- function(covariance) {
 a<-.cr_m_matrix(covariance,'covariance');n<-nrow(a)
 if(n!=ncol(a)||!n||any(diag(a)<0)) .cr_error('INVALID_COVARIANCE','Square covariance with nonnegative variances required')
 scale<-sqrt(diag(a));active<-scale>0
 if(any(a[!active,,drop=FALSE]!=0)||any(a[,!active,drop=FALSE]!=0)) .cr_error('NON_PSD_MATRIX','Zero variance requires zero cross covariance')
 root<-matrix(0,n,n);ev<-numeric()
 if(any(active)) {
  corr<-a[active,active,drop=FALSE]/outer(scale[active],scale[active])
  if(max(abs(corr-t(corr)))>1e-10) .cr_error('INVALID_COVARIANCE','Symmetry required after unit normalization')
  eig<-eigen((corr+t(corr))/2,symmetric=TRUE);ev<-eig$values
  if(min(ev)< -1e-10) .cr_error('NON_PSD_MATRIX','Covariance is not PSD; no spectral repair inferred')
  root[active,active]<-diag(scale[active],sum(active))%*%eig$vectors%*%diag(sqrt(pmax(ev,0)),length(ev))%*%t(eig$vectors)
 }
 list(root=root,diagnostics=list(factorization='CORRELATION_STANDARDIZED_PSD_FACTOR',minimum_correlation_eigenvalue=if(length(ev))min(ev)else 0,roundoff_clipped_eigenvalues=as.list(sort(ev[ev<0])),maximum_covariance_roundoff=max(abs(root%*%t(root)-a))))
}
.cr_m_measure <- function(model,context) {
 ctx<-.cr_result(NULL,'macro.context',context=context)$context
 if(!is.null(ctx$probability_measure)&&!identical(ctx$probability_measure,model$target$probability_measure)) .cr_error('TARGET_MISMATCH','Context measure conflicts with frozen macro model')
 ctx
}
.cr_m_stationary <- function(model) {
 p<-model$parameters;k<-length(model$feature_order);lags<-.cr_m_integer(p$lags,'lags',1)
 if(!p$dynamics %in% c('var','vecm')||any(unlist(p$trend)!=0)) .cr_error('TTC_INITIALIZATION','Gaussian equilibrium requires constant linear dynamics without trend')
 coefs<-lapply(p$lag_coefficients,function(v).cr_m_matrix(v,'lag_coefficients'));f<-matrix(0,k*lags,k*lags);f[seq_len(k),]<-do.call(cbind,coefs)
 if(lags>1L)f[(k+1L):(k*lags),seq_len(k*(lags-1L))]<-diag(k*(lags-1L))
 radius<-max(Mod(eigen(f,only.values=TRUE)$values));if(radius>=1-1e-10) .cr_error('NONSTATIONARY_TTC','Unit-root or explosive process has no finite Gaussian stationary law')
 mean<-rep(as.vector(solve(diag(k)-Reduce(`+`,coefs),unlist(p$intercept))),lags);covariance<-matrix(0,k*lags,k*lags);covariance[seq_len(k),seq_len(k)]<-.cr_m_matrix(p$covariance,'covariance');power<-f;converged<-FALSE
 for(i in seq_len(100L)){addition<-power%*%covariance%*%t(power);covariance<-covariance+addition;power<-power%*%power;if(max(abs(addition))<=1e-12*max(max(abs(covariance)),.Machine$double.xmin)){converged<-TRUE;break}}
 if(!converged) .cr_error('NUMERICAL_FAILURE','Stationary covariance doubling did not converge')
 list(mean=mean,covariance=covariance,f=f,radius=radius)
}
.cr_m_sampling_initial <- function(model,input,initial_state,sampling_contract,context) {
 p<-model$parameters;s<-dim(input$z)[1];k<-dim(input$z)[3];lags<-.cr_m_integer(p$lags,'lags',1);contract<-if(is.null(sampling_contract))list(law='FORECAST_CONDITIONAL')else sampling_contract
 if(!is.list(contract)||length(contract$law)!=1L) .cr_error('TTC_INITIALIZATION','Explicit sampling law required')
 law<-contract$law;ctx<-.cr_m_measure(model,context);initial<-if(is.null(initial_state))p$initial_history else initial_state
 if(law=='FORECAST_CONDITIONAL') {
  if(identical(ctx$conditioning,'TTC')) .cr_error('TTC_INITIALIZATION','TTC requires equilibrium or cycle-mixture law')
  contract$conditioning_scope<-'PIT';contract$initialization<-if(is.null(initial_state))'LAST_OBSERVED_STATE'else 'DECLARED_FIXED_STATE'
 } else if(law=='STATIONARY_GAUSSIAN') {
  if(!is.null(initial_state)||input$kind!='STANDARD_NORMAL') .cr_error('TTC_INITIALIZATION','Stationary Gaussian requires initial and future normal draws')
  if(any(!vapply(c('cycle_law_id','equilibrium_assumptions'),function(v)is.character(contract[[v]])&&length(contract[[v]])==1L&&nzchar(trimws(contract[[v]])),logical(1)))) .cr_error('TTC_INITIALIZATION','Cycle law ID and equilibrium assumptions required')
  draws<-.cr_m_matrix(contract$initial_state_draws,'initial_state_draws');contract$initial_state_draws<-NULL
  if(!identical(dim(draws),c(s,lags*k))) .cr_error('INCOMPATIBLE_SHAPE','Initial draws require scenario by companion-state axes')
  station<-.cr_m_stationary(model);fd<-.cr_m_sqrt_cov(station$covariance);states<-sweep(draws%*%t(fd$root),2,station$mean,'+');initial<-array(0,c(s,lags,k))
  for(i in seq_len(s))initial[i,,]<-matrix(states[i,],nrow=lags,ncol=k,byrow=TRUE)[lags:1L,,drop=FALSE]
  contract$conditioning_scope<-'TTC_STATIONARY';contract$initial_draw_hash<-.cr_hash(.cr_m_rows(draws));contract$stationary_mean<-as.list(station$mean);contract$stationary_covariance<-.cr_m_rows(station$covariance);contract$spectral_radius<-station$radius;contract$state_order<-'MOST_RECENT_FIRST';contract$factorization<-fd$diagnostics
 } else if(law=='DECLARED_CYCLE_MIXTURE') {
  if(any(!vapply(c('cycle_law_id','equilibrium_assumptions'),function(v)is.character(contract[[v]])&&length(contract[[v]])==1L&&nzchar(trimws(contract[[v]])),logical(1)))) .cr_error('TTC_INITIALIZATION','Cycle law and assumptions required')
  phases<-unlist(contract$phase_ids);prob<-contract$phase_probabilities
  if(length(phases)!=s||!is.character(phases)||!is.list(prob)||!length(prob)||is.null(names(prob))||any(!phases %in% names(prob))) .cr_error('TTC_INITIALIZATION','Every scenario requires declared cycle phase')
  pw<-.cr_numeric(unlist(prob),'phase_probabilities',vector_only=TRUE)
  if(any(pw<0)||abs(sum(pw)-1)>1e-10||any(vapply(names(prob),function(v)abs(sum(input$w[phases==v])-prob[[v]])>1e-10,logical(1)))) .cr_error('INVALID_WEIGHTS','Scenario weight mass must match cycle-phase law')
  initial<-.cr_m_cube_values(initial_state,'initial_state');if(!identical(dim(initial),c(s,lags,k))) .cr_error('TTC_INITIALIZATION','Cycle mixture requires scenario-specific histories')
  contract$conditioning_scope<-'TTC_DECLARED_CYCLE_MIXTURE';contract$initial_state_hash<-.cr_hash(.cr_m_array(initial));contract$equilibrium_verified<-FALSE
 } else .cr_error('TTC_INITIALIZATION','Unknown sampling law')
 list(initial=initial,contract=contract)
}
.cr_m_model <- function(method,parameters,h,dates,training,target,model_id,model_version,diagnostics,context) {
 train<-training;train$data_fingerprint<-.cr_hash(list(dates=as.list(dates),values=parameters[['_training_values']]));parameters[['_training_values']]<-NULL
 inverse_state<-h$inverse_state
 if(!is.null(inverse_state))for(i in seq_along(inverse_state))inverse_state[[i]]$last_original_value<-inverse_state[[i]]$training_last_original_value
 preprocessing<-list(frequency=h$frequency,units=as.list(h$units),inverse_state=inverse_state,training_end=tail(dates,1L),training_observation_count=length(dates))
 model<-.cr_model(list(schema_version='1.0',method_id=paste0('macro.',method),model_id=model_id,model_version=model_version,feature_order=as.list(h$variable_ids),parameters=parameters,preprocessing=preprocessing,target=.cr_cal_target(target),training=train,diagnostics=diagnostics,provenance=list(data_origin=if(is.null(training$data_origin))'USER_SUPPLIED'else training$data_origin,parameter_origin='ESTIMATED')))
 .cr_m_measure(model,context)
 .cr_fit_result(model,paste0('macro.',method),diagnostics,context)
}
fit_var <- function(transformed_history,lags,deterministic='constant',covariance_estimator='df_adjusted',training,target,model_id,model_version,context=NULL) {
 dat<-.cr_m_history(transformed_history,training);x<-dat$x;dates<-dat$dates;h<-dat$h;n<-nrow(x);k<-ncol(x);p<-.cr_m_integer(lags,'lags',1)
 if(length(deterministic)!=1L||!deterministic %in% c('none','constant','trend','constant_trend')||length(covariance_estimator)!=1L||!covariance_estimator %in% c('mle','df_adjusted')) .cr_error('MACRO_SPEC','Declare deterministic terms and covariance convention')
 constant<-deterministic %in% c('constant','constant_trend');trend<-deterministic %in% c('trend','constant_trend');d<-k*p+as.integer(constant)+as.integer(trend)
 if(n-p<=d) .cr_error('INSUFFICIENT_OBSERVATIONS','VAR requires positive residual degrees of freedom')
 design<-matrix(numeric(),nrow=n-p,ncol=0L)
 if(constant)design<-cbind(design,1)
 if(trend)design<-cbind(design,(p+1L):n)
 for(j in seq_len(p))design<-cbind(design,x[(p+1L-j):(n-j),,drop=FALSE])
 if(qr(design)$rank<d) .cr_error('RANK_DEFICIENT','VAR design rank deficient')
 if(k==1L) {
  fitted<-stats::lm.fit(design,x[(p+1L):n,1]);b<-matrix(fitted$coefficients,ncol=1);residual<-matrix(fitted$residuals,ncol=1);intercept<-if(constant)b[1,]else 0;trend_coef<-if(trend)b[as.integer(constant)+1L,]else 0
  coefs<-lapply(seq_len(p),function(j)matrix(b[as.integer(constant)+as.integer(trend)+j,],1,1));backend<-'stats::lm.fit (univariate AR)'
 } else {
  colnames(x)<-paste0('v',seq_len(k));exogen<-if(trend)matrix(seq_len(n),ncol=1,dimnames=list(NULL,'time_index'))else NULL
  fitted<-vars::VAR(x,p=p,type=if(constant)'const'else'none',exogen=exogen);b<-vars::Bcoef(fitted);residual<-as.matrix(stats::residuals(fitted));coefs<-lapply(seq_len(p),function(j)unname(b[,(j-1L)*k+seq_len(k),drop=FALSE]));intercept<-if(constant)unname(b[,'const'])else rep(0,k);trend_coef<-if(trend)unname(b[,'time_index'])else rep(0,k);backend<-'vars::VAR'
 }
 covariance<-crossprod(residual)/if(covariance_estimator=='df_adjusted')n-p-d else n-p;factorization<-.cr_m_sqrt_cov(covariance)$diagnostics
 companion<-matrix(0,k*p,k*p);companion[seq_len(k),]<-do.call(cbind,coefs)
 if(p>1L)companion[(k+1L):(k*p),seq_len(k*(p-1L))]<-diag(k*(p-1L))
 roots<-eigen(companion,only.values=TRUE)$values;roots<-roots[order(Re(roots),Im(roots))];radius<-max(Mod(roots))
 params<-list(dynamics='var',lags=p,lag_coefficients=lapply(coefs,.cr_m_rows),intercept=as.list(intercept),trend=as.list(trend_coef),covariance=.cr_m_rows(covariance),initial_history=.cr_m_rows(x[(n-p+1L):n,,drop=FALSE]),next_time_index=n+1L,deterministic=deterministic,covariance_estimator=covariance_estimator,'_training_values'=.cr_m_rows(x))
 diag<-list(backend=backend,observations=n-p,residual_df=n-p-d,residuals=.cr_m_rows(residual),residual_dates=as.list(dates[(p+1L):n]),companion_eigenvalues=lapply(roots,function(v)list(Re(v),Im(v))),spectral_radius=radius,stable=radius<1,stationarity_assumed=TRUE,eigenvalue_repair='NONE',factorization=factorization,training_only=TRUE)
 .cr_m_model('fit_var',params,h,dates,training,target,model_id,model_version,diag,context)
}
.cr_m_scenario_inputs <- function(innovations,variables,frequency) {
 value<-.cr_m_unwrap(innovations);if(!.cr_object(value)) .cr_error('MACRO_INNOVATIONS','Explicit innovation cube and axes required')
 ids<-.cr_m_ids(value$scenario_ids,'scenario_ids');factors<-.cr_m_ids(value$factor_ids,'factor_ids')
 if(!identical(factors,unlist(variables,use.names=FALSE))) .cr_error('FEATURE_MISMATCH','Innovation factor order differs from model channels')
 z<-.cr_m_cube_values(value$values,'innovations');times<-.cr_numeric(unlist(value$time_grid),'time_grid',vector_only=TRUE)
 if(!identical(dim(z),c(length(ids),length(times),length(factors)))||!length(times)) .cr_error('INCOMPATIBLE_SHAPE','Innovation axes must match scenario,time,factor')
 periods<-c(monthly=12,quarterly=4,annual=1)[frequency];if(length(periods)!=1L||is.na(periods)||max(abs(times-seq_along(times)/periods))>1e-12) .cr_error('INVALID_TIME_GRID','Consecutive frequency-aligned forecast times in years required')
 w<-if(is.null(value$weights))rep(1/length(ids),length(ids))else .cr_numeric(unlist(value$weights),'weights',vector_only=TRUE)
 if(length(w)!=length(ids)||any(w<0)||abs(sum(w)-1)>1e-12) .cr_error('INVALID_WEIGHTS','Nonnegative scenario weights summing one required');w<-w/sum(w)
 if(length(value$kind)!=1L||!value$kind %in% c('STANDARD_NORMAL','RAW_INNOVATIONS')) .cr_error('MACRO_INNOVATIONS','Declare standard normal or raw innovations')
 list(z=z,ids=ids,times=times,w=w,kind=value$kind)
}
.cr_m_scenario_model <- function(model) {
 m<-.cr_model(model);p<-m$parameters;k<-length(m$feature_order)
 if(!m$method_id %in% c('macro.fit_var','macro.fit_vecm','macro.fit_regime_switching','macro.fit_dynamic_factor')) .cr_error('UNSUPPORTED_MODEL','Frozen macro scenario model required')
 if(length(p$dynamics)!=1L||!p$dynamics %in% c('var','vecm','regime','dynamic_factor')) .cr_error('INVALID_MODEL','Unknown macro dynamics')
 expected<-c('macro.fit_var'='var','macro.fit_vecm'='vecm','macro.fit_regime_switching'='regime','macro.fit_dynamic_factor'='dynamic_factor');if(!identical(p$dynamics,unname(expected[m$method_id]))) .cr_error('INVALID_MODEL','Model method and dynamics disagree')
 if(p$dynamics %in% c('var','vecm')){if(!identical(dim(.cr_m_matrix(p$covariance,'covariance')),c(k,k))) .cr_error('INVALID_MODEL','Innovation covariance dimension differs');.cr_m_integer(p$next_time_index,'next_time_index',1)}
 list(model=m,p=p,k=k)
}
.cr_m_linear_paths <- function(parameters,initial_history,innovations) {
 e<-innovations;s<-dim(e)[1];h<-dim(e)[2];k<-dim(e)[3];p<-.cr_m_integer(parameters$lags,'lags',1);a<-lapply(parameters$lag_coefficients,function(v).cr_m_matrix(v,'lag_coefficients'));c<-unlist(parameters$intercept);trend<-unlist(parameters$trend);initial<-if(is.array(initial_history)&&length(dim(initial_history))==3L)initial_history else .cr_m_matrix(initial_history,'initial_history')
 if(!(identical(dim(initial),c(p,k))||identical(dim(initial),c(s,p,k)))||length(a)!=p||any(!vapply(a,function(v)identical(dim(v),c(k,k)),logical(1)))||length(c)!=k||length(trend)!=k) .cr_error('INVALID_MODEL','VAR/VECM recursion dimensions inconsistent')
 paths<-array(0,c(s,h,k))
 for(i in seq_len(s)) {
  state<-if(length(dim(initial))==3L)matrix(initial[i,,],nrow=p,ncol=k)else initial
  for(t in seq_len(h)) {
   value<-c+trend*(parameters$next_time_index+t-1L)+e[i,t,]
   for(lag in seq_len(p))value<-value+as.vector(a[[lag]]%*%state[p-lag+1L,])
   paths[i,t,]<-value;state<-rbind(if(p>1L)state[-1L,,drop=FALSE]else matrix(numeric(),0,k),value)
  }
 }
 if(any(!is.finite(paths))) .cr_error('NUMERICAL_FAILURE','Explosive recursion exceeded finite representation')
 paths
}
.cr_m_cube <- function(model,paths,ids,times,weights,diagnostics,context,method) {
 if(any(!is.finite(paths))) .cr_error('NUMERICAL_FAILURE','Macro recursion exceeded finite representation')
 units<-if(identical(diagnostics$output_space,'level'))lapply(model$preprocessing$inverse_state,`[[`,'unit')else model$preprocessing$units
 result<-list(schema_version='1.0',kind='ScenarioCube',axes=list('scenario','time','factor'),scenario_ids=as.list(ids),time_grid=as.list(times),time_unit='years',factor_ids=model$feature_order,values=.cr_m_array(paths),weights=as.list(weights),units=units,frequency=model$preprocessing$frequency,origin=model$preprocessing$training_end,information_cutoff=model$training$known_at,sampling_contract=diagnostics$sampling_contract,probability_measure=model$target$probability_measure,model_hash=.cr_hash(model),scenario_origin='SIMULATED',data_origin=model$provenance$data_origin)
 .cr_result(result,paste0('macro.',method),'scenario_cube',context,diagnostics=diagnostics,provenance=model$provenance)
}
.cr_m_inverse_paths <- function(paths,state) {
 if(!is.list(state)||length(state)!=dim(paths)[3]) .cr_error('MACRO_TRANSFORM','Frozen inverse state required')
 out<-paths
 for(j in seq_along(state)) {
  spec<-state[[j]];value<-matrix(paths[,,j],nrow=dim(paths)[1],ncol=dim(paths)[2])*spec$scale+spec$center;kind<-spec$transform
  if(kind %in% c('difference','log_difference')) {
   initial<-if(kind=='log_difference')log(spec$last_original_value)else spec$last_original_value
   for(i in seq_len(nrow(value)))value[i,]<-initial+cumsum(value[i,])
  }
  if(kind %in% c('log','log_difference'))value<-exp(value)
  out[,,j]<-value
 }
 if(any(!is.finite(out))) .cr_error('NUMERICAL_FAILURE','Inverse transformation exceeded finite representation')
 out
}
simulate_macro_scenarios <- function(fitted_model,innovations,initial_state=NULL,regime_uniforms=NULL,output_space='model',sampling_contract=NULL,context=NULL) {
 dat<-.cr_m_scenario_model(fitted_model);model<-dat$model;p<-dat$p
 if(!p$dynamics %in% c('var','vecm')) return(.cr_m_simulate_latent(model,innovations,initial_state,regime_uniforms,output_space,sampling_contract,context))
 if(!is.null(regime_uniforms)) .cr_error('MACRO_INNOVATIONS','Linear dynamics do not consume regime draws')
 input<-.cr_m_scenario_inputs(innovations,model$feature_order,model$preprocessing$frequency);fd<-.cr_m_sqrt_cov(p$covariance);e<-input$z
 if(input$kind=='STANDARD_NORMAL')for(i in seq_len(dim(e)[1]))for(t in seq_len(dim(e)[2]))e[i,t,]<-as.vector(fd$root%*%e[i,t,])
 sampling<-.cr_m_sampling_initial(model,input,initial_state,sampling_contract,context);paths<-.cr_m_linear_paths(p,sampling$initial,e)
 if(output_space=='level'){if(!is.null(initial_state)||sampling$contract$law!='FORECAST_CONDITIONAL') .cr_error('MACRO_TRANSFORM','Level reconstruction requires fitted fixed-state anchor');paths<-.cr_m_inverse_paths(paths,model$preprocessing$inverse_state)}else if(output_space!='model') .cr_error('MACRO_TRANSFORM','output_space must be model or level')
 .cr_m_cube(model,paths,input$ids,input$times,input$w,list(innovation_hash=.cr_hash(.cr_m_unwrap(innovations)),factorization=fd$diagnostics,output_space=output_space,shared_macro_across_entities=TRUE,sampling_contract=sampling$contract),context,'simulate_macro_scenarios')
}
block_bootstrap_scenarios <- function(residual_history,shared_draws,factor_ids,scenario_ids,time_grid,frequency,block_length,scheme='moving',weights=NULL,context=NULL) {
 residual<-.cr_m_matrix(residual_history,'residual_history');factors<-.cr_m_ids(factor_ids,'factor_ids');ids<-.cr_m_ids(scenario_ids,'scenario_ids')
 if(ncol(residual)!=length(factors)) .cr_error('INCOMPATIBLE_SHAPE','Residual columns must match factors')
 n<-nrow(residual);draws<-.cr_m_cube_values(shared_draws,'shared_draws');times<-.cr_numeric(unlist(time_grid),'time_grid',vector_only=TRUE);h<-length(times)
 if(!identical(dim(draws),c(length(ids),h,2L))||any(draws<0|draws>=1)) .cr_error('INVALID_DRAWS','Scenario,time,[start,restart] uniforms in [0,1) required')
 block<-.cr_numeric(block_length,'block_length');if(length(block)!=1L||block<1||block>n||length(scheme)!=1L||!scheme %in% c('moving','circular','stationary')) .cr_error('MACRO_BOOTSTRAP','Valid block scheme/length required')
 if(scheme!='stationary'&&block!=floor(block)) .cr_error('MACRO_BOOTSTRAP','Moving/circular lengths must be integers')
 selected<-matrix(0L,length(ids),h);blocks<-selected;values<-array(0,c(length(ids),h,length(factors)))
 for(i in seq_along(ids)) {
  position<-0L;block_id<- -1L
  for(t in seq_len(h)) {
   restart<-t==1L||if(scheme=='stationary')draws[i,t,2]<1/block else (t-1L)%%as.integer(block)==0L
   if(restart){choices<-if(scheme=='moving')n-as.integer(block)+1L else n;position<-floor(draws[i,t,1]*choices);block_id<-block_id+1L}else position<-(position+1L)%%n
   selected[i,t]<-position;blocks[i,t]<-block_id;values[i,t,]<-residual[position+1L,]
  }
 }
 result<-list(kind='RAW_INNOVATIONS',values=.cr_m_array(values),factor_ids=as.list(factors),scenario_ids=as.list(ids),time_grid=as.list(times),weights=as.list(if(is.null(weights))rep(1/length(ids),length(ids))else weights))
 .cr_m_scenario_inputs(result,factors,frequency)
 .cr_result(result,'macro.block_bootstrap_scenarios','innovations_cube',context,diagnostics=list(scheme=scheme,block_length=block,selected_indices=.cr_m_rows(selected),block_ids=.cr_m_rows(blocks),shared_draw_hash=.cr_hash(if(is.array(shared_draws)).cr_m_array(shared_draws)else shared_draws),joint_variables=TRUE),limitations='Historical blocks retain empirical local dependence, not a guarantee about future regimes or tail events.')
}
.cr_m_gaussian_moments <- function(model,horizon,initial_state=NULL) {
 p<-model$parameters;k<-length(model$feature_order)
 if(p$dynamics=='dynamic_factor')return(.cr_m_factor_moments(model,horizon,initial_state))
 if(!p$dynamics %in% c('var','vecm')) .cr_error('UNSUPPORTED_CONDITIONER','Gaussian conditioning cannot replace a hidden-regime mixture')
 initial<-if(is.null(initial_state))p$initial_history else initial_state
 mean_path<-.cr_m_linear_paths(p,initial,array(0,c(1L,horizon,k)));mean<-as.vector(t(matrix(mean_path[1,,],nrow=horizon,ncol=k)));dimension<-horizon*k
 impulses<-array(0,c(dimension,horizon,k));for(j in seq_len(dimension))impulses[j,(j-1L)%/%k+1L,(j-1L)%%k+1L]<-1
 response<-.cr_m_linear_paths(p,matrix(0,p$lags,k),impulses)-.cr_m_linear_paths(p,matrix(0,p$lags,k),array(0,dim(impulses)))
 mapping<-matrix(0,dimension,dimension);for(j in seq_len(dimension))mapping[,j]<-as.vector(t(matrix(response[j,,],nrow=horizon,ncol=k)))
 covariance<-mapping%*%kronecker(diag(horizon),.cr_m_matrix(p$covariance,'covariance'))%*%t(mapping)
 list(mean=mean,covariance=covariance)
}
condition_macro_paths <- function(fitted_model,innovations,conditioning_points,initial_state=NULL,sampler='gaussian_equality',sampling_contract=NULL,context=NULL) {
 dat<-.cr_m_scenario_model(fitted_model);model<-dat$model;p<-dat$p;k<-dat$k
 if(!identical(sampler,'gaussian_equality')) .cr_error('UNSUPPORTED_CONDITIONER','Only linear Gaussian equality conditioning supported')
 if(p$dynamics=='regime') .cr_error('UNSUPPORTED_CONDITIONER','Hidden-regime mixture requires mixture-specific conditioning')
 unconditional<-simulate_macro_scenarios(model,innovations,initial_state,sampling_contract=sampling_contract,context=context);cube<-unconditional$value;paths<-.cr_m_cube_values(cube$values,'paths');s<-dim(paths)[1];h<-dim(paths)[2]
 if(!is.list(conditioning_points)) .cr_error('MACRO_CONDITION','Explicit conditioning point records required')
 if(!length(conditioning_points))return(.cr_result(cube,'macro.condition_macro_paths','scenario_cube',context,diagnostics=list(constraint_count=0,maximum_constraint_error=0,conditioning='UNCONDITIONAL'),provenance=model$provenance))
 if(!identical(.cr_m_unwrap(innovations)$kind,'STANDARD_NORMAL')) .cr_error('UNSUPPORTED_CONDITIONER','Gaussian conditioning requires standard-normal draws, not bootstrap residuals')
 indexes<-integer();values<-numeric();factors<-unlist(model$feature_order)
 for(point in conditioning_points) {
  t<-.cr_m_integer(point$time_index,'time_index',0);name<-point$factor_id;value<-.cr_numeric(point$value,'condition_value')
  if(t>=h||length(name)!=1L||!name %in% factors||length(value)!=1L) .cr_error('MACRO_CONDITION','Valid zero-based time, factor and scalar value required')
  ix<-t*k+match(name,factors)
  if(ix %in% indexes) .cr_error('DUPLICATE_CONSTRAINT','Constrained coordinate must be unique')
  indexes<-c(indexes,ix);values<-c(values,value)
 }
 law<-cube$sampling_contract$law
 if(law=='DECLARED_CYCLE_MIXTURE') .cr_error('UNSUPPORTED_CONDITIONER','Cycle mixture requires mixture-specific conditioner')
 if(p$dynamics=='dynamic_factor')moments<-.cr_m_factor_moments(model,h,initial_state,sampling_contract)else if(law=='STATIONARY_GAUSSIAN') {
  station<-.cr_m_stationary(model);initial<-matrix(station$mean,nrow=p$lags,ncol=k,byrow=TRUE)[p$lags:1L,,drop=FALSE];moments<-.cr_m_gaussian_moments(model,h,initial);power<-diag(nrow(station$f));mapping<-matrix(0,h*k,nrow(power))
  for(t in seq_len(h)){power<-power%*%station$f;mapping[((t-1L)*k+1L):(t*k),]<-power[seq_len(k),,drop=FALSE]}
  moments$covariance<-moments$covariance+mapping%*%station$covariance%*%t(mapping)
 } else moments<-.cr_m_gaussian_moments(model,h,initial_state)
 mean<-moments$mean;covariance<-moments$covariance;c<-covariance[indexes,indexes,drop=FALSE];delta<-values-mean[indexes]
 .cr_m_sqrt_cov(c);scales<-sqrt(diag(c));active<-scales>0
 if(any(delta[!active]!=0)) .cr_error('INFEASIBLE_CONSTRAINT','Nonzero departure from deterministic constraint')
 inverse<-matrix(0,nrow(c),ncol(c));ev<-numeric();positive<-logical()
 if(any(active)) {
  corr<-c[active,active,drop=FALSE]/outer(scales[active],scales[active]);eig<-eigen((corr+t(corr))/2,symmetric=TRUE);ev<-eig$values;positive<-ev>1e-10
  ci<-eig$vectors[,positive,drop=FALSE]%*%diag(1/ev[positive],sum(positive))%*%t(eig$vectors[,positive,drop=FALSE]);standardized<-delta[active]/scales[active]
  if(max(abs(standardized-corr%*%ci%*%standardized))>1e-8) .cr_error('INFEASIBLE_CONSTRAINT','Constraint outside Gaussian covariance support')
  inverse[active,active]<-ci/outer(scales[active],scales[active])
 }
 gain<-covariance[,indexes,drop=FALSE]%*%inverse;flat<-matrix(0,s,h*k);for(i in seq_len(s))flat[i,]<-as.vector(t(matrix(paths[i,,],nrow=h,ncol=k)))
 conditioned<-flat+(matrix(values,s,length(values),byrow=TRUE)-flat[,indexes,drop=FALSE])%*%t(gain);error<-max(abs(sweep(conditioned[,indexes,drop=FALSE],2,values)))
 if(max(abs(sweep(sweep(conditioned[,indexes,drop=FALSE],2,values),2,ifelse(active,scales,1),'/')))>1e-7) .cr_error('NUMERICAL_FAILURE','Scale-normalized conditional residual exceeds tolerance')
 conditional_cov<-covariance-gain%*%covariance[indexes,,drop=FALSE];conditional_mean<-mean+as.vector(gain%*%delta);rank<-sum(positive);logdensity<-if(rank)-.5*(rank*log(2*pi)+sum(log(ev[positive]))+as.numeric(crossprod(delta,inverse%*%delta)))else 0
 if(rank==length(indexes))logdensity<-logdensity-sum(log(scales))
 out<-array(0,c(s,h,k));for(i in seq_len(s))out[i,,]<-matrix(conditioned[i,],nrow=h,ncol=k,byrow=TRUE)
 cube$values<-.cr_m_array(out);cube$conditioning_points<-conditioning_points;cube$sampling_contract$parent_conditioning_scope<-cube$sampling_contract$conditioning_scope;cube$sampling_contract$conditioning_scope<-'CONDITIONAL_STRESS'
 .cr_result(cube,'macro.condition_macro_paths','scenario_cube',context,diagnostics=list(constraint_count=length(indexes),constraint_rank=rank,maximum_constraint_error=error,conditional_mean=.cr_m_rows(matrix(conditional_mean,nrow=h,ncol=k,byrow=TRUE)),conditional_covariance=.cr_m_rows(conditional_cov),log_constraint_density_on_support=logdensity,density_coordinates=if(rank==length(indexes))'PHYSICAL_CONSTRAINTS'else 'STANDARDIZED_SUPPORT',density_is_not_event_probability=TRUE,conditioning='JOINT_GAUSSIAN_CROSS_COVARIANCE',mean_adjustment=.cr_m_rows(matrix(gain%*%delta,nrow=h,ncol=k,byrow=TRUE))),provenance=model$provenance)
}
reverse_stress_scenario <- function(fitted_model,loss_model,horizon,target_loss,shock_metric,initial_state=NULL,shock_bounds=NULL,initial_shock=NULL,tolerance=1e-7,max_iterations=500,context=NULL) {
 dat<-.cr_m_scenario_model(fitted_model);model<-dat$model;p<-dat$p;k<-dat$k;.cr_m_measure(model,context);h<-.cr_m_integer(horizon,'horizon',1)
 if(!p$dynamics %in% c('var','vecm')) .cr_error('UNSUPPORTED_REVERSE_MODEL','Reverse stress currently requires linear VAR/VECM dynamics')
 threshold<-.cr_numeric(target_loss,'target_loss');tol<-.cr_numeric(tolerance,'tolerance');limit<-.cr_m_integer(max_iterations,'max_iterations',1)
 if(length(threshold)!=1L||length(tol)!=1L||tol<=0||tol>=.01) .cr_error('MACRO_OPTIMIZATION','Scalar target and tolerance in (0,.01) required')
 dimension<-h*k;metric<-.cr_m_matrix(shock_metric,'shock_metric')
 if(!all(dim(metric)==c(dimension,dimension))||max(abs(metric-t(metric)))>1e-12||min(eigen(metric,symmetric=TRUE,only.values=TRUE)$values)<=0) .cr_error('MACRO_OPTIMIZATION','Symmetric positive-definite shock metric required')
 if(is.null(shock_bounds)){lower<-rep(-Inf,dimension);upper<-rep(Inf,dimension)}else{
  bound<-.cr_m_matrix(shock_bounds,'shock_bounds');if(!all(dim(bound)==c(dimension,2L))||any(bound[,1]>bound[,2])) .cr_error('MACRO_OPTIMIZATION','Lower/upper bounds per shock required');lower<-bound[,1];upper<-bound[,2]
 }
 start<-if(is.null(initial_shock))rep(0,dimension)else .cr_numeric(unlist(initial_shock),'initial_shock',vector_only=TRUE)
 if(length(start)!=dimension||any(start<lower|start>upper)) .cr_error('MACRO_OPTIMIZATION','Initial shock must satisfy bounds')
 fd<-.cr_m_sqrt_cov(p$covariance);initial<-if(is.null(initial_state))p$initial_history else initial_state
 paths<-function(z){shocks<-matrix(z,nrow=h,ncol=k,byrow=TRUE)%*%t(fd$root);value<-.cr_m_linear_paths(p,initial,array(as.vector(shocks),dim=c(1L,h,k)));matrix(value[1,,],nrow=h,ncol=k)}
 if(is.function(loss_model)) {
  response<-function(z){value<-.cr_numeric(loss_model(paths(z)),'loss_model output');if(length(value)!=1L) .cr_error('MACRO_OPTIMIZATION','Callback must return a finite scalar');as.numeric(value)}
  response_metadata<-list(kind='LOCAL_CALLBACK',portable=FALSE)
 } else {
  if(!.cr_object(loss_model)||!identical(loss_model$kind,'linear_predictor')||length(loss_model$link)!=1L||!loss_model$link %in% c('identity','logit','log')) .cr_error('MACRO_OPTIMIZATION','Callable or explicit linear_predictor response required')
  coefficients<-.cr_m_matrix(loss_model$coefficients,'loss_coefficients');intercept<-.cr_numeric(loss_model$intercept,'loss_intercept')
  if(!identical(dim(coefficients),c(h,k))||length(intercept)!=1L) .cr_error('INCOMPATIBLE_SHAPE','Response coefficients must match horizon and factors')
  response<-function(z){eta<-intercept+sum(coefficients*paths(z));value<-if(loss_model$link=='identity')eta else if(loss_model$link=='logit')stats::plogis(eta)else exp(eta);if(!is.finite(value)) .cr_error('NUMERICAL_FAILURE','Loss response overflow');as.numeric(value)}
  response_metadata<-loss_model
 }
 z<-start;multiplier<-0;penalty<-10;trace<-list();success<-FALSE
 for(outer in 0:11) {
  objective<-function(v){violation<-threshold-response(v);as.numeric(.5*crossprod(v,metric%*%v)+(max(0,multiplier+penalty*violation)^2-multiplier^2)/(2*penalty))}
  fit<-stats::optim(z,objective,method='L-BFGS-B',lower=lower,upper=upper,control=list(maxit=limit,factr=45,pgtol=tol*.1,ndeps=rep(1e-6,dimension)))
  z<-fit$par;achieved<-response(z);violation<-threshold-achieved;new_multiplier<-max(0,multiplier+penalty*violation)
  trace[[length(trace)+1L]]<-list(outer_iteration=outer,penalty=penalty,achieved_loss=achieved,positive_violation=max(0,violation),inner_converged=fit$convergence==0L,inner_message=fit$message,metric_distance_squared=as.numeric(crossprod(z,metric%*%z)))
  if(max(0,violation)<=tol&&abs(new_multiplier-multiplier)<=max(tol,1e-6)*max(1,new_multiplier)){success<-TRUE;break}
  multiplier<-new_multiplier;penalty<-penalty*10
 }
 achieved<-response(z);feasible<-achieved>=threshold-tol
 result<-list(status=if(feasible&&success)'FEASIBLE_LOCAL_SOLUTION'else if(feasible)'FEASIBLE_OPTIMIZATION_INCOMPLETE'else'INFEASIBLE_OR_LOCAL_SEARCH_FAILED',shocks=.cr_m_rows(matrix(z,nrow=h,ncol=k,byrow=TRUE)),candidate_path=.cr_m_rows(paths(z)),factor_ids=model$feature_order,target_loss=threshold,achieved_loss=achieved,feasible=feasible,maximum_constraint_violation=max(0,threshold-achieved),metric_distance_squared=as.numeric(crossprod(z,metric%*%z)),objective=.5*as.numeric(crossprod(z,metric%*%z)),shock_metric=.cr_m_rows(metric),response=response_metadata,model_hash=.cr_hash(model),global_optimality_guaranteed=FALSE)
 .cr_result(result,'macro.reverse_stress_scenario','reverse_stress_result',context,diagnostics=list(algorithm='POWELL_HESTENES_AUGMENTED_LAGRANGIAN_LBFGSB',trace=trace,innovation_factorization=fd$diagnostics,metric_is_not_probability=TRUE),provenance=model$provenance,limitations='A local feasible stress point is not a global minimum or an event probability. Failure to find feasibility does not generally certify global infeasibility.')
}
.cr_m_validate_cube <- function(cube) {
 value<-.cr_m_unwrap(cube)
 if(!is.list(value)||!identical(value$schema_version,'1.0')||!identical(value$kind,'ScenarioCube')||!identical(unlist(value$axes),c('scenario','time','factor'))) .cr_error('INVALID_SCENARIO_CUBE','Versioned scenario,time,factor cube required')
 ids<-.cr_m_ids(value$scenario_ids,'scenario_ids');factors<-.cr_m_ids(value$factor_ids,'factor_ids');x<-.cr_m_cube_values(value$values,'values');time<-.cr_numeric(unlist(value$time_grid),'time_grid',vector_only=TRUE)
 if(!identical(dim(x),c(length(ids),length(time),length(factors)))||!length(time)||any(diff(time)<=0)||time[1]<=0||!identical(value$time_unit,'years')) .cr_error('INVALID_SCENARIO_CUBE','Named axes require increasing positive year grid')
 periods<-switch(value$frequency,monthly=12,quarterly=4,annual=1,NULL);if(is.null(periods)||any(abs(time-seq_along(time)/periods)>1e-12)) .cr_error('INVALID_TIME_GRID','Macro grid must match declared frequency')
 weight<-.cr_numeric(unlist(value$weights),'weights',vector_only=TRUE)
 if(length(weight)!=length(ids)||any(weight<0)||abs(sum(weight)-1)>1e-10) .cr_error('INVALID_WEIGHTS','Cube weights must already normalize')
 units<-unlist(value$units);if(!is.character(units)||length(units)!=length(factors)||any(!nzchar(units))) .cr_error('MACRO_UNITS','One unit per factor required')
 if(length(value$probability_measure)!=1L||!value$probability_measure %in% c('PHYSICAL','RISK_NEUTRAL')) .cr_error('INCOMPATIBLE_MEASURE','Explicit macro measure required')
 origin<-.cr_date(value$origin,'origin');cutoff<-.cr_date(value$information_cutoff,'information_cutoff');if(cutoff<origin) .cr_error('INVALID_DATE','Cutoff cannot precede last macro observation')
 sampling<-value$sampling_contract;if(!is.list(sampling)||!is.character(sampling$law)||length(sampling$law)!=1L||length(sampling$conditioning_scope)!=1L||!sampling$conditioning_scope %in% c('PIT','TTC_STATIONARY','TTC_DECLARED_CYCLE_MIXTURE','CONDITIONAL_STRESS')) .cr_error('TTC_INITIALIZATION','Explicit PIT/TTC/stress sampling contract required')
 value
}
