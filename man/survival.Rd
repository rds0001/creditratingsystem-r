\name{survival_models}
\alias{estimate_survival_curve}
\alias{fit_cox_ph}
\alias{fit_aft_model}
\alias{fit_discrete_hazard}
\alias{fit_cause_specific_hazards}
\alias{fit_fine_gray}
\alias{predict_event_curves}
\alias{estimate_multistate_transitions}
\title{Frozen survival, competing-risk and multistate models}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Native estimation and data-only event-curve prediction under explicit entry, censoring, landmark, feature and target contracts.}
\usage{
estimate_survival_curve(entry_time, exit_time, event_indicator,
  weight_policy, weights = NULL, confidence_level = .95, target,
  training, model_id, model_version, context = NULL)
fit_cox_ph(start_stop_panel, X, feature_order, ties, strata = NULL,
  covariance = "model_based", target, training, model_id,
    model_version,
  max_iter = 200, tolerance = 1e-9, context = NULL)
fit_aft_model(durations, events, X, feature_order, distribution,
    target,
  training, model_id, model_version, max_iter = 500,
  tolerance = 1e-9, context = NULL)
fit_discrete_hazard(person_period_panel, X, feature_order,
    period_grid,
  link, partial_period_policy, target, training, model_id,
    model_version,
  max_iter = 200, tolerance = 1e-9, context = NULL)
fit_cause_specific_hazards(start_stop_panel, X, event_types,
  feature_order, causes, ties, strata = NULL, target, training,
    model_id,
  model_version, covariance = "model_based", max_iter = 200,
  tolerance = 1e-9, context = NULL)
fit_fine_gray(event_times, event_types, X, cause_of_interest,
  feature_order, censoring_spec, target, training, model_id,
  model_version, max_iter = 200, tolerance = 1e-9, context = NULL)
predict_event_curves(fitted_model, covariate_paths, horizon_grid,
  feature_order, target, initial_state = NULL, event_states = NULL,
  start_time = 0, extrapolation = "error", quantile_levels = NULL,
  context = NULL)
estimate_multistate_transitions(event_history, states,
  allowed_transitions, initial_state, target, training, model_id,
  model_version, context = NULL)
}
\arguments{
\item{entry_time,exit_time}{Entry and exit vectors with zero or positive entry strictly less than exit. NULL entry means zero.}
\item{event_indicator,events}{Numeric observed zero/one events; missing outcomes are not non-events.}
\item{weight_policy}{frequency: weights represent independently replicated observations.}
\item{weights}{Nonnegative integer frequency weights with positive total.}
\item{confidence_level}{Interior probability for Greenwood log-log KM intervals.}
\item{target}{Full model target: event, positive horizon, time_unit (days/months/years), probability_measure=PHYSICAL, and conditioning. Units are supplied consistently, not converted automatically.}
\item{training}{data_fingerprint, split_id, ISO known_at, optional data_origin.}
\item{model_id,model_version}{Nonempty immutable model identifiers.}
\item{context}{Optional calculation context.}
\item{start_stop_panel}{List of entity_id/start/stop/event records; covariate_time is required with features and cannot exceed start. Subject intervals are contiguous and cannot continue after a terminal event.}
\item{X}{Finite numeric design matrix, including n by zero for no covariates.}
\item{feature_order}{Explicit unique feature names in matrix order. Prediction must match the artifact.}
\item{ties}{breslow or efron partial-likelihood ties. Both use explicitly uncentered Breslow baseline increments for frozen prediction.}
\item{strata}{Optional per-row named Cox baseline strata, otherwise all.}
\item{covariance}{model_based or cluster_robust, with subjects as clusters. PH score diagnostics retain an independent-subject assumption.}
\item{max_iter,tolerance}{Positive integer iteration bound and positive likelihood convergence tolerance below one.}
\item{durations}{Positive right-censored or event times for AFT.}
\item{distribution}{weibull, lognormal or loglogistic, each fitted by its full censored likelihood.}
\item{person_period_panel}{Counting records plus zero-based period index and interval_end_type scheduled or censor. A positive binary outcome requires a prespecified scheduled window.}
\item{period_grid}{Increasing fitted period boundaries. Every observed interval must lie within its declared period.}
\item{link}{logit or cloglog.}
\item{partial_period_policy}{exclude_censored drops incomplete censored periods, requires boundary entry and rejects partial positive-event windows. cloglog_exposure integrates prespecified exposure using a log-duration offset and requires cloglog.}
\item{event_times}{Positive event/censor times from time zero for Fine--Gray.}
\item{event_types}{List of cause strings or NULL censoring values.}
\item{causes}{Complete distinct competing terminal causes for joint cause-specific estimation.}
\item{cause_of_interest}{Observed selected cause for Fine--Gray.}
\item{censoring_spec}{estimator=kaplan_meier, independence_assumption=independent_within_censoring_group, optional per-subject groups. Reverse-KM censoring is fitted separately per group.}
\item{fitted_model}{Frozen survival ModelArtifact or FitResult.}
\item{covariate_paths}{Empty list for KM/Aalen--Johansen. Regression records have path_id/start/stop/covariates/stratum/covariate_time. Paths must cover the requested horizon contiguously, using the absolute model clock. AFT requires a fixed profile.}
\item{horizon_grid}{Nonnegative increasing elapsed horizons after start_time.}
\item{initial_state}{Known starting state, defaulting to the fitted multistate initial state.}
\item{event_states}{Marked states for a separate absorbing first-hit projection. Defaults to the target event name for multistate prediction.}
\item{start_time}{Absolute landmark on the model clock; prediction conditions on eligibility at this landmark.}
\item{extrapolation}{error rejects unsupported times/risk; flat explicitly carries cumulative hazards or AFT survival after observed support; parametric permits AFT tails.}
\item{quantile_levels}{Optional ordered interior AFT event probabilities, requiring parametric extrapolation. Output quantiles are remaining time conditional on landmark survival.}
\item{event_history}{entity_id/start/stop/from_state/to_state records. NULL or unchanged destination denotes no transition; exact transition times, repeated transitions and delayed entry are supported.}
\item{states}{Unique state names defining matrix order.}
\item{allowed_transitions}{List of distinct directed origin/destination pairs. Actual different-state transitions must belong to this graph.}
}
\details{
All risk intervals are open at entry/start and closed at exit/stop. Events at a given time precede censoring there. KM uses a product of one minus observed event/risk fractions, with Greenwood log-log intervals. Boundary intervals are explicitly asymptotic and may degenerate, not exact small-sample bounds. Empty observation-risk intervals are stored; prediction rejects crossings by default or reports explicit flat continuation.

Native Cox fits use survival::coxph, including Efron/Breslow ties, counting-process time-varying covariates and strata. No intercept is identified. The baseline is uncentered Breslow in both languages. The PH diagnostic is an efficient score test for identity-time interactions with coefficient nuisance information eliminated by a Schur complement. Event sums of Schoenfeld residuals, global and individual tests are included; unidentified interaction information is reported. Independent reference tests use survival::cox.zph.

AFT uses log(T)=intercept+X beta+scale*epsilon. Epsilon is minimum extreme value for Weibull, standard normal for lognormal and standard logistic for loglogistic. Usual Weibull/loglogistic shape is reciprocal scale, with distribution scale exp(intercept+X beta). Native fitting uses survival::survreg. Covariance coordinates are intercept, beta, log(scale). Event likelihood contributions are density and censored contributions are survival. Prediction uses stable landmark log-survival ratios and optionally their inverse quantiles.

Discrete hazards include period dummy intercepts. Incomplete censoring is never silently coded as a full-period non-event. The cloglog exposure offset is log(observed duration/full period duration) under constant within-period intensity and noninformative censoring. Grouped event windows must be prespecified; an exact event time is not a valid endogenous exposure endpoint for a Bernoulli likelihood. Logit predictions require complete periods; cloglog allows fractional periods.

Cause-specific estimation censors other terminal causes but integrates all fitted hazards jointly. For a step with total hazard increment dH, survival S is multiplied by exp(-dH), and cause k gains S*(1-exp(-dH))*dH_k/dH. This declared exponential-increment convention preserves total probability and is different from empirical AJ transition fractions. One minus cause-specific survival is not a competing-risk CIF.

Fine--Gray retains competing failures after their observed time T with IPCW G(t-)/G(T-), using group-specific reverse KM. Terminal failures are removed before tied censoring events in that censoring model. The expanded weighted counting-process fit uses Efron ties and an uncentered Breslow baseline. Only fixed baseline covariates and right censoring from time zero are supported; no post-competing-event feature path or delayed-entry IPCW law is invented. Stored covariance is conditional on estimated IPCW; sampling inference requires subject resampling and censor-model refitting. The complement of the selected-cause CIF is not all-event survival, and independently fitted cause CIFs are not assumed jointly coherent. Native expansion and coefficients are tested independently against survival::finegray.

Aalen--Johansen multiplies I+dA, where off-diagonal increments are observed origin-to-destination counts divided by origin risk sets and diagonals are negative row sums. The artifact includes event times, risk sets, transition increments/products and occupancy. Subject intervals must be contiguous and state-consistent; unchanged/censored rows remain risk intervals. Transition probabilities conditional on a landmark/state require a time-inhomogeneous Markov interpretation, or adequate state expansion. Interval-censored state snapshots are not exact transition histories. Missing-risk rows have identity increments with unidentifiable intensity: default prediction rejects positive mass with unsupported outgoing risk, while flat continuation records those assumptions.

Current default-state occupancy may fall after cure. A separate absorbing projection of marked states yields monotone first-hit incidence. For redefault after cure, use a cured initial state, matching absolute landmark/clock and default as the marked state. No causal action effect is inferred.

Prediction returns elapsed horizon_grid, start_time and a list of curves. Curves contain path_id, survival, cause-keyed cumulative_incidence and marginal_probabilities. AJ adds states, horizon-by-state occupancy, transition matrices and first_hit_probability. Optional AFT quantiles add quantile_levels and conditional_event_quantiles. Feature/target mismatches, unknown strata and unsupported default extrapolation fail explicitly. Public data-only artifacts are exchanged and replayed in both languages.
}
\value{FitResult with ModelArtifact for estimators; CalcResult with frozen conditional event curves for prediction. Parameters and diagnostics preserve risk, censoring, likelihood and prediction conventions.}
\references{
R survival documentation: \url{https://stat.ethz.ch/R-manual/R-devel/library/survival/html/survfit.formula.html}, \url{https://stat.ethz.ch/R-manual/R-devel/library/survival/html/survreg.html}, \url{https://stat.ethz.ch/R-manual/R-devel/library/survival/html/cox.zph.html}, \url{https://stat.ethz.ch/R-manual/R-devel/library/survival/html/finegray.html}.
Competing events and multistate product integrals: \url{https://stat.ethz.ch/R-manual/R-devel/library/survival/doc/compete.pdf}.
Python Cox backend: \url{https://www.statsmodels.org/stable/generated/statsmodels.duration.hazard_regression.PHReg.html}.
}
