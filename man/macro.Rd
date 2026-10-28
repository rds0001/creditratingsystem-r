\name{macro}
\alias{transform_macro_history}
\alias{fit_var}
\alias{fit_vecm}
\alias{fit_regime_switching}
\alias{fit_dynamic_factor}
\alias{block_bootstrap_scenarios}
\alias{simulate_macro_scenarios}
\alias{condition_macro_paths}
\alias{fit_satellite_model}
\alias{apply_satellite_model}
\alias{reverse_stress_scenario}
\title{Release-Aware Macro Dynamics, Cycle Laws and Satellite Transmission}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Actual native VAR, Johansen cointegration, Gaussian hidden-regime and dynamic-factor estimation, explicit shared scenario innovations, conditional Gaussian and reverse stress, and dated satellite regressions. Forecast averages do not silently become through-the-cycle estimates.}
\usage{
transform_macro_history(history, variable_transforms, frequency,
    fit_window, 
    information_cutoff, context = NULL) 
fit_var(transformed_history, lags, deterministic = "constant", 
    covariance_estimator = "df_adjusted", training, target,
    model_id, 
    model_version, context = NULL) 
fit_vecm(level_history, rank, lags,
    deterministic = "constant_inside", 
    integration_order = "I1", training, target, model_id,
    model_version, 
    context = NULL) 
fit_regime_switching(transformed_history, regime_count, lags = 0,
    initialization, 
    max_iterations = 200, tolerance = 1e-08, training, target, 
    model_id, model_version, context = NULL) 
fit_dynamic_factor(transformed_history, factor_count,
    initialization, 
    ar_limit = 0.995, variance_bounds = c(1e-08, 10000),
    max_iterations = 300, 
    tolerance = 1e-08, training, target, model_id, model_version, 
    context = NULL) 
block_bootstrap_scenarios(residual_history, shared_draws,
    factor_ids, scenario_ids, 
    time_grid, frequency, block_length, scheme = "moving",
    weights = NULL, 
    context = NULL) 
simulate_macro_scenarios(fitted_model, innovations,
    initial_state = NULL, regime_uniforms = NULL, 
    output_space = "model", sampling_contract = NULL,
    context = NULL) 
condition_macro_paths(fitted_model, innovations,
    conditioning_points, initial_state = NULL, 
    sampler = "gaussian_equality", sampling_contract = NULL, 
    context = NULL) 
fit_satellite_model(target_series, macro_history, feature_spec,
    estimator, 
    link, split_plan, training, target, target_unit, model_id, 
    model_version, weights = NULL, covariance = "classical", 
    hac_lags = 0, residual_process = "white_noise", context = NULL) 
apply_satellite_model(fitted_satellite_model, scenario_cube,
    idiosyncratic_innovations, 
    initial_target_state, context = NULL) 
reverse_stress_scenario(fitted_model, loss_model, horizon,
    target_loss, shock_metric, 
    initial_state = NULL, shock_bounds = NULL, initial_shock = NULL,
    
    tolerance = 1e-07, max_iterations = 500, context = NULL) 
}
\arguments{
\item{history}{Long macro release records with variable_id, economic date, release_date and numeric value.}
\item{variable_transforms}{Records containing variable_id, transform identity/log/difference/log_difference, logical standardize and original unit. Fit-only population centers/scales are frozen.}
\item{frequency}{monthly, quarterly or annual; observations and forecast grids must be regular. No implicit interpolation.}
\item{fit_window}{ISO start, end and known_at. Transformation estimation uses only the vintage released by known_at.}
\item{information_cutoff}{ISO cutoff controlling the output vintage, separate from transformation estimation cutoff.}
\item{transformed_history}{MacroHistory or wide dated object: dates, values, variable_ids, per-cell release_dates, units and frequency. Training values/vintage are retained separately when transformed.}
\item{level_history}{Undifferenced level history with the same dated wide schema; integration order is assumed explicitly.}
\item{lags}{Nonnegative observed lags for regimes; positive levels lag count for VAR/VECM. A VECM has lags minus one differenced lags.}
\item{deterministic}{VAR: none, constant, trend or constant_trend. VECM: none, constant_inside, constant_outside, constant_outside_trend_inside or constant_trend_outside. Trend uses original training observation index.}
\item{covariance_estimator}{mle or df_adjusted VAR innovation covariance divisor; no hidden dynamics repair.}
\item{rank}{Declared Johansen cointegration rank from zero to number of variables minus one. No automatic rank or critical-value inference.}
\item{integration_order}{I1. The caller supplies the economic integration assumption; fitting does not establish it.}
\item{training}{start, end, known_at, split_id and optional data_origin. Actual fitted observations determine data_fingerprint. Labels and releases cannot exceed their cutoffs.}
\item{target}{Common target: event, positive horizon, time_unit days/months/years, probability_measure PHYSICAL/RISK_NEUTRAL and conditioning.}
\item{model_id,model_version}{Nonempty frozen artifact identifiers.}
\item{context}{Optional common calculation context, which cannot contradict the probability measure or invent a TTC initial law.}
\item{regime_count}{Positive number of joint hidden Gaussian regimes.}
\item{initialization}{Regimes: nonempty list of starts containing transition, initial_probabilities, coefficients and covariances. Coefficients have regime by (intercept plus lag blocks) by response axes; transition rows are origin states. Factors: identified loadings, mean, ar and variances, with factors preceding idiosyncratic states.}
\item{max_iterations}{Positive optimizer/EM iteration cap. Fit diagnostics expose nonconvergence; local optimization never claims global optimality.}
\item{tolerance}{Positive convergence or feasibility tolerance; numerical guards additionally verify support and constraints.}
\item{factor_count}{Positive latent factor count strictly below observed variable count. Loading anchors have unit diagonal and zero upper triangle in the first factor_count rows.}
\item{ar_limit}{Declared absolute stationary AR coefficient bound strictly between zero and one for factor estimation.}
\item{variance_bounds}{Positive increasing lower/upper bounds on factor and idiosyncratic innovation variances, used during estimation.}
\item{residual_history}{Finite residual row matrix, one column per joint macro factor; entire rows are resampled together.}
\item{shared_draws}{Uniform array scenario by time by two: block-start and restart draws in [0,1).}
\item{factor_ids}{Unique ordered macro innovation channel identifiers.}
\item{scenario_ids}{Unique ordered scenario identifiers, shared throughout downstream calculations.}
\item{time_grid}{Consecutive forecast times in years matching the declared frequency.}
\item{block_length}{Moving/circular block integer length; stationary-bootstrap expected block length. Must not exceed available residual history.}
\item{scheme}{moving, circular or stationary multivariate block bootstrap.}
\item{weights}{For bootstrap, optional nonnegative scenario masses. For satellites, a named list mapping exactly training observation IDs to positive precision weights (WLS) or integer frequency weights (GLM); OLS requires unit weights.}
\item{fitted_model}{Portable fitted VAR, VECM, regime or dynamic-factor artifact, or corresponding FitResult. Reverse stress currently requires linear VAR/VECM dynamics.}
\item{innovations}{Named cube with kind STANDARD_NORMAL or RAW_INNOVATIONS, values on scenario/time/channel axes, scenario_ids, factor_ids, time_grid in years and optional weights. Factors use latent state_ids as innovation channels.}
\item{initial_state}{Linear fixed history of shape lags by variables, or scenario-specific histories for a declared cycle mixture. Factors accept a state vector only with DECLARED_FIXED_STATE. Defaults retain fitted terminal state information.}
\item{regime_uniforms}{Scenario by (one initial plus forecast steps) uniform matrix in [0,1), required only for hidden-regime simulation.}
\item{output_space}{model or level. Level inversion uses frozen original-level anchors; alternative fixed/cycle laws require separate explicit anchoring and are rejected by convenience inversion.}
\item{sampling_contract}{Explicit law and assumptions. Linear defaults FORECAST_CONDITIONAL/PIT. STATIONARY_GAUSSIAN needs cycle_law_id, equilibrium_assumptions and initial_state_draws, stable constant dynamics and no trend. DECLARED_CYCLE_MIXTURE needs phase_ids, phase_probabilities matching scenario mass and scenario-specific histories. Factors use FILTERED_GAUSSIAN with initial normal draws, stationary Gaussian, or DECLARED_FIXED_STATE. Zero-lag regimes also permit STATIONARY_MARKOV_EMISSION with a unique invariant regime law.}
\item{conditioning_points}{Records with zero-based time_index, factor_id and exact value. Empty records preserve the unconditional result.}
\item{sampler}{gaussian_equality: full joint Gaussian conditioning with cross-covariance, never isolated overwrites. Empirical bootstrap and regime/cycle mixtures are not silently approximated as Gaussian.}
\item{loss_model}{A function taking a horizon by factor path, or portable kind linear_predictor with same-shaped coefficients, intercept and identity/logit/log link.}
\item{horizon}{Positive number of forecast periods for reverse stress.}
\item{target_loss}{Finite response threshold to reach or exceed.}
\item{shock_metric}{Positive definite precision matrix on flattened time-major standard-normal shocks; objective is half the quadratic form.}
\item{shock_bounds}{Optional per-shock lower/upper bounds for reverse optimization.}
\item{initial_shock}{Optional starting shock vector, one value per horizon/factor coordinate.}
\item{target_series}{Target records: observation_id, entity_id, date, information_date, known_at and value. Only assigned training values are read for estimation.}
\item{macro_history}{Wide dated macro history with ordered variables, units and per-cell release dates. Explicit lagged features must be released at each target information date.}
\item{feature_spec}{Records with feature_id, variable_id, nonnegative lag, physical unit and expected_sign positive/negative/unrestricted. Sign hypotheses are diagnosed rather than silently imposed.}
\item{estimator}{OLS, WLS or Bernoulli GLM. Continuous estimators use native weighted least squares; GLM uses established native binary MLE with rank/separation guards.}
\item{link}{identity/log for continuous response; logit/probit/cloglog for binary GLM. Log inversion without residual draws is a conditional median, not an invented mean adjustment.}
\item{split_plan}{split_id, mode TEMPORAL/ENTITY_TIME, training_cutoff and exhaustive disjoint train_ids plus optional calibration_ids, validation_ids, test_ids. Nonempty holdout follows model availability; entity-time roles also have disjoint entities.}
\item{target_unit}{Explicit unit of the satellite response, e.g. probability, currency or index.}
\item{covariance}{classical, HC1, HAC or cluster coefficient uncertainty. GLM uses classical expected Fisher information. Cluster requires multiple entities; HAC requires one regular entity series.}
\item{hac_lags}{Nonnegative Bartlett/Newey-West covariance lag, smaller than the number of training rows.}
\item{residual_process}{none, white_noise or fitted zero-intercept AR1. Binary GLM requires none. AR1 uses consecutive within-entity pairs and rejects nonstationary estimates.}
\item{fitted_satellite_model}{Frozen macro.fit_satellite_model artifact.}
\item{scenario_cube}{Shared ScenarioCube with axes scenario/time/factor, IDs, normalized weights, units, frequency, year grid, economic origin, information cutoff, measure and full sampling contract.}
\item{idiosyncratic_innovations}{STANDARD_NORMAL object with scenario by time values and exactly matching scenario_ids and time_grid; no hidden independent macro draws.}
\item{initial_target_state}{as_of matching macro origin, known_at covering model/macro availability, scalar residual_state and mode absolute/baseline_delta. macro_history covers exactly the maximum lag ending at origin. Delta mode also supplies baseline_link_path and reference_feature_path.}
}
\details{
Transformation fitting uses training-only release vintages and retains an original-level inverse anchor. Later revisions cannot alter frozen training values. VAR reports estimated companion roots without repair. VECM preserves explicit rank, integration and deterministic placement, including rank-zero and one-lag cases. Trace statistics do not automatically infer rank.

The hidden-state EM likelihood uses regime-specific regressions on observed lagged levels. Every supplied start is evaluated and failures retained. Regimes are ordered by intercept, filtered probabilities are distinct from training-window smoothed probabilities, and local convergence is explicit. A one-regime fit reproduces the corresponding Gaussian regression.

Dynamic factors use genuine Gaussian Kalman likelihood with latent and idiosyncratic AR(1) states, identified loading anchors and explicit parameter bounds. Missing cells are integrated out without future imputation. Native R uses KFAS. Frozen artifacts retain observation/transition/covariance matrices and the terminal filtered distribution. Nonconvergence remains visible in fit diagnostics.

Stationary Gaussian TTC initialization solves equilibrium moments only for stable constant dynamics. Unit-root VECMs and deterministic trends fail this law. Declared cycle mixtures verify phase-weight mass but do not establish the caller's economic equilibrium assumptions. Regime stationary probabilities alone do not establish an autoregressive joint invariant law. Factor forecasts include filtered state uncertainty through explicit initial draws. All output cubes preserve the full sampling contract, named axes, measure and information cutoff.

Gaussian equality conditioning updates the entire correlated path and includes latent or stationary initial uncertainty. Support checks use unit-normalized covariance. Singular density is labeled in standardized-support coordinates; full-rank density uses physical coordinates. Density is not probability of an exact continuous event. Conditional stress retains its parent PIT/TTC scope. Reverse optimization reports a local feasible shock or explicit failure; it makes no general global optimality or infeasibility claim.

Satellite fits exclude future releases, immature training labels and held-out target values. WLS covariance conventions distinguish precision weights from binary GLM frequency weights. AR residual pairs do not cross entity boundaries or calendar gaps. Application exposes baseline, macro and idiosyncratic link contributions; zero macro deviations preserve a supplied baseline in delta mode. Shared macro paths and their cycle contract are preserved.
}
\value{FitResult containing a common portable ModelArtifact for estimators; otherwise CalcResult with a MacroHistory, innovation cube, ScenarioCube or reverse-stress optimization result and diagnostics. Artifact replay uses explicit innovations, not merely matching random seeds.}
\references{
\url{https://www.statsmodels.org/stable/vector_ar.html}

\url{https://search.r-project.org/CRAN/refmans/urca/html/ca.jo.html}

\url{https://search.r-project.org/CRAN/refmans/KFAS/html/SSModel.html}
}
