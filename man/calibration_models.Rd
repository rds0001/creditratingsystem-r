\name{calibration_models}
\alias{calibrate_beta}
\alias{calibrate_grade_pd}
\alias{low_default_bound}
\alias{fit_hierarchical_calibration}
\alias{calibrate_lgd_level}
\alias{estimate_margin_of_conservatism}
\alias{population_reweighting}
\alias{fit_stepwise}
\alias{fit_regularization_path}
\alias{resample_fit_uncertainty}
\alias{fit_model_selection_protocol}
\title{Explicit Calibration, Uncertainty and Model Selection}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Native likelihood, pooling, uncertainty and selection methods with frozen artifacts, explicit data roles and retained failed candidates.}
\usage{
calibrate_beta(base_pd, outcomes, weights = NULL, metadata,
  monotonic = TRUE, boundary_policy = "reject", epsilon = 1e-12,
  context = NULL)
calibrate_grade_pd(counts, defaults, grades = NULL, prior_alpha = 0,
  prior_beta = 0, monotonic = TRUE, target_default_rate = NULL,
  default_grade = NULL, confidence = .95, target_weights = NULL,
  context = NULL)
low_default_bound(defaults, counts, confidence = .95,
  dependence = "independent", correlation = 0, pooling = "none",
  integration_tolerance = 1e-8, context = NULL)
fit_hierarchical_calibration(base_pd, outcomes, groups,
    weights = NULL,
  metadata, variance_bounds = c(1e-8, 25), context = NULL)
calibrate_lgd_level(base_lgd, target_lgd, weights = NULL, metadata,
  transformation = "additive", context = NULL)
estimate_margin_of_conservatism(base_estimate, components,
  confidence = .95, correlation = NULL,
    already_included = character(),
  bounds = NULL, context = NULL)
population_reweighting(sample_features, target_distribution,
  method = "poststratification", cap = NULL,
    outcome_dependent = FALSE,
  context = NULL)
fit_stepwise(X, outcomes, feature_order, candidate_groups,
  mandatory_terms = character(), hierarchy = NULL,
  direction = "bidirectional", criterion = "AIC", estimator = "GLM",
  weights = NULL, max_steps = 100, lr_alpha = .05,
  training = NULL, context = NULL)
fit_regularization_path(X, outcomes, feature_order, penalty_grid,
  split_plan, estimator = "GLM", elastic_net_alpha = 0,
  scoring = "log_loss", weights = NULL, context = NULL)
resample_fit_uncertainty(X, outcomes, feature_order,
    resampling_indices,
  resampling_units, unit_kind, estimator = "GLM", weights = NULL,
  confidence = .95, prediction_X = NULL, selection_spec = NULL,
  context = NULL)
fit_model_selection_protocol(X, outcomes, feature_order, candidates,
  split_plan, metadata, search_budget, scoring = "log_loss",
    context = NULL)
}
\arguments{
\item{base_pd}{Aligned input probabilities; hierarchical fits require interior values.}
\item{outcomes}{Observed binary events for probability methods, or numeric Gaussian responses for OLS/WLS.}
\item{weights}{Positive row weights for selection: unit OLS, inverse-variance WLS, integer frequency GLM. Calibration permits zero frequency weights but requires positive total. LGD also permits real exposure weights when explicitly declared in metadata.}
\item{metadata}{Calibration object with target (event, horizon, time_unit, probability_measure, conditioning), base_model_version, population_definition, outcome_policy, weight_type, data_origin, and calibration (start, end, known_at, split_id, prediction_origin). Empirical predictions must be HOLDOUT or OUT_OF_FOLD. The full protocol requires HOLDOUT and a window covering actual calibration rows. LGD external targets also permit EXTERNAL_TARGET.}
\item{monotonic}{Constrain beta coefficients a,b nonnegative, or pool ordered grade means monotonically.}
\item{boundary_policy}{Beta input policy: reject exact zero/one, or clip with the declared epsilon.}
\item{epsilon}{Beta clipping threshold in [1e-15,.01].}
\item{context}{Optional calculation context; supplied target fields must match the artifact.}
\item{counts,defaults}{Aligned positive integer observation counts and integer event counts between zero and counts. Scalars also accepted by low_default_bound.}
\item{grades}{Unique best-to-worst grade names. Default zero-based string names.}
\item{prior_alpha,prior_beta}{Nonnegative beta pseudo-counts per grade; both zero means empirical rates.}
\item{target_default_rate}{Optional performing-grade weighted mean reached by a finite log-odds shift.}
\item{default_grade}{Grade name excluded from performing calibration, required to contain only defaults.}
\item{confidence}{Probability level for intervals or one-sided conservatism; MoC requires at least .5.}
\item{target_weights}{Nonnegative grade weights for target reconciliation only; default counts. Does not change the binomial pooling likelihood.}
\item{dependence}{independent or gaussian_one_factor.}
\item{correlation}{Low-default fixed Gaussian asset correlation in [0,.999], zero for independence; for MoC, a symmetric PSD correlation matrix in component order, default independence.}
\item{pooling}{none or ordered_tail, the latter pooling each ordered grade with all subsequent grades.}
\item{integration_tolerance}{Absolute/relative tolerance for normal-factor integration on [-10,10].}
\item{groups}{Nonempty group IDs, aligned with probabilities and passed again to apply_calibrator.}
\item{variance_bounds}{Positive lower and upper bounds, up to 100, for estimated normal random-intercept variance.}
\item{base_lgd,target_lgd}{Loss fractions and desired weighted loss mean. Raw losses may lie outside [0,1] in additive mode.}
\item{transformation}{additive shift, multiplicative scaling of nonnegative losses, or logit shift of strictly interior losses.}
\item{base_estimate}{Scalar risk estimate before explicit conservatism.}
\item{components}{List of objects with unique source_id, category (data, method, general), nonnegative adverse_bias and standard_error.}
\item{already_included}{Source IDs whose uncertainty is already incorporated in the base estimate. Their listed contributions are zeroed.}
\item{bounds}{Optional lower/upper limits for final MoC-adjusted estimate; unbounded value and clipping adjustment remain reported.}
\item{sample_features}{Stratum strings for poststratification, or supplied strictly interior target-membership probabilities for membership_odds.}
\item{target_distribution}{Named target proportions summing to one, or an object containing sample_prior and target_prior for the membership sampling design.}
\item{method}{poststratification or membership_odds.}
\item{cap}{Optional positive cap on raw weights before mean-one normalization; final weights may exceed this cap.}
\item{outcome_dependent}{Explicit declaration of outcome-dependent sampling.}
\item{X}{Complete finite numeric design matrix, with no implicit row omission or global preprocessing.}
\item{feature_order}{Distinct names in exact matrix-column order.}
\item{candidate_groups}{Named list of disjoint feature-name groups; excluded columns cannot enter.}
\item{mandatory_terms}{Candidate group names always retained.}
\item{hierarchy}{Named list mapping group names to required parent groups. Cycles and absent mandatory parents fail.}
\item{direction}{forward, backward, or bidirectional. Backward starts full; other modes start with mandatory groups and intercept.}
\item{criterion}{AIC, BIC, or LR; full likelihood on identical observations.}
\item{estimator}{OLS, WLS, or Bernoulli-logit GLM; estimator and criterion accept either letter case.}
\item{max_steps}{Positive integer limit on accepted search moves.}
\item{lr_alpha}{Nested likelihood-ratio p-value threshold for additions/removals.}
\item{training}{Optional object with ISO known_at and role training_only or development_only. Without it the standalone selection record explicitly reports an unknown cutoff. No temporal guarantee is inferred.}
\item{penalty_grid}{Unique nonnegative lambda values for weighted-average-loss penalization.}
\item{split_plan}{Object with rows and folds. Rows declare entity_id, as_of, label_known_at, role (development, calibration, test). Folds declare train and validation lists of unique zero-based indices. Both languages use zero-based supplied indices.}
\item{elastic_net_alpha}{Zero for ridge, one for lasso, intermediate for elastic net. Intercept is unpenalized.}
\item{scoring}{log_loss or brier for GLM; mse for Gaussian response.}
\item{resampling_indices}{List of complete shared zero-based index arrays, including repetitions, one per replicate.}
\item{resampling_units}{Predeclared cluster or time-block IDs for each original row. Selected units must repeat all constituent rows equally.}
\item{unit_kind}{cluster or time_block; appropriate block length and dependence structure remain user choices.}
\item{prediction_X}{Optional aligned prediction matrix, default original X.}
\item{selection_spec}{Optional stepwise argument object to repeat adaptive selection within each resample.}
\item{candidates}{List of objects with unique id, optional features, penalty_grid and elastic_net_alpha. Full protocol uses GLM candidates.}
\item{search_budget}{Positive integer at least the number of declared candidates; no candidate is silently omitted.}
}
\value{CalcResult. Fitted transformations return a hashed CalibrationArtifact; apply_calibrator validates its target, version, schema, domain and content. Other methods return their estimates and complete diagnostics. Selection methods return a hashed SelectionModel with explicit scoring parameters, training role/cutoff and diagnostic hash. The full protocol additionally returns a common selected_frozen_model for predict_pd, a separate calibration artifact, all candidate evidence, and untouched test indices; it reports no test metrics.}
\details{
Beta calibration fits plogis(c + a*log(p) - b*log(1-p)); (c,a,b)=(0,1,1)
is the identity. Active monotonic constraints are reported: interior
inverse-information covariance is not boundary-aware inference.

Hierarchical calibration uses supplied logits as offsets, a global intercept,
and independent normal group intercepts. Variance is estimated using Laplace
marginal likelihood with analytic profile derivatives; conditional modes are
solved individually. Existing groups use their mode. Unknown groups integrate
over the fitted population using 40-node Gaussian quadrature. Reported group
uncertainty conditions on estimated hyperparameters; it excludes hyperparameter
and Laplace-approximation error. Variance-boundary estimates are explicit.

Grade PAVA pools empirical or beta-prior means in grade order. Optional target
shifts preserve structural zeros and ones and reject unattainable finite shifts.
Intervals are exact binomial without a prior, beta posterior with a prior. They
describe pre-target pools conditional on pooling, not post-selection or
simultaneous coverage. Zero-default cells retain positive upper uncertainty.

Low-default roots solve P(D <= observed defaults | pd)=1-confidence. For zero
independent defaults the answer is 1-(1-confidence)^(1/n). The Gaussian factor
mode integrates conditional independent binomial probabilities; ordered-tail
pooling also reports monotone upper bounds. Coverage is conditional on the
declared model, not universal under unknown dependence. Integration diagnostics
exclude negligible standard-normal probability outside [-10,10].

Additive LGD leaves raw economic losses unclipped. Multiplicative mode requires
nonnegative losses and a positive base mean; a zero target ties all predictions.
Logit mode requires interior fractions. Exposure weighting is separate from
frequency weighting and must be declared explicitly.

MoC is total adverse bias plus qnorm(confidence)*sqrt(s' C s). Component source
IDs prevent duplicate counting; already-incorporated components contribute zero.
This declared normal uncertainty policy is not an automatic regulatory default.
Poststratification rejects missing positive target strata. Membership odds are
q/(1-q)*sample_prior/target_prior; externally supplied propensities do not prove
covariate overlap. Clipping occurs before normalization; effective sample size
and achieved stratum balance expose its effect.

Stepwise Gaussian likelihood includes residual variance and, for WLS, the weight
determinant. AIC/BIC use identical rows; LR compares nested likelihoods and records
degrees of freedom. Ties within 1e-10 use lexical action/group order; visited
states prevent cycling. Rank deficiency, separation and zero residual variance
are failures, retained in candidate evidence. No adaptive selected-model fixed-
model inference is claimed.

Regularization minimizes weighted average negative log likelihood (Gaussian half
squared error) plus lambda*((1-alpha)*sum(beta^2)/2+alpha*sum(abs(beta))). Each
fold scales using only its training data; final refit uses development only.
Every fold must succeed; exact loss ties prefer stronger penalty. Entity sets
are disjoint across fold and stage boundaries, and earlier labels must be known
strictly before later observations. Supplied data relationships remain a user
responsibility. Standalone selection scoring is intercept plus centered/scaled
matrix times coefficients, followed by plogis for GLM. Preserve feature order,
training cutoff and the content hash when replaying a SelectionModel.

Resampling repeats complete declared clusters/time blocks and retains every
failure. Optional selection repeats within each replicate. Percentile intervals
use successful replicates and require at least two; a large failure proportion
can invalidate their interpretation. The full protocol performs inner development
selection, final development refit, then independent logistic calibration with
score tolerance 1e-8. Test labels and predictions never enter optimization.
Synthetic engineering evidence is not validation of performance on bank data.
}
\references{
The lme4 authors, glmer documentation.\url{https://lme4.github.io/lme4/reference/glmer.html}.
Friedman, Hastie and Tibshirani, regularization paths and elastic-net penalties.
\url{https://www.jstatsoft.org/article/view/v033i01}.

Kull, Silva Filho and Flach (2017), Beta calibration.
\url{https://proceedings.mlr.press/v54/kull17a.html}.
Pluto and Tasche, Estimating Probabilities of Default for Low Default Portfolios.
\url{https://arxiv.org/abs/cond-mat/0411699}.
R stats, step.
\url{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/step.html}.
}
\examples{
low_default_bound(0, 100)$value$bounds
calibrate_grade_pd(c(100, 100, 100), c(1, 4, 2))$value$grades
population_reweighting(c("A", "A", "A", "B"), list(A=.5, B=.5))$value
}
