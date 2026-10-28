\name{validation}
\alias{auc_gini}
\alias{ks_discrimination}
\alias{brier_score}
\alias{log_loss}
\alias{binomial_pd_test}
\alias{calibration_intercept_slope}
\alias{calibration_curve}
\alias{spiegelhalter_test}
\alias{poisson_binomial_pd_test}
\alias{hosmer_lemeshow_test}
\alias{dependent_pd_backtest}
\alias{default_count_backtest}
\title{Observed binary-outcome validation and exact common-PD count testing}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Compute weighted discrimination and proper scoring metrics on observed
binary outcomes. Test a count under an explicitly independent common-probability
binomial model. Applicability and exact-boundary conventions remain visible.}
\usage{
auc_gini(outcomes, scores, weights = NULL, score_direction,
    context = NULL)
ks_discrimination(outcomes, scores, weights = NULL, score_direction,
                  context = NULL)
brier_score(outcomes, predicted_pd, weights = NULL, context = NULL)
log_loss(outcomes, predicted_pd, weights = NULL,
    boundary_policy = "exact",
          epsilon = NULL, context = NULL)
binomial_pd_test(n, observed_defaults, common_pd, alternative,
                 independence_assumption, confidence_level = 0.95,
                 context = NULL)
calibration_intercept_slope(predicted_pd, outcomes, design,
    covariance_policy,
  cluster_ids = NULL, confidence_level = .95,
    boundary_policy = "reject",
  epsilon = NULL, max_iter = 100, tolerance = 1e-10, context = NULL)
calibration_curve(predicted_pd, outcomes, bin_edges, design,
    interval_method,
  dependence_assumption, confidence_level = .95, minimum_count = 5,
  context = NULL)
spiegelhalter_test(predicted_pd, outcomes, design, alternative,
  independence_assumption, context = NULL)
poisson_binomial_pd_test(individual_pd, observed_default_count,
    design,
  alternative, independence_assumption,
    method = "dynamic_programming",
  context = NULL)
hosmer_lemeshow_test(predicted_pd, outcomes, bin_edges, design,
  degrees_of_freedom_policy, independence_assumption,
    minimum_expected = 5,
  context = NULL)
dependent_pd_backtest(predicted_pd, outcomes, cluster_ids, time_ids,
    design,
  null_model, innovations, alternative, confidence_level = .95,
    context = NULL)
default_count_backtest(observation_panel, frozen_forecasts,
    group_by, unit,
  horizon_days, default_definition, redefault_policy,
    evaluation_cutoff,
  context = NULL)
}
\arguments{
\item{outcomes}{Nonempty finite numeric vector of observed zero/one outcomes.
Missing, censored or unfunded outcomes must not be encoded as zero.}
\item{scores}{Finite numeric score vector of the same length as outcomes.}
\item{predicted_pd}{Finite numeric probability vector in [0,1], of the same length
as outcomes, matching the observed event and outcome horizon.}
\item{weights}{Optional nonnegative finite vector, same length as outcomes,
with positive finite total. Defaults to one per observation. No silent recycling.}
\item{score_direction}{Required higher_is_risk or lower_is_risk; risk predicts
outcome one.}
\item{boundary_policy}{For log loss: exact or clip. For diagnostic calibration: reject or clip.}
\item{epsilon}{Required scalar in (0,0.5) for clipping, producing representable
interior lower and upper bounds. Must be NULL under exact/reject policy.}
\item{n}{Nonnegative exact integer number of independent trials, no larger than
2\eqn{^{53}} minus one.}
\item{observed_defaults}{Exact integer count from zero through n.}
\item{common_pd}{Common Bernoulli probability under the null, in [0,1].}
\item{alternative}{Required two-sided, less or greater.}
\item{independence_assumption}{independent_common_probability for the binomial test; independent_fixed_bernoulli for Spiegelhalter, Poisson-binomial and HL tests.}
\item{confidence_level}{Scalar strictly between zero and one.}
\item{design}{Named frozen-forecast contract with prediction_origin HOLDOUT/EXTERNAL,
forecast_known_at, evaluation_at, split_id and complete physical target. Optional
forecast_fingerprint is verified; computed hash is always returned. Dependent tests
also require ordered observation_ids. FITTED_SAMPLE is allowed only for its explicit
HL design policy.}
\item{covariance_policy}{iid, cluster_cr1 or none for diagnostic logistic fit.}
\item{cluster_ids, time_ids}{One nonempty cluster/time identifier per observation.
Cluster covariance treats distinct clusters as independent. Simulation maps unique
cluster/time pairs to correlated factors.}
\item{max_iter, tolerance}{Native GLM iteration and convergence controls.}
\item{bin_edges}{Prespecified strictly increasing numeric edges covering [0,1].}
\item{interval_method}{none or clopper_pearson reliability-bin intervals.}
\item{dependence_assumption}{descriptive_only, or
independent_common_probability_within_bins for exact binomial intervals.}
\item{minimum_count}{Positive integer count threshold for descriptive small-bin flag.}
\item{individual_pd}{Numeric vector of independent heterogeneous Bernoulli PDs.}
\item{observed_default_count}{Integer realized count between zero and trial count.}
\item{method}{dynamic_programming exact Bernoulli convolution.}
\item{degrees_of_freedom_policy}{external_common_bin_pd or
fitted_logistic_approximation; must match prediction origin.}
\item{minimum_expected}{Positive expected event/non-event count threshold for
withholding unreliable chi-square p-values.}
\item{null_model}{Named Gaussian-factor model with family gaussian_factor,
asset_correlation scalar/vector, factor_correlation row-list matrix and
parameter_uncertainty conditional_fixed or supplied_predictive_pd_draws. The latter
requires parameter_draw_source.}
\item{innovations}{Data-only object with draw_assumption
independent_standard_normal_rows, ordered observation_ids, sorted factor_keys
(cluster_id/time_id objects), factor_normals B-by-G and idiosyncratic_normals B-by-n.
Matrices are explicit row lists. Optional parameter_pd_draws is B-by-n.}
\item{observation_panel}{Rows with observation_id, default-unit key, as_of,
performance_start, outcome_end, eligible, prior_default, default_definition,
explicit target (possibly NULL), outcome_known_at when observed, and grouping fields.}
\item{frozen_forecasts}{Rows with observation_id, scalar pd, forecast_at, known_at,
default_definition, horizon_days, unit, probability_measure PHYSICAL,
model_id and model_version.}
\item{group_by}{Distinct observation field names defining descriptive cohorts.}
\item{unit}{obligor (entity_id key) or facility (facility_id key).}
\item{horizon_days}{Positive integer calendar-day observation horizon.}
\item{default_definition}{Explicit common event-definition string.}
\item{redefault_policy}{Required exclude_previous_default.}
\item{evaluation_cutoff}{ISO calendar date for available mature outcomes.}
\item{context}{Optional calculation context or context CalcResult.}
}
\value{CalcResult with named-list value, units, diagnostics, provenance and
limitations. Scalar input is one observation; vectors never recycle.

auc_gini returns auc, gini, applicability/reason, pair_count, tied_pair_count,
concordant_pair_mass and tied_pair_mass. Pair counts count rows with positive
weight; masses use class-normalized pair weights. ks_discrimination returns ks,
threshold, signed_difference, full cdf row list and applicability/reason.

brier_score returns brier, individual normalized-weight contributions, unweighted
squared_errors, observed_rate and outcome_variance_baseline. log_loss returns
log_loss, loss_status, contributions, infinite_loss_flags; positive infinity is
represented by NULL plus INFINITE so output remains finite-JSON compatible.

binomial_pd_test returns p_value, lower/upper confidence limits, observed_rate,
applicability and reason.}
\details{
AUC compares every event/non-event pair. An event with higher oriented risk score
is concordant; equal scores receive half credit. Pair weights are products of
within-event and within-non-event normalized weights. The algorithm groups tied
scores before accumulating lower-score non-event mass. Gini is 2*AUC-1. Zero-weight
rows contribute neither pair count nor pair mass, but inputs are still validated.
If either class has no positive weight, AUC/Gini and pair masses are NULL with
NOT_APPLICABLE and ONE_CLASS. No confidence interval is inferred from these
weighted descriptive statistics.

KS is the maximum absolute difference between the weighted non-event and event
empirical CDFs of oriented risk score. Ties move together. The returned signed
difference is non-event CDF minus event CDF. Equal maximal distances select the
first threshold in ascending oriented score order. Under lower_is_risk, original
score thresholds consequently appear in descending order. A missing weighted
class yields NOT_APPLICABLE, empty cdf and NULL summary values. Weighted, tied or
dependent credit observations do not imply the classical continuous IID KS null
or a valid classical KS p-value.

The binary Brier convention is the mean squared probability error on [0,1],
without multiplication by two. The baseline is the observed weighted event rate
times one minus that rate; it is a descriptive in-sample constant-prediction
baseline, not an independent model benchmark.

Log loss uses natural logarithms (nats). Correct exact predictions p=0/y=0 and
p=1/y=1 have zero loss. An impossible prediction with positive weight has infinite
loss, recorded as NULL plus INFINITE rather than a silently clipped number.
Its individual contribution is NULL and infinite_loss_flags entry TRUE. An
impossible prediction of zero weight contributes zero and does not make total
loss infinite. Clip policy explicitly truncates to [epsilon,1-epsilon]; diagnostics
record the epsilon and number of probabilities changed. Clipping changes the
score at those boundaries. Very small epsilon values that round the upper bound
to one are rejected. Missing/nonfinite inputs are not silently removed.

The binomial test uses native stats::binom.test, mapping the public two-sided
spelling to its two.sided alternative. Its two-sided p-value sums outcomes whose
null probability mass is no greater than the observed mass, using the numerical
tolerance of that implementation. Confidence intervals are exact Clopper-Pearson
intervals; one-sided alternatives produce matching one-sided limits. An empty
cohort returns NOT_APPLICABLE, EMPTY_COHORT and NULL metrics. The method explicitly
requires independent common-probability trials. Correlated obligors or heterogeneous
PDs require a different null model; the assertion is supplied by the caller.

All observations must refer to the same intended default definition, target
horizon and evaluation population as their predictions. These functions cannot
repair selection, leakage, censored targets or wrongly measured outcomes.
}
\section{Frozen calibration diagnostics}{
Extended diagnostics compute a canonical forecast hash and verify an optional
expected hash. Forecast known_at must not follow evaluation_at. Design includes
split_id, origin and a target with event, positive horizon, time_unit, physical
probability_measure and conditioning. These are explicit evidence assertions,
not automatic proof of an independent test sample.

calibration_intercept_slope fits logit Pr(Y=1)=intercept+slope*logit(frozen PD).
It returns intercept, slope, covariance, standard_errors, confidence_intervals,
applicability/reason and converged. It does not apply the fitted coefficients to
original predictions. Constant PDs make the joint coefficients unidentified;
zero/one PDs yield BOUNDARY_PD unless explicit clipping is chosen. One class,
separation, rank failure and nonconvergence return missing estimates with reasons.
The native GLM checks identification and likelihood convergence.

IID covariance is inverse expected Fisher information with normal intervals.
Cluster CR1 aggregates observation likelihood score vectors within each named
cluster, sandwiches their outer products and applies G/(G-1)*(n-1)/(n-2).
It requires at least two independent clusters and n>2; intervals use Student t
with G-1 degrees of freedom. Original model parameter uncertainty is not included.
The none policy returns coefficients without covariance/intervals.

calibration_curve uses left-closed bins, except that the last also includes one.
Returned bins retain empty cells and their missing rates/intervals, counts,
defaults, mean PD, observed rate, residual, limits, small_bin flag and status.
Assignments and totals permit reconciliation. Exact Clopper-Pearson limits require
independent_common_probability_within_bins; descriptive_only with no interval is
appropriate when that assumption is unsupported. Bins must not be chosen by
inspecting the outcomes they evaluate.
}
\section{Independent fixed-forecast tests}{
spiegelhalter_test computes numerator sum((y-p)*(1-2*p)) and variance
sum(p*(1-p)*(1-2*p)^2), with Z their standardized ratio. It returns numerator,
variance, z, normal-approximation p_value and applicability/reason. Zero variance,
including all p=0.5, is NOT_APPLICABLE. Maximum single-term variance share is exposed.
A small or dominated sum can invalidate normal approximation. This is one calibration
moment, not an omnibus test, and one-sided directions refer to this moment.

poisson_binomial_pd_test multiplies Bernoulli probability polynomials by successive
native dynamic-programming convolutions. Returned counts, probability_mass,
expected_count, variance and inclusive-tail p_value describe independent
heterogeneous Bernoulli trials. Two-sided probability-mass ordering uses relative
tolerance 1e-7. Equal PDs recover the binomial distribution, and endpoint PDs are
deterministic trials. Mass error and zeroed feasible-support cells are reported.
Unrepresentable positive tails yield missing p_value and NUMERICAL_UNDERFLOW;
a truly impossible count can have exact zero probability. An empty cohort is
NOT_APPLICABLE. Work is quadratic in trial count; model estimation uncertainty
and dependence are excluded from this conditional null.

hosmer_lemeshow_test reports per-bin (O-E)^2/[E*(1-E/n)], total statistic,
degrees_of_freedom, p_value, bin evidence and applicability/reason. External fixed
forecasts use external_common_bin_pd, requiring equal individual probabilities
within each bin and degrees of freedom equal to non-deterministic occupied bins.
The fitted_logistic_approximation policy requires FITTED_SAMPLE and uses G-2 as
an explicit conventional approximation, not a universal fitted-model correction.
Empty/deterministic bins remain visible. Small expected event/non-event cells,
heterogeneous external-bin PDs, impossible deterministic outcomes and nonpositive
degrees of freedom withhold a p-value. An impossible cell makes the statistic
missing rather than finite. Independence within and between bins remains required.
}
\section{Declared dependent null simulation}{
dependent_pd_backtest uses unique ordered observation_ids and sorted distinct
(cluster_id,time_id) factor keys. Supplied standard-normal innovation matrices have
B>=2 rows. No RNG is called. A Cholesky factor of the positive-definite unit-diagonal
factor-correlation matrix induces the declared cross-factor dependence. The latent
value for observation i is sqrt(rho_i)*factor_i+sqrt(1-rho_i)*idiosyncratic_i,
with default at or below qnorm(p_i). Rho lies in [0,1]; shared keys preserve common
shocks and factor correlation can represent time dependence. Perfectly common
factors should share a key instead of using a singular correlation matrix.

Under conditional_fixed, probabilities are fixed across paths. Under
supplied_predictive_pd_draws, supplied B-by-n probability paths determine the
thresholds and parameter_draw_source must explain the predictive ensemble.
Independent standard-normal rows and their relationship to parameter draws are
caller assertions; the library cannot certify sampling assumptions from realized
innovations. A predictive ensemble is not an automatically fitted frequentist
parameter-sampling distribution.

The statistic is observed count minus fixed expected count. Inclusive one-sided
tails and two-sided absolute count residuals give p=(extreme+1)/(B+1). Output retains
all null_counts/null_statistics, observed/expected counts, statistic, p_value,
null_count_interval and tail_probability_interval. The count interval uses the
empirical inverse CDF (R quantile type 1). The exact binomial tail-probability
interval measures Monte Carlo error, not PD uncertainty. Diagnostics disclose
finite p-value resolution, factor keys, null model and exact innovation fingerprint.
}
\section{Mature default-count cohorts}{
default_count_backtest joins frozen forecasts by observation_id. Creation time
must not exceed forecast knowledge, which must not exceed the observation as_of.
Both forecast and observation must match the declared event, physical measure,
unit and positive calendar-day horizon; performance cannot start before as_of.
Observed outcomes require knowledge dates, and a nondefault zero cannot be known
before the outcome window completes. Missing forecasts are explicit exclusions.

Obligor policy deduplicates identical entity/as_of/performance-start/end windows
across facilities. PD, target, group and eligibility disagreement raises a conflict.
Facility policy uses facility identity. The function does not infer an obligor
outcome by combining inconsistent facility labels. Previous-default units are
excluded by the declared policy. Only mature, complete, known and eligible windows
contribute to both observed and expected counts, even if default was observed early.
Future, immature, missing and ineligible rows are exposed as exclusions.

Output groups and overall totals give unit_windows (with available forecasts),
observed_count, observed_defaults, expected_defaults, O/E ratio/status, observed_rate
and mean_pd. Group totals reconstruct the compared population. Missing forecasts
are excluded before unit aggregation. Input/forecast hashes and duplicate-row
counts retain evidence. An expected count of zero produces missing O/E with
UNDEFINED when observed count is zero, or INFINITE when defaults occurred.
Repeated time windows can remain dependent: this report supplies no binomial test.
}
\references{
Spiegelhalter (1986), Probabilistic prediction in patient management and clinical
trials, doi:10.1002/sim.4780050506. Formula cross-checked against Harrell's primary
rms validation implementation at https://github.com/harrelfe/rms/blob/master/R/val.prob.s.

BIS (2005), Studies on the Validation of Internal Rating Systems, Working Paper 14,
https://www.bis.org/publications/studies-validation-internal-rating-systems-revised, fixed-bin chi-square construction and caveats.

Phipson and Smyth (2010), Permutation P-values Should Never Be Zero,
https://gksmyth.github.io/pubs/PermPValuesPreprint.pdf, Monte Carlo tail-rank principle.
}
\seealso{\code{stats::binom.test}}

\examples{
auc_gini(c(0, 1, 0, 1), c(0, 1, 1, 2), c(2, 2, 3, 4),
          score_direction = "higher_is_risk")$value$auc
brier_score(c(0, 1), c(0.25, 0.75), c(1, 3))$value$brier
log_loss(c(0, 1), c(0, 0))$value$loss_status
binomial_pd_test(4, 0, 0.5, alternative = "less",
                 independence_assumption = "independent_common_probability")$value
}
