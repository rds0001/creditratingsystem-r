\name{ead_models}
\alias{fit_ead_model}
\alias{predict_ead}
\title{Frozen CCF Conditional Means and Direct EAD Distributions}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Fit historically aligned conversion-factor mean regressions or
nonnegative direct-exposure distributions, and replay models or contractual schedules.}
\usage{
fit_ead_model(exposure_episodes, X_train, feature_order,
    target_mode, model_spec,
  target, training, model_id, model_version, currency,
    weights = NULL,
  minimum_undrawn = 0, fit_intercept = TRUE, context = NULL)
predict_ead(fitted_model_or_schedule, facility_covariates,
    feature_order,
  expected_target, reference_dates, default_dates, currency,
  reference_drawn = NULL, reference_limit = NULL,
    quantile_levels = NULL,
  context = NULL)
}
\arguments{
\item{exposure_episodes}{Rows with episode_id, reference_date, reference_known_at,
default_date, default_known_at, features_observed_at, features_known_at,
reference_drawn, reference_limit, ead_at_default, currency. Amounts nonnegative;
reference and feature knowledge no later than reference date; default exactly
horizon days later and known by training cutoff.}
\item{X_train, facility_covariates}{Finite numeric matrices with ordered columns.
For schedule prediction, facility_covariates must be NULL.}
\item{feature_order}{Distinct predictor names; character() for no predictors.}
\item{target_mode}{ccf or direct_ead.}
\item{model_spec}{CCF: mean_model="identity", covariance="HC0". Direct EAD:
positive_distribution="lognormal", zero_model="none", "logit" or "all_zero".}
\item{target, expected_target}{Physical given_default target, integer-day horizon,
time_unit="days", event="ead" and explicit currency. Prediction requires exact match.
For schedule prediction expected_target is NULL.}
\item{training}{Metadata with data_fingerprint, split_id, known_at, optional data_origin.}
\item{model_id, model_version}{Nonempty artifact identifiers.}
\item{currency}{Explicit reporting currency, matching episodes, target and context.}
\item{weights}{Nonnegative exact integer frequency weights, positive finite total
at most 2^53-1, representing independent replicated observations.}
\item{minimum_undrawn}{Nonnegative threshold. CCF denominators at or below it are
excluded with reason during fitting and rejected during prediction.}
\item{fit_intercept}{Whether the regression mean includes an intercept.}
\item{fitted_model_or_schedule}{FitResult, frozen model, or bound exposure_schedule
result/value. Schedule integrity is checked against schedule_hash.}
\item{reference_dates, default_dates}{One ISO reference/default date per prediction.
Model reference cannot precede training knowledge and horizon must match. Schedules
require reference equal to as_of and exact period-end default dates.}
\item{reference_drawn, reference_limit}{Aligned nonnegative reference amounts for
CCF reconstruction only; NULL for direct-EAD or schedule prediction.}
\item{quantile_levels}{Optional increasing interior probabilities, only for direct
EAD distributions. These describe predictive distribution, not estimation uncertainty.}
\item{context}{Optional calculation context, compatible with frozen target/currency.}
}
\value{FitResult with chronological diagnostics, excluded rows, regressions,
optional positive-EAD gate, covariance, feature support and provenance. CalcResult
predictions contain conditional_ead, dates and currency; CCF adds conditional_ccf
and coefficient-based mean standard errors; direct EAD adds positive_probability
and optional quantiles.}
\details{CCF is (EAD-drawn)/(limit-drawn), without clipping. Frequency least squares
and HC0 covariance estimate an identity conditional mean. Reconstructed negative
EAD fails rather than being floored. Mean standard errors hold reference amounts
fixed; a CCF mean alone does not identify exposure quantiles.

Direct EAD models zero probability separately from positive lognormal amounts.
The positive log regression uses maximum-likelihood residual variance, requires
nonzero residual dispersion and full rank, and yields mean exp(mu+squared_sigma/2).
Multiplication by positive probability gives total mean. Quantiles include the zero
atom. none requires all positive targets; logit requires both classes; all_zero
fits an exact point mass without positive-stage parameters. Positive-stage support
is diagnosed separately. Predictive quantiles assume a correctly specified
lognormal distribution and omit parameter-estimation uncertainty.

Predictors and knowledge dates are supplied explicitly; truth of source evidence
cannot be inferred. Models use native declarative artifacts with input fingerprints,
retained provenance and cross-language replay. Schedule prediction interpolates
nothing and respects its before/after-payment default convention. No PD or
regulatory conversion factor is supplied.}
