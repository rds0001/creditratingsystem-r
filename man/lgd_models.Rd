\name{lgd_models}
\alias{fit_fractional_lgd}
\alias{fit_beta_lgd}
\alias{fit_zero_one_inflated_lgd}
\alias{predict_lgd}
\title{Frozen Conditional LGD Mean and Distribution Models}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Estimate fractional logit, variable-precision beta and structural
zero-and-one inflated beta models, preserving explicit conditional targets.}
\usage{
fit_fractional_lgd(X_train, lgd_targets, feature_order, link,
    covariance,
  target, training, model_id, model_version, weights = NULL,
  weight_type = "frequency", fit_intercept = TRUE, max_iter = 200,
  tolerance = 1e-10, context = NULL)
fit_beta_lgd(X_train, lgd_targets, feature_order, mean_link,
    precision_link,
  target, training, model_id, model_version,
    precision_design = NULL,
  precision_feature_order = NULL, weights = NULL,
    weight_type = "frequency",
  fit_intercept = TRUE, max_iter = 500, tolerance = 1e-10,
    context = NULL)
fit_zero_one_inflated_lgd(X_train, lgd_targets, feature_order,
    component_spec,
  target, training, model_id, model_version,
    precision_design = NULL,
  precision_feature_order = NULL, weights = NULL,
    weight_type = "frequency",
  fit_intercept = TRUE, max_iter = 500, tolerance = 1e-10,
    context = NULL)
predict_lgd(fitted_model, X, feature_order, expected_target,
    default_condition,
  precision_design = NULL, precision_feature_order = NULL,
  quantile_levels = NULL, context = NULL)
}
\arguments{
\item{X_train, X}{Finite numeric matrices with explicitly ordered columns. An
intercept-only design is a matrix with zero columns and the required row count.}
\item{lgd_targets}{Closed-workout LGD values. Fractional and inflated models accept
[0,1]; beta requires (0,1). Raw values outside support are rejected, never clipped.}
\item{feature_order}{Distinct predictor names, or character() for no predictors.}
\item{link, mean_link, precision_link}{Explicit logit mean and log precision links.}
\item{covariance}{HC0 or HC1 independent-observation robust covariance.}
\item{target, expected_target}{Physical target with event, positive horizon,
time_unit and conditioning equal to given_default. Prediction requires exact match.}
\item{training}{Named metadata including data_fingerprint, split_id, known_at and
workout_population equal to closed_defaulted_episodes. Optional data_origin. The
caller establishes row-level feature chronology and complete workout outcomes.}
\item{model_id, model_version}{Nonempty frozen model identifiers.}
\item{weights, weight_type}{Nonnegative integer frequencies, positive total, at
most 2^53-1. These represent independent replicated observations.}
\item{fit_intercept}{Whether the mean design contains an intercept.}
\item{max_iter, tolerance}{Positive iteration limit and score tolerance in (0,1).}
\item{precision_design, precision_feature_order}{Optional separate precision matrix
and column names. Precision always has an intercept. NULL means constant precision.}
\item{component_spec}{Named list with boundary_origin="structural" and
gate_link="logit"; does not support rounded/censored endpoints.}
\item{fitted_model}{Frozen FitResult or validated declarative model artifact.}
\item{default_condition}{The explicit string given_default.}
\item{quantile_levels}{Optional strictly increasing probabilities inside (0,1).
Fractional models reject quantiles because their distribution is unidentified.}
\item{context}{Optional calculation context.}
}
\value{FitResult with coefficients, covariance, support ranges, input fingerprints
and fit diagnostics. predict_lgd returns CalcResult with mean_lgd, distribution,
optional precision, endpoint probabilities and quantiles, plus model fingerprint
and marginal training-range violations.}
\details{Fractional logit maximizes the Bernoulli quasi likelihood and uses the
sandwich covariance. Frequency weights enter the meat linearly; HC1 applies
N/(N-k). Endpoints remain unchanged, and constant interior targets can yield zero
empirical covariance. Full-rank and finite-MLE separation checks are enforced.

Beta uses shape parameters mu*phi and (1-mu)*phi, with logit mean and log precision.
Analytic score and observed information include mean/precision cross derivatives.
Covariance assumes independent, correctly specified beta observations. The
effective normalized score threshold is max(sqrt(tolerance),1e-7), recorded in
diagnostics. Native R reports gradient evaluations rather than iteration counts. Perfectly
explained interior responses lack a finite precision estimate and are rejected.

The inflated model estimates boundary probability b, conditional-one probability
o and an interior beta model. Endpoint probabilities are b*(1-o) and b*o; the mean
is b*o+(1-b)*mu. All components require positive-frequency support and identifiable
fits. Quantiles include the endpoint atoms. Component marginal support violations
are reported; these do not establish joint support.

Prediction retains fitted provenance and rejects conflicting context target or
model-version fields; currency is checked when declared in the target. Models are
native declarative artifacts replayable without fitting code callbacks.
Native optimizers can take different iteration counts. No downturn margin,
regulatory approval or open-workout censoring correction is implied.}
\references{Papke and Wooldridge (1996), Journal of Applied Econometrics 11,
619--632. Cribari-Neto and Zeileis (2010), Journal of Statistical Software 34(2),
\doi{10.18637/jss.v034.i02}. Ospina and Ferrari, Inflated Beta Distributions,
\url{https://arxiv.org/abs/0705.0700}.}
