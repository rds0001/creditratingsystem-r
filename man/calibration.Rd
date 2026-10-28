\name{calibrate_intercept}
\alias{calibrate_intercept}
\alias{calibrate_logistic}
\alias{calibrate_isotonic}
\alias{apply_calibrator}
\title{Fit and Apply Frozen Probability Calibration}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}

Fit an intercept shift, logistic recalibration, or weighted isotonic step map
with explicit target and sample metadata. Apply the frozen artifact with target
and model-version checks. All calculations are native R and offline.
}
\usage{
calibrate_intercept(base_pd, target_default_rate, weights = NULL,
  metadata, context = NULL, tolerance = 1e-12, max_iterations = 200)
calibrate_logistic(base_pd, outcomes, weights = NULL, metadata,
  context = NULL, tolerance = 1e-10, max_iterations = 100)
calibrate_isotonic(scores, outcomes, weights = NULL, metadata,
  increasing = TRUE, out_of_bounds = "error", context = NULL)
apply_calibrator(calibrator, values, target, base_model_version,
  context = NULL, groups = NULL)
}
\arguments{
\item{base_pd}{Numeric probability vector. Closed unit interval for intercept
shifts; strictly interior probabilities for logistic calibration.}
\item{target_default_rate}{Scalar weighted target default rate.}
\item{weights}{Nonnegative integer frequency weights of matching length with
positive finite sum. Defaults to one per row. Zero-weight rows do not identify
parameters or define isotonic support.}
\item{metadata}{Named list containing \code{target}, \code{calibration},
\code{base_model_version}, \code{population_definition}, \code{outcome_policy},
\code{weight_type="frequency"} and \code{data_origin}. See Details.}
\item{groups}{Optional aligned group IDs for a hierarchical calibration artifact; see calibration_models.}
\item{context}{Optional calculation context. Any supplied horizon, unit,
probability measure and conditioning must agree with the target.}
\item{tolerance}{Positive numerical convergence tolerance smaller than 0.01.}
\item{max_iterations}{Positive integer iteration limit.}
\item{outcomes}{Fully observed numeric binary first-default outcomes.}
\item{scores}{Finite numeric scores, not necessarily probabilities.}
\item{increasing}{Whether calibrated probability is nondecreasing in score.
Set to \code{FALSE} for a nonincreasing map.}
\item{out_of_bounds}{Frozen isotonic support policy: \code{"error"} rejects
scores outside positive-weight calibration support; \code{"clip"} uses the
nearest endpoint probability and counts affected inputs.}
\item{calibrator}{Calibration \code{CalcResult} or its plain artifact list.}
\item{values}{Numeric vector to transform, in its original order. Empty
application vectors are allowed.}
\item{target}{Explicit target list matching the frozen calibration target.}
\item{base_model_version}{Version matching the calibrated base model.}
}
\details{
The target contains \code{event}, positive \code{horizon}, \code{time_unit}
(days, months or years), \code{probability_measure} (PHYSICAL or RISK_NEUTRAL)
and \code{conditioning}. Calibration metadata contains ISO dates \code{start},
\code{end}, \code{known_at} with start no later than end no later than known-at,
a nonempty \code{split_id}, and \code{prediction_origin}. Empirical fitting
requires HOLDOUT or OUT_OF_FOLD; EXTERNAL_TARGET is additionally allowed for
intercept shifts. Data origin is USER_SUPPLIED, SYNTHETIC or REFERENCE.
These are caller declarations: the functions do not independently establish
sample independence or outcome maturity.

Intercept calibration solves the weighted mean equation for
\eqn{\mathrm{logit}^{-1}(\mathrm{logit}(p)+a)} while retaining
structural zero/one probabilities. Finite-shift endpoint targets and a sample
without interior positive-weight probabilities are rejected.

Logistic calibration fits unpenalized binomial maximum likelihood on the
base logit. Constant logits, complete separation and quasi separation are
reported as controlled failures. Endpoints are never silently clipped.
Nonpositive slope is explicitly diagnosed. Fisher covariance assumes
independent frequency repetitions; it is not cluster-robust uncertainty.

Isotonic calibration first combines tied scores, then uses weighted
pool-adjacent-violators. Its application is left-constant on intervals
\eqn{[x_i,x_{i+1})}; there is no linear interpolation. Original input order is
preserved. The support policy is stored in the artifact.

Artifacts carry parameters, target, metadata, diagnostics, an input fingerprint
and a checked content hash. To exchange across languages, place the artifact
in an additional \code{calibration_artifact} field of a valid base model and
use \code{serialize_fitted_model}/\code{load_fitted_model}. These retain full
numeric precision. Rounded JSON may fail integrity checks intentionally.
Hashes detect changed content; they do not authenticate authorship.
}
\value{
A \code{CalcResult}. Fit functions return a versioned CalibrationArtifact in
\code{$value}; application returns a probability vector. Diagnostic and
provenance fields remain separate. No result implies validation or approval.
}
\references{
\url{https://scikit-learn.org/stable/modules/calibration.html}

\url{https://docs.scipy.org/doc/scipy/reference/generated/scipy.optimize.isotonic_regression.html}

\url{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html}
}
\examples{
metadata <- list(
  target = list(event = "first_default", horizon = 12, time_unit = "months",
                probability_measure = "PHYSICAL", conditioning = "PIT"),
  calibration = list(start = "2020-01-01", end = "2021-12-31",
                     known_at = "2022-01-01", split_id = "synthetic",
                     prediction_origin = "EXTERNAL_TARGET"),
  base_model_version = "synthetic_v1", population_definition = "Hand example",
  outcome_policy = "Fully observed first default", weight_type = "frequency",
  data_origin = "SYNTHETIC")
fit <- calibrate_intercept(c(.1, .1), 2/11, metadata = metadata)
apply_calibrator(fit, c(.1, .2, .5), metadata$target, "synthetic_v1")$value
}
