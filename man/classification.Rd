\name{fit_binary_glm}
\alias{fit_binary_glm}
\alias{predict_pd}
\title{Estimate and Apply Binary Probability Models}
\usage{
fit_binary_glm(X_train, y_train, link, feature_order, fit_intercept,
  target, training, model_id, model_version, weights = NULL,
  weight_type = "frequency", offset = NULL, max_iter = 100L,
  tolerance = 1e-10, context = NULL)
predict_pd(fitted_model, X, feature_order, expected_target,
  offset = NULL, context = NULL)
}
\arguments{
\item{X_train, X}{Training uses a finite numeric matrix; prediction accepts numeric matrices or numeric row lists, typed categorical tables for naive Bayes, X/groups lists for grouped Bayesian models, or dated masked records for GRU/LSTM. A zero-column numeric matrix specifies an intercept-only model. Features must match the frozen transformation contract.}
\item{y_train}{Observed numeric zero/one outcomes, one per training row.}
\item{link}{Explicit \code{"logit"}, \code{"probit"} or \code{"cloglog"}.}
\item{feature_order}{Distinct nonempty names in exact matrix-column order; empty for a zero-column matrix.}
\item{fit_intercept}{Explicit logical indicating whether to add an intercept.}
\item{target, expected_target}{Named list with event, positive horizon, time unit, probability measure and conditioning. Prediction requires the exact fitted target.}
\item{training}{Named list with data fingerprint, split identifier and ISO knowledge date. Optional data origin declares synthetic or user-supplied observations.}
\item{model_id, model_version}{Nonempty model identity strings.}
\item{weights}{Nonnegative integer frequency counts per row; default one. Zero-weight rows are excluded.}
\item{weight_type}{Only \code{"frequency"} is supported.}
\item{offset}{Optional known additive link-scale vector. Its presence at prediction must match fitting.}
\item{max_iter}{Positive integer IRLS iteration limit.}
\item{tolerance}{Positive convergence tolerance below one.}
\item{context}{Optional calculation context, with compatible horizon, measure and conditioning.}
\item{fitted_model}{Supported frozen ModelArtifact, FitResult, or model-loading CalcResult. Supported families include GLM, discriminant, naive Bayes, Firth, penalized/ordinal, GAM, forests, boosting, Bayesian, MLP, GRU/LSTM, scorecards, stacked models, selection protocols, joint-selection models and market-to-physical bridges; raw SVM margins remain uncalibrated scores.}}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Fits an unpenalized Bernoulli likelihood natively in R and applies frozen coefficients using an explicit target and feature contract.}
\details{The estimator uses \code{stats::glm.fit}. No preprocessing, feature selection or calibration is inferred. Integer frequency weights mean repeated identical observations, not sampling, class-balancing or EAD weights. Covariance is inverse expected Fisher information with independent Bernoulli observations and unit dispersion. It is not cluster-robust. AIC and BIC use the Bernoulli likelihood, with BIC sample size equal to the sum of frequency counts.

The design must have full rank at relative singular-value tolerance 1e-10. A bounded linear program checks complete and quasi separation: maximize mean signed margin with every signed margin nonnegative and each direction coefficient between minus one and one, after column normalization. Mean margin above 1e-8 is rejected; feasibility tolerance is 1e-7. Nonconvergence, unusable information, excessive scaled likelihood score or fitted numerical probability boundaries raise controlled errors. No penalized fallback occurs. Offsets are supplied anew for prediction. Unsupported models or preprocessing are rejected. Generic prediction checks method/parameter-kind agreement, complete target and feature order; a supplied context knowledge date must not precede model availability. Common diagnostics bind canonical model/input hashes, training fingerprint/date, target and prediction count. Typed categorical, sequence and grouped inputs are preserved. No empirical fit or implicit calibration is performed, and PIT outputs are never relabelled TTC.}
\value{Fitting returns a \code{FitResult} with \code{model} and \code{value$model}, including coefficients, expected-Fisher covariance, target, training, provenance and diagnostics. Prediction returns a probability-valued \code{CalcResult}. Fit diagnostics include likelihood, deviance, AIC/BIC, frequency count, rank, condition number, score and its threshold, iteration count and warnings.}
\references{R Core Team, \url{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html}; lpSolve, \url{https://cran.r-project.org/package=lpSolve}.}
\examples{
target <- list(event = "default", horizon = 1, time_unit = "years",
  probability_measure = "PHYSICAL", conditioning = "performing")
training <- list(data_fingerprint = "analytic-counts", split_id = "train",
  known_at = "2025-12-31", data_origin = "SYNTHETIC")
fit <- fit_binary_glm(matrix(numeric(), 2, 0), c(0, 1), "logit",
  character(), TRUE, target, training, "intercept-example", "1",
  weights = c(7, 3))
predict_pd(fit, matrix(numeric(), 1, 0), character(), target)$value
}
