\name{scorecard}
\alias{fit_scorecard}
\alias{score_scorecard}
\alias{scale_score_points}
\alias{apply_qualitative_adjustments}
\alias{scorecard_reason_codes}
\title{Estimated logistic scorecards, odds scales and contribution explanations}
\usage{
fit_scorecard(design_matrix, outcomes, feature_order, fit_spec,
    target, training,
  model_id, model_version, weights = NULL, max_iter = 100,
  tolerance = 1e-10, context = NULL)
score_scorecard(fitted_model, transformed_features, feature_order,
  expected_target, context = NULL)
scale_score_points(log_odds, base_score, base_odds,
    points_to_double_odds,
  odds_convention, score_direction, context = NULL)
apply_qualitative_adjustments(qualitative_inputs, coefficient_spec,
  neutral_levels, base_value, adjustment_scale, context = NULL)
scorecard_reason_codes(score_contributions, reference_profile,
    reason_policy,
  context = NULL)
}
\arguments{
\item{design_matrix, transformed_features}{Finite numeric matrix of already frozen WOE/expert features.}
\item{outcomes}{Observed binary labels, default=1.}
\item{feature_order}{Explicit ordered feature names.}
\item{fit_spec}{neutral_profile numeric values and feature_kinds (woe/expert), one per column.}
\item{target, expected_target}{Common model target; prediction requires an exact match.}
\item{training}{Common training metadata, including fingerprint, split ID, knowledge date and optional data origin.}
\item{model_id, model_version}{Model identity and version.}
\item{weights}{Optional nonnegative integer frequency weights.}
\item{max_iter, tolerance}{Native GLM convergence controls.}
\item{context}{Optional calculation context.}
\item{fitted_model}{Scorecard FitResult or ModelArtifact.}
\item{log_odds}{Default-over-nondefault log-odds.}
\item{base_score}{Score at the stated base odds.}
\item{base_odds}{Strictly positive base odds under odds_convention.}
\item{points_to_double_odds}{Strictly positive absolute score change for doubling odds.}
\item{odds_convention}{default_over_nondefault or nondefault_over_default.}
\item{score_direction}{higher_is_safer or higher_is_riskier.}
\item{qualitative_inputs}{Named observed categorical levels. Missing is not neutral.}
\item{coefficient_spec}{Named feature lists of level coefficients on the declared adjustment scale.}
\item{neutral_levels}{Named reference level for every adjusted feature.}
\item{base_value}{Scalar predictor or rating-notch base.}
\item{adjustment_scale}{log_odds or rating_notches.}
\item{score_contributions, reference_profile}{Matching named scalar feature-contribution maps.}
\item{reason_policy}{adverse_direction positive/negative, positive integer max_reasons and named reason_codes for every feature.}}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Fits a logit scorecard using the native GLM engine with separation, rank and convergence checks, then replays contributions and explicitly interpretable score transformations.}
\details{Preprocessing and supervised binning must already be frozen within the training fold. Feature-kind declarations do not independently establish absence of leakage. The model contains an intercept and neutral-profile contribution reference. Complete or quasi separation is a controlled error; no proprietary coefficients are supplied.

Points are affine in default log-odds, with slope minus PDO/log(2) for higher-is-safer and plus PDO/log(2) for higher-is-riskier. Base odds may use either direction, which is explicitly converted. Returned inverse parameters reconstruct PD without rounding.

Qualitative effects subtract neutral-level coefficients. Log-odds effects return adjusted PD; integer rating-notch differences do not invent a PD or unknown rating-scale boundaries. Reasons compare model contributions with a declared reference, rank adverse differences, break ties lexically and retain full reconciliation. Explanations do not imply causality.}
\value{FitResult for fitting; otherwise CalcResult. Scoring returns PD, log_odds, intercept, feature and reference contributions and differences. Scaling returns points and inverse parameters. Adjustments return contributions, total, adjusted value and optional PD. Reasons return selected reasons, all differences, total difference and unreported difference.}
\references{Native binomial logit estimation follows \code{stats::glm.fit}; independent tests recover grouped binomial maximum-likelihood estimates and verify score inversion and contribution arithmetic.}
