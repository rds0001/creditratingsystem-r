\name{lgd_downturn}
\alias{estimate_downturn_lgd}
\title{Declared Adverse-Scenario Conditional LGD Comparison}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Compare matched portfolio covariates under a frozen conditional LGD
model and dated scenario policy, optionally with supplied paired model draws.}
\usage{
estimate_downturn_lgd(fitted_lgd_model, baseline_covariates,
    adverse_covariates,
  aggregation_weights, feature_order, expected_target,
    downturn_definition,
  baseline_precision_design = NULL, adverse_precision_design = NULL,
  precision_feature_order = NULL, model_draws = NULL,
  uncertainty_contract = NULL, interval_level = 0.95,
    context = NULL)
}
\arguments{
\item{fitted_lgd_model}{Frozen supported conditional LGD model or FitResult.}
\item{baseline_covariates, adverse_covariates}{Matched numeric predictor matrices.}
\item{aggregation_weights}{Nonnegative finite portfolio weights, positive total;
normalized by scaled summation. They are not fitting frequency weights.}
\item{feature_order, expected_target}{Exact frozen predictor order and target.}
\item{downturn_definition}{Named object with definition_id, version, selection_basis,
baseline_scenario_id, adverse_scenario_id, known_at, as_of, ordered unique unit_ids,
extrapolation_policy. Policy and model must be known at as_of. Extrapolation policy
is error or report for positive-weight marginal training-support violations.}
\item{baseline_precision_design, adverse_precision_design, precision_feature_order}{
Separate optional precision predictors matching the frozen beta model.}
\item{model_draws}{Optional list of at least two compatible frozen model artifacts.}
\item{uncertainty_contract}{For supplied draws: source is bootstrap, posterior or
user_supplied; interpretation is a nonempty statement of inferential meaning.}
\item{interval_level}{Central empirical interval coverage in (0,1).}
\item{context}{Optional compatible calculation context; as_of must match policy.}
}
\value{CalcResult with baseline_lgd, downturn_lgd, signed difference, unit-level
predictions, normalized weights and uncertainty. Diagnostics retain policy/model
hashes, explicit policy and baseline/adverse marginal range flags.}
\details{No scenario selection, regulatory surcharge or positivity floor is inferred.
A model can imply a negative adverse-minus-baseline difference. Identical scenarios
produce zero difference. Marginal training ranges do not establish joint support
or causality. Uncertainty is NOT_ESTIMATED unless paired model draws are supplied.
Each draw evaluates both scenarios; empirical linear quantiles (type7) summarize
baseline, adverse and paired difference, retaining the declared interpretation.
No statistical coverage guarantee is created from user-supplied illustrative draws.}
