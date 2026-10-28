\name{explainability_attribution}
\alias{explain_linear_credit_prediction}
\alias{estimate_credit_shapley_values}
\title{Frozen Linear and Grouped Shapley Credit Attribution}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Exact linear-predictor contributions and exact or permutation grouped Shapley attribution with explicit reference distributions.}
\usage{
explain_linear_credit_prediction(linear_model, transformed_inputs,
    baseline,
  feature_groups, offset = NULL, baseline_offset = 0,
    context = NULL)
estimate_credit_shapley_values(predictor, record, background,
    model_contract,
  feature_groups, masking_contract, method = "exact",
    permutations = NULL,
  max_coalitions = 4096, context = NULL)
}
\arguments{
\item{linear_model}{Frozen model contract plus coefficients, intercept, link (identity, logit, probit, cloglog) and transformed_feature_lineage, a named list of raw-source-name lists.}
\item{transformed_inputs}{Numeric row-by-feature matrix already using frozen transformations.}
\item{baseline}{Numeric reference feature vector.}
\item{feature_groups}{Named list partitioning every transformed feature exactly once into groups. Group IDs are sorted for coalition indexing.}
\item{offset}{Optional numeric row offsets.}
\item{baseline_offset}{Scalar reference offset.}
\item{predictor}{Deterministic local function from numeric row matrix to numeric predictions or CalcResult with numeric value. No fitting takes place.}
\item{record}{Numeric feature vector to explain.}
\item{background}{Interventional empirical reference list with values (matrix or row lists) and weights. NULL for supplied conditional distributions.}
\item{model_contract}{model_id, model_version, ordered feature_order, output_scale (probability, log_odds, score, currency), deterministic_frozen TRUE and target with event, horizon, time_unit, probability_measure and conditioning. Use explicit lists for portable content hashes.}
\item{masking_contract}{masking, reference_id and reference_version. Masking is INTERVENTIONAL_EMPIRICAL or SUPPLIED_CONDITIONAL_DISTRIBUTIONS. Conditional coalition_backgrounds maps EMPTY or comma-separated zero-based sorted-group indices to values/weights distributions. Observed coalition features must equal the record. Permutation mode declares permutation_design FIXED_PERMUTATIONS or IID_RANDOM_PERMUTATIONS.}
\item{method}{exact or permutation.}
\item{permutations}{Explicit list of complete group-ID permutation lists, required for permutation mode.}
\item{max_coalitions}{Positive integer budget for evaluated coalitions; exact mode requires all powerset coalitions.}
\item{context}{Optional calculation context consistent with the target.}
}
\details{
Linear contributions are coefficient times displacement from the reference and add on the linear predictor scale. Offsets are separate. Nonlinear response and response differences are shown separately; logit contributions are not additive PD changes.

Interventional masking breaks dependence between observed and masked features. Conditional distributions are supplied by the user and require independent validation. Exact Shapley weights are factorial subset weights. Permutation contributions telescope and are averaged; only declared IID random permutations with at least two draws receive a sampling standard error. Fixed permutation plans have none. This uncertainty excludes model and reference estimation uncertainty. Local accuracy and complete-coalition prediction consistency are checked. Coalitions are cached within the explicit budget.

For the product function x*y at (2,3) against (0,0), exact contributions are (3,3). With perfectly correlated equal-probability (0,0)/(1,1), explaining f=x conditionally at (1,1) yields (.25,.25) about baseline .5. Attributions explain a supplied reference game, not causal effects or credit approval.
}
\value{CalcResult containing contributions, reference and prediction, reconstruction/local-accuracy diagnostics and content hashes.}
