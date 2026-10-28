\name{credit_permutation_importance}
\alias{credit_permutation_importance}
\title{Frozen Held-Out Grouped Permutation Importance}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Compare paired prediction losses under explicit shared permutation plans, without refitting or selecting features.}
\usage{credit_permutation_importance(predictor, X, y, model_contract,
  feature_groups, permutations, design, loss_contract, row_metadata,
  weights = NULL, strata = NULL, custom_loss = NULL,
  support_evaluator = NULL, support_contract = NULL, context = NULL)}
\arguments{
\item{predictor}{Frozen local prediction function accepting a row matrix.}
\item{X}{Held-out feature matrix or row lists.}
\item{y}{Numeric held-out mature labels.}
\item{model_contract}{Frozen identity, feature order, output scale and prediction target.}
\item{feature_groups}{Named nonoverlapping partition of all features.}
\item{permutations}{Matrix or row lists of complete zero-based permutation bijections. Each plan is shared across blocks.}
\item{design}{split OUT_OF_TIME/HOLDOUT, training_end, data_cutoff, train_row_ids, train_subject_ids, model_and_calibration_frozen TRUE, selection_use FORBIDDEN and permutation_design FIXED_PERMUTATIONS/IID_RANDOM_PERMUTATIONS.}
\item{loss_contract}{loss_id, version and kind BRIER, LOG_LOSS, MSE, MAE or CUSTOM. Binary probability losses require binary outcomes and probability outputs.}
\item{row_metadata}{One row_id, subject_id, observation_time, target_end and label_known_time record per evaluation row.}
\item{weights}{Optional nonnegative row weights with positive total; normalized internally.}
\item{strata}{Optional row labels; every permutation must preserve each recipient stratum.}
\item{custom_loss}{For CUSTOM only, function(y, predictions) returning one finite loss per row.}
\item{support_evaluator}{Optional local function from row matrix to one boolean support flag per row.}
\item{support_contract}{support_id and version, required with support_evaluator.}
\item{context}{Optional common calculation context.}
}
\details{Blocks are permuted jointly; outcomes and weights remain with recipients. Fitting-identity overlap, incomplete targets, future-known labels and invalid permutation plans are rejected. Positive-weight infinite log loss is not clipped and raises an explicit non-estimability error. Unsupported rows are reported and retained in denominators. Weighted feature correlation is descriptive; cross-block dependence can be broken. Only declared IID random permutations with at least two plans receive a standard error of the mean loss increase. This excludes cohort, model and selection uncertainty. Fixed plans receive none. No refitting or feature selection occurs; supplied provenance is checked but not authenticated.}
\value{CalcResult with baseline and per-plan losses, grouped paired increases, permutation uncertainty, support findings, correlations and content hashes.}
