\name{monitoring_metrics}
\alias{build_monitoring_cohorts}
\alias{monitor_rating_cohort}
\alias{detect_credit_feature_drift}
\alias{detect_pd_calibration_drift}
\alias{compare_challenger_cohorts}
\alias{diagnose_fairness_slices}
\title{Mature-outcome monitoring, distribution drift and paired diagnostics}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Local quantitative monitoring under explicit coverage, maturity, cluster resampling and null-path contracts. No model promotion or recalibration is performed.}
\usage{
build_monitoring_cohorts(predictions, outcome_events, coverage,
  target_contract, evaluation_cutoff, context = NULL)
monitor_rating_cohort(cohort, group_by, reference_window,
    minimum_count,
  uncertainty, cluster_resamples = NULL, confidence_level = .95,
  context = NULL)
detect_credit_feature_drift(reference, current, feature_order,
    method,
  permutations, multiple_testing, dependence_assumption,
  reference_weights = NULL, current_weights = NULL,
    bandwidth = NULL,
  frozen_bins = NULL, smoothing = .5, permutation_units = NULL,
  context = NULL)
detect_pd_calibration_drift(residuals, method, parameters,
    null_paths,
  null_spec, alpha, validation_null_paths = NULL,
  confidence_level = .95, context = NULL)
compare_challenger_cohorts(champion, challenger, cluster_resamples,
  selection_protocol, confidence_level = .95, context = NULL)
diagnose_fairness_slices(cohort, group_labels, reference_group,
  use_basis, minimum_count, risk_threshold, cluster_resamples,
  confidence_level = .95, context = NULL)
}
\arguments{
\item{predictions}{List of records: forecast_id, case_id, entity_id, cluster_id, cohort, segment, model_id, model_version, forecast_at, known_at, pd and exact target contract. Optional weight, origin_grade, override, decision and prediction_origin.}
\item{outcome_events}{Records with unique event_id, entity_id, event_at, known_at, default_definition and event (default, exit or rating). A rating also requires grade.}
\item{coverage}{Records with entity_id, complete_through and known_at at or after completion, declaring complete event ascertainment.}
\item{target_contract}{Named list: event=default, probability_measure=PHYSICAL, default_definition, unit, positive integer horizon_days, optional default_grade. Entity identifiers denote the declared unit.}
\item{evaluation_cutoff}{ISO date; later-known outcomes and coverage are excluded.}
\item{cohort,champion,challenger}{Builder CalcResult or unchanged content-hashed cohort bundle.}
\item{group_by}{Unique grouping fields from cohort rows.}
\item{reference_window}{Named start/end ISO forecast dates, inclusive, applied within each group.}
\item{minimum_count}{Positive integer support threshold.}
\item{uncertainty}{none, iid_homogeneous_binomial, or independent_cluster_bootstrap.}
\item{cluster_resamples}{At least two supplied lists/vectors of cluster identifiers. Every path samples the original number of distinct clusters with replacement. All rows of selected clusters remain together.}
\item{confidence_level}{Interior probability for pointwise percentile or exact rate intervals.}
\item{reference,current}{Nonempty lists of feature records.}
\item{feature_order}{Unique feature names.}
\item{method}{Feature drift: ks, wasserstein, mmd_rbf or psi. Sequential drift: cusum or ewma.}
\item{permutations}{B by pooled-N numeric matrix of zero-based complete permutations, even in R. At least two rows; NULL for PSI.}
\item{multiple_testing}{none, holm, bonferroni; not_applicable for descriptive PSI.}
\item{dependence_assumption}{exchangeable_rows or exchangeable_clusters for inference; descriptive for PSI.}
\item{reference_weights,current_weights}{Finite nonnegative weights with positive sums; default unit weights.}
\item{bandwidth}{Positive prespecified RBF kernel bandwidth.}
\item{frozen_bins}{Per-feature named specifications: numeric kind, increasing cutpoints and two support endpoints; or categorical kind and unique string levels.}
\item{smoothing}{Positive pseudo-count per weighted PSI bin.}
\item{permutation_units}{Pooled cluster labels for intact equal-size, internally ordered cluster permutation.}
\item{residuals}{Strictly ordered records with ISO time, finite centered value, mature=TRUE. The caller derives counts, rates or standardized residuals under the declared scale.}
\item{parameters}{CUSUM allowance at least zero; or EWMA smoothing in (0,1].}
\item{null_paths}{B by T finite matrix of independent complete null simulation paths matching the monitored horizon. Within-path dependence must reflect the null model.}
\item{null_spec}{model_id, model_version, calibration_id, residual_scale, probability_measure=PHYSICAL, dependence (independent or prespecified_dependent), calibrated_before no later than monitoring start. Independent validation additionally requires validation_paths_independent=TRUE.}
\item{alpha}{Interior target finite-horizon null crossing probability.}
\item{validation_null_paths}{Optional independent C by T paths for empirical false-alarm assessment with exact binomial interval.}
\item{selection_protocol}{protocol_id, evaluation_role=OUT_OF_TIME_TEST, and multiple_challenger_policy single_prespecified or separate_selection_and_final_test. Paired predictions require prediction_origin=OUT_OF_TIME.}
\item{group_labels}{Unique case_id/group records; labels are supplied, never inferred.}
\item{reference_group}{Observed group used for paired differences.}
\item{use_basis}{Nonempty documented permission/purpose for group analysis.}
\item{risk_threshold}{Positive prediction is PD greater than or equal to this probability.}
\item{context}{Optional calculation context.}
}
\details{
The default window is open at forecast time and closed at forecast plus horizon_days. Zero labels require mature, fully covered windows. Early known defaults remain positive but ineligible before horizon maturity. Prior defaults/exits exclude forecasts; later exits censor unobserved outcomes. Cure never removes the first default within the event horizon. LGD and incomplete workout labels are not treated as binary default observations.

Migration grades derive from dated, known rating events; prediction outcome_grade is ignored. Origin grade carries forward only under complete mature coverage. Coverage used for migration must also certify complete rating-event capture. First default uses the declared default grade. Hashes detect bundle edits but do not prove source declarations.

Cohort metrics use weighted AUC/Gini, Brier, exact log loss and calibration errors. One-class AUC is null. Impossible log loss retains infinite status and null numerical loss. Exact Clopper--Pearson rate intervals require unit weights, unique clusters and independent homogeneous Bernoulli outcomes. Bootstrap quantiles use type 7; unidentified replicate counts and degenerate bands remain visible.

KS is a weighted empirical CDF supremum. Wasserstein integrates the absolute CDF difference. MMD is the squared weighted empirical RBF embedding distance including diagonals, not the unbiased U-statistic. Missing values are complete-case exclusions for numeric tests and distinct PSI bins. PSI uses smoothed fixed-bin shares and separate unknown-category bins; it has no universal significance threshold.

Numeric permutation p-values equal (1 + null exceedance count)/(B+1), using a relative 1e-14 comparison tolerance. The reported Monte Carlo standard error is approximate and requires independent uniform valid permutations. A 95 percent exact binomial interval for the underlying permutation-tail probability remains nondegenerate at sample boundaries. Hand-selected paths check arithmetic only; exhaustive enumeration inference is not claimed. Cluster paths must move complete equal-size clusters, preserving within-cluster order; weights travel with observations. Marginal multiplicity correction is explicitly chosen. Input drift does not establish concept drift.

CUSUM initializes two sums at zero and updates max(0, up+residual-allowance) and max(0, down-residual-allowance); statistic is their maximum. EWMA initializes at zero, updates smoothing*residual+(1-smoothing)*previous, and uses the absolute value. Threshold is ascending null-path-maximum order statistic ceiling((B+1)*(1-alpha)). Insufficient simulation count is rejected. Crossing is strictly greater than threshold. Exchangeability of calibration maxima and a future whole null path yields an unconditional finite-horizon rank bound; it does not guarantee performance under a misspecified or estimated null. Calibration exceedance frequency and optional independent validation are distinct. Horizon extension requires recalibration.

Challenger differences are challenger minus champion on jointly eligible matching cases. Missing counterparts and eligibility disagreements are separate outputs. Group selection rates use all known decisions, while outcome errors, TPR/FPR and calibration use mature observed cases. Fairness gaps use shared cluster paths; intervals are pointwise. Selective outcomes, small groups and boundary samples limit interpretation. No automatic deployment or fairness certification is implied.
}
\value{A CalcResult with the cohort bundle, monitoring panel, feature findings, sequential path and calibrated threshold, paired metric differences, or group slices, plus diagnostics and explicit limitations.}
\references{
Gretton et al. (2012), A Kernel Two-Sample Test, Section 2.2, equation (5): \url{https://www.jmlr.org/papers/volume13/gretton12a/gretton12a.pdf}.
NIST CUSUM and EWMA recurrences: \url{https://www.itl.nist.gov/div898/handbook/pmc/section3/pmc323.htm} and \url{https://www.itl.nist.gov/div898/handbook/pmc/section3/pmc324.htm}.
Kleinberg, Mullainathan and Raghavan (2017), Inherent Trade-Offs in the Fair Determination of Risk Scores: \url{https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.ITCS.2017.43}.
}
