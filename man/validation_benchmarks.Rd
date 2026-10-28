\name{validation_benchmarks}
\alias{benchmark_predictions}
\alias{benchmark_ratings}
\title{Paired Frozen Predictions and Dated Rating Benchmarks}
\usage{
benchmark_predictions(model_cohorts, reference_model_id, metrics,
 matching_policy, multiple_comparisons, design, uncertainty,
 cluster_resamples = NULL, confidence_level = 0.95, context = NULL)
benchmark_ratings(internal_ratings, benchmark_records,
    scale_mapping,
 matching_policy, design, uncertainty, cluster_resamples = NULL,
 confidence_level = 0.95, context = NULL)
}
\arguments{
\item{model_cohorts}{Named model entries containing sealed monitoring cohort, model and parameter versions, fingerprint and training knowledge date.}
\item{reference_model_id}{Reference model key.}
\item{metrics}{Brier, log loss, AUC, Gini, calibration error or mean PD, using lower-case names with underscores.}
\item{matching_policy}{Prediction strict/intersection policy; for ratings a list specifying provider, exact_date/latest_known method, max_age_days, watch_policy, kappa_weighting and benchmark_origin.}
\item{multiple_comparisons}{none or bonferroni across candidate/metric comparisons.}
\item{design}{Frozen HOLDOUT/EXTERNAL validation model, parameter, data, population and split identifiers, evaluation_cutoff and target contract.}
\item{uncertainty}{none or cluster_bootstrap.}
\item{cluster_resamples}{Shared lists of cluster IDs sampled with replacement, preserving original cluster count.}
\item{confidence_level}{Interior confidence level.}
\item{context}{Optional matching calculation context.}
\item{internal_ratings}{Dated internal records with common prediction identity, dates, model versions and rating entity/unit, scale, status, grade and concept fields.}
\item{benchmark_records}{Local external records with unique benchmark_id, entity/unit/provider, effective and knowledge dates, target, scale, status, grade and rating concepts.}
\item{scale_mapping}{Frozen identified mapping, ordered common grades, identified internal/benchmark scales and complete order-preserving maps to common grades.}
}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Compares genuine paired mature prediction cases or explicitly mapped dated local ratings, preserving exclusions and non-estimability.}
\details{Prediction cohorts must agree on case identity, dates, weights and known labels. Negative outcomes require complete-through coverage; maturity precedes all outcome metrics. Differences are candidate minus reference. Calibration error is observed minus predicted. Infinite boundary log loss and single-class AUC remain explicit. Shared cluster percentile intervals condition on frozen models; undefined replicates suppress intervals. Bonferroni is an approximate bootstrap family adjustment, not correction for selecting candidates.

Rating concepts, targets and units must match exactly; no PIT/TTC, currency, issuer/issue or support conversion is inferred. Latest known matching chooses a record before testing withdrawal, watch, age and scale, without falling back to an earlier active record. Ambiguous equal-date records error. Signed notches are positive for worse internal ratings. Weighted Cohen kappa uses empirical margins; Spearman uses tied midranks. Degenerate margins are non-estimable. Derived mappings are not independent outcome calibration evidence. Rating agreement does not require outcome maturity. Declared provenance is not authenticated, and observed performance differences are not causal effects.}
\value{CalcResult with ValidationResult metrics, uncertainty, applicability, explicit coverage, matching diagnostics, frozen design, fingerprints and NOT_ASSESSED decision.}
\references{Cohen (1968), Psychological Bulletin 70, 213--220, doi:10.1037/h0026256.}
