\name{selection_bias_sensitivity}
\alias{selection_bias_sensitivity}
\title{Validation of Frozen Selection Candidates}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Compare common application populations without fitting models; separate funded factual, synthetic and transported-proxy evidence and audit assumption-conditioned selection arithmetic.}
\usage{
selection_bias_sensitivity(candidates, evaluation_rows,
    selection_results,
  design, reference_model_id, uncertainty = "none",
    cluster_resamples = NULL,
  confidence_level = .95, outcome_comparability = NULL,
  tolerance = 1e-10, context = NULL)
}
\arguments{
\item{candidates}{Frozen model records with model identity/hash, training date/entities and application-aligned dated PD predictions.}
\item{evaluation_rows}{Standard dated validation prediction records, application identifiers, funding, label origins, known-at dates, exact target and complete-through negative-label coverage.}
\item{selection_results}{Records binding exact population, target and ordered applications to IPW, BOUNDS, MNAR_SHIFT or AIPW fixed arithmetic inputs and raw results.}
\item{design}{Frozen holdout/external validation design, physical target and evaluation cutoff.}
\item{reference_model_id}{Paired comparison reference candidate.}
\item{uncertainty}{None or supplied cluster_bootstrap.}
\item{cluster_resamples}{Whole-cluster draws with replacement.}
\item{confidence_level}{Interior confidence level for paired percentile intervals.}
\item{outcome_comparability}{Optional explicit external-proxy transport assumption and target.}
\item{tolerance}{Nonnegative absolute and relative artifact reconciliation tolerance.}
\item{context}{Optional calculation context, consistent with the declared target and cutoff.}
}
\value{CalcResult with ValidationResult, scoped metrics, paired differences, exclusions, actual cluster intervals, assumption-labelled transport results and reconciliation violations.}
\details{Whole horizons must mature even for known early defaults. Negative labels require complete coverage. Pseudo labels never become validation truth. Funding differs from acceptance. Each candidate covers the same population; time/entity leakage is excluded. IPW ESS and trimmed mass remain explicit; zero support prevents full-population identification. AIPW retains mandatory frozen out-of-fold nuisance lineage. No selection model is refitted. Model hashes and source declarations are not externally authenticated. Arithmetic agreement does not approve a policy. Brier and AUC contrasts are descriptive, without an implicit multiple-testing acceptance rule. Bootstrap uncertainty conditions on frozen models.}
\references{Ehrhardt et al., Reject Inference Methods in Credit Scoring, sections 1--2. \url{https://adimajo.github.io/assets/publications/rejectInference.pdf}}
