\name{retail_pool_diagnostics}
\alias{retail_pool_diagnostics}
\title{Validation of Frozen Retail Risk Pools}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Replay dated pool assignments and separately assess mature PD, closed-workout LGD, observed default EAD, concentration, residual heterogeneity and migration.}
\usage{
retail_pool_diagnostics(fitted_model, pool_assignments,
    assignment_inputs,
  predictions, cohort_outcomes, workouts, links, estimation_profile,
  design, previous_assignments = NULL, uncertainty = "none",
  cluster_resamples = NULL, confidence_level = .95,
    tolerance = 1e-10,
  context = NULL)
}
\arguments{
\item{fitted_model}{Frozen retail pooling model.}
\item{pool_assignments}{Sealed retail pool assignments.}
\item{assignment_inputs}{Original assignment arguments except model and context.}
\item{predictions}{Dated validation rows with observation ID, PD, nullable LGD/EAD, frozen reference exposure, explicit training/calibration exclusion.}
\item{cohort_outcomes}{Dated cohort records accepted by estimate_pool_risk_parameters; negative labels additionally require complete_through.}
\item{workouts}{Dated recovery and exposure records accepted by estimate_pool_risk_parameters.}
\item{links}{Observation, facility, obligor, household, risk-group, diagnostic-group identifiers and known-at date. Optional previous_observation_id.}
\item{estimation_profile}{Predated diagnostic weighting profile.}
\item{design}{Common physical validation target, currency and cutoff.}
\item{previous_assignments}{Optional earlier sealed assignments.}
\item{uncertainty}{None or supplied cluster_bootstrap.}
\item{cluster_resamples}{Whole-cluster draws respecting connected units.}
\item{confidence_level}{Interior percentile level.}
\item{tolerance}{Assignment reconciliation tolerance.}
\item{context}{Compatible optional calculation context.}
}
\value{CalcResult containing ValidationResult with separate parameter metrics and applicability, residual diagnostics, concentration, migrations, exclusions and cluster intervals.}
\details{No pool refit is performed. One forecast per declared default unit is required. Whole horizons mature before PD metrics; missing/open workouts do not invent final LGD. Constant pool PD has no identifiable within-pool rank. Relationships never propagate default labels. Observed diagnostic summaries do not replace frozen forecasts. Independent-unit binomial and episode intervals are omitted; supplied whole-cluster draws provide conditional uncertainty. A residual range or sparse sample does not establish pool homogeneity.}
\references{BCBS CRE36, published 2022-12-08, paragraphs 36.16--17 and 36.24. \url{https://www.bis.org/committees/bcbs/basel-framework/standard/cre/36/inforce/2023-01-01/published/2022-12-08}}
