\name{estimate_long_run_pd}
\alias{estimate_long_run_pd}
\title{Estimate observed long-run annual default rates}
\usage{estimate_long_run_pd(cohorts, selected_cohort_ids, knowledge_date,
    weighting,
  uncertainty, dependence_assumption, representativeness, target,
  confidence_level = .95, context = NULL)}
\arguments{
\item{cohorts}{List of records or data frame: cohort_id, period_start, period_end, known_at, defaults, denominator, mature, default_definition.}
\item{selected_cohort_ids}{Explicit unique cohort identifiers.}
\item{knowledge_date}{ISO calendar information cutoff.}
\item{weighting}{annual_mean or pooled_obligor.}
\item{uncertainty}{none, year_t or clopper_pearson.}
\item{dependence_assumption}{unspecified, independent_years or independent_homogeneous_obligors.}
\item{representativeness}{List with assessment (representative, not_representative or unknown), rationale and reference_period.}
\item{target}{List with event, default_definition, horizon=1, time_unit=years, probability_measure=PHYSICAL, conditioning.}
\item{confidence_level}{Confidence level in (0,1).}
\item{context}{Optional calculation context.}}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Aggregates explicitly selected complete mature annual cohorts available at the knowledge date.}
\details{Periods are start-inclusive, end-exclusive calendar years; February 29 anniversaries use February 28. Annual mean equally weights cohort rates. Pooled weighting divides total defaults by total denominator. Year-t intervals require independent nonoverlapping years and at least two cohorts; they are approximate and intersected with [0,1]. Exact binomial intervals require independent homogeneous obligors and nonoverlapping periods. Representativeness and independence remain unverified user assumptions. Repeated obligors and dependent defaults require other uncertainty methods. An observed long-run rate is not automatically TTC.}
\value{CalcResult with value list pd, lower, upper and cohort, target, weighting and assumption diagnostics. Bounds are NULL when uncertainty is none.}
\references{Clopper and Pearson (1934), as documented in \code{stats::binom.test}.}
