\name{affordability_stress_backtest}
\alias{affordability_stress_backtest}
\title{Verify Frozen Household Stress Assessments}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Checks historically verified economic evidence, household source allocations and frozen contractual stress projections, with separately observed factual payments.}
\usage{
affordability_stress_backtest(cases, economic_evidence, allocations,
  design, payment_outcomes = NULL, tolerance = 1e-10,
  uncertainty = "none", cluster_resamples = NULL,
  confidence_level = .95, context = NULL)
}
\arguments{
\item{cases}{Common dated validation cases, household IDs, genuine baseline/scenario/assessment payloads, dated threshold profiles, expected economic IDs and exact evidence bindings.}
\item{economic_evidence}{Dated verified economic source records with normalized monthly net income or explicit gross-to-net reconciliation and complete contractual terms.}
\item{allocations}{Dated household proportional shares or explicit full-recourse contractual liability assignments.}
\item{design}{Frozen model/data/split/population, currency, target horizon and evaluation cutoff.}
\item{payment_outcomes}{Optional dated complete-capture factual scheduled and paid amounts.}
\item{tolerance}{Nonnegative numerical reconciliation tolerance.}
\item{uncertainty}{\code{none} or supplied \code{cluster_bootstrap}.}
\item{cluster_resamples}{Shared whole-case-cluster draws for factual payment metrics.}
\item{confidence_level}{Interior percentile coverage level.}
\item{context}{Optional context consistent with the design.}
}
\details{The actual stress engine is rerun with frozen inputs and compared with the original assessment. Unknown or late income is incomplete evidence, not an invented zero. Proportional household shares cannot duplicate an economic source; full recourse is explicit and contractual only. Gross less deductions reconciles to normalized monthly net income. Contractual resets, amortization, FX and policy thresholds remain governed by the original stress input.

Observed scheduled-minus-paid metrics are descriptive facts under complete mature capture; they are not presumed realizations of the scenario debt service. Positive residual income is not zero PD or a lending approval.}
\value{A CalcResult with a ValidationResult, verified projection flags, reconciliation findings, separate factual payment metrics, uncertainty and missing/censored period coverage.}
