\name{estimate_tranche_credit_metrics}
\alias{estimate_tranche_credit_metrics}
\alias{stress_tranche_structure}
\title{Definition-specific tranche risk and paired structure stress}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Separates payment-event probability, ultimate capital loss and discounted contractual cash shortfall; compares explicit structures on shared scenarios.}
\usage{
estimate_tranche_credit_metrics(tranche_paths, target,
    discount_factors,
  confidence_level = .95, context = NULL)
stress_tranche_structure(baseline_paths, alternatives, target,
  discount_factors, confidence_level = .95, context = NULL)
}
\arguments{
\item{tranche_paths, baseline_paths}{Bound TrancheWaterfallPaths artifact.}
\item{target}{Explicit event TIMELY_INTEREST, LEGAL_MATURITY_PRINCIPAL or ULTIMATE_PRINCIPAL, full calendar-day horizon, days time unit, physical measure and matching conditioning.}
\item{discount_factors}{Named list of positive origin-based factors for all payment dates.}
\item{confidence_level}{Interior confidence level for IID exact binomial intervals.}
\item{context}{Optional compatible target/currency context.}
\item{alternatives}{Rows with unique alternative_id, evaluated tranche_paths and declared assumption. Scenario axes, weights, origin, currency and shared innovation hash must match the baseline.}
}
\details{Discounted shortfall is the positive part of principal promised at legal maturity plus coupons due on actual opening balances minus dated receipts. Signed components are also returned. This includes payment timing and differs from undiscounted principal loss. Loss outside the selected event remains explicit. Open runoff leaves ultimate loss and discounted shortfall unidentified.

Exact Clopper-Pearson intervals and sample-mean discounted loss standard errors apply only to declared IID equally weighted scenarios. Fixed weighted or dependent/QMC scenarios have no IID uncertainty. Conditional principal severity is tied to the selected event; zero event probability yields null severity.

Stress outputs amount and rate deltas with changed notionals disclosed. Different joint default mappings may share identical innovations. Individual scenario-risk intervals are not paired-difference confidence intervals. No causal or optimization claim is made.}
\value{CalcResult containing bound TrancheCreditMetrics or TrancheStructureStress with source and innovation bindings.}
\references{\url{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/binom.test.html}}
