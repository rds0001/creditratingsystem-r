\name{estimate_simulation_risk}
\alias{estimate_simulation_risk}
\title{Sampling Aware Default and Joint Loss Estimates}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Estimate risk for one initially performing subject, keeping exact
IID binomial uncertainty separate from weighted or dependent scenario summaries.}
\usage{estimate_simulation_risk(path_records, sampling_contract,
  confidence_level = 0.95, context = NULL)}
\arguments{
\item{path_records}{Records with unique scenario_id, subject_id, logical
initial_default, defaulted and horizon_complete, nonnegative weight and
ead_at_default, and signed economic_loss. Nondefault rows have zero default EAD
and loss. Initially defaulted rows are excluded. Other positive-weight rows must
have a complete target horizon.}
\item{sampling_contract}{List with contract_id, version, subject_id, currency,
probability_measure (PHYSICAL/RISK_NEUTRAL), positive horizon, time_unit
(days/months/years), conditioning (initially_performing), event (first_default),
and sampling_design (IID_UNWEIGHTED/FIXED_WEIGHTED_SCENARIOS/DEPENDENT_OR_QMC).}
\item{confidence_level}{Two-sided exact binomial confidence level in (0,1).}
\item{context}{Optional compatible calculation context.}}
\details{IID estimates require unit weights and return a Clopper--Pearson interval.
The zero-default upper bound is positive. A zero plug-in standard error is not
certainty. Loss Monte Carlo standard error uses the IID sample variance and does
not establish tail robustness. Weighted scenarios and dependent or QMC points
receive no invented IID confidence interval. Kish effective weight count is a
concentration diagnostic, not a substitute binomial trial count.
Expected loss is the mean joint default loss. Conditional mean EAD and conditional
mean LGD generally do not factor into it; EAD-weighted LGD provides the monetary
factorization when defined. A zero-EAD default leaves individual LGD undefined.
Economic losses and LGDs are not capped. Censored positive-weight performing
trials are rejected instead of silently dropped.}
\value{A CalcResult with PD, its applicable interval, expected loss, conditional
loss/EAD/LGD, EAD-weighted LGD, sample standard errors where justified, effective
weight count, excluded initial defaults, and explicit empty-risk-set status.}
\references{Clopper and Pearson (1934), Biometrika 26(4), 404--413,
\doi{10.1093/biomet/26.4.404}. See also
\url{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/binom.test.html}.}
