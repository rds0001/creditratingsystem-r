\name{pit_ttc_transform}
\alias{pit_ttc_transform}
\title{Explicit Gaussian factor PIT and TTC transformation}
\usage{pit_ttc_transform(pd, systematic_factor, asset_correlation,
    direction,
  factor_distribution, horizon, time_unit, probability_measure,
    context = NULL)}
\arguments{
\item{pd}{Physical default probabilities in [0,1].}
\item{systematic_factor}{Finite standard-normal factor values; positive means good conditions.}
\item{asset_correlation}{Asset correlation in [0,1).}
\item{direction}{Either ttc_to_pit or pit_to_ttc.}
\item{factor_distribution}{Named list with family standard_normal, factor_id, reference_period, model_version.}
\item{horizon}{Positive common horizon.}
\item{time_unit}{days, months or years.}
\item{probability_measure}{PHYSICAL.}
\item{context}{Optional calculation context.}}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Conditions or unconditions a latent Gaussian default threshold under an explicit factor model. Scalar inputs expand to equal-length vectors.}
\details{PIT equals Phi((Phi-inverse(TTC)-sqrt(rho)*z)/sqrt(1-rho)). TTC integrates over the factor distribution; a zero factor does not generally yield TTC. Correlation and factor suitability are user assumptions. Boundary saturation can lose inverse information and is reported.}
\value{CalcResult containing probabilities and factor, horizon and conditioning diagnostics.}
\references{BIS (2005), An Explanatory Note on the Basel II IRB Risk Weight Functions, \url{https://www.bis.org/publications/explanatory-note-basel-ii-irb-risk-weight-functions}.}
