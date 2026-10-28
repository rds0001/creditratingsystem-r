\name{cumulative_pd_from_conditional}
\alias{cumulative_pd_from_conditional}
\alias{marginal_pd_from_conditional}
\title{First-Default Probabilities from Period Hazards}
\usage{
cumulative_pd_from_conditional(conditional_pd, context = NULL)
marginal_pd_from_conditional(conditional_pd, context = NULL)
}
\arguments{
\item{conditional_pd}{A finite numeric vector of chronological first-default probabilities, each conditional on survival to the period start. Values must lie between zero and one.}
\item{context}{Optional calculation context.}}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Converts conditional probabilities into cumulative first-default probabilities or unconditional period first-default masses.}
\details{Survival through period k is the product of one minus each preceding hazard, including the current hazard. Cumulative PD is one minus survival. Period default mass is the current hazard times survival to its start. Calculations use log-survival for numerical accuracy; marginal masses are computed directly rather than by differencing cumulative probabilities. A unit hazard absorbs all remaining survival. Empty inputs return empty vectors. No cure, repeated default, interpolation or annualization is implied.}
\value{A \code{CalcResult} whose \code{value} is a probability vector in chronological order.}
\examples{
cumulative_pd_from_conditional(c(0.1, 0.2))$value
marginal_pd_from_conditional(c(0.1, 0.2))$value
}
