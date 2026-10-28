\name{expected_loss}
\alias{expected_loss}
\title{Expected Default Loss}
\usage{expected_loss(pd, lgd, ead, context = NULL)}
\arguments{
\item{pd}{Finite probabilities in the closed interval from zero to one.}
\item{lgd}{Finite economic loss ratios. Negative ratios and ratios above one are permitted.}
\item{ead}{Finite nonnegative exposures in a consistent monetary unit.}
\item{context}{Optional calculation context.}}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Computes undiscounted expected default loss as PD times LGD times EAD.}
\details{All inputs are scalar or one-dimensional numeric vectors. Length-one inputs expand; all other lengths must agree. Empty inputs and implicit vector recycling are rejected. The horizon is the horizon of the supplied PD. LGD and EAD must be deterministic or jointly specified conditional on default. Multiplying separately estimated marginal means is generally invalid when residual dependence remains. No currency is inferred and LGD is not capped. Binary exponent scaling preserves representable products when a naive intermediate multiplication would underflow; non-finite final results are rejected.}
\value{A \code{CalcResult} whose \code{value} is the expected loss, in the EAD monetary unit.}
\examples{
expected_loss(0.02, 0.4, 100000)$value
expected_loss(c(0, 0.1, 1), 0.5, 200)$value
}
