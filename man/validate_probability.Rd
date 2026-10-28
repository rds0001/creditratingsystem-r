\name{validate_probability}
\alias{validate_probability}
\title{Validate Probability Values}
\usage{validate_probability(value, boundary_policy = "closed",
    context = NULL)}
\arguments{
\item{value}{A finite numeric scalar, vector, matrix or array.}
\item{boundary_policy}{\code{"closed"} admits zero and one; \code{"open"} excludes both endpoints.}
\item{context}{Optional calculation context.}}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Checks the probability domain without clipping or coercing text or logical input.}
\value{A \code{CalcResult} with the validated value and its dimensions preserved. Invalid inputs raise a \code{creditratingsystem_error} condition with a stable \code{code}.}
\examples{validate_probability(c(0, 0.2, 1))$value}
