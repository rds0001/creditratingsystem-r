\name{validate_rating_scale}
\alias{validate_rating_scale}
\alias{map_pd_to_grade}
\title{Validate an Explicit Rating Scale and Assign PD Grades}
\usage{
validate_rating_scale(rating_scale, boundary_convention = NULL)
map_pd_to_grade(pd, rating_scale, context = NULL)
}
\arguments{
\item{rating_scale}{A named list with \code{schema_version = "1.0"}, nonempty \code{scale_id} and \code{version}, \code{boundary_convention}, and an ordered unnamed list \code{grades}. Each grade is a named list with \code{grade}, \code{lower}, \code{upper}, \code{representative_pd} and logical \code{is_default}. A scale validation result is also accepted.}
\item{boundary_convention}{Optional assertion of \code{"left_closed"} or \code{"right_closed"}. If supplied it must match the scale.}
\item{pd}{A finite scalar or vector of probabilities in [0,1].}
\item{context}{Optional context named list or context-building result.}}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Checks a complete user-defined probability scale and maps probabilities to its uniquely binding grade. No proprietary or default scale is supplied.}
\details{Grades run from lower to higher risk, cover [0,1] without gaps or overlaps, have distinct identifiers, and have strictly increasing representative PDs within their own intervals. Left-closed intervals contain their lower boundary; right-closed intervals contain their upper boundary. Zero and one are always covered. A final default singleton [1,1] is permitted and takes precedence at one under either convention. Only the final grade may have the default flag. That flag describes a supplied scale label and does not establish an observed default event. A grade is not itself a point probability.}
\value{A \code{CalcResult} with a validated scale or character grade vector. Mapping diagnostics contain scale identity, convention and binding intervals.}
\examples{
s <- list(schema_version = "1.0", scale_id = "example", version = "1",
  boundary_convention = "left_closed", grades = list(
    list(grade = "A", lower = 0, upper = 0.05,
         representative_pd = 0.01, is_default = FALSE),
    list(grade = "B", lower = 0.05, upper = 1,
         representative_pd = 0.2, is_default = FALSE)))
validate_rating_scale(s)$value$scale_id
map_pd_to_grade(c(0, 0.05, 1), s)$value
}
