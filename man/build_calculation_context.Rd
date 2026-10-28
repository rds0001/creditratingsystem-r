\name{build_calculation_context}
\alias{build_calculation_context}
\title{Build an Explicit Reproducible Calculation Context}
\usage{
build_calculation_context(as_of_date = NULL, knowledge_date = NULL,
  horizon = NULL, time_unit = NULL, currency = NULL,
  probability_measure = NULL, conditioning = NULL,
  model_version = NULL, data_manifest = NULL, scenario_id = NULL,
  policy_id = NULL)
}
\arguments{
\item{as_of_date}{Observation date as a real ISO YYYY-MM-DD calendar date.}
\item{knowledge_date}{Knowledge date with the same strict representation. Both dates are supplied together or omitted together.}
\item{horizon}{Optional positive finite scalar horizon, paired with a time unit.}
\item{time_unit}{\code{"days"}, \code{"months"} or \code{"years"}.}
\item{currency}{Optional nonempty unit identifier; no exchange conversion is performed.}
\item{probability_measure}{Optional \code{"PHYSICAL"} or \code{"RISK_NEUTRAL"}.}
\item{conditioning}{Optional nonempty description of probability conditioning.}
\item{model_version}{Optional model version identifier.}
\item{data_manifest}{Optional named list of JSON-compatible data identity information.}
\item{scenario_id}{Optional scenario identifier.}
\item{policy_id}{Optional local policy identifier.}}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Records explicit time, model and data identities with a stable SHA-256 context hash. The system clock is never consulted.}
\details{Without dates the mode is TIME_INDEPENDENT. With both dates the mode is DATED; the dates can be in either order to represent forecast or replay contexts. Hashing sorts object keys and uses a specified binary64 numeric representation, preserving text exactly. Named lists are JSON objects; unnamed lists are arrays. Length-one atomic values are scalars. An unnamed list expresses an explicit empty or length-one array, while \code{structure(list(), names = character())} expresses an empty object. Object keys must be unique printable ASCII text; values may contain UTF-8 text. Non-finite values are rejected. For cross-language interchange, avoid integral JSON literals outside the exact binary64 integer range; represent large non-integer quantities using floating-point notation.}
\value{A \code{CalcResult} whose \code{value} is a context named list with a \code{context_hash}. Context-aware functions accept this value or the whole result.}
\examples{
ctx <- build_calculation_context("2025-12-31", "2026-01-15",
  horizon = 1, time_unit = "years", currency = "EUR",
  probability_measure = "PHYSICAL", conditioning = "survival")
expected_loss(0.02, 0.4, 100000, context = ctx)$unit
}
