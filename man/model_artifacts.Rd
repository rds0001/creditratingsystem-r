\name{serialize_fitted_model}
\alias{serialize_fitted_model}
\alias{load_fitted_model}
\title{Exchange Versioned Local Model Artifacts}
\usage{
serialize_fitted_model(fitted_model, destination,
    schema_version = "1.0")
load_fitted_model(manifest_path, allowed_schema_versions = "1.0")
}
\arguments{
\item{fitted_model}{A model named list, or a \code{CalcResult} containing it. Required fields are \code{schema_version}, \code{model_id}, \code{model_version}, \code{method_id}, \code{feature_order}, \code{parameters}, \code{preprocessing}, \code{target}, \code{training}, \code{diagnostics} and \code{provenance}.}
\item{destination}{Explicit local UTF-8 JSON file path. Existing files are never overwritten.}
\item{schema_version}{Supported envelope version, currently \code{"1.0"}.}
\item{manifest_path}{Explicit local model JSON path. Network URLs and UNC paths are rejected.}
\item{allowed_schema_versions}{Allowed envelope versions; unsupported versions are always rejected.}}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Writes and reads non-executable language-neutral model metadata and numeric parameters with canonical SHA-256 integrity verification.}
\details{Features must have distinct nonempty names in prediction order. Parameters, preprocessing, target, training, diagnostics and provenance are named JSON objects. Target includes event, positive horizon, time unit, probability measure and conditioning. Training includes data fingerprint, split identifier and knowledge date. Provenance includes data and parameter origin. Empty objects use \code{structure(list(), names = character())}; explicit arrays use unnamed lists. All resources are inline. Numeric output uses seventeen significant decimal digits to preserve binary64 values exactly across the JSON exchange. Integral JSON literals outside the exact binary64 integer range are rejected on loading; large floating-point quantities use decimal or exponent notation. The loader rejects duplicate keys, malformed schemas, non-finite numbers and mismatched hashes. A hash establishes integrity, not authenticated authorship or empirical validation. Method-specific prediction functions remain responsible for validating parameter meaning and method compatibility. No RDS, external resources or executable payloads are evaluated.}
\value{A \code{CalcResult} containing path, hash and schema version on writing, or a validated model named list on loading.}
