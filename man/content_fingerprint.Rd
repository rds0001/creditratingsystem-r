\name{content_fingerprint}
\alias{content_fingerprint}
\title{Canonical Content Fingerprint for Local Data Contracts}
\usage{content_fingerprint(value, context = NULL)}
\arguments{
\item{value}{Finite JSON-compatible data. Use named lists for objects, unnamed
lists for arrays and NULL for null. A length-one R atomic vector is a scalar;
wrap it in an unnamed list to express a one-element array.}
\item{context}{Optional calculation context.}
}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Computes the SHA256 canonical identifier used by local frozen
simulation recipes, parameter snapshots and artifact bindings.}
\details{Object keys must be nonempty printable ASCII and are sorted; string
values are UTF-8. Numbers use finite binary64 encoding, with negative zero
normalized to zero; equivalent integer and floating values share a digest.
Booleans, numbers, strings, arrays, objects and null remain distinct. R named
empty lists represent empty objects, while list() is an empty array.
The identifier is not a digital signature, authorship proof, or financial schema
validation. Exclude an artifact's own digest field before hashing its payload.}
\value{A CalcResult whose value is a hexadecimal SHA256 string.}
\examples{
content_fingerprint(list(principal = 100, currency = "EUR"))$value
}
