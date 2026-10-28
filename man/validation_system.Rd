\name{validate_system_coverage}
\alias{validate_system_coverage}
\alias{validation_report}
\alias{release_candidate_assessment}
\title{Assess Explicit System and Release Evidence}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Read-only assessment of five-axis coverage, predeclared validation criteria and exact-candidate installed execution evidence.}
\usage{
validate_system_coverage(cells, function_inventory, journeys,
    evidence,
  scope, context = NULL)
validation_report(results, acceptance_policy, manifests,
    context = NULL)
release_candidate_assessment(candidate, evidence, requirements,
  context = NULL)
}
\arguments{
\item{cells}{Unique explicit five-axis applicability cells with contract, function, journey and evidence links.}
\item{function_inventory}{Function identifiers, implementation statuses and native language coverage.}
\item{journeys}{Journey identifiers and covered function identifiers.}
\item{evidence}{Referenced coverage evidence or exact-candidate installed run evidence.}
\item{scope}{Versioned customer-segment, product, lifecycle, method and granularity levels with explicit required cell IDs.}
\item{results}{Named-by-ID ValidationResult records with origin, reference and model/data/split manifest bindings.}
\item{acceptance_policy}{Predeclared required results and separate LE, GE, EQ or BETWEEN scalar acceptance criteria.}
\item{manifests}{Hashed model, data and split manifests and evaluation start date.}
\item{candidate}{Exact candidate ID, wheel/sdist/native-R SHA-256 hashes and resource hashes.}
\item{requirements}{Required cells, journeys, generators, profiles, resources and positive runtime/memory budgets.}
\item{context}{Optional calculation context.}
}
\details{Coverage does not infer a Cartesian product. Required scope cells cannot be waived by changing an applicability record. Missing negative routing, native R or independent-oracle evidence is a visible gap. Non-applicability requires a versioned justified decision and cannot mean missing implementation.

Report policies must precede evaluation. Missing or non-applicable required results and undefined metrics remain blocking. P-values are not combined; synthetic evidence is separately tagged.

Release gates bind every record to the exact artifact and resource manifest. Mandatory installed runs must be executed, offline and isolated from the source tree, separately for Python wheel, Python source distribution and native R. Missing, failed or non-applicable mandatory evidence cannot pass. Measured runtime and memory must satisfy declared budgets. The assessor evaluates supplied evidence without executing tests or publishing.}
\value{A CalcResult with a ValidationResult, individual findings, explicit open issues, fingerprints and a PASS or BLOCKED decision confined to the declared evidence contract. Empirical performance and regulatory approval are not granted.}
