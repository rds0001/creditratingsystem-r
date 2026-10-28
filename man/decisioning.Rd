\name{compile_credit_decision_policy}
\alias{compile_credit_decision_policy}
\alias{apply_credit_decision_policy}
\alias{decompose_policy_decision}
\title{Compile, evaluate and explain local credit decision policies}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Deterministic rules keep probability estimates, affordability,
eligibility and operational information separate. Evaluation returns a local
recommendation with fingerprints and a complete rule trace.}
\usage{
compile_credit_decision_policy(specification, context = NULL)
apply_credit_decision_policy(policy, risk, affordability,
    eligibility,
  operations, as_of, known_at, context = NULL)
decompose_policy_decision(decision, reason_dictionary,
    context = NULL)
}
\arguments{
\item{specification}{Named policy list with schema version, identifier, version,
approval reference, knowledge and effective dates, physical risk target,
typed source fields, explicit default and missing actions, and rule array.}
\item{policy}{Compiled policy result or its fingerprinted value.}
\item{risk}{Named list with risk target, freshness, observation and knowledge
dates and risk fields required by the policy.}
\item{affordability}{Separate named list of dated affordability evidence and status.}
\item{eligibility}{Separate named list of dated eligibility evidence and status.}
\item{operations}{Separate named list of dated operational evidence and status.}
\item{as_of}{Decision date in ISO YYYY-MM-DD format.}
\item{known_at}{Knowledge cutoff, not before the decision date.}
\item{decision}{Previously evaluated decision result or its fingerprinted value.}
\item{reason_dictionary}{Named list mapping every matched rule's reason code to
a supplied explanation. Unmatched rules need no description.}
\item{context}{Optional calculation context.}
}
\details{
Rules have a unique rule_id, nonnegative integer priority, action, reason_code
and a list of conjunctive predicates. Lower priorities execute first; equal
priorities sort by rule identifier. Predicates name a declared source field and
use eq, ne, lt, le, gt or ge. Categories allow equality operators only. Numeric
thresholds are finite binary64 values. Contradictory rules and overlapping
equal-priority rules with different actions are rejected. Lower-priority
overlap and shadowing are reported as diagnostics.

The policy effective interval includes its start and excludes its end.
Missing, future, stale or incompatible information takes the explicit missing
action before the rule result. Under specification 1.0, approval also requires AFFORDABLE, ELIGIBLE and
CLEAR statuses from the three separate nonrisk sources; otherwise it becomes
REFER. Gate evaluation never alters a probability. Default action cannot be
APPROVE. Approval references record caller assertions and do not establish
institutional authorization. No credit, payment, limit or price is executed.

Specification 1.1 requires approval_contract with exactly domain,
affordability and required_gates. Domains are RETAIL_CREDIT,
WHOLESALE_CREDIT, INVESTMENT and COUNTERPARTY_LIMIT. Affordability mode
REQUIRED preserves AFFORDABLE. Only investment/counterparty domains permit
NOT_APPLICABLE, with nonempty reason and reference and nonnegative whole-day
maximum_age_days. The dated runtime affordability object must then declare
NOT_APPLICABLE, the same domain, matching applicability_reference and an
exclusive valid_until date. The age limit is inclusive. Missing, stale,
future-known, expired or differently bound evidence prevents approval.

Additional gates are distinct named lists with field and value, referencing
an explicitly declared categorical field and one allowed category. All gates
must pass together. Mandatory ELIGIBLE, CLEAR, current risk and target checks
remain independent of rule priority. Failed approval status gates yield REFER.
The decision adds approval_domain and approval_gate_trace. Empty gate arrays
are permitted but do not waive mandatory gates. Contract domains and legal
applicability conclusions are supplied assertions, not inferred approvals.

Specification 1.0 rejects approval_contract, retains its existing hashes and
behavior, and cannot silently ignore the new semantics. Artifact envelopes
remain schema 1.0; the nested specification carries version 1.1.

Array fields, including category values, rules and predicate lists, must be
unnamed lists even for one or zero elements. This preserves canonical JSON
array identity across native R and Python. Objects must be named lists.
Fingerprints detect content changes; they are not digital signatures.
}
\value{A CalcResult. Compilation returns a DecisionPolicy artifact; application
returns a DecisionArtifact containing action, findings, rule trace, input and
policy hashes. Decomposition returns recorded matched reasons and identifies
which rule was selected, without recalculating a decision.}
