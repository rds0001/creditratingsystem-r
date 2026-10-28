\name{retail_evidence}
\alias{build_bureau_features}
\alias{build_thin_file_features}
\alias{reconcile_income_evidence}
\title{Dated Bureau, Thin-File and Income Evidence}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Describe local credit files, evidence availability and reconciled income without inferring zero debt from missing sources or summing the same salary across sources.}
\usage{
build_bureau_features(snapshots, tradelines, inquiries,
    provider_mapping,
  customer_id, provider_id, as_of, information_cutoff,
    lookback_days,
  max_age_days, currency, context = NULL)
build_thin_file_features(bureau_features, evidence,
    eligibility_profile,
  customer_id, as_of, information_cutoff, context = NULL)
reconcile_income_evidence(declarations, documentary_evidence,
  transaction_evidence, inclusion_policy, customer_id, as_of,
  information_cutoff, currency, period, basis, context = NULL)
}
\arguments{
\item{snapshots}{Rows: snapshot_id, release_seq, customer_id, provider_id, observed_at, reported_at, known_at, coverage_status (COMPLETE/PARTIAL/NO_HIT), currency. Observation precedes reporting and receipt.}
\item{tradelines}{Rows: snapshot_id, snapshot_release_seq, tradeline_id, opened_at, nullable closed_at, status (OPEN/CLOSED/UNKNOWN), nullable balance, limit and integer max_dpd. Amounts and DPD are nonnegative.}
\item{inquiries}{Rows: inquiry_id, release_seq, customer_id, provider_id, occurred_at, known_at, kind (HARD/SOFT/UNKNOWN).}
\item{provider_mapping}{Dated mapping_id, version, known_at and canonical-to-source field maps snapshots, tradelines, inquiries. Empty maps use identity; nullable fields must be explicit.}
\item{customer_id, provider_id}{Explicit local identities.}
\item{as_of, information_cutoff}{ISO economic and knowledge dates; cutoff cannot exceed as_of.}
\item{lookback_days, max_age_days}{Positive inquiry lookback and nonnegative maximum bureau age, in calendar days.}
\item{currency, period, basis}{Exact monetary currency, income period and NET/GROSS basis. No automatic conversion.}
\item{context}{Optional compatible dated calculation context.}
\item{bureau_features}{Matching customer/date bureau CalcResult or value.}
\item{evidence}{Rows: evidence_id, release_seq, known_at, customer_id, evidence_type, observed_from, observed_until, coverage_status (COMPLETE/PARTIAL/UNKNOWN). BUREAU is reserved for bureau_features.}
\item{eligibility_profile}{Dated profile_id, version, known_at, minimum_tradelines, minimum_history_days, maximum_evidence_age_days, logical allow_partial_evidence and feature_requirements mapping names to lists of required evidence kinds.}
\item{declarations, documentary_evidence, transaction_evidence}{Rows: globally unique evidence_id, release_seq, known_at, customer_id, component_id, component_type, observed_from, observed_until, nonnegative amount, basis, period, currency, evidence_reference. Revisions retain evidence identity.}
\item{inclusion_policy}{Dated policy_id, version, known_at, source_priority (each DECLARED/DOCUMENT/TRANSACTION once), included_component_types, expected_component_ids, coverage_status, maximum_age_days, logical allow_stale, nonnegative relative_discrepancy_threshold.}
}
\details{
Latest knowledge-visible revisions are selected before economic filters. Bureau
tradelines bind both snapshot_id and snapshot_release_seq; late corrections cannot
rewrite earlier evidence. Inquiry counts use the interval
(as_of-lookback_days, as_of] and supplied local history. Closed balances remain
separate from open balances. Missing values and partial coverage prevent complete
open totals; zero limits give NULL utilization. Absent and no-hit files never
establish zero household debt. Maximum reported DPD describes the supplied
tradeline history rather than an invented lookback maximum. No provider score is
reproduced.

Thin-file strata describe evidence, not default classes. Each feature requires
all named evidence kinds with acceptable age and coverage. Supplied thresholds
and partial-evidence policy are explicit. Common direct outcome labels cannot
be designated input features. Use lists for JSON-style arrays, including
single-element arrays.

Income types are SALARY, BONUS, PENSION, RENT_NET, NET_BUSINESS_INCOME and
OTHER_NET_INCOME. Each stable component is selected once from its newest eligible
source evidence according to the declared priority. Salary and bonus need distinct
identities. Business revenue, borrowing, owner transfers and refunds are not
inferred as disposable income. Gross/net, period and currency mismatches are
excluded without guessed conversion. Document support is supplied evidence, not
independent authenticity verification. Source values and discrepancies from the
declaration remain visible; a zero declaration has no relative denominator.
Incomplete coverage or missing expected components leave total income NULL while
retaining the known subtotal.
}
\value{CalcResult with bureau file state, coverage, age and features; evidence strata and supported/unsupported features; or reconciled components, selected subtotals, nullable total and discrepancies. Diagnostics retain source revisions, policy or mapping fingerprints and excluded evidence.}
\seealso{\code{validate_retail_dataset}, \code{aggregate_household_obligations}}
