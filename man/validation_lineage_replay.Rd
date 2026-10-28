\name{validate_source_lineage}
\alias{validate_source_lineage}
\alias{replay_consistency}
\title{Audit Source Lineage and Frozen Historical Replay}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Checks actual row-level source lineage, join allocation and dated use, and compares actual supplied replay checkpoints with immutable original checkpoints.}
\usage{
validate_source_lineage(nodes, edges, joins, reconciliations, uses,
  contract, context = NULL)
replay_consistency(event_log, original_checkpoints,
    replayed_checkpoints,
  snapshot_manifest, replay_contract, context = NULL)
}
\arguments{
\item{nodes}{Source/transform/feature/target/prediction/decision nodes with hashes, schemas, vintages, revisions, dates and actual weighted records.}
\item{edges}{Parent-child lineage edges; the graph must be acyclic.}
\item{joins}{Equality-join mapping ledgers, declared cardinalities, excluded IDs and conservative left-weight/left-amount allocations.}
\item{reconciliations}{Independently expected record identities, total weights and material amounts.}
\item{uses}{Dated uses with purpose, observation/knowledge cutoffs and exact ancestral source bindings.}
\item{contract}{Versioned required node/use/join IDs, separate amount-field currencies and comparison tolerances.}
\item{event_log}{Immutable event deliveries with IDs, dates, sequence, idempotency keys, payload hashes and append-only revision references.}
\item{original_checkpoints}{Frozen original states, dates, applied event IDs and log/snapshot bindings.}
\item{replayed_checkpoints}{Actual replay outputs with identical contracts and explicit REPLAY mode.}
\item{snapshot_manifest}{Frozen model, calibration, policy and source snapshots with hashes, knowledge and effective intervals.}
\item{replay_contract}{Explicit stable ordering, identical-delivery idempotency, append-revision semantics, required checkpoints and numeric tolerances.}
\item{context}{Optional calculation context.}
}
\details{Lineage audits compute totals from supplied rows. Join business-key multiplicities, all matching pairs, unmatched IDs, weights and amounts are checked. Retained left-row allocation fractions sum to one; duplicated full amounts cannot be hidden by a successful technical join. Predictive paths exclude future observations and outcome/post-decision ancestors.

Replay compares actual outputs rather than executing an alternative event engine. Historical event eligibility follows occurrence and knowledge dates. Identical duplicate deliveries are applied once; conflicting duplicates are errors. Later revisions cannot alter earlier knowledge states. Both original and replay applied sequences are checked, together with frozen model/calibration/policy/source snapshots. New-method recomputation is not historical replay. Explicit lists preserve JSON array distinctions in native R.}
\value{A CalcResult with a ValidationResult containing individual violations, totals or state differences, fingerprints and PASS/BLOCKED status for the declared audit contract. Upstream provenance is not authenticated and original decisions are not modified.}
