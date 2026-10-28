\name{retail_features}
\alias{validate_retail_dataset}
\alias{build_customer_household_graph}
\alias{aggregate_household_obligations}
\title{Point-in-Time Retail Records and Household Obligations}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Validate explicit local retail schemas, freeze typed dated relationships, and separate unique household obligations, budget allocations and person-level legal liability.}
\usage{
validate_retail_dataset(tables, data_contract, as_of,
  information_cutoff, context = NULL)

build_customer_household_graph(entities, relationships, as_of,
  information_cutoff, context = NULL)

aggregate_household_obligations(obligations, household_graph,
  coverage, currency, period, guarantee_budget_policy,
    context = NULL)
}
\arguments{
\item{tables}{Named list of local data frames or row lists.}
\item{data_contract}{Version1.0 object with contract_id, version, table_specs, prohibited_feature_fields and source_coverage. See Details.}
\item{as_of, information_cutoff}{ISO economic and information dates. Information cutoff cannot exceed as_of.}
\item{context}{Optional compatible calculation context. Core known_at and information_cutoff aliases, when supplied, must agree with the cutoff.}
\item{entities}{Rows with unique entity_id, entity_type and known_at. Types are CUSTOMER, HOUSEHOLD, APPLICATION, FACILITY and BUSINESS.}
\item{relationships}{Dated release rows with relationship_id, positive integer release_seq, source_id, target_id, role, effective_from, nullable effective_until, known_at, nullable liability_fraction, nullable budget_share, and evidence_reference.}
\item{obligations}{Snapshots with facility_id, release_seq, observed_at, known_at, source_id, debt_stock, scheduled_debt_service, currency, period and status. Amounts are nonnegative or explicitly NULL; status is ACTIVE, CLOSED or UNKNOWN.}
\item{household_graph}{Content-bound graph CalcResult or its artifact value.}
\item{coverage}{Object with status COMPLETE/PARTIAL/UNKNOWN, named sources and known_at. Complete coverage requires at least one source.}
\item{currency, period}{Exact reporting currency and debt-service period. No automatic FX or frequency conversion.}
\item{guarantee_budget_policy}{Explicit exclude or include_declared_share. Guarantees never automatically become current debt service.}
}
\details{
Each table specification has table, role, primary_key, fields, foreign_keys,
date_order, event_field, known_field, feature_fields and allow_extra_fields.
Roles are IDENTITY, FEATURE, OUTCOME or DECISION. Fields declare name, type
(string/date/number/integer/boolean) and boolean nullable. Foreign keys declare
field, table and target_field. Date-order pairs declare earlier and later fields.
Keys and features are explicit arrays; use lists for JSON-style arrays, including
single-element arrays. Event and known fields must have date types.
Source coverage rows declare source_id, status and known_at. Future coverage
assessments appear UNKNOWN. Eligible children must reference cutoff-eligible
parents. Repeated applications remain separate when application IDs differ.
Violations use zero-based row indices in both languages.

Only FEATURE tables can designate model inputs. User-prohibited fields and
common direct outcome names cannot be designated features. This contract does
not infer the meaning of arbitrary derived variables or establish their lending
suitability. Self-referencing payment foreign keys can preserve reversal identity;
economic allocation remains separate.

Relationships run from customers to households (MEMBER_OF), applications
(COAPPLICANT_ON), facilities (BORROWER_ON or GUARANTOR_ON), or businesses
(OWNER_OF). The latest cutoff-known release ordered by known_at and release_seq
is selected before filtering its half-open effective interval. Identity is never
inferred from address or name. Duplicate active customer/target/role edges and
multiple active economic households per customer are rejected.

Debt fractions belong only to borrower/guarantor roles and lie in [0,1] or are
NULL. Internal borrower budget shares cannot sum above one per facility. Legal
fractions can each equal one for joint borrowers. Household economic membership
is not an automatic legal default unit, guarantee or owner-PD transfer.

Aggregation selects the latest known obligation snapshot per facility and counts
each linked facility once per household. Legal liability and internal budget
allocation remain distinct. Closed facilities require explicit zero residual
amounts. Missing snapshots, amounts or statuses and partial/unknown coverage
leave totals NULL even if known subtotals equal zero. Missing budget shares
separately prevent complete budget totals. Snapshot age, release and source
remain visible. Content bindings prevent silent modification of a supplied graph.
}
\value{A CalcResult. Validation returns valid, applicability, violations, row counts, eligible row indices and source coverage. Graph construction returns a dated, content-bound typed graph, memberships and liability paths. Aggregation returns household known subtotals and nullable totals, budget allocations, individual legal liabilities and source coverage.}
\examples{
entities <- list(
  list(entity_id = "person", entity_type = "CUSTOMER", known_at = "2024-01-01"),
  list(entity_id = "house", entity_type = "HOUSEHOLD", known_at = "2024-01-01"))
relationships <- list(list(relationship_id = "membership", release_seq = 1,
  source_id = "person", target_id = "house", role = "MEMBER_OF",
  effective_from = "2024-01-01", effective_until = NULL,
  known_at = "2024-01-01", liability_fraction = NULL, budget_share = NULL,
  evidence_reference = "verified household membership"))
graph <- build_customer_household_graph(entities, relationships,
  "2024-01-31", "2024-01-31")
graph$value$memberships
}
\seealso{\code{compute_days_past_due}, \code{join_as_of}}
