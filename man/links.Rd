\name{credit_links}
\alias{build_entity_graph}
\alias{resolve_parent_links}
\alias{assign_default_groups}
\alias{apply_parent_support}
\alias{allocate_guarantee_coverage}
\alias{apply_guarantee_risk}
\alias{apply_rating_constraints}
\alias{apply_override}
\alias{solve_contagion_network}
\title{Dated relationships and explicit risk adjustment contracts}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Granular native methods for typed relationship graphs, conditional support,
shared guarantee caps, rating constraints, local override audits and static financial clearing.}
\usage{
build_entity_graph(entities, edges, relationship_policy, as_of,
    known_at,
  context = NULL)
resolve_parent_links(entity_graph, control_policy, context = NULL)
assign_default_groups(entity_graph, default_link_policy,
    facility_mappings,
  context = NULL)
apply_parent_support(standalone_risk, parent_risk, support_contract,
  joint_default_probability, context = NULL)
allocate_guarantee_coverage(guarantees, claims, allocation_policy,
    as_of,
  known_at, currency, context = NULL)
apply_guarantee_risk(borrower_risk, guarantor_risk,
    coverage_allocation,
  claim_id, loss_contract, joint_default_probability,
    context = NULL)
apply_rating_constraints(base_result, rating_scale, constraints,
  representative_pd_policy, context = NULL)
apply_override(base_result, override_request, policy, as_of,
    known_at,
  context = NULL)
solve_contagion_network(liability_matrix, external_assets,
    external_liabilities,
  entity_ids, currency, model_assumptions, asset_scenarios = NULL,
  tolerance = 1e-8, max_iterations = 10000, context = NULL)
}
\arguments{
\item{entities, edges}{Record lists with unique entity/edge IDs. Edges contain source/target,
type, validity dates, known-at and evidence reference; ownership also has a fraction.}
\item{relationship_policy}{Versioned policy with permitted edge types.}
\item{as_of, known_at}{ISO observation and information-cutoff dates.}
\item{context}{Optional compatible calculation context.}
\item{entity_graph}{Content-bound graph result or its value.}
\item{control_policy}{Versioned control edge types, ownership threshold and inclusive flag.}
\item{default_link_policy}{Versioned edge types, mutual-only or explicit-undirected direction,
and Boolean transitivity. Nontransitive overlapping non-cliques remain unresolved.}
\item{facility_mappings}{Records with facility ID and entity ID.}
\item{standalone_risk, parent_risk, borrower_risk, guarantor_risk}{Risk records with entity ID,
PD, stage, explicit default target and applied effect IDs.}
\item{support_contract}{Effect, assumption reference, currency, total/reserved/allocated
capacity, required rescue amount and explicit parent-alive/joint-default state terms.}
\item{joint_default_probability}{Explicit event intersection within Frechet bounds.}
\item{guarantees}{Nominal non-reinstating cap, used cap, covered claims, coverage fraction,
guarantor, currency, validity, knowledge, legal-effectiveness and evidence records.}
\item{claims}{Known claim ID, entity ID, amount, currency and future due date records.}
\item{allocation_policy}{Versioned complete guarantee/claim priority orders and
\code{cap_convention='non_reinstating_total'}.}
\item{currency}{Reporting currency shared by all amounts; no implicit FX conversion.}
\item{coverage_allocation}{Sealed allocated guarantee artifact.}
\item{claim_id}{Claim belonging to the supplied borrower.}
\item{loss_contract}{New effect ID, currency, assumption reference,
\code{stage='PRE_GUARANTEE'}, valuation date equal to claim due date,
and borrower-only/joint-default losses, payout fractions and evidence.}
\item{base_result}{PD/grade record for constraints; named original or sealed prior
artifact for overrides.}
\item{rating_scale}{Versioned validated scale ordered by increasing risk.}
\item{constraints}{Ordered unique rules with evidence: grade bounds (best/worst allowed)
or notch bounds (reference grade, maximum improvement/deterioration).}
\item{representative_pd_policy}{\code{preserve_pd} or \code{representative_on_binding}.}
\item{override_request}{ID, target, value, reason, author, approver, recorded-at and validity.}
\item{policy}{Versioned override target paths with probability, allowed-grade or bounded
numeric domains and Boolean dual-control requirement.}
\item{liability_matrix}{Nonnegative matrix with debtor rows, creditor columns and zero diagonal.}
\item{external_assets, external_liabilities}{Aligned nonnegative external amounts.}
\item{entity_ids}{Distinct IDs in matrix order.}
\item{model_assumptions}{Exactly: model eisenberg_noe, seniority single_pari_passu,
bankruptcy_costs none, outside_creditor sink.}
\item{asset_scenarios}{Optional unique scenario IDs and alternative external asset vectors.}
\item{tolerance}{Node-relative residual/default tolerance between 1e-12 and 1e-4.}
\item{max_iterations}{Monotone lower-iterate cap, integer between 1 and 1000000.}
}
\details{
Graph intervals are left closed and right open. Parent ambiguity and cycles remain
unresolved; ownership never establishes common PD or support by itself. Support
integrates conditional rescue in parent-alive and joint-default states, requiring
funding sufficient for the whole stated rescue need. Its output is before transfer.

Guarantee allocation consumes each shared cap once across the supplied claims.
Payment protection preserves borrower PD and reduces conditional monetary loss by
min(covered amount, positive loss) times supplied conditional payout fraction.
Previously guarantee-adjusted inputs are rejected even under a new effect ID.
Several guarantors require a joint event model. Nominal cap and loss use the claim
due-date valuation basis; discount state losses separately.

Rating rules are intersected and inconsistent bounds fail. Nonbinding constraints
preserve continuous PD exactly. Grade-only policy exposes any PD/grade inconsistency;
representative policy changes PD only on binding. Override history preserves original
values and content hashes, rejects duplicate IDs, and records expired requests without
applying. A changed evaluation cutoff requires replay from the original; copying an
overridden current value is not an unadjusted baseline. Only the requested field changes.

Clearing uses limited liability, absolute priority and one pro-rata debt class.
Native lpSolve maximizes payment fractions to select greatest Eisenberg--Noe clearing.
A monotone lower iteration and greatest/lower gap diagnose numerical enclosure or
multiple clearing points; a small iterate change alone does not prove uniqueness.
Outside creditors do not recycle payments. Extreme amount dynamic ranges below
1e-10 of the largest amount fail explicitly. This is a static financial network,
not a group PD estimator, dynamic insolvency model or multi-seniority waterfall.
}
\value{A CalcResult containing the graph, mapping, adjusted risk, immutable local
artifact or clearing payments and sensitivity, with diagnostics and limitations.}
\references{Eisenberg, L. and Noe, T. H. (2001). Systemic Risk in Financial Systems.
Management Science 47(2), 236--249. \doi{10.1287/mnsc.47.2.236.9835}.}
\examples{
a <- list(model="eisenberg_noe", seniority="single_pari_passu",
          bankruptcy_costs="none", outside_creditor="sink")
solve_contagion_network(matrix(c(0,50,50,0),2), c(20,10), c(50,50),
  c("A","B"), "EUR", a)$value$base$clearing_payments
}
