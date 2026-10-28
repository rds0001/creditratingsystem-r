\name{rating_system}
\alias{build_rating_subjects}
\alias{route_rating_profile}
\alias{assess_rating_applicability}
\alias{register_rating_model_version}
\alias{select_effective_model}
\alias{compose_risk_assessment}
\alias{check_assessment_freshness}
\alias{transition_assessment_state}
\alias{rate_subject_batch}
\alias{apply_assessment_event}
\alias{replay_assessment_history}
\title{Dated local ratings, immutable model versions and historical replay}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Build typed rating subjects, route profiles, assess evidence, select
explicit historical model versions, preserve risk quantities and trace local
lifecycle events. Batch inference and replay use frozen declarative recipes.}
\usage{
build_rating_subjects(tables, links, as_of, known_at)
route_rating_profile(snapshot, rules, as_of, known_at)
assess_rating_applicability(snapshot, contract, as_of, known_at)
register_rating_model_version(registry, bundle, registration)
select_effective_model(registry, assignments, profile_id, purpose,
    as_of, known_at)
compose_risk_assessment(assessment_id, subject, components, binding,
    as_of,
                        known_at, calculated_at, effects = NULL)
check_assessment_freshness(assessment, policy, events, as_of,
    known_at)
transition_assessment_state(assessment, event, transitions)
rate_subject_batch(requests, registry, batch_id, known_at)
apply_assessment_event(state, event, dependencies, known_at)
replay_assessment_history(requests, registry, expected, known_at,
                          replay_id = "replay")
}
\arguments{
\item{tables}{Named tables keyed by customer, obligor, facility, issue, transaction
or portfolio. Rows: subject_id, version, attributes object, effective_from,
effective_to (NULL allowed), known_at. Subject type comes from table name.}
\item{links}{Rows: link_id, relation, from_type, from_id, to_type, to_id,
effective_from, effective_to, known_at.}
\item{as_of}{ISO YYYY-MM-DD economic observation date.}
\item{known_at}{ISO knowledge cutoff, no earlier than as_of where both are used.}
\item{snapshot}{Named list with subject_id, subject_type, as_of, known_at,
attributes. Each attribute is a named list containing value (possibly NULL),
observed_at, known_at and optional conflict flag.}
\item{rules}{Rows: rule_id, profile_id, profile_version, numeric priority,
conditions list and effective/knowledge dates. Conditions have field, operator,
and value except for present/absent operators.}
\item{contract}{Named applicability contract: contract_id, version,
required_features list, allowed_values object, support_bounds object,
maximum_age_days object.}
\item{registry}{Hashed registry returned by register_rating_model_version,
or NULL to start empty. A CalcResult wrapper is accepted.}
\item{bundle}{Named list: model (frozen ModelArtifact), dependencies object,
configuration object. The entire bundle is content-hashed.}
\item{registration}{Named list with matching model_id, model_version,
registered_at and optional data-only metadata.}
\item{assignments}{Rows with assignment_id, profile_id, purpose, model_id,
model_version, model_hash, effective_from, effective_to, known_at.}
\item{profile_id, purpose}{Explicit profile and calculation purpose strings.}
\item{assessment_id}{Unique string for the supplied assessment.}
\item{subject}{Named list containing subject_id and subject_type.}
\item{components}{Rows: component_id, quantity, value, subject_id, subject_type,
variant, definition. EAD/EL require currency; GRADE requires scale_id,
scale_version and graded_quantity. A different subject needs relationship_id.}
\item{binding}{Named list of nonempty strings: profile_id, profile_version,
model_id, model_version, model_hash, input_hash.}
\item{calculated_at}{ISO calculation date, not before knowledge cutoff.}
\item{effects}{Optional rows: effect_id, kind, order, level, input_component_id,
output_component_id, reason.}
\item{assessment}{Hashed composed assessment or its CalcResult wrapper.}
\item{policy}{Freshness policy: maximum_age_days, maximum_information_age_days,
trigger_types list and optional effective_to.}
\item{events}{Freshness event rows: event_id, subject_id, type, occurred_at,
known_at, optionally supersedes_assessment_id.}
\item{event}{Lifecycle event: event_id, type, occurred_at, known_at, reason,
gates object. Projection event: event_id, revision string, occurred_at, known_at,
dependencies list of type/id objects, payload, optional corrects_revision.}
\item{transitions}{Rows: from_state, to_state, event_type, required_gates list.}
\item{requests}{Rows with row_id, known_at, model_id, model_version, model_hash,
recipe and input; see Details.}
\item{batch_id, replay_id}{Explicit deterministic run identifier.}
\item{state}{Hashed event state, or NULL to start empty.}
\item{dependencies}{Rows with assessment_id, dependency_type, dependency_id.}
\item{expected}{Prior successful batch rows or rows with row_id, input_hash,
recipe_hash, model_hash, result_hash for exact replay comparison.}
}
\value{CalcResult with named-list value. Objects and row arrays are data only.
Registry, assessment, batch and event state have canonical SHA-256 content hashes.
Outputs are detached R values; consumers reject mutated hashes.}
\details{
All schemas use version 1.0. Dates have calendar-day resolution. Effective intervals
are [effective_from,effective_to); NULL end means unbounded. Knowledge cutoff is
inclusive. Use named lists for objects, unnamed lists for arrays including singletons,
and structure(list(), names=character()) for empty objects. Row tables also accept
data.frame. Hashes detect changed content; they do not authenticate an author.

Subject types remain distinct. An overlapping effective version creates an explicit
conflict and is excluded from the resolved graph. Relationships are legal_obligor,
joint_liability, economic_dependency, default_correlation, data_identity, contains,
pd_reference. Unresolved link endpoints are reported. No PD, grade or loss is copied
between subject levels.

Routing requires matching snapshot economic date and snapshot knowledge no later
than the query. Attribute observation cannot follow its knowledge date. Future
observations and facts learned after snapshot cutoff are unavailable. Conditions
combine with AND. Operators are eq, ne, lt, le, gt, ge, in, not_in, present, absent.
Missing values fail all comparisons except absent; ordered comparisons are finite
numeric. Membership values are arrays. Empty conditions explicitly match all.
Smallest matching priority wins. Equal best priority yields AMBIGUOUS, no match
NO_ROUTE, unique match ROUTED with complete trace. No expression evaluation occurs.

Applicability distinguishes missing, unavailable, stale, conflicting and outside
training support facts. These yield REFER. A known disallowed applicability field
yields NOT_APPLICABLE; otherwise APPLICABLE. Support bounds are inclusive marginal
ranges, ages are nonnegative integer days, and required feature names are distinct.
These statuses are neither default labels nor lending decisions.

Registry versions are immutable content-bound entries, sorted by model ID/version.
Model training knowledge must precede registration. Identical bundle and registration
are idempotent; a differing bundle or registration under the same model ID/version
fails. Selection requires one effective known assignment and the exact pinned bundle
hash registered by historical cutoff. It reports SELECTED, MISSING_MODEL,
AMBIGUOUS_MODEL, NOT_YET_EFFECTIVE or EXPIRED_MODEL. Later registered models do not
replace an explicit historical version. Dependency/configuration maps participate
in the bundle hash and are supplied by the caller.

Composition retains PD, GRADE, LGD, EAD, EL and SCORE as separate quantities and
variants. Every definition includes event, positive horizon, time_unit
(days/months/years), probability_measure (PHYSICAL/RISK_NEUTRAL), conditioning.
PD lies in [0,1]; EAD is nonnegative. LGD and EL are finite and may reflect net gains.
Grades name their scale and graded quantity (PD, EL or SCORE). Definition differences
within subject/variant, duplicate quantities, unlinked other subjects and mixed
currencies are explicit composition conflicts; no automatic inference or aggregation
occurs. Effects have kind support/transfer/override/cashflow and unique integer order.
Input/output component references must exist, output level must match, and producer
order must precede consumer order. Repeated output writes and cycles fail. Caller
must identify the same economic effect consistently to avoid hidden double counting.

Composed assessments start DRAFT with history. Freshness checks age, information
age, effective expiry and known new material events, including late corrections
of older facts. Equality to an age limit remains current. Status is CURRENT, STALE
or SUPERSEDED. Historical queries cannot use assessment lifecycle state from a later
knowledge date.

Lifecycle state names are DRAFT, SUBMITTED, REVIEWED, FINAL, SUPERSEDED, with explicit
caller-supplied transitions. Exactly one transition must match, all required gates
must be TRUE, reasons nonempty, and dates/history ordered. Duplicate events fail.
Conflicted assessments cannot finalize. FINAL only permits a declared move to
SUPERSEDED, which is terminal. Append-only history includes previous assessment hash,
old/new state and full event; earlier input objects remain unchanged. FINAL is local
processing status and conveys no institutional approval.

Batch requests pin model identity and bundle hash. A recipe has method, version 1.0
and parameters. The application_risk method accepts input=list(snapshot=...) and
parameters target plus optional calibrator/offset. Its snapshot cutoff must not
exceed request knowledge. The binary_glm method accepts input X (row matrix or row
lists) and feature_order; parameters expected_target plus optional offset. This
finite dispatcher executes supported library inference only; no user code, callbacks
or executable serialization. Model registration must be available by each individual request cutoff, and request
knowledge must be available by the batch cutoff. Each unique string row_id retains success or a controlled
error and missing value; sorted row order is input-permutation invariant. Hashes bind
input, recipe, model, numeric result, sorted request collection and registry.

Event projection matches explicit dependency type/ID pairs and returns sorted unique
affected assessment IDs without recalculating or mutating risk/accounts. Same
ID/revision/content is DUPLICATE with unchanged state; differing content under that
key fails. Corrections have distinct revisions referencing the latest processed
revision and retain original history. Future events are DEFERRED; state containing
future processed events is rejected for a historical cutoff.

Replay executes supplied pinned historical requests and compares input/recipe/model/
result hashes. Missing bundles or references and changed hashes yield
NOT_REPRODUCIBLE with findings; complete agreement yields REPRODUCED. Numerical
hashes are exact, so numerical runtime differences remain visible. No current
champion or external source is loaded. Reproducibility is not empirical validation.
}
\section{Content-bound assessment pipelines}{
The unchanged batch signature additionally accepts recipe method
\code{assessment_pipeline}, version \code{2.0}. Its identical data-only recipe
must be stored in the registered bundle's \code{configuration$assessment_recipe}.
Dependencies are ID-keyed \code{artifacts}, each with kind, content, canonical
content sha256 and known_at; kind \code{innovations} identifies supplied random
inputs. Empirical fitting consumes frozen method outputs; numerical market pricing
and explicitly driven simulations execute their native granular methods.

Input contains snapshot, ID-keyed datasets (value/as_of/known_at), assessment_id,
assessment_known_at, calculated_at, lifecycle_events and dependency events.
Snapshot/data/model/artifact information must be available at assessment_known_at.
Dataset economic dates cannot follow the subject snapshot. Calculation lies between
assessment knowledge and the historical request cutoff; lifecycle events cannot
exceed that request cutoff. Later review history never changes the original input
information set.

Recipe parameters explicitly contain routing_rules, applicability_contract, steps,
components, effects, transitions, event_dependencies and nullable decision.
Exactly one applicable route is required before execution. Steps contain step_id,
method_id, arguments, argument_types and output_contract. References have the exact
object form list(`$ref`=list(source="step", id="pd", path=list("value",0))).
Sources input/artifact/model/step resolve dataset envelopes, frozen content, primary
model under ID primary, or predecessor outputs. Both languages use zero-based array
indices. Ready DAG steps run in sorted ID order; unknown predecessors and cycles
fail. No user code or user-selected imports are evaluated.

Argument types explicitly select numeric_matrix, numeric_vector, character_vector
or logical_vector conversion. Other arguments retain their typed list structure.
Output contracts require shape (scalar/vector/matrix/object/records), unit and
nullable target. Optional fields contain path, unit, target and nullable target_path;
the latter binds an actual nested target, including scenario sampling law.
Component references must match producing quantity/currency and full target.
Physical and risk-neutral measures and PIT/TTC sampling laws are never inferred or
interchanged. A PRE_FITTED empirical step requires exactly one artifact argument
referencing a frozen output with the same method_id. Registry, lifecycle and recursive
batch operations use dedicated stages, not arbitrary DAG calls.

The Shapley operation accepts a data-only predictor with kind FROZEN_PD, model,
feature_order and expected_target. A trusted internal generic-PD call is constructed;
model identity and probability scale must match its explanation contract.

Composition is followed by declared lifecycle transitions and revisioned dependency
event projection. Repeated identical lifecycle events are idempotent; different
content under the same event identity fails. A dependency correction retains prior
history. Projection does not silently change risk; recomputation uses a new dated
request. Optional decision arguments invoke the frozen policy separately, with PD
and target equal to a composed PD component.

Version-2 rows include assessment_hash, event_state_hash, artifact_manifest_hash,
innovation_hash (manifest entries tagged innovations) and nullable decision_hash,
in addition to legacy input/recipe/model/result hashes. Expected replay rows must
contain these fields. Full calculation, composition and ordered history are rerun;
corrupt/missing artifacts produce NOT_REPRODUCIBLE findings. Successful values
retain routing/applicability, ordered step argument/value hashes, final assessment,
artifact manifest, event state/trace and separate decision. Numerical hashes are
exact; cross-runtime last-bit differences remain visible. Hashes establish content
integrity, not signature authenticity, empirical validity or institutional approval.
}
\examples{
empty <- structure(list(), names = character())
s <- list(subject_id = "O", subject_type = "obligor", as_of = "2024-01-01",
          known_at = "2024-01-01", attributes = empty)
r <- list(list(rule_id = "R", profile_id = "P", profile_version = "1",
               priority = 1, conditions = list(), effective_from = "2024-01-01",
               effective_to = NULL, known_at = "2023-12-01"))
route_rating_profile(s, r, "2024-01-01", "2024-01-01")$value$status
}
