\name{monitoring}
\alias{assign_model_role}
\alias{rollback_model_assignment}
\alias{classify_monitoring_findings}
\title{Dated Local Model Roles and Monitoring Escalation}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Preserve model-role event history and apply explicit monitoring policies without external deployment or recalibration.}
\usage{
assign_model_role(history, registry, assignment, context = NULL)
rollback_model_assignment(history, registry, target_event_id,
  rollback_event, context = NULL)
classify_monitoring_findings(metric_panel, policy, context = NULL)
}
\arguments{
\item{history}{NULL initially, or a ModelAssignmentHistory value or CalcResult.}
\item{registry}{An immutable local rating model registry.}
\item{assignment}{Named event object with event_id, profile_id, purpose, role, slot_id, model_id, model_version, model_hash, effective_from, known_at, evaluation_reference, reason and required_feature_order. Arrays are unnamed lists, including an empty feature array.}
\item{target_event_id}{Explicit historical assignment event identifying the rollback model and scope.}
\item{rollback_event}{New event_id, effective_from, known_at, reason, evaluation_reference and required_feature_order. Other model/scope fields cannot contradict the target.}
\item{metric_panel}{Rows with series_id, metric, period, status and value (possibly NULL). One row per series/metric/period.}
\item{policy}{Schema1.0 named object with policy_id, version, periods, series_ids and rules. Periods and series IDs are distinct nonempty arrays. Each rule has rule_id, metric, operator, threshold, consecutive_periods, severity and action_code.}
\item{context}{Optional calculation context.}
}
\details{
Roles are champion or challenger. A profile/purpose has one champion slot named
champion; named challenger slots allow parallel evaluations. Events append in
nondecreasing knowledge order. Repeating identical content is idempotent;
conflicting IDs or equal role/slot effective boundaries fail. Registered model
content and exact feature order must match. References are user assertions.

\code{select_effective_model} accepts the history and reconstructs champion
intervals using only events known at its cutoff. Intervals include the start
and exclude the next known event's start. Events remain unchanged; a backdated
later-known assignment cannot affect an earlier knowledge replay. History
alone has no final expiry. Rollback is a new event and does not cancel
independently scheduled future changes.

Monitoring operators are gt, ge, lt or le. Persistence is a positive integer;
severity is WARNING or CRITICAL. Every expected series is required, so an
entire absent series is insufficient information. Supported input statuses
are OK, MISSING_OUTCOME, INSUFFICIENT_INFORMATION, TECHNICAL_ERROR and
NOT_APPLICABLE. Missing information breaks the breach streak. A premature
breach is WATCH; a known nonbreach is WITHIN_POLICY. The trace retains every
period; overall status describes only the last declared period. Precedence
is technical error, critical, warning, missing outcome, insufficient
information, not applicable, watch, within policy. Only persisted
warning/critical breaches receive the supplied action code.
}
\value{A CalcResult containing a content-hashed append-only assignment history or a monitoring status with full rule trace.}
