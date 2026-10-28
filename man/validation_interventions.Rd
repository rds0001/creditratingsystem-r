\name{validation_interventions}
\alias{override_monitoring}
\alias{support_transfer_backtest}
\title{Dated Override and Separately Observed Stage Validation}
\usage{
override_monitoring(pre_override, post_override, override_ledger,
    policy,
 design, metrics, uncertainty, cluster_resamples = NULL,
 confidence_level = 0.95, context = NULL)
support_transfer_backtest(records, stage_contract, union_contract,
 design, uncertainty, cluster_resamples = NULL,
 confidence_level = 0.95, context = NULL)
}
\arguments{
\item{pre_override,post_override}{Distinct model entries in the paired benchmark format, containing sealed monitoring cohorts and frozen version/knowledge metadata.}
\item{override_ledger}{Unique override_id records with case_id, reason, authority, applied_at, known_at, expires_at, from_pd and to_pd.}
\item{policy}{Reference, approved_authorities and explicit concentration_threshold.}
\item{design}{Frozen validation identities, target, HOLDOUT/EXTERNAL origin and evaluation cutoff; currency for amount stages.}
\item{metrics}{Paired prediction metrics accepted by benchmark_predictions.}
\item{uncertainty}{none or cluster_bootstrap.}
\item{cluster_resamples}{Actual shared cluster IDs sampled with replacement.}
\item{confidence_level}{Interior confidence level.}
\item{context}{Optional matching context.}
\item{records}{Common dated validation records with stage_predictions, stage_outcomes and optional nonnegative weight.}
\item{stage_contract}{Named stages with binary/amount kind, observable Boolean, event_definition and reference; amount stages also currency.}
\item{union_contract}{Four distinct binary names: component_a, component_b, joint and union.}
}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Replays active override chains and validates separately observable default, transfer, joint and cash-flow stages.}
\details{An override is active if applied_at is no later than forecast, forecast precedes expires_at and known_at is no later than forecast. Active approved actions must chain exactly from pre-PD to post-PD. Expired/future-known and multiple actions are separately reported. Equal-date actions require upstream resolution. Rates cover all paired forecasts; performance covers common mature eligible cases. Changes are descriptive, not causal.

Observed stages provide value, known_at, complete_through and event_ids. Binary events also require aligned event_times inside the forecast horizon; shared IDs have consistent dates. Negative binary labels have empty event arrays. Every observed stage requires mature complete follow-up, and stage-specific missingness remains explicit. Unobservable counterfactual stages cannot carry observed labels. Amounts are nonnegative with declared common currency. Predicted joint and union obey Frechet and inclusion-exclusion; observed four-stage labels reconcile only when separately available. Union source IDs deduplicate component events. Missing labels are never reconstructed from final defaults. Outputs give weighted bias, MAE/RMSE, binary Brier and genuine cluster intervals; undefined resamples suppress intervals. No correction for informative stage missingness or counterfactual support identification is implied.}
\value{CalcResult with ValidationResult metrics, stage or ledger traces, coverage, applicability, actual uncertainty and NOT_ASSESSED decision.}
