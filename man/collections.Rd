\name{collections}
\alias{build_collections_episodes}
\alias{build_cure_targets}
\alias{build_collections_action_history}
\alias{simulate_repayment_arrangement}
\alias{expected_collections_value}
\title{Dated Collections Episodes, Operational Cure and Recovery Economics}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Construct collections histories from explicit dated states and
coverage, preserve censored cure and redefault outcomes, separate pre-action
information from treatment stages, and evaluate local repayment/recovery scenarios.}
\usage{
build_collections_episodes(states, actions, coverage, episode_rule,
  evaluation_cutoff, context = NULL)
build_cure_targets(episodes, horizon_days, redefault_horizon_days,
  evaluation_cutoff, context = NULL)
build_collections_action_history(actions, features, outcomes,
    coverage,
  index_stage, required_features, horizon_days,
    maximum_feature_age_days,
  evaluation_cutoff, context = NULL)
simulate_repayment_arrangement(contract, baseline, candidate,
  affordability_paths, discount_factors, period_end_dates, currency,
  context = NULL)
expected_collections_value(actions, discount_factors, ead, currency,
  period_end_dates, context = NULL)
}
\arguments{
\item{states}{Data frame or list of rows with unique state_event_id, account_id,
state, occurred_at and known_at. These are complete state changes, with a baseline
at or before observed_from, not intermittent state samples. One state per account
and occurrence date; simultaneous states require upstream resolution.}
\item{actions}{For episode/history construction, rows with unique action_id,
account_id, action_type, stage, occurred_at and known_at. Stage is ASSIGNED,
OFFERED, ACCEPTED, PERFORMED or CANCELLED. Action history additionally requires
a treatment_id joining stage events for one account/action type, with at most one
record per stage. For expected_collections_value, action scenario rows are described
in the economic inputs section below.}
\item{coverage}{Rows with unique account_id, observed_from, observed_through and
known_at. Coverage is an independent assertion of complete capture between the
inclusive dates; known_at cannot precede observed_through. Unknown-at-cutoff
coverage is not used to establish observation completeness.}
\item{episode_rule}{Named list with rule_id, version, problem_states,
clean_states, terminal_states, default_states, probation_days and separation_days.
State sets are unique strings; problem and clean sets must be nonempty and
mutually disjoint from terminal states. Default states are a subset of problem
states. Durations are explicit nonnegative integer calendar days.}
\item{evaluation_cutoff}{Inclusive ISO calendar date. Events must both occur and
be known by this date; earlier occurrence alone does not establish availability.}
\item{context}{Optional named calculation context or context result. Economic
methods reject a conflicting context currency.}
\item{episodes}{Result/value from build_collections_episodes, with canonical
history_hash and episode_hash values intact. Rebuild at a new cutoff.}
\item{horizon_days}{Positive integer calendar days from episode start or the
actual indexed action occurrence.}
\item{redefault_horizon_days}{Positive integer calendar days after sustained cure.}
\item{features}{Rows with unique feature_record_id, account_id, feature, value,
observed_at and known_at. A value key is required, and NULL is permitted to express
missing information. Tied latest observation/knowledge dates are rejected.}
\item{outcomes}{Rows with unique outcome_id, account_id, event_type, occurred_at
and known_at. Outcomes on or before index date remain an audit list, not
prospective outcomes. Later known events up to the horizon end are retained.}
\item{index_stage}{ASSIGNED or OFFERED. No action index is inferred from eventual
acceptance, performance or successful recovery.}
\item{required_features}{Nonempty vector/list of distinct required feature names.}
\item{maximum_feature_age_days}{Nonnegative integer age limit at the index date.
Older latest available features are retained but flagged stale.}
\item{contract}{Named list with currency, principal, principal_arrears,
accrued_interest, fees and as_of. Amounts are finite nonnegative scalars. Arrears
principal is a subset of principal and is never added again to the claim.}
\item{baseline, candidate}{Explicit plan specifications, each with plan_id,
payments, period_interest_rates, new_fees, servicing_costs, receipt_fractions and
logical capitalize_unpaid_interest. Numeric vectors have one entry per period;
all are nonnegative and receipt fractions are at most one.}
\item{affordability_paths}{Rows with unique path_id, currency and finite numeric
available_cash vector per period. Negative available cash is retained.}
\item{discount_factors}{Nonnegative finite numeric vector, one factor per period.
No interest-rate or discount convention is inferred from the factors.}
\item{period_end_dates}{Nonempty vector/list of strictly increasing ISO dates.
Repayment period ends must follow the contract as_of date.}
\item{currency}{Explicit nonempty currency, matching contract/budget/action rows
and any context currency. No conversion is inferred.}
\item{ead}{Finite nonnegative scalar exposure denominator. At zero EAD the
expected-net-recovery ratio is NULL; monetary recovery remains available.}
}
\value{Each method returns a CalcResult with explicit limitations.

build_collections_episodes returns episodes, complete cutoff-filtered state_history,
unlinked_actions, rule, coverage, evaluation_cutoff and history_hash. Episodes
contain identifiers, start/end, OPEN/CURED/TERMINATED status, previous_episode_id,
timeline, actions, ever_default, observation_cutoff, cure_candidate_at and
 episode_hash. Histories and rules are bound by canonical hashes.

build_cure_targets returns one row per episode with horizon_end, cure_label/status,
cure_at, redefault_label/status, redefault_at, ever_default and cutoff. Labels are
1 or 0 only when their corresponding event or non-event is established; otherwise
NULL with censoring status.

build_collections_action_history returns rows and unindexed_treatments. Rows
contain preaction_features, feature_sources, missing/stale features, stage_dates,
invalid_stage_order, descriptive_eligible, accepted/performed indicators,
prior_or_same_day_outcome_ids, subsequent outcomes, outcome_complete and row_hash.
causal_effect_identified is always FALSE.

simulate_repayment_arrangement returns baseline/candidate ledgers, NPV change,
principal_arrears_included, contract_changed=FALSE and default_status_changed=FALSE.
Each plan contains schedule, discounted net-cashflow NPV, remaining_claim and
household residual cash/shortfall dates.

expected_collections_value returns one row per supplied action with
expected_net_recovery, its EAD ratio, positive-mass scenario min/max, expanded
accepted/declined scenarios, support_status, assumption_reference and
causal_effect_identified=FALSE.}
\section{Episode and cure boundaries}{
An episode starts in a declared problem state. A clean transition starts a quiet
candidate interval. Episode separation requires complete coverage through
max(probation_days, separation_days). A problem event exactly on that boundary
interrupts the interval. Terminal states close the episode as TERMINATED. An open
data endpoint, a single payment or an unobserved gap never establishes cure.
A later problem spell carries the previous episode identifier. Historical default
is not erased by operational cure.

The cure target uses probation_days, independently of the longer possible episode
separation duration. A supported cure on or before the episode horizon end is a
positive observed event, even if later follow-up is censored. A negative label
requires coverage from episode start through horizon end and a mature cutoff.
A terminal exit within the target window censors a non-cured episode.

Redefault uses a separate risk set starting strictly after cure through the
inclusive post-cure horizon end. The first default versus terminal event determines
positive redefault versus exit censoring. A negative label requires complete
post-cure coverage. The full sealed state history retains terminal events after
an episode has already closed, so they cannot disappear from follow-up.
}
\section{Action information and interpretation}{
Features must both be observed and known strictly before the actual assignment
or offer date. Same-day feature records are excluded because intraday ordering
is not established by calendar dates. Eligible records are selected by account/name,
ordered by observation date, knowledge date and record identifier. Late logging
of the action does not move its information cutoff. Missing and stale values
remain visible; no imputation is performed. Duplicate latest observation/knowledge
timestamps require explicit resolution.

ASSIGNED, OFFERED, ACCEPTED and PERFORMED dates are checked for chronological order;
invalid order makes a row descriptively ineligible. CANCELLED remains a separate
stage record. A later accepted/performed status is a post-index outcome of the
process, not a pre-index feature. Treatments without the declared index stage are
listed separately. Same-day outcomes have unresolved intraday order and are never
used as prospective outcomes. Outcome absence is not a zero label when coverage
is incomplete. No causal effects, treatment eligibility or action ranking are
inferred from these factual groups.
}
\section{Economic inputs and cash reconciliation}{
For repayment simulation, interest is an effective rate for each supplied period
on its opening principal. It is not an annual rate and receives no hidden day-count
conversion. New fees and interest enter one claim ledger. Scheduled cash is capped
by the outstanding claim; receipt fractions determine actual cash. The waterfall
pays fees, then interest, then principal. Remaining interest is capitalized only
when explicitly selected. Unused payment capacity, all waterfall amounts and the
closing claim remain visible.

NPV sums received cash less servicing cost after applying period factors. Remaining
claims carry no assumed terminal recovery. Household residual cash subtracts
scheduled obligations, not the lower cash receipts under assumed nonpayment.
Plans do not change a legal contract or a recorded default status.

For expected_collections_value each action row requires action_id, currency,
uptake_probability, accepted_scenarios, declined_scenarios, offer_cost,
support_status and assumption_reference. Each branch must contain a complete
conditional scenario distribution, including when branch probability is zero.
A scenario contains unique scenario_id, probability and cashflows. Conditional
probabilities lie in [0,1] and sum to one within 1e-12; they are used as supplied.
Each cashflow has unique cashflow_id within its scenario, kind RECOVERY or COST,
period_end matching a declared date, and finite nonnegative amount.

Joint scenario masses are uptake times accepted conditional mass, or one minus
uptake times declined conditional mass. Recovery and servicing costs are discounted
per scenario. Offer cost occurs once at time zero; it must not also appear as a
scenario cost. Expected recovery weights these exclusive net values. Cashflow IDs
prevent repeated records within one scenario; callers must give the same economic
cashflow one identifier. Scenario ranges are not confidence intervals and inputs
are conditional assumptions, not identified treatment effects.
}
\examples{
plan <- list(plan_id = "base", payments = c(50, 50),
  period_interest_rates = c(0, 0), new_fees = c(0, 0),
  servicing_costs = c(0, 0), receipt_fractions = c(1, 1),
  capitalize_unpaid_interest = FALSE)
contract <- list(currency = "EUR", principal = 100, principal_arrears = 20,
  accrued_interest = 0, fees = 0, as_of = "2024-01-01")
simulate_repayment_arrangement(contract, plan, plan, list(), c(1, 0.95),
  c("2024-02-01", "2024-03-01"), "EUR")$value$npv_change
}
