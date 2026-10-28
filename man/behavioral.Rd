\name{behavioral}
\alias{compute_days_past_due}
\alias{assign_delinquency_state}
\alias{build_account_month_panel}
\alias{build_behavioral_targets}
\alias{estimate_retail_roll_rates}
\alias{project_retail_roll_rates}
\alias{predict_behavioral_risk}
\alias{build_vintage_performance}
\title{Dated Behavioral Account Risk and State Dynamics}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Reconstruct payment arrears, build dated account snapshots and
first-event targets, estimate empirical transitions and calculate descriptive
vintage performance. Active zero-balance accounts remain in their risk set.}
\usage{
compute_days_past_due(obligations, payments, as_of, known_at,
  allocation_policy = "fifo", currency, context = NULL)
assign_delinquency_state(dpd, past_due_amount, exposure, events,
  state_profile, context = NULL)
build_account_month_panel(accounts, statements, account_events,
  observation_dates, reporting_lag_days = 0, obligations = NULL,
  payments = NULL, context = NULL)
build_behavioral_targets(panel, events, horizon_months,
    target_event,
  competing_events, exit_treatment, evaluation_cutoff, coverage,
  context = NULL)
estimate_retail_roll_rates(panel, states, from_date, to_date,
    group_by,
  weighting, missing_successor, context = NULL)
project_retail_roll_rates(initial_distribution, transition_matrices,
  states, default_states, closure_states, exposure_by_period,
  context = NULL)
predict_behavioral_risk(snapshot, model, target, calibrator = NULL,
  offset = NULL, context = NULL)
build_vintage_performance(accounts, panel, events, months_on_book,
  evaluation_cutoff, coverage, default_event, delinquency_states,
  exit_events, context = NULL)
}
\arguments{
\item{obligations}{Rows with unique obligation_id, account_id, due_at, known_at,
nonnegative amount and currency.}
\item{payments}{Rows with unique payment_id, account_id, paid_at, known_at,
amount, currency, kind (payment/reversal). Reversals require reverses_payment_id.}
\item{as_of,known_at}{ISO observation and information-cutoff dates.}
\item{allocation_policy}{fifo applies net available cash to oldest due obligations;
explicit restricts each payment to its obligation_id.}
\item{currency}{Common currency of the single-account payment calculation.}
\item{dpd,past_due_amount,exposure}{Nonnegative integer arrears days and finite amounts.}
\item{events}{Dated event rows for target/vintage methods; named boolean event flags
for state assignment. Event rows require event_id, account_id, event_type,
occurred_at and known_at. Optional LOSS/RECOVERY rows require amount and currency.}
\item{state_profile}{Named profile with schema_version, profile_id,
immaterial_state, materiality, buckets, event_priority, optional dpd_default_rule.}
\item{accounts}{Rows with unique account_id, customer_id, product_id, opened_at,
known_at, currency. Vintage additionally requires original_balance.}
\item{statements}{Rows with statement_id, account_id, observed_at, known_at,
balance, credit_limit, dpd, past_due_amount and currency.}
\item{account_events}{Dated lifecycle events: ACTIVE, DEFAULT, CHARGEOFF,
CURED, CLOSED, PREPAID or SOLD.}
\item{observation_dates}{Distinct calendar-month-end ISO dates.}
\item{reporting_lag_days}{Nonnegative conservative evidence delay: subtract
from observation date to obtain the information cutoff.}
\item{panel}{Account state rows. Targets require account_id, customer_id,
as_of, at_risk, ever_default; transition/vintage methods require state.}
\item{horizon_months}{Positive integer monthly-anniversary target horizon.}
\item{target_event,default_event}{Explicit first-event/default event name.}
\item{competing_events,exit_events}{Distinct competing exit event names.}
\item{exit_treatment}{competing assigns zero to a first exit; censor leaves its
target unobserved. Neither estimates hypothetical continuing-account risk.}
\item{evaluation_cutoff}{Date at which outcome evidence is evaluated.}
\item{coverage}{Rows with account_id, observed_through and known_at, explicitly
declaring outcome observation completeness.}
\item{states}{Ordered distinct state names.}
\item{from_date,to_date}{Increasing ISO transition endpoint dates.}
\item{group_by}{Origin grouping column names, possibly empty.}
\item{weighting}{count or opening_balance; latter needs balance or features.balance.}
\item{missing_successor}{error or censor; censor reports missing mass separately
and estimates transitions conditional on observed successors.}
\item{initial_distribution}{Nonnegative state probability vector summing to one.}
\item{transition_matrices}{One row-stochastic numeric matrix or nested row list
per period; unresolved null rows cannot be projected.}
\item{default_states,closure_states}{Disjoint individually absorbing states.}
\item{exposure_by_period}{Nonnegative period-by-state matrix of separately
supplied conditional exposures. Closed-state exposure must be zero.}
\item{snapshot}{A fingerprinted account snapshot from the panel builder.}
\item{model}{Frozen model artifact or FitResult with behavioral_contract.}
\item{target}{Exact physical event, horizon, time-unit and conditioning contract.}
\item{calibrator}{Optional frozen calibrator compatible with target/model version.}
\item{offset}{Optional offset required by the fitted model.}
\item{months_on_book}{Distinct nonnegative calendar month ages. MOB zero ends
at the origination month's final day.}
\item{delinquency_states}{States counted in the current delinquency stock.}
\item{context}{Optional calculation context.}
}
\details{Payment reversals are dated and never rewrite an earlier information
cutoff. DPD excludes grace/materiality rules. Amounts due today have zero days
past due and are excluded from past_due_amount, but retained as unpaid_due_amount.
FIFO is a current net-cash allocation contract, not a bank servicing waterfall.

State materiality contains absolute, relative thresholds and and/or logic.
Positive arrears and threshold equality qualify. At zero exposure positive
arrears satisfy relative materiality. Buckets have state, lower inclusive and
upper exclusive integer bounds, gap-free from zero to a final NULL upper bound.
event_priority records have event, state, at_risk and absorbing. Explicit events
precede the optional dpd_default_rule (minimum_dpd, state, material_required).
No bucket is a universal regulatory default definition.

Panel snapshots choose the latest economic statement/revision available by the
cutoff. Statements may use a different billing-cycle date. Missing statements
produce missing features, not zero balances. Account features include balance,
credit_limit, dpd, past_due_amount and utilization. Legal termination is retained;
default history survives cure. Customer sums are currency-specific and not PDs.

Targets use (as_of,horizon_end] first-event windows. Previously defaulted and
terminated accounts are outside that risk set. Observed events can identify
positive labels early. Non-events require explicit mature coverage. Unknown
follow-up is NULL when neither coverage nor event is available. Same-day
competing target/exit events require upstream resolution. Overlapping account
windows and shared customers are dependent observations.

Roll-rate outputs reconcile origin counts/masses to observed destinations plus
censoring. Empty observed rows are NULL, never identity rows. Opening-balance
weighting measures monetary flows, not account probabilities. Projection keeps
first-default states absorbing and separates transition probabilities from
conditional exposures. balance_flows are probability flows times destination
exposure, not cash transfers. Supplied exposures must reflect drawdowns,
amortization and maturity independently of the probability matrix.

Vintage cells use fixed origination-account denominators. Immature or incompletely
observed cells remain NULL. Current delinquency stock differs from ever-default;
losses are supplied observed loss/recovery cashflows, not ultimate estimates.
Age plus cohort equals calendar period, preventing unrestricted causal separation.

Prediction validates snapshot hash, first-default account eligibility,
product/state support and maximum_source_age_days from behavioral_contract.
The contract requires risk_unit account and event_policy first_default.
It invokes supported frozen shared model prediction, then optional calibration.
Model-training and calibration knowledge dates cannot exceed the snapshot cutoff.
An active zero balance does not force PD to zero. Isotonic calibration must
declare metadata input_scale probability. Model actions and future EAD remain
separate. Arrays use unnamed lists and objects named lists.}
\value{A CalcResult containing reconciled payment records, state assignments,
account/customer panels, event labels, empirical transition tables, projected
probability/balance paths, behavioral risk predictions or vintage cells.}
