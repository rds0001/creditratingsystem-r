\name{ead}
\alias{exposure_schedule}
\alias{realized_ccf}
\title{Dated Drawn Exposure Schedules and Realized Conversion Factors}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Accrue balances through explicit dated events with known default
timing, and retain raw realized CCF values with undefined-denominator statuses.}
\usage{
exposure_schedule(contract, events, annual_interest_rates,
    period_end_dates,
  day_count, default_timing, allow_overlimit, currency,
    context = NULL)
realized_ccf(reference_drawn, reference_limit, ead_at_default,
    reference_date,
  default_date, reference_known_at, default_known_at,
    evaluation_cutoff,
  currency, minimum_undrawn = 0, context = NULL)
}
\arguments{
\item{contract}{Named list with facility_id, currency, ISO as_of, and nonnegative
drawn_principal, credit_limit and accrued_interest. Snapshot balances already
include all activity through as_of.}
\item{events}{Data frame or list of rows with unique event_id, event_date,
known_at, nonnegative integer sequence, kind and nonnegative amount. Kinds are
DRAW, PRINCIPAL_REPAYMENT, INTEREST_PAYMENT and LIMIT_CHANGE. A limit change amount
is the new total limit. All events follow the snapshot, fit the horizon and must
already be known at snapshot time. Duplicate date/sequence pairs are rejected.}
\item{annual_interest_rates}{One finite nonnegative simple annual rate per period.}
\item{period_end_dates}{Nonempty character vector/list of increasing ISO calendar
dates after the snapshot.}
\item{day_count}{ACT/365F or ACT/360, explicitly selected for annual simple interest.}
\item{default_timing}{BEFORE_PERIOD_END_EVENTS or AFTER_PERIOD_END_EVENTS, referring
to all scheduled events on the endpoint date.}
\item{allow_overlimit}{Explicit logical permission for new draws above the limit.
Initial overlimit amounts or limit cuts remain visible separately.}
\item{currency}{Explicit nonempty currency, matching contract and context.}
\item{context}{Optional calculation context or context result.}
\item{reference_drawn, reference_limit, ead_at_default}{Finite nonnegative monetary
amounts under a common currency/exposure definition.}
\item{reference_date, default_date, reference_known_at, default_known_at,
evaluation_cutoff}{ISO dates satisfying reference_known_at at or before
reference_date, strictly before default_date, at or before default_known_at,
at or before evaluation_cutoff.}
\item{minimum_undrawn}{Nonnegative monetary threshold; a positive denominator
at or below this threshold does not produce a numerical CCF.}
}
\value{A CalcResult. exposure_schedule returns a schedule and event ledger bound
by schedule_hash, with facility/valuation conventions. Each period contains opening
and closing principal, accrued interest, limit, nonnegative undrawn commitment,
overlimit amount, exposures before/after endpoint events and selected conditional_ead.
realized_ccf returns raw ccf, denominator_status, reference amounts, undrawn
commitment, exposure increase and reconstructed EAD when defined.}
\details{Interest accrues on drawn principal between successive events using
actual calendar days and the declared divisor. It is not capitalized. Principal
and interest repayments are explicit separate events and cannot exceed the
corresponding balance. The event ledger exposes every applied balance change.
A bullet payment can give positive before-event exposure and zero after-event
principal; interest persists until paid explicitly.

CCF is (EAD at default minus reference drawn) divided by (reference limit minus
reference drawn). Negative and above-one CCF values are retained. Nonpositive
undrawn commitment yields NULL CCF with NO_POSITIVE_UNDRAWN; a small positive
commitment at/below the threshold yields BELOW_MINIMUM_UNDRAWN. No artificial
floor, ceiling, limit-change adjustment, cancellation-right model, future borrower
behavior or legal netting rule is inferred.}
\examples{
realized_ccf(40, 100, 70, "2023-12-01", "2024-01-01",
  "2023-12-01", "2024-01-02", "2024-01-31", "EUR")$value
}
