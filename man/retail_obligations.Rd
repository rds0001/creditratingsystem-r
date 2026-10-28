\name{retail_obligations}
\alias{build_bnpl_obligation_features}
\title{Dated BNPL Purchase Plans and Allocated Obligations}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Aggregate known purchase plans, payment/refund allocations, separate disputes and explicit provider coverage.}
\usage{
build_bnpl_obligation_features(plans, installments, events,
  provider_coverage, customer_id, as_of, information_cutoff,
  lookback_days, near_term_days, currency, context = NULL)
}
\arguments{
\item{plans}{Rows: plan_id, release_seq, customer_id, provider_id, purchased_at, known_at, currency, nullable actual closed_at, nullable refinances_plan_id, logical schedule_complete.}
\item{installments}{Rows: installment_id, plan_id, release_seq, known_at, due_at, nonnegative amount, currency.}
\item{events}{Rows: event_id, release_seq, known_at, occurred_at, plan_id, nullable installment_id, kind, nonnegative amount, currency, nullable related_event_id.}
\item{provider_coverage}{Dated status COMPLETE/PARTIAL/UNKNOWN, sources (provider IDs), known_at.}
\item{customer_id}{Explicit matching customer.}
\item{as_of, information_cutoff}{Economic and information dates; cutoff cannot follow as_of.}
\item{lookback_days, near_term_days}{Positive past observation and future due-cashflow windows in calendar days.}
\item{currency}{Exact reporting currency.}
\item{context}{Optional compatible dated context.}
}
\details{
Latest knowledge-visible revisions are used. Financial events PAYMENT,
MERCHANT_REFUND, PAYMENT_REVERSAL and REFUND_REVERSAL require explicit
same-plan installment allocation. No allocation rule is guessed. Reversals
identify earlier original events and cannot cumulatively exceed them. Net paid
plus refunded amounts cannot exceed the contractual installment; excess customer
cash refunds are separate cashflows. Financial events cannot follow actual plan
closure. Closed plans require zero residual known unpaid installments.

DISPUTE_OPEN and DISPUTE_CLOSE have zero financial amount. Closing identifies one
earlier opening on the same plan. Disputes do not themselves cancel debt or imply
default. Explicit refinancing must reference an earlier visible plan; repeated
purchases alone are not refinancing.

Concurrency uses actual half-open purchase/closure intervals visible at the
feature cutoff, not assumed contractual maturity. It is a retrospective known
history rather than a contemporaneous-knowledge replay. Active contracts and
positive-outstanding plans are separate counts. Near-term cashflows use
(as_of, as_of+near_term_days]; overdue and due-today amounts are separate.
Incomplete provider coverage, incomplete schedules or a plan without installments
leave total outstanding NULL. Known subtotals remain available.
}
\value{CalcResult with plan and provider concurrency, repeat use, explicit refinancing, outstanding installments, near-term due cashflows, separate disputes and source-release diagnostics.}
\seealso{\code{build_transaction_features}, \code{aggregate_household_obligations}}
