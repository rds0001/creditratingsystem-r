\name{retail_behavior}
\alias{build_payment_history_features}
\alias{build_revolving_features}
\title{Historical Retail Payment and Revolving Features}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Replay point-in-time payment ledgers and separate balance movement, limit changes and minimum/full repayment behavior.}
\usage{
build_payment_history_features(obligations, payments, coverage,
    account_id,
  as_of, information_cutoff, lookback_days, allocation_policy,
  currency, grace_days, context = NULL)
build_revolving_features(balance_history, statements, coverage,
    account_id,
  as_of, information_cutoff, lookback_days, currency,
    context = NULL)
}
\arguments{
\item{obligations, payments}{Single-account complete historical contractual ledger with the fields accepted by compute_days_past_due, including pre-window balances, prepayments and explicit payment reversals.}
\item{coverage}{Dated object: status COMPLETE/PARTIAL/UNKNOWN, known_at, observed_from, complete_through.}
\item{account_id}{Explicit matching single-account identity.}
\item{as_of, information_cutoff}{ISO economic and information dates; cutoff cannot exceed as_of.}
\item{lookback_days}{Positive calendar-day observation window, excluding its left boundary.}
\item{allocation_policy}{Explicit fifo or explicit payment allocation.}
\item{currency}{Exact reporting currency; no automatic conversion.}
\item{grace_days}{Nonnegative delay for payment assessment only. Contractual DPD is unchanged.}
\item{context}{Optional compatible dated context.}
\item{balance_history}{Rows: snapshot_id, release_seq, account_id, observed_at, known_at, nonnegative balance and credit_limit, activity_status (ACTIVE/INACTIVE/CLOSED), currency.}
\item{statements}{Rows: statement_id, release_seq, account_id, issued_at, due_at, observed_at, known_at, statement_balance, minimum_due, paid_by_due, currency. Amounts are nonnegative and minimum_due cannot exceed the statement payable balance.}
}
\details{
Payment histories replay each day using the earlier of the feature cutoff and
that historical date. Later-known reversals do not rewrite earlier days. Supply
the complete contractual ledger, not only window-dated records. Grace changes
only the assessment date, never the actual due date or DPD. Each obligation due
inside the window is assessed at due date plus grace. Unelapsed grace and
obligations not contemporaneously known are reported as unassessable. Payment
ratios aggregate allocated amounts over assessed amounts; a zero denominator is
NULL. CURRENT and ARREARS describe unpaid-ledger states and are not regulatory
default classifications. Partial/unknown history leaves complete features NULL
while retaining known-history summaries.

Revolving snapshots select latest-known revisions and require one resolved record
per observed day. Missing daily balances are not carried forward. Complete
overlimit-day totals require all calendar dates and complete coverage. A zero
limit gives NULL utilization. Explicit inactive accounts differ from undrawn
active accounts. Closed accounts require zero residual balance and limit.
Utilization change equals balance change divided by the new limit plus previous
utilization times the relative limit cut using the new limit denominator. This
separates net balance movement from limit effects, not gross draw transactions.
Trend is endpoint utilization change per elapsed calendar day.

Statement paid_by_due must mean allocated payments by contractual due date, not
eventual receipts. Observation must extend through due date. Full, minimum without
full, below minimum and no-amount-due states remain separate. Statement counts
refer to observed supplied evidence rather than asserting unseen history exists.
}
\value{CalcResult containing daily history, assessments and descriptive summaries with explicit missing coverage. Diagnostics retain cutoff policy, coverage and excluded revisions.}
\seealso{\code{compute_days_past_due}, \code{build_bureau_features}}
