\name{retail_cashflows}
\alias{build_transaction_features}
\title{Booked Cashflow and Retail Liquidity Features}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Compute locally classified dated cashflows, own-transfer reconciliation, uncertainty, volatility and observed liquidity buffers.}
\usage{
build_transaction_features(transactions, daily_balances,
    category_mapping,
  coverage, customer_id, own_account_ids, as_of, information_cutoff,
  lookback_days, currency, context = NULL)
}
\arguments{
\item{transactions}{Rows: transaction_id, release_seq, account_id, posted_at, value_at, known_at, signed amount, currency, kind ORIGINAL/REVERSAL, category_code. Reversals additionally identify reverses_transaction_id. Own transfers identify transfer_id and counterparty_own_account_id.}
\item{daily_balances}{Rows: balance_id, release_seq, account_id, observed_at, known_at, signed closing_balance, currency.}
\item{category_mapping}{Dated mapping_id, version, known_at and categories mapping local codes to canonical categories.}
\item{coverage}{Dated status COMPLETE/PARTIAL/UNKNOWN, known_at, observed_from, complete_through.}
\item{customer_id, own_account_ids}{Explicit customer and owned-account identities; ownership is asserted, not inferred.}
\item{as_of, information_cutoff}{Economic and knowledge dates; cutoff cannot follow as_of.}
\item{lookback_days}{Positive calendar-day window excluding the left boundary.}
\item{currency}{Exact reporting currency.}
\item{context}{Optional compatible dated context.}
}
\details{
Latest knowledge-visible releases are selected before the posting-date window.
Value dates remain visible. Reversals must reference earlier visible same-account
originals of opposite sign and cannot cumulatively exceed them. They inherit the
original classification while retaining their own cash date. Supply pre-window
originals needed for lineage. Cross-window reversals remain actual cash movements.

Categories are SALARY, OTHER_INCOME, BUSINESS_REVENUE, CONSUMPTION, DEBT_SERVICE,
BORROWING, REFUND, OWN_TRANSFER, OWNER_DRAW, TAX and UNKNOWN. Unmapped codes remain
UNKNOWN. Only salary and other income enter classified income; consumption,
debt service and tax enter classified expense. These are classified subtotals,
not claims about the economic nature of unknown amounts. Borrowing, business
sales, refunds and owner draws are not household income.

Own-transfer legs must identify two explicitly owned accounts and opposite
same-currency amounts. Single visible legs remain unmatched transfers, not income.
Own transfers and owner draws are excluded from external cashflow statistics.
Daily external net cashflow includes zero posting days; its standard deviation
uses the population denominator and numerical scaling. Incomplete history leaves
complete statistics NULL while retaining known-record statistics.

Balance snapshots require one resolved observation per account-day. Negative
accounts count separately as overdraft account-days. Household buffer observations
require all declared accounts that day. Missing balance days are never carried
forward or interpreted as zero overdrafts. Full overdraft totals require complete
source coverage and every declared account-day.
}
\value{CalcResult with category credit/debit/net totals, known and coverage-qualified cashflows, daily volatility, category uncertainty, observed liquidity, classified transaction lineage and unmatched own transfers.}
\seealso{\code{build_payment_history_features}, \code{reconcile_income_evidence}}
