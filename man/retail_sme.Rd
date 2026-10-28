\name{retail_sme}
\alias{build_retail_sme_features}
\title{Explicit Small-Business and Household Cash Interactions}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Separate business operating cash, matched owner transfers, household debt and typed legal liability evidence.}
\usage{
build_retail_sme_features(business_cashflows, owner_drawings,
  household_obligations, liability_links, coverage, business_id,
  owner_customer_ids, business_debt_facility_ids,
  household_debt_service_window, as_of, information_cutoff,
  lookback_days, currency, context = NULL)
}
\arguments{
\item{business_cashflows}{Rows: transaction_id, release_seq, business_id, posted_at, known_at, signed amount, category, currency. SALES additionally needs counterparty_id; DEBT_SERVICE needs facility_id.}
\item{owner_drawings}{Rows: drawing_id, release_seq, known_at, business_transaction_id, household_transaction_id, owner_customer_id, business_posted_at, household_posted_at, business_amount, household_amount, currency.}
\item{household_obligations}{Full aggregate_household_obligations CalcResult or result envelope, with matching dated context and currency.}
\item{liability_links}{Rows: liability_id, release_seq, known_at, business_id, customer_id, liability_type, effective_from, nullable effective_until, nullable liability_fraction, nullable maximum_amount, evidence_reference.}
\item{coverage}{Dated history status COMPLETE/PARTIAL/UNKNOWN, known_at, observed_from, complete_through.}
\item{business_id, owner_customer_ids, business_debt_facility_ids}{Explicit business, owners and business debt identities.}
\item{household_debt_service_window}{Object with from, through, period; dates must equal the cash window and period must match the household result.}
\item{as_of, information_cutoff}{Economic and information dates; cutoff cannot follow as_of.}
\item{lookback_days}{Positive calendar-day window excluding its left boundary.}
\item{currency}{Exact reporting currency.}
\item{context}{Optional compatible dated context.}
}
\details{
Business categories are SALES, OPERATING_COST, TAX, CAPEX, DEBT_SERVICE,
FINANCING, OWNER_DRAW and OWNER_CONTRIBUTION. Signed operating cash comprises
sales, operating costs and tax; adding capex and then debt service yields cash
before and after business debt. Financing and owner transfers are excluded.
Sales are not household income. Owner draws are business debits, contributions
credits. Other categories can include explicitly classified signed reimbursements.

Owner drawing legs must match a supplied business posting and opposite household
amount once. Local household evidence is asserted, not independently obtained.
Unmatched draws remain visible without becoming operating income. Owner households
are joined by explicit IDs and counted once. Missing household totals or shared
business/household debt facility IDs suppress combined debt subtraction. Cash
after household debt excludes living costs and is not affordability or
distributable income. No currency or debt-service period conversion is guessed.

Legal types are LIMITED_OWNERSHIP, PERSONAL_GUARANTEE and UNLIMITED_LIABILITY.
Fractions lie in [0,1] or are NULL; caps are nonnegative or NULL. Half-open dated
links remain explicit evidence; ownership never automatically becomes a guarantee
or copied owner PD. Customer concentration uses squared net-sales shares only
when total sales are positive and all customer amounts nonnegative. Calendar-month
sales and peak-month share are observations, not a seasonal forecast. Negative
monthly net amounts make the peak-share statistic undefined.
}
\value{CalcResult with business cash sequence, owner transfers, monthly and customer sales, matched household debt, overlap exclusions and typed legal evidence.}
\seealso{\code{aggregate_household_obligations}, \code{build_transaction_features}}
