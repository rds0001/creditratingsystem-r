\name{simulation_cashflows}
\alias{build_debt_schedule}
\alias{apply_cash_waterfall}
\title{Contractual Debt Service and Cash Conserving Waterfalls}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Build dated debt obligations and allocate actual cash and restricted
reserves by explicit priority, retaining unpaid claims across periods.}
\usage{
build_debt_schedule(loan_terms, calendar, base_rate_fixings = NULL,
  context = NULL)
apply_cash_waterfall(cash_periods, waterfall_policy, currency,
  context = NULL)
}
\arguments{
\item{loan_terms}{List with facility_id, currency, positive principal, rate_type
(fixed/floating), rate_convention (nominal_simple/effective_annual), day_count
(ACT/365F or ACT/360), amortization (bullet/equal_principal/annuity/custom),
maturity_date; fixed rates require annual_rate, floating rates require spread.}
\item{calendar}{Contiguous accrual-period records with start_date, end_date and
payment_date. Floating periods also require fixing_date at/before start_date;
custom amortization requires principal_due. Final payment equals maturity_date.}
\item{base_rate_fixings}{Floating-rate records with unique fixing_date and base_rate;
all must be consumed. None for fixed-rate contracts.}
\item{cash_periods}{Scenario/date records with scenario_id, date, currency,
nonnegative cash_available, reserve_target and amounts_due. The last is a named
list specifying every claim's new due amount, including explicit zeros.}
\item{waterfall_policy}{List with policy_id, version, nonnegative initial_reserve,
reserve_priority, excess_reserve_policy (retain/release_before_claims),
arrears_policy (carry_without_interest), and claims. Each claim has claim_id,
unique priority and logical reserve_eligible. Reserve top-up has its own priority.}
\item{currency}{Single reporting currency for all waterfall amounts.}
\item{context}{Optional compatible calculation context.}
}
\details{Debt interest accrues on opening principal. Nominal simple growth is
one plus rate times actual day-count fraction; effective annual growth is
(one plus rate) to that fraction. A fixed-rate annuity discounts each payment
using the specified accrual factors, supporting irregular periods. Floating
annuity resets require explicit principal amounts rather than future-rate
perfect foresight. Negative interest is preserved, with net lender refunds
separated from positive debt service. Maturity repayment never assumes refinancing.
There are no fees, capitalization or embedded prepayment options.

Waterfalls first optionally release excess reserves, then execute claims and
reserve top-up by increasing priority. Eligible claims draw restricted reserves
only after current cash. Unpaid amounts carry exactly once without interest.
Remaining free cash is distributed to equity. Opening cash plus opening reserves
equals payments plus closing reserves plus equity distributions. Reserve movements
are not operating revenue. Liquidity shortfall alone does not establish regulatory
default: materiality, duration and UTP require the separate event evaluator.}
\value{A CalcResult containing the debt schedule and frozen terms, or waterfall
periods with payment source, carried arrears, reserves, equity cash and the cash
identity residual. Policy/terms content hashes preserve the declared contract.}
\seealso{\code{detect_cashflow_default}}
