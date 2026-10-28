\name{affordability}
\alias{debt_to_income}
\alias{debt_service_to_income}
\alias{residual_income}
\alias{assess_affordability}
\alias{normalize_income_period}
\alias{estimate_recurring_expenses}
\alias{combined_loan_to_value}
\alias{stress_affordability}
\title{Explicit Affordability Ratios and Income Surplus}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Calculate stock and service ratios and signed income surplus, then apply user-provided thresholds independently of PD and credit approval.}
\usage{
debt_to_income(debt_stock, income, income_frequency, currency,
    ratio_period,
               income_basis = "net", context = NULL)
debt_service_to_income(debt_service, income, period, currency,
                       income_basis = "net", context = NULL)
residual_income(net_income, recurring_expenses,
    existing_debt_service,
                new_debt_service, period, currency, context = NULL)
assess_affordability(metrics, threshold_profile, evidence,
    context = NULL)
normalize_income_period(components, target_period, target_basis,
    currency,
  payments_per_year, variable_income_factor, context = NULL)
estimate_recurring_expenses(expenses, target_period, currency,
  payments_per_year, household_profile, benchmarks = NULL,
    context = NULL)
combined_loan_to_value(claims, collateral, allocations, currency,
    context = NULL)
stress_affordability(baseline, scenario, context = NULL)
}
\arguments{
\item{debt_stock}{Nonnegative debt stock.}
\item{income,net_income}{Finite period income; negative income is allowed.}
\item{income_frequency,period}{One of weekly, biweekly, four_weekly, monthly, quarterly or annual.}
\item{currency}{Explicit common currency.}
\item{ratio_period}{Annualize income with \code{"annual"}, or retain \code{"income_period"}.}
\item{income_basis}{Explicit \code{"net"} or \code{"gross"} basis.}
\item{debt_service,existing_debt_service,new_debt_service}{Nonnegative, same-period service amounts.}
\item{recurring_expenses}{Nonnegative expenses, disjoint from debt service.}
\item{metrics}{Named list of finite scalar metrics or NULL for unavailable metrics.}
\item{threshold_profile}{Named list with schema_version 1.0, policy_id, version, required_evidence and rules. Each rule has rule_id, metric, operator and threshold.}
\item{evidence}{Named list of logical evidence-availability flags.}
\item{components}{Unnamed list of income records: economic_id, amount,
frequency, basis, currency, kind (recurring, variable, internal_transfer).
Crossing net/gross basis requires same-period tax_amount.}
\item{target_period,target_basis}{Target frequency key and net/gross income basis.}
\item{payments_per_year}{Named list of explicit positive annual payment counts,
including the target and every source frequency.}
\item{variable_income_factor}{Inclusion fraction in [0,1] for positive variable
earnings. Negative earnings are fully retained.}
\item{expenses}{Unnamed list of records: economic_id, category, amount,
frequency, currency, kind (essential, discretionary, debt_service,
internal_transfer). Null amounts denote unknown expenditure.}
\item{household_profile}{Named list with profile_id and array required_categories.}
\item{benchmarks}{Optional local rows with profile_id, category, amount,
frequency, currency, kind. Matching category amounts are floors.}
\item{claims}{Secured claim records with claim_id, amount and currency.}
\item{collateral}{Collateral records with collateral_id, value, currency,
valuation_date and declared freshness CURRENT or STALE.}
\item{allocations}{Rows with claim_id, collateral_id, amount and integer priority.
Amounts must partition each claim once across assets.}
\item{baseline}{Named household object with as_of, reporting currency,
liquid_buffer, incomes, expenses, contracts, collateral, cltv_scope and thresholds.
See details for the monthly contract schema.}
\item{scenario}{Named object with schema_version 1.0, scenario_id and monthly
periods. Each period supplies date, income_factors, expense_factors,
collateral_factors, rate_shifts and fx_to_reporting.}
\item{context}{Optional calculation context or its result.}
}
\details{Length-one amounts expand; other vectors require equal lengths. Annualization uses 52 weekly, 26 biweekly, 13 four-weekly, 12 monthly, 4 quarterly and 1 annual payment. Nonpositive income returns a NULL ratio and NONPOSITIVE_INCOME status. No universal thresholds or approval rules are supplied. Threshold comparisons are unrounded, and strict inequalities fail at equality. Missing inputs give INSUFFICIENT_EVIDENCE; known breaches remain visible. Otherwise the assessment is AFFORDABLE or NOT_AFFORDABLE. Household obligations must be reconciled before supplying disjoint totals.

Income normalization instead uses the explicitly supplied payment
calendar. Duplicate economic sources count once; conflicting economics fail.
Transfers contribute zero. Unknown included income or tax bridges make total
income NULL. No taxes are estimated. Expense reconciliation takes the maximum
known normalized amount per economic obligation, then applies category floors.
It does not infer recurring obligations from raw transactions. Debt service is
separate from living costs; unknown debt service stays NULL. Missing required
categories and obligations remain explicit.

CLTV counts each allocated collateral once. Lower ranks consume available value
first and equal ranks share proportionally. Claim allocation reconciliation uses
relative or absolute tolerance 1e-12. Zero collateral value gives NULL ratios.
Rank coverage is not an LGD estimate or a legal enforceability assessment.

Stress baseline incomes have economic_id, monthly_amount, net basis and currency;
expenses have economic_id, monthly_amount and currency. Contracts have economic_id,
currency, principal, remaining_months, annual_rate, repayment (annuity or
interest_only), balloon_amount, rate_type (fixed or reset), reset_dates,
rate_floor and rate_cap. Rates are nonnegative nominal annual rates divided by
twelve. Interest-only requires balloon_amount equal to principal. Stress
collateral records use economic_id, value, currency. cltv_scope must declare
all_contracts_secured_by_pool or not_applicable. Thresholds explicitly contain
minimum_residual, maximum_dsti and maximum_cltv.

Scenario dates are consecutive monthly anniversaries clamped to month-end.
Every factor map covers the respective baseline economic IDs exactly, including
empty named maps for absent groups. FX is reporting-currency units per source
unit, with reporting FX equal to one. Rate shifts apply relative to original
coupon only at declared resets, bounded by floor and cap. Projected resets must
coincide with payment dates. Annuity payment solves the present-value equation
including the terminal balloon and is recomputed at reset. At maturity, actual
remaining principal plus interest is due; no refinancing is assumed. Fixed
coupons ignore shifts. Residuals accumulate into liquidity, with first strictly
negative buffer date reported. Projection continues after exhaustion to expose
the funding gap; no default or collections transition is inferred. Annual DTI
uses opening debt and current monthly income times twelve; CLTV uses closing
debt and the declared common collateral pool. No independent shocks are drawn.
Arrays are unnamed lists; objects are named lists, including empty maps.}
\value{A CalcResult with ratios and denominator status, signed residuals and component trace, or a separate affordability assessment with rules and headroom.}
\examples{
debt_to_income(60000, 2500, "monthly", "EUR", "annual")$value
debt_service_to_income(900, 3000, "monthly", "EUR")$value
residual_income(3000, 1000, 900, 200, "monthly", "EUR")$value
}
