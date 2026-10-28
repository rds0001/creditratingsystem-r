\name{ead_counterparty}
\alias{aggregate_netting_set_exposure}
\alias{simulate_collateral_margin}
\alias{closeout_exposure_at_default}
\title{Signed Netting, Collateral Unit Ledgers and Frozen Closeout Exposure}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Apply explicit legal membership, margin, market and timing contracts
to user-valued aligned counterparty scenarios.}
\usage{
aggregate_netting_set_exposure(trade_value_paths,
    netting_agreements, fx_paths,
  as_of, reporting_currency, valuation_convention, context = NULL)
simulate_collateral_margin(netting_paths, margin_policies,
    call_calendar,
  collateral_market_paths, context = NULL)
closeout_exposure_at_default(netting_paths, collateral_paths,
    default_events,
  intermediate_cashflows, freeze_timing, cashflow_convention,
    context = NULL)
}
\arguments{
\item{trade_value_paths}{Rows with path_id, date, trade_id, currency, signed value.
A complete common scenario/date/trade grid is required.}
\item{netting_agreements}{Rows containing netting_set_id, counterparty_id, trade_ids,
legally_enforceable Boolean, evidence_reference, known_at, effective_from and
explicit effective_until date or NULL. Unique complete membership is required.
Multiple-trade sets require enforceability; singleton sets may declare FALSE.}
\item{fx_paths}{Exact path_id/date/currency/rate rows. Positive reporting-currency
units per trade-currency unit; explicit reporting-currency identity rate one.}
\item{as_of}{ISO snapshot date. Agreements must be known then and effective for
all valuation dates under the half-open interval [effective_from,effective_until).}
\item{reporting_currency}{Explicit currency consistent with calculation context.}
\item{valuation_convention}{The string EX_DIVIDEND_BEFORE_MARGIN.}
\item{netting_paths, collateral_paths}{Bound CalcResult or value artifacts from
aggregation and margin simulation. Hash and shared netting binding are verified.}
\item{margin_policies}{One row per set: known_at, collateral_currency, initial_units,
threshold_received, threshold_posted, minimum_transfer_amount, ownership,
received_enforceable, posted_returnability, posted_claim_netting_enforceable, reuse.
Ownership is TITLE_TRANSFER or SECURITY_INTEREST; received_enforceable must be TRUE.
Posted returnability is UNSECURED_CLAIM or BANKRUPTCY_REMOTE, with the latter
incompatible with TITLE_TRANSFER. Posted claim netting is an explicit Boolean.
Reuse is ALLOWED or PROHIBITED. Thresholds and MTA are nonnegative.}
\item{call_calendar}{Rows with call_id, path_id, netting_set_id, valuation_date,
call_date, settlement_date, dispute_delay_days. Valuation is no later than call,
settlement no earlier; dispute delay adds nonnegative calendar days. One call per
path/set/date. Effective settlement is on the grid or beyond its end.}
\item{collateral_market_paths}{Complete path_id/date/netting_set_id grid with
currency, positive unit_price, positive fx_rate, received_haircut and posted_haircut
in [0,1). A single homogeneous asset per set is tracked in signed units.}
\item{default_events}{Unique path/set rows containing default_date, closeout_date
and margin_observation_date. Observation cannot follow default; closeout cannot
precede default. Default and closeout require exact grid dates.}
\item{intermediate_cashflows}{Unique cashflow_id rows with path/set, date, signed
amount, reporting currency, and included_in_closeout_value=FALSE. All are unsettled
claims excluded from clean values and dated in (default,closeout].}
\item{freeze_timing}{BEFORE_DATE_EVENTS freezes opening collateral units;
AFTER_DATE_EVENTS freezes closing units after all that day's events.}
\item{cashflow_convention}{UNSETTLED_EXCLUDED_FROM_CLOSEOUT_VALUE.}
\item{context}{Optional calculation context; currency must match.}
}
\value{CalcResult with signed net values and positive exposures, a bound collateral
scenario artifact with unit balances, pending transfers and call ledger, or closeout
components and exposures. Margin-period days use the declared observation anchor;
default-to-closeout days are a separate output.}
\details{Signed values aggregate only within sets before positive-part flooring.
No cross-set offset, interpolation or automatic legal opinion is supplied.
Prior settlements precede calls, followed by immediate settlements. Target units
use declared valuation-date net value, price, FX and haircut. Pending units are
included in the committed balance to prevent duplicate calls. MTA is measured as
gross reporting market value; future pending settlements remain visible.

Positive collateral is recognized after received haircut. Nonremote posted
collateral is a full nominal return claim, while posted haircut only sizes required
posting. Enforceable posted-claim netting gives max(V-received+posted,0); otherwise
exposure is max(V-received,0)+posted. Reuse does not imply reinvestment benefits.
Initial units are the balance before first-date events; pre-snapshot unsettled
obligations require an explicitly extended scenario.

At default, frozen units are revalued at closeout market, FX and haircut; later
margin is ignored. Outstanding intervening claims are included exactly once.
Same-day intraday default cashflows are outside this date-only contract. Missing
path extension fails. The caller establishes the margin observation anchor;
regulatory MPOR floors and capital approval are not inferred.}
\references{BCBS CRE50.15--19 and 50.23, published 5 July 2024,
\url{https://www.bis.org/committees/bcbs/basel-framework/standard/cre/50/inforce/2019-12-15/published/2024-07-05}.}
