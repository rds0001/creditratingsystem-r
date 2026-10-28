\name{asset_pool_backtest}
\alias{asset_pool_backtest}
\title{Observed Asset-Pool and Tranche Cash Validation}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Replay frozen pool forecasts and reconcile independent actual servicing, dated ownership, pool accounts and tranche cash.}
\usage{
asset_pool_backtest(asset_pool, pool_inputs, forecast_paths,
    forecast_inputs,
  servicing, allocations, pool_accounts, coverage, design,
    cash_contract,
  tranche_contracts = list(), tranche_payments = list(),
  ownership_history = NULL, tolerance = 1e-10, context = NULL)
}
\arguments{
\item{asset_pool}{Frozen CreditAssetPool artifact.}
\item{pool_inputs}{Original pool builder keyword arguments.}
\item{forecast_paths}{Frozen AssetPoolPaths artifact.}
\item{forecast_inputs}{Original simulator keyword arguments.}
\item{servicing}{Independent dated whole-asset principal and recovery ledger.}
\item{allocations}{Event-to-pool ownership shares and complete allocated amount maps.}
\item{pool_accounts}{Dated actual opening/closing cash and all receipts/disbursements.}
\item{coverage}{Per-asset completeness, through date, known-at, currency and exact target.}
\item{design}{Common physical validation design, currency and origin-known forecast date.}
\item{cash_contract}{Origin-known opening cash, date and currency.}
\item{tranche_contracts}{Origin-known tranche identifiers, notionals and currency.}
\item{tranche_payments}{Dated actual tranche principal and interest.}
\item{ownership_history}{Optional dated ownership assignments, otherwise original assignments.}
\item{tolerance}{Nonnegative reconciliation tolerance.}
\item{context}{Compatible optional calculation context.}
}
\value{CalcResult with ValidationResult metrics, explicit applicability, coverage exclusions, ledger violations and cash residuals.}
\details{Whole horizons mature before default/prepayment metrics. Incomplete coverage is unknown. Recovery ownership follows the referenced default claim; post-default claim sales require a separate ledger. Original static holdings must match for forecast comparisons. Dynamic or changed holdings receive ledger audits without risk-set-mismatched forecast scores. Tranche cash and principal caps are checked, but contractual waterfall priority, coupons and deficiency ledgers are not inferred. Purchase/sale consideration and completeness are supplied evidence. Forecast scenario weights do not yield empirical confidence intervals. A clean ledger is not release approval.}
\references{BCBS CRE40.93, dated 2019-12-15, transparency of receipts, payments and ledger entries. \url{https://www.bis.org/committees/bcbs/basel-framework/standard/cre/40/inforce/2022-01-01/published/2019-12-15}}
