\name{collections_backtest}
\alias{collections_backtest}
\title{Validate Dated Collections Cashflow Forecasts}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Factual fixed-horizon assessment of frozen repayments, recoveries, costs and recovery timing with explicit historical knowledge and complete cash-capture coverage.}
\usage{
collections_backtest(forecasts, episodes, cashflows, actions,
  cashflow_coverage, discount_contract, design, uncertainty,
  cluster_resamples = NULL, confidence_level = .95, context = NULL)
}
\arguments{
\item{forecasts}{Common validation forecast records plus account, sealed episode, index action and dated predicted cashflows.}
\item{episodes}{Sealed history from \code{build_collections_episodes} at the evaluation cutoff.}
\item{cashflows}{Unique dated episode cashflows with explicit REPAYMENT, RECOVERY or COST kind, nonnegative amount, currency and knowledge date.}
\item{actions}{Historically known action records; selected assignment or offer must match the sealed episode record.}
\item{cashflow_coverage}{Per-episode complete cash-capture intervals with knowledge dates.}
\item{discount_contract}{Explicit historically known flat effective annual rate greater than minus one, ACT_365F day count, matching currency and reference.}
\item{design}{Frozen model, parameter, split, population, currency, target and evaluation-cutoff identities.}
\item{uncertainty}{\code{none} or \code{cluster_bootstrap}.}
\item{cluster_resamples}{Supplied shared original-cluster bootstrap draws.}
\item{confidence_level}{Percentile coverage strictly between zero and one.}
\item{context}{Optional calculation context consistent with the design.}
}
\details{Amounts are discounted to forecast date by the effective annual rate over actual days divided by 365. Bias is prediction minus observation. Forecast and actual dates use an inclusive forecast-to-horizon window. Only mature horizons with complete cash capture and positive evaluation weight are scored. Late-known cash is excluded; state-history coverage alone cannot establish complete cash capture. Open episodes stay open. Recovery timing is recovery-amount-weighted days, defined only for positive forecast and observed recovery mass. Bootstrap intervals condition on frozen forecasts and become non-estimable when any supplied resample lacks the required outcome. Action-group metrics describe factual assignment cohorts, not causal effects.}
\value{A CalcResult containing a ValidationResult, amount and timing metrics, coverage, uncertainty and episode/action/cashflow traces. It does not grant release or empirical approval.}
