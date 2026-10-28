\name{counterparty_exposure_backtest}
\alias{counterparty_exposure_backtest}
\title{Validate Frozen Counterparty Exposure and Closeout Forecasts}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Physical per-netting-set exposure quantile and amount validation with separate mature observed-default closeout assessment.}
\usage{
counterparty_exposure_backtest(forecasts, observations,
    quantile_levels,
  design, uncertainty, cluster_resamples = NULL,
  confidence_level = .95, context = NULL)
}
\arguments{
\item{forecasts}{Dated common validation records plus frozen counterparty/netting-set identities, legal/CSA versions, sealed netting/collateral paths, optional scenario weights and joint default-event closeout contracts.}
\item{observations}{Matching observed or censored endpoint records with actual path artifacts, knowledge dates, reported exposure and optional observed default closeout.}
\item{quantile_levels}{Distinct probabilities strictly between zero and one.}
\item{design}{Frozen model, parameter, data, population, split, currency, physical horizon and evaluation-cutoff contract.}
\item{uncertainty}{\code{none} or \code{cluster_bootstrap}.}
\item{cluster_resamples}{Caller-supplied shared whole-cluster bootstrap draws.}
\item{confidence_level}{Interior percentile confidence level.}
\item{context}{Optional calculation context consistent with the design.}
}
\details{Quantiles use the weighted empirical inverse CDF; exceptions are strictly greater than the threshold. Atom-dependent model tail mass is reported separately from the nominal tail probability. Market observations require exact horizon alignment and known information. Frozen netting/CSA content is compared, and reported exposure reconciles to the exact per-set value without cross-set offset.

Default-conditioned closeout is separately calculated using the public frozen-collateral-unit routine and declared joint market/default path events. Only mature known closeouts are scored; missing defaults are not zero-loss observations. A mature closeout can remain applicable when a horizon market value is censored. Bootstrap intervals condition on the frozen paths and become non-estimable if a resample lacks required default outcomes.}
\value{A CalcResult with a ValidationResult, market exposure and quantile scores, separate conditional closeout errors, coverage, uncertainty and reconciliation findings. Reconciliation conflicts fail the declared contract; performance approval is not inferred.}
\references{Basel Committee on Banking Supervision, CRE50, publication 2024-07-05, definitions 50.15, 50.19 and 50.22--50.28. \url{https://www.bis.org/committees/bcbs/basel-framework/standard/cre/50/inforce/2019-12-15/published/2024-07-05}}
