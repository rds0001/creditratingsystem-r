\name{vintage_backtest}
\alias{vintage_backtest}
\title{Fixed Origination Vintage Backtest}
\usage{
vintage_backtest(vintage_table, account_predictions,
    account_outcomes,
 origin_cohorts, denominator_contract, design, uncertainty,
 cluster_resamples = NULL, confidence_level = 0.95, context = NULL)
}
\arguments{
\item{vintage_table}{Existing operational vintage cells, for example build_vintage_performance output.}
\item{account_predictions}{Account/MOB frozen opening-date forecasts with model/version/parameter IDs and cumulative_pd/interval_pd.}
\item{account_outcomes}{Separate account/MOB dated observed targets with period_end, default_definition, complete/exited flags, cumulative_default and interval_default. Events require event_id and first_event_at; complete no-event observations require complete_through.}
\item{origin_cohorts}{Unique account ID, opening/knowledge dates, original_balance, currency and cluster_id membership.}
\item{denominator_contract}{fixed_origination method, count/original_balance weighting, first_competing_exit treatment, reference/default_definition and origination_month_end_plus_mob horizon convention.}
\item{design}{Frozen physical performing-at-origin validation target; whole-month horizon gives maximum MOB.}
\item{uncertainty}{none or cluster_bootstrap.}
\item{cluster_resamples}{Shared original sampling-cluster draws, preserving repeated accounts across MOBs.}
\item{confidence_level}{Interior confidence level.}
\item{context}{Optional matching context.}
}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Reconciles supplied operational vintage cells with fixed original membership, frozen account probabilities and independently supplied observed account outcomes.}
\details{Exits never shrink original denominators. Counts, original balances, default/incident/exit totals and complete follow-up must reconcile to operational cells. No substitute target builder is introduced. MOB uses cohort month-end, not equal elapsed days from every opening; MOB zero is the opening month's end. Cumulative forecasts must increase and consecutive interval forecasts equal their increments. Positive/exit events must be observed within the origin-to-end window; interval defaults belong to the named calendar month. Negative completed paths require coverage through the endpoint. Future-known evidence does not enter observed counts.

A cell is estimable only when calendar-mature and every original account has complete follow-up. Incomplete cells retain event counts and visible missing accounts but no fabricated rates or survivor reweighting. Metrics include cumulative and unconditional interval Brier, bias, MAE/RMSE and weighted means. Interval denominators are original cohorts, not survivor hazards. Original-balance weighting is monetary incidence, not account PD. Actual shared cluster resampling supplies conditional intervals; undefined replicates suppress intervals. No censoring adjustment or empirical model approval is inferred.}
\value{CalcResult with ValidationResult cell metrics, maturity, original denominators, remaining-first-event risk counts, explicit uncertainty and fingerprints.}
