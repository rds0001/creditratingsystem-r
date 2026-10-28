\name{lgd}
\alias{discount_recoveries}
\alias{workout_lgd}
\title{Dated Recovery Valuation and Raw Economic Workout LGD}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Value known realized recovery/cost cashflows using explicit discount
and FX contracts, preserving open-workout incompleteness and raw economic LGD.}
\usage{
discount_recoveries(cashflows, discount_curve, fx_contract,
    default_date,
  evaluation_cutoff, reporting_currency, context = NULL)
workout_lgd(ead_at_default, discounted_net_recoveries,
    episode_status,
  default_date, evaluation_cutoff, currency, context = NULL)
}
\arguments{
\item{cashflows}{Data frame or list of rows with unique cashflow_id,
cashflow_date, known_at, currency, kind and nonnegative amount. Kind is RECOVERY,
DIRECT_COST or INDIRECT_COST. Dates are ISO calendar dates.}
\item{discount_curve}{Named list with anchor_date equal to default_date, known_at,
discount_basis description and points containing cashflow_date/discount_factor.
Factors are finite nonnegative exact-date values; the anchor factor, if supplied,
is one. No interpolation is performed.}
\item{fx_contract}{Named list with reporting_currency, known_at, conversion_basis
(cashflow_date or default_date), and rates containing currency/date/rate. Rates
are reporting-currency units per cashflow-currency unit, strictly positive, with
explicit rate one for reporting currency.}
\item{default_date, evaluation_cutoff}{Default and inclusive evaluation dates.
Default cannot follow evaluation. Cashflows occur at or after default, become
known no earlier than realization, and are included only if realized and known
by evaluation. Curve and FX contracts must be known by evaluation.}
\item{reporting_currency, currency}{Explicit nonempty currency, consistent with
any calculation context currency.}
\item{context}{Optional calculation context or context result.}
\item{ead_at_default}{Finite nonnegative exposure denominator.}
\item{discounted_net_recoveries}{Finite signed scalar or discount_recoveries
CalcResult with matching currency, default date and evaluation cutoff.}
\item{episode_status}{OPEN or CLOSED, as established by caller evidence.}
}
\value{A CalcResult. discount_recoveries returns discounted gross recovery, costs,
net recovery, signed individual contributions and excluded cashflows with reasons.
Diagnostics retain valuation dates, discount/FX convention and input fingerprint.
workout_lgd returns economic_loss, economic_lgd_to_date, economic_lgd, completeness,
denominator status and the input exposure/net recovery. Final economic_lgd is NULL
for OPEN workouts; both ratios are NULL at zero EAD while monetary loss remains.}
\details{Each contribution is amount times FX times discount factor; costs reduce
net recovery. Exact date/currency keys are required, and duplicates are rejected.
Future or not-yet-known cashflows are excluded visibly, not replaced with forecasts.
The supplied discount and FX contracts define the economic valuation convention;
no regulatory rate is supplied.

Loss is EAD minus net recovery. Raw economic LGD can be below zero or above one and
is never clipped. An open workout does not establish a final loss merely because
data ended. These methods do not estimate remaining recovery. Products use scaling
to avoid intermediate overflow; nonfinite final products, sums or ratios fail.}
\examples{
workout_lgd(100, 80, "CLOSED", "2024-01-01", "2024-06-30", "EUR")$value
workout_lgd(100, -20, "OPEN", "2024-01-01", "2024-06-30", "EUR")$value
}
