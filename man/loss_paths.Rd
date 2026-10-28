\name{loss_paths}
\alias{expected_loss_from_paths}
\alias{lifetime_expected_loss}
\alias{event_conditioned_loss}
\alias{counterparty_expected_loss}
\alias{aggregate_portfolio_loss}
\title{Joint Path, Exclusive Event and Portfolio Loss Expectations}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Integrate losses on explicitly aligned probability scenarios. Preserve
joint default, LGD, exposure, discount and currency dependence, with finite
partition expectations and discrete portfolio tail allocations.}
\usage{
expected_loss_from_paths(default_indicator, lgd_paths, ead_paths,
  scenario_weights, discount_factors, scenario_ids, time_grid,
  currency, probability_measure, uncertainty = "none",
    context = NULL)
lifetime_expected_loss(marginal_pd, conditional_loss,
    discount_factors,
  time_grid, currency, probability_measure, context = NULL)
event_conditioned_loss(states, event_names, currency,
  probability_measure, context = NULL)
counterparty_expected_loss(default_indicator, net_exposure_paths,
  recovery_paths, scenario_weights, discount_factors,
    counterparty_id,
  netting_set_id, scenario_ids, time_grid, currency,
    probability_measure,
  uncertainty = "none", context = NULL)
aggregate_portfolio_loss(facility_loss_paths, portfolio_mapping,
  scenario_weights, fx_rates, scenario_ids, reporting_currency,
  probability_measure, quantile_levels, context = NULL)
}
\arguments{
\item{default_indicator}{Finite numeric matrix: scenarios in rows and periods in
columns. Entries are zero or one, with at most one first default per scenario.}
\item{lgd_paths}{Finite numeric matrix matching the default matrix. Raw economic
LGD may be negative or above one; values are not clipped.}
\item{ead_paths}{Nonnegative finite EAD matrix, matching default indicators.}
\item{scenario_weights}{Nonnegative finite vector of scenario probabilities,
one per row. Sum must be within 1e-12 of one; this small residual is reported and
weights are normalized. Zero-weight scenarios remain identified.}
\item{discount_factors}{Nonnegative finite scalar, period vector or matching
scenario-by-period matrix for joint path methods. For lifetime loss an explicit
period vector is required. Factors above one are allowed.}
\item{scenario_ids}{Unique nonempty character vector in global scenario order.}
\item{time_grid}{Positive, strictly increasing period-end times in years.}
\item{currency, reporting_currency}{Explicit nonempty reporting currency. Any
currency in context must match. No currency conversion occurs except through
explicit fx_rates in portfolio aggregation.}
\item{probability_measure}{PHYSICAL or RISK_NEUTRAL, consistent with context.
These produce different interpretations; the method does not transform measures.}
\item{uncertainty}{none, or iid for a Monte Carlo standard error using at least
two equal-probability paths. Independence is a caller assumption, not established
from the supplied values.}
\item{context}{Optional named calculation context or calculation-context result.}
\item{marginal_pd}{Vector of first-default period probability masses in [0,1]
whose sum is at most one within 1e-12. These are not cumulative PDs.}
\item{conditional_loss}{Finite vector of joint conditional monetary expectations
E[LGD * EAD given first default in period]. Negative economic losses are allowed.}
\item{states}{List of all exclusive Boolean event states, including zero-mass
states. Each row contains unique state_id, named logical events, scalar
probability in [0,1] and finite scalar conditional_loss. Probabilities sum to one
within 1e-12. State masses are used as given, without normalization.}
\item{event_names}{Unique nonempty character vector, at most twelve names.
Exactly all 2 to the power length(event_names) Boolean combinations are required.}
\item{net_exposure_paths}{Finite signed exposure matrix for one externally
established netting set. Negative values are floored at zero for counterparty loss.}
\item{recovery_paths}{Matching finite recovery matrix in [0,1]. Counterparty LGD
is one minus recovery.}
\item{counterparty_id, netting_set_id}{Explicit nonempty scalar identifiers.}
\item{facility_loss_paths}{List of facility rows with facility_id, currency,
scenario_ids and numeric losses. Each facility must cover exactly the global
scenario IDs; local order may differ and is explicitly aligned. Losses are already
monetary loss amounts under a common horizon and measure.}
\item{portfolio_mapping}{List of facility_id, portfolio_id, share rows. Shares
are finite scalar fractions in [0,1]. Repeated facility/portfolio pairs are
rejected; total allocation of any facility cannot exceed one within 1e-12.
Unallocated shares are returned.}
\item{fx_rates}{Named list by each facility currency. A positive scalar or
scenario vector supplies reporting-currency units per local-currency unit.
Scenario vectors use global scenario order. Reporting-currency FX must equal one.}
\item{quantile_levels}{Nonempty vector of distinct levels strictly between zero
and one. Level order is retained.}
}
\value{A CalcResult with monetary unit and explicit assumptions.
expected_loss_from_paths returns expected_loss, path_losses, period_contributions,
marginal_default_probability, default_weighted_lgd,
default_weighted_discounted_ead, conditional_independence_loss,
conditional_lgd_exposure_covariance_contribution and standard_error.
Undefined conditional means in zero-default-mass periods are NULL. Diagnostics
retain time grid, IDs, measure, residual and canonical input-path fingerprint.

lifetime_expected_loss returns lifetime_expected_loss, period_contributions and
total_default_probability. event_conditioned_loss returns expected_loss, state
contributions and probability_sum. counterparty_expected_loss extends joint-path
results with independent_default_exposure_loss and
 default_exposure_dependence_contribution; diagnostics retain the netting-set
identity and number of negative net values floored.

aggregate_portfolio_loss returns portfolios and unallocated_shares. Each sorted
portfolio contains aligned path_losses, expected_loss, named facility_expected_loss
and tail_metrics. Each tail includes level, value_at_risk, expected_shortfall and
named facility_es_contributions.}
\details{
For scenario s and period t, joint path loss is the sum of
D(s,t) * LGD(s,t) * EAD(s,t) * DF(s,t). The probability-weighted mean integrates
all supplied dependencies before aggregation. At most one first default prevents
counting the same default again in later periods. The conditional independence
comparator multiplies conditional mean LGD and conditional mean discounted EAD
within each default period, weighted by its default mass. The difference from
joint loss is the aggregate within-default covariance contribution; it is not
necessarily positive. The counterparty comparator instead separates default from
the complete recovery/exposure/discount product under the supplied scenarios.

The lifetime function multiplies marginal first-default mass by conditional
joint loss and fixed period discount. Stochastic loss/discount dependence needs
joint path integration. The event function integrates a full mutually exclusive
partition exactly once, which allows default and transfer or support events to
co-occur without counting their overlap twice. Event meanings and state losses
remain explicit caller inputs.

Portfolio paths are aligned, converted with scenario FX, allocated, and summed
before tail calculation. Weighted VaR uses the left inverse empirical CDF.
Expected Shortfall averages exactly the upper (1 - level) probability mass:
all mass strictly above VaR plus only the necessary fraction at VaR. Boundary
mass is allocated proportionally across tied scenarios, so facility tail
contributions sum to portfolio ES. Zero-weight scenarios do not affect the CDF.
The result is not the sum of standalone facility VaRs or a simple conditional
mean given loss greater than or equal to VaR.

Products use power-of-two scaling to avoid intermediate overflow when the final
product is representable; nonfinite final products or sums fail with controlled
NUMERICAL_FAILURE. Native floating-point underflow is still possible. No stochastic
paths are generated and no dependence model is inferred by these methods.
}
\section{Interpretation and limitations}{
The reported iid standard error measures Monte Carlo integration uncertainty
only. It excludes parameter, model and time-discretization uncertainty and is
not supplied for unequal scenario probabilities. Finite scenarios can instead
represent an explicitly supplied discrete probability law.

All inputs must share their declared scenario meanings, target event, horizon,
valuation date and measure. IDs only verify alignment, not economic compatibility.
A risk-neutral discounted credit loss is not automatically a complete CVA or
capital calculation. Legal netting, collateral closeout, own-default effects and
regulatory acceptance are not inferred. Signed raw economic LGD is retained in
general loss methods; the counterparty method explicitly uses bounded recovery.
}
\references{
Acerbi and Tasche (2002), On the coherence of Expected Shortfall,
\url{https://arxiv.org/abs/cond-mat/0104295}, Definition 2.6 and equation (3.4),
with sign and confidence-level convention translated to upper-tail monetary losses.
}
\examples{
r <- expected_loss_from_paths(matrix(c(1, 0), 2, 1),
  matrix(c(0.6, 0), 2, 1), matrix(c(100, 100), 2, 1),
  c(0.5, 0.5), 1, c("default", "survive"), 1,
  currency = "EUR", probability_measure = "PHYSICAL")
r$value$expected_loss
lifetime_expected_loss(c(0.1, 0.05), c(40, 50), c(1, 0.95), c(1, 2),
  currency = "EUR", probability_measure = "PHYSICAL")$value
}
