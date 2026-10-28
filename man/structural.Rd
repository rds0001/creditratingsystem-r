\name{structural}
\alias{merton_equity_value}
\alias{infer_merton_assets}
\alias{distance_to_default}
\alias{merton_default_probability}
\alias{first_passage_pd}
\alias{simulate_structural_paths}
\alias{calibrate_distance_pd_mapping}
\title{Asset-based valuation, terminal default, first passage and empirical calibration}
\usage{
merton_equity_value(asset_value, debt_face, asset_volatility,
    risk_free_rate,
  maturity, payout = 0, currency, context = NULL)
infer_merton_assets(equity_value, equity_volatility, debt_face,
    risk_free_rate,
  maturity, asset_bounds, volatility_bounds, currency, payout = 0,
  tolerance = 1e-9, max_iterations = 1000, context = NULL)
distance_to_default(asset_value, default_barrier, asset_volatility,
  physical_drift, horizon, payout = 0, context = NULL)
merton_default_probability(asset_value, debt_face, asset_volatility,
  drift_or_rate, horizon, measure, payout = 0, context = NULL)
first_passage_pd(asset_value, barrier_spec, volatility, drift,
    horizon,
  measure, payout = 0, context = NULL)
simulate_structural_paths(asset_dynamics, barrier_schedule,
    time_grid,
  innovations, measure, jump_counts = NULL, jump_innovations = NULL,
  bridge_uniforms = NULL, context = NULL)
calibrate_distance_pd_mapping(distances, outcomes, metadata,
    weights = NULL,
  evaluation_distances = NULL, bootstrap_indices = NULL,
  confidence_level = .95, out_of_bounds = "error", context = NULL)
}
\arguments{
\item{asset_value}{Strictly positive initial asset value.}
\item{debt_face, default_barrier}{Nonnegative terminal debt or threshold in the same currency as assets.}
\item{asset_volatility, volatility}{Nonnegative annualized asset volatility.}
\item{risk_free_rate}{Annual continuously compounded risk-free rate.}
\item{maturity, horizon}{Nonnegative years; inversion requires positive maturity.}
\item{payout}{Annual continuous payout yield, subtracted from drift.}
\item{currency}{Explicit common currency for monetary valuation inputs.}
\item{context}{Optional consistent calculation context.}
\item{equity_value, equity_volatility}{Strictly positive observed equity price and annualized volatility.}
\item{asset_bounds, volatility_bounds}{Strictly positive lower and upper inversion bounds.}
\item{tolerance}{Maximum permitted scaled residual for each inversion equation.}
\item{max_iterations}{Positive integer optimizer iteration budget.}
\item{physical_drift}{Physical expected annual asset return before payout.}
\item{drift_or_rate, drift}{Physical expected return or risk-free rate according to measure.}
\item{measure}{PHYSICAL or RISK_NEUTRAL, explicitly required.}
\item{barrier_spec}{Named list kind constant or exponential, initial_level, and growth_rate for exponential.}
\item{asset_dynamics}{List asset_value, volatility, drift and optional payout, jump_intensity, jump_log_mean, jump_log_sd.}
\item{barrier_schedule}{Nonnegative barrier at each grid time.}
\item{time_grid}{Strictly increasing years starting at zero.}
\item{innovations}{Paths by intervals matrix of supplied independent standard normals.}
\item{jump_counts}{Same-shaped matrix of independent Poisson(intensity times interval) counts.}
\item{jump_innovations}{Independent standard normals for aggregate conditional log jumps.}
\item{bridge_uniforms}{Optional same-shaped independent uniforms in (0,1), for pure diffusion and positive barriers only.}
\item{distances}{Finite observed distance-to-default signals.}
\item{outcomes}{Mature observed binary default outcomes.}
\item{metadata}{Common calibration target, sample window, split, data origin, population and outcome policy; requires PHYSICAL measure.}
\item{weights}{Optional nonnegative integer frequency weights for fitting.}
\item{evaluation_distances}{Optional evaluation signals; defaults to fitting signals.}
\item{bootstrap_indices}{Optional matrix of complete row resamples, zero-based in both languages. Requires at least two rows and unit weights.}
\item{confidence_level}{Pointwise percentile bootstrap quantile level in (0,1).}
\item{out_of_bounds}{error or clip for fitted mapping support.}
}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Granular structural methods with explicit probability measure and default-event contracts. All analytic valuation inputs are scalars; rates are annual and times are years.}
\details{Equity is valued as a terminal European residual claim. Inversion jointly matches price and equity volatility using native bounded optimization and refinement; failed equation checks raise NONCONVERGENCE. It does not estimate physical drift. Distance-to-default returns NULL standardized values and explicit status for deterministic cases. Terminal default includes equality at the terminal debt. First passage includes initial breaches and subsequent barrier crossings and supports constant or exponential boundaries.

Simulation uses exact log steps with compensated compound-Poisson lognormal jumps. Supplied draws determine every path. Brownian bridges detect continuous diffusion crossing against log-linearly interpolated positive boundaries. Bridges with jumps are rejected: grid monitoring may miss jump-path crossings. Returned default times are interval upper bounds. Asset paths remain hypothetical unabsorbed paths after default. Monte Carlo standard errors assume independent paths and exclude model and discretization error.

Empirical distance calibration fits a decreasing isotonic mapping using the common calibration artifact. Optional bootstrap refits use zero-based row indices and type-7 percentile quantiles, clipping each resample to its own support. These pointwise empirical summaries do not guarantee coverage for data-adaptive isotonic blocks. Independent time-separated validation remains necessary.}
\value{CalcResult. Equity value has equity_value, delta, d1, d2; inferred parameters have asset_value and asset_volatility; distance results have standardized and log distances and boundary_status. PD functions return a scalar probability. Simulation returns asset_paths, default_time_upper_bounds, survival_curve and survival_standard_error. Calibration returns calibration_artifact, evaluation_distances, pd and optional lower/upper bands.}
\references{Merton (1974), On the Pricing of Corporate Debt, equations 3b and 10--12, \doi{10.1111/j.1540-6261.1974.tb03058.x}. Black and Cox (1976), Valuing Corporate Securities, section III, equation 7, \doi{10.1111/j.1540-6261.1976.tb01891.x}.}
