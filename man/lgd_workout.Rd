\name{lgd_workout}
\alias{fit_recovery_hurdle}
\alias{estimate_open_workout_lgd}
\title{Residual Terminal Payouts and Open Workout Valuation}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Fit a cure and recovery hurdle with censored resolution timing, then value remaining terminal payouts without duplicating known cashflows.}
\usage{
fit_recovery_hurdle(episodes, cashflows, X_train, feature_order,
  stage_spec, target, training, model_id, model_version, currency,
  fit_intercept = TRUE, context = NULL)

estimate_open_workout_lgd(episodes, cashflows,
    fitted_recovery_model,
  X, valuation_contracts, future_cost_cashflows, feature_order,
  expected_target, stage_spec, as_of, currency, extrapolation,
  context = NULL)
}
\arguments{
\item{episodes}{List of episode records with unique episode ID; default date and knowledge date; reference date and knowledge date; feature observation and knowledge dates; reporting currency; default EAD; residual exposure at reference; nullable resolution date and knowledge date. Training also requires observation end. Observed resolutions require numeric zero/one cured and nonnegative cure value.}
\item{cashflows}{Realized cashflow records with unique cashflow ID, episode ID, cashflow date, knowledge date, reporting currency, nonnegative amount, and kind RECOVERY, DIRECT_COST or INDIRECT_COST.}
\item{X_train, X}{Numeric covariate matrices aligned to episodes. Empty feature designs have the appropriate number of rows and zero columns.}
\item{feature_order}{Distinct covariate names in frozen order.}
\item{stage_spec}{Exact frozen residual stage contract. See Details.}
\item{target, expected_target}{Physical given-default target with event residual_terminal_loss_fraction, currency, time unit days, positive horizon, and horizon_role TIMING_REPORTING_REFERENCE. Expected target must match the fitted target.}
\item{training}{Frozen training metadata with data fingerprint, split ID, known_at and workout_population open_and_closed_defaulted_landmarks.}
\item{model_id, model_version}{Nonempty model identifiers.}
\item{currency}{Explicit reporting currency; episode and modeled payout amounts must already use this currency.}
\item{fit_intercept}{Whether cure, positive-payout and positive-amount designs include an intercept. The AFT timing design includes its usual intercept.}
\item{context}{Optional calculation context compatible with target, currency, model version and evaluation date.}
\item{fitted_recovery_model}{Frozen recovery hurdle FitResult or model artifact.}
\item{valuation_contracts}{One per episode: discount_curve, fx_contract, future_resolution_dates, resolution_time_convention RIGHT_ENDPOINT, nullable tail_discount_factor and tail_assumption_reference, and unmodeled_future_costs_assumption NONE_BEYOND_SUPPLIED.}
\item{future_cost_cashflows}{Explicit dated future cost records in reporting currency, known by as_of, with IDs distinct from realized flows. These are unconditional costs, not survival-weighted estimates.}
\item{as_of}{ISO evaluation date.}
\item{extrapolation}{Explicit error, flat or parametric timing extrapolation policy.}
}
\details{
The stage requires payout_definition RESIDUAL_TERMINAL_GROSS_RECOVERY_OR_CURE_VALUE;
cashflow_pattern SINGLE_TERMINAL_PAYOUT_AFTER_REFERENCE;
timing_outcome_independence CONDITIONAL_ON_REFERENCE_COVARIATES;
cashflow_completeness COMPLETE_THROUGH_OBSERVATION_END;
payout_horizon ULTIMATE; positive_distribution lognormal;
timing_distribution weibull, lognormal or loglogistic;
a nonnegative cure_residual_value_ratio; and a nonempty landmark_definition.
The complete stage object must match prediction. Completeness is a user assertion
about supplied histories, not an imputation of unreported payments.

The core target horizon is a resolution reporting reference. It does not truncate
ultimate terminal payout. \code{predict_lgd} returns payout_horizon ULTIMATE and
an undiscounted residual loss ratio, not whole-workout discounted LGD or loss at
the finite reporting horizon. If cure probability is p, positive recovery
probability among noncured cases is q, fixed cure ratio is c and positive
log-ratio parameters are mu and variance v, expected terminal value ratio is
p*c + (1-p)*q*exp(mu+v/2). Loss is one minus this ratio. Its quantiles respect
both the zero-recovery and cure-value atoms and can be negative.

Default and all reference features must be known at the reference landmark.
Only resolutions observed by observation end and known at training cutoff count
as events. Other episodes are right-censored. Training ratios require positive
residual exposure. At least one observed cure, one noncured zero payout and two
positive noncured payouts are required, with identified nonseparated designs
and positive lognormal dispersion. Observations have unit frequency.

Positive post-reference cash recoveries must occur at terminal resolution.
Cure asset value must equal residual exposure times the declared cure ratio and
cannot duplicate a terminal cash recovery. Costs are not gross payout targets.
The closed-case outcome factorization assumes conditional independence of
terminal outcome/amount and resolution timing, and independent right censoring
given reference covariates. Model availability and feature definitions must
match the prediction stage; later features are not silently substituted.

Open-workout valuation includes known past net flows once. Conditional remaining
survival is S(age+t)/S(age). Resolution mass in each grid interval receives its
right-endpoint default-anchored discount factor. Supplied future costs are
deducted once without inferred costs beyond the schedule. Unknown tail discount
value leaves final loss and final LGD null; within-grid amounts remain available.
A supplied conditional average tail discount factor requires an economic
assumption reference. Closed cases use actual cashflows and cure value without
forecast. Zero EAD leaves the ratio null and preserves monetary loss.
}
\value{\code{fit_recovery_hurdle} returns a FitResult with frozen gate models, positive amount regression, censored AFT model, target, stage, provenance and identification diagnostics. \code{estimate_open_workout_lgd} returns a CalcResult with per-episode known cashflows, cure value, future terminal value within the grid, supplied future costs, unresolved probability and value, economic losses and nullable final LGD.}
\references{
NIST/SEMATECH e-Handbook, Lognormal Distribution:
\url{https://www.itl.nist.gov/div898/handbook/eda/section3/eda3669.htm}.

lifelines WeibullAFTFitter parameterization:
\url{https://lifelines.readthedocs.io/en/latest/fitters/regression/WeibullAFTFitter.html}.
The specific hurdle factorization and terminal stage are explicit modeling assumptions.
}
\seealso{\code{predict_lgd}, \code{discount_recoveries}, \code{fit_aft_model}, \code{predict_event_curves}}
