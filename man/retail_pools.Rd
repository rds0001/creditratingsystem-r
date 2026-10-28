\name{retail_pools}
\alias{fit_retail_risk_pools}
\alias{assign_retail_risk_pool}
\alias{estimate_pool_risk_parameters}
\title{Frozen Retail Pools and Local Risk Parameters}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Fit deterministic constrained adjacent score pools, replay frozen assignments and estimate separately weighted local PD, LGD, EAD and CCF with explicit maturity and default conditioning.}
\usage{
fit_retail_risk_pools(observations, pooling_spec, target, training,
  model_id, model_version, context = NULL)
assign_retail_risk_pool(observations, fitted_model, as_of,
  information_cutoff, expected_target, uncovered_policy,
    context = NULL)
estimate_pool_risk_parameters(pool_assignments, cohort_outcomes,
  workouts, estimation_profile, evaluation_cutoff, calibration_id,
  calibration_version, context = NULL)
}
\arguments{
\item{observations}{Training or scoring row lists/data frames. All rows identify observation_id, index_date, feature_observed_at, feature_known_at, declared strata and score. Training adds unique default_unit_id, role TRAIN, label_window_end, label_known_at, logical defaulted, nullable default_date, lgd_status, nullable lgd/ead and their nullable lgd_known_at/ead_known_at.}
\item{pooling_spec}{Dated spec_id, version, known_at, stratum_fields, score_field, segments, minimum_cases, minimum_defaults, minimum_lgd_count, minimum_ead_count, maximum_bin_mean_spread. See Details.}
\item{target, expected_target}{Exact target with event default, positive integer day horizon, PHYSICAL measure, conditioning not_defaulted_at_index, default_unit facility/obligor, default_definition and currency.}
\item{training}{Metadata with role TRAIN, known_at, split_id, data_fingerprint and optional data_origin.}
\item{model_id, model_version}{Explicit fitted model identity and version.}
\item{context}{Optional compatible dated and target context.}
\item{fitted_model}{Bound retail pool FitResult or frozen model artifact.}
\item{as_of, information_cutoff}{Scoring economic and knowledge dates; cutoff cannot follow as_of.}
\item{uncovered_policy}{Explicit unassigned or error.}
\item{pool_assignments}{Bound assignment CalcResult or its value.}
\item{cohort_outcomes}{Rows: observation_id, unique default_unit_id, index_date, label_window_end, label_known_at, outcome_status MATURE/CENSORED/OPEN_HORIZON, nullable defaulted/default_date, nonnegative reference_exposure, reference_known_at.}
\item{workouts}{Rows: episode_id, observation_id, default_date, default_known_at, ead_at_default, episode_status OPEN/CLOSED, valuation_known_at, reference_date, reference_known_at, reference_drawn, reference_limit, recovery_cashflows, discount_curve, fx_contract.}
\item{estimation_profile}{Dated profile_id, version, known_at; pd_weighting default_unit/reference_exposure; lgd_weighting episode/ead; ead_weighting episode/reference_exposure; ccf_weighting episode/undrawn; confidence; minimum_pool_cases; independence_assumption independent_default_units.}
\item{evaluation_cutoff}{ISO outcome and workout knowledge cutoff.}
\item{calibration_id, calibration_version}{Explicit local parameter-table identity and version.}
}
\details{
Strata explicitly include product, borrower_type, facility_type and
delinquency_state. Each segment declares segment_id, exact values for all strata,
finite score_min/score_max, interior increasing cutpoints and separate pd/lgd/ead
monotonicity directions none/increasing/decreasing. Score and strata must not be
direct outcome fields. Spread thresholds separately declare nullable pd/lgd/ead
maximum ranges of original nonempty-bin means; NULL disables a restriction.
Zero support minima explicitly permit absent conditional-parameter evidence.
Use lists for JSON-style arrays, including single-element and empty arrays.

Training features must be known by index. The whole horizon and label receipt
must precede training cutoff. Default dates are in (index, horizon end]. Nondefault
rows cannot carry LGD/EAD. Only CLOSED workout LGD enters as a numeric LGD label;
OPEN final LGD is NULL. Conditional outcome knowledge dates lie between actual
default and training cutoff. Missing numeric outcomes have NULL knowledge dates.

Initial bins are left closed/right open with the final endpoint included.
Greedy adjacent merging prioritizes undersupported blocks, choosing admissible
neighbors by the sum of squared normalized available parameter-mean differences,
then the leftmost index on ties. With no support merge available, it considers
adjacent monotonicity violations. Every merge must satisfy enabled original-bin
mean-spread limits. The complete merge path and separate PD/LGD/EAD diagnostics
are retained. This is a local heuristic, not a proof of global optimality or
infeasibility. Spread constraints describe between-bin means, not statistical
proof of homogeneity. Any remaining violation leaves the affected whole segment
INSUFFICIENT_SUPPORT and unassignable. No global monotone LTV law is assumed.

Assignment never refits the artifact. Missing features, unseen strata, out-of-domain
scores and unsupported segments remain uncovered. IDs bind model/version,
ruleset, segment and merged bin interval. Artifact integrity, target identity,
model availability and feature chronology are checked. A dated delinquency change
can legitimately change the pool.

Parameter estimation uses mature whole-horizon unit outcomes with exposure weights
frozen at index. Censored, unknown and unassigned outcomes are separate exclusions,
not nondefaults. A default unit appears once. Workouts must match cohort default
and reference date. discount_recoveries values explicit cashflows at the supplied
workout valuation knowledge date; workout_lgd and realized_ccf preserve raw economic
ratios, open-workout incompleteness and undefined zero denominators. One workout
per observation is allowed.

Table status describes mature-cohort support only; separate parameter statuses
identify defined and absent/zero-weight conditional samples.
Weighted estimates retain numerators, denominators, positive-weight counts,
observed ranges and independent-episode weighted-mean standard errors; fewer than
two contributing episodes leaves the error NULL. Open workouts, missing default
workouts, zero-EAD closed workouts and undefined CCF remain separate. Sparse pools
retain descriptive estimates with insufficient-support status.

calibrate_grade_pd without priors or monotonic recalibration supplies unweighted
exact binomial intervals; low_default_bound supplies a one-sided independent
binomial bound. These refer to unit counts, not exposure-weighted PD. Inference
is conditional on chosen pools, without pool-selection uncertainty. Local cohort
PD is not automatically TTC or long-run PD. Missingness, censoring, dependence
and representativeness require separate assessment; these routines do not establish
regulatory approval.
}
\value{FitResult with bound pooling ModelArtifact; CalcResult with bound RETAIL_POOL_ASSIGNMENTS; or CalcResult with a bound CalibrationArtifact of type RETAIL_POOL_RISK_PARAMETERS and pool risk table. The latter is not a score transform for apply_calibrator.}
\seealso{\code{serialize_fitted_model}, \code{calibrate_grade_pd}, \code{low_default_bound}, \code{discount_recoveries}, \code{workout_lgd}, \code{realized_ccf}}
