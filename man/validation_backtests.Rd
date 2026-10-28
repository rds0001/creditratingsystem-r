\name{validation_backtests}
\alias{lgd_backtest}
\alias{ead_backtest}
\alias{survival_backtest}
\alias{population_stability}
\alias{rating_migration_backtest}
\title{Frozen Loss, Survival, Population and Migration Validation}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Dated observed-outcome backtests and descriptive population distances, separating metrics, uncertainty, applicability and decisions. Open workouts and censored endpoints are not fabricated as complete outcomes.}
\usage{
lgd_backtest(records, design, workout_policy, uncertainty,
  cluster_resamples = NULL, confidence_level = .95, context = NULL)
ead_backtest(records, design, weighting_policy, uncertainty,
  cluster_resamples = NULL, confidence_level = .95, context = NULL)
survival_backtest(predicted_event_curves, observations, horizons,
  censoring, design, uncertainty, cluster_resamples = NULL,
  confidence_level = .95, context = NULL)
population_stability(reference, current, binning, metrics,
    smoothing,
  design, reference_weights = NULL, current_weights = NULL,
    context = NULL)
rating_migration_backtest(records, predicted_transition_matrices,
  states, state_contract, design, uncertainty, weights = NULL,
  cluster_resamples = NULL, confidence_level = .95, context = NULL)
}
\arguments{
\item{records,observations}{Dated forecast records with unique prediction ID, entity/cluster/segment, as_of, forecast_known_at, horizon_end, matching model/parameter identities and outcome_known_at. Method-specific fields are described below.}
\item{design}{Model ID/version, parameter version, data fingerprint, population, split, HOLDOUT/EXTERNAL origin, evaluation cutoff and target. Target declares default event, horizon, unit, measure and conditioning.}
\item{workout_policy}{Closed-only with reference, or frozen closure IPCW with reference, identification assumption, model fingerprint/known-at and minimum observation probability.}
\item{weighting_policy}{Method equal, reference_limit or supplied, plus reference. Supplied weights use each row's weight field.}
\item{uncertainty}{None or cluster_bootstrap using supplied shared draws.}
\item{cluster_resamples}{At least two lists/vectors each sampling the original number of known sampling clusters with replacement. Entire clusters move together.}
\item{confidence_level}{Interior confidence level for percentile intervals.}
\item{context}{Optional matching target context; its knowledge date must equal evaluation cutoff.}
\item{predicted_event_curves}{Aligned row-by-horizon nondecreasing cumulative default probabilities.}
\item{horizons}{Increasing positive event-curve times within the target horizon.}
\item{censoring}{Complete-followup contract or frozen marginal IPCW step survival with model provenance, support, positivity threshold, independence and tie convention.}
\item{reference,current}{Numeric or categorical population values, with missing values separately identified.}
\item{binning}{Frozen binning ID/version/known-at and numeric cutpoints/reference support or categorical levels.}
\item{metrics}{Any of psi, jsd and numeric wasserstein.}
\item{smoothing}{Explicit nonnegative PSI pseudocount per fixed cell. JSD remains unsmoothed.}
\item{reference_weights,current_weights}{Optional aligned nonnegative observation weights.}
\item{predicted_transition_matrices}{One row-stochastic state matrix or one aligned matrix per forecast.}
\item{states}{Distinct ordered state IDs defining matrix rows and columns.}
\item{state_contract}{Scale ID/version/reference/known-at, disjoint default/cure/exit states and explicit absorbing states.}
\item{weights}{Optional aligned nonnegative migration weights.}
}
\details{
Outcome backtests require physical target probabilities and frozen predictions. Records must match declared model/parameter identities. Forecast knowledge cannot follow as_of. Loss and migration windows use integer days or whole calendar months with month-end clamping. Immature windows and future/unknown outcomes are excluded with reasons. Bootstrap intervals use type-7 percentiles conditional on frozen prediction and nuisance models; if any draw cannot estimate a metric, its interval is non-estimable instead of discarding that draw. Weight effective sample size measures concentration, not independent sample count. No automatic acceptance threshold or p-value is produced.

LGD conditions on default. Rows add default_date, predicted_lgd, OPEN/CLOSED workout status; completed rows add workout_closed_at, realized_lgd and nonnegative ead_at_default. Default lies between forecast date and horizon end; default-date LGD forecasts are allowed. Closure follows default and precedes outcome knowledge. Open rows never supply final realized LGD. Closed-only metrics explicitly describe a selected completed-workout population. IPCW requires closure_independent_of_final_lgd_given_recorded_covariates, frozen model fingerprint/known-at before forecasts and a positive interior minimum_observation_probability. Supplied per-row observation_probability lies in [0,1]; below-threshold support is excluded and the estimand is reduced explicitly. Count and EAD weighting are separate. Self-normalized inverse-closure weights produce bias (prediction minus outcome), MAE, RMSE, means and row bias contributions, overall/by segment with corresponding bootstrap intervals. Raw economic LGD is not clipped.

EAD is default-conditioned and requires matching design/row currency, strictly pre-default forecasts, nonnegative predicted/realized EAD, realized_ead_date equal to default_date, and reference drawn amount/limit. Unweighted and selected weights are reported separately. CCF equals (EAD_default minus drawn_reference) divided by (limit_reference minus drawn_reference). Fully drawn and overlimit lines have undefined CCF, not zero, while remaining EAD observations. No clipping occurs.

Survival conditions on performing. Each observation adds observed_time and Boolean event; FALSE means censoring. The design declares days_per_time_unit: days 1, months 30 or 365/12 or 365.25/12, years 365 or 365.25. These are elapsed-time conventions. Horizon end is as_of plus ceiling of target horizon times that factor; followup cannot be known before its corresponding elapsed date. Complete followup requires every nonevent strictly beyond the final horizon. Frozen IPCW supplies increasing times, nonincreasing survival, support_end, interior minimum_survival, model fingerprint/known-at, event_before_censoring tie rule and censoring_independent_of_event_features_and_across_clusters assumption. The routine does not fit censoring on test outcomes.

The censoring curve G(t)=P(C>t) is right continuous and one before its first knot. At h, event cases with time<=h use 1/G(time-), controls with observed time>h use 1/G(h). Censored observations at/before h contribute no invented label. Brier divides by the full eligible cohort, not IPCW sum; its finite-sample value can exceed one. Unsupported horizons receive no metric. Dynamic AUC compares cases/controls from different declared clusters, with half credit for score ties. Truncated concordance uses the last-horizon PD score, event time t_i<=last horizon and t_j>t_i in different clusters; event-time ties are excluded. Pair weights are 1/(G(t_i-)*G(t_i)), reducing to Uno 1/G squared under continuous censoring. Integrated Brier uses the supplied-grid trapezoidal average only. The marginal censoring contract cannot repair informative/feature-dependent censoring. Bootstrap holds the censoring model fixed.

Population stability adds reference_as_of/current_as_of to design; bins must be known before both samples and samples before evaluation. Numeric cutpoints assign equality to the lower bin and reserve a missing bin. Categorical bins reserve separate missing and unseen categories. Missingness and novel/out-of-support observations are reported separately. PSI uses explicit fixed-cell pseudocounts; without smoothing, unequal zero/nonzero masses yield null plus INFINITE, while two zeros contribute zero. JSD is unsmoothed symmetric KL-to-mixture in natural logarithms. Wasserstein is the first raw numeric distance excluding and separately reporting missingness. Empty weighted populations are non-estimable. No universal drift threshold or significance claim follows.

Migration conditions on origin_state. States and scale/version are fixed before forecasts; default, cure and exit roles are distinct. Nonnegative row-stochastic matrices are checked within 1e-12 without normalization; declared absorbing rows must be exact identity. Records add matching scale identity, origin state, observed/censored status, destination state and state_observed_at. Nonabsorbing endpoints require exact horizon observation; earlier observations imply endpoints only for declared absorbing states. Censored rows are not self-transitions. Observed departures from an absorbing state are reported as data/definition violations and excluded. Empty origin grades have no probability estimates or perfect goodness. Multiclass Brier sums K squared errors (range 0 to 2); impossible outcomes retain null plus INFINITE log loss. Destination-default share is not first-default incidence. Observed/expected counts, residuals and cluster-bootstrap transition intervals remain visible.
}
\value{CalcResult containing a ValidationResult payload with metrics, uncertainty, null p-value, NOT_ASSESSED decision, explicit applicability, design, coverage/exclusion reasons, diagnostics and content fingerprints. Population stability returns analogous descriptive distribution diagnostics.}
\references{
BCBS validation background: \url{https://www.bis.org/publications/studies-validation-internal-rating-systems-revised}.
Censored scoring documentation: \url{https://scikit-survival.readthedocs.io/en/stable/api/generated/sksurv.metrics.brier_score.html}, \url{https://scikit-survival.readthedocs.io/en/stable/api/generated/sksurv.metrics.cumulative_dynamic_auc.html}, \url{https://scikit-survival.readthedocs.io/en/stable/api/generated/sksurv.metrics.concordance_index_ipcw.html}. Discrete event-left/control-right and cross-cluster conventions are explicit local extensions derived from observation probabilities; no regulatory approval is claimed.
}
