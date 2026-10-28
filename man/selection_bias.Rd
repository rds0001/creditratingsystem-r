\name{selection_bias}
\alias{build_selection_dataset}
\alias{fit_selection_propensity}
\alias{selection_ipw_weights}
\alias{fuzzy_reject_augmentation}
\alias{parcel_rejected_applications}
\alias{fit_joint_selection_model}
\alias{estimate_population_risk_aipw}
\alias{reject_inference_bounds}
\alias{reject_inference_sensitivity}
\alias{prepare_reject_evaluation}
\title{Selection Mechanisms, Reject Inference and Honest Evidence Scopes}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Native dated selection panels, logit propensities, sequential inverse probability weights, hypothetical augmentation, binary bivariate-probit selection likelihood, augmented population means and partial-identification diagnostics. Missing own-contract outcomes never become measured validation labels.}
\usage{
build_selection_dataset(applications, decisions, bookings, outcomes,
  as_of, target_population, target, context = NULL)
fit_selection_propensity(X, selected, feature_order, mechanism,
    folds,
  row_metadata, target, training, model_id, model_version,
    context = NULL)
selection_ipw_weights(observed, propensities, target_weights = NULL,
  stabilization = NULL, trim = NULL, context = NULL)
fuzzy_reject_augmentation(outcomes, observed, baseline_pd,
    application_ids,
  augmentation_weight = 1, context = NULL)
parcel_rejected_applications(bands, accepted_defaults,
    accepted_counts,
  rejected_counts, log_odds_shift = 0, uniforms = NULL,
    context = NULL)
fit_joint_selection_model(X_outcome, X_selection, selected,
    outcomes,
  outcome_features, selection_features, target, training, model_id,
  model_version, rho_grid = NULL, selected_rho = NULL,
    exclusion = NULL,
  context = NULL)
estimate_population_risk_aipw(outcomes, observed,
    outcome_predictions,
  propensities, fold_lineage, target_weights = NULL,
    clusters = NULL,
  context = NULL)
reject_inference_bounds(outcomes, observed, target_weights = NULL,
  strata = NULL, risk_bounds = NULL, context = NULL)
reject_inference_sensitivity(baseline_pd, observed, outcomes,
  log_odds_shifts, target_weights = NULL, threshold = NULL,
    context = NULL)
prepare_reject_evaluation(candidates, evaluation_rows, as_of,
    target,
  outcome_comparability = NULL, context = NULL)
}
\arguments{
\item{applications}{List of unique application records containing application_id, entity_id and submitted_at. Grouping entities should identify independent people or households including repeat applications.}
\item{decisions}{Records with application_id, as_of, known_at, policy_version and decision: offered, rejected, withdrawn or not_decisionable.}
\item{bookings}{Records with application_id, as_of, known_at and logical taken_up/funded. Funding requires uptake and a recorded offer.}
\item{outcomes}{For the panel builder, records with application_id, as_of, known_at and outcome_scope. Own-contract records require exact target, status observed/immature/censored and binary outcome only if observed. External-contract records are retained separately. Elsewhere, aligned binary observed labels and missing unobserved labels: use a list containing NULL or a numeric vector with NA on unobserved rows.}
\item{as_of}{ISO audit cutoff; later-known information is excluded from panel construction and rejected for evaluation.}
\item{target_population}{Nonempty description of the target applicant population.}
\item{target}{Common target object: event, positive horizon, time_unit days/months/years, probability_measure PHYSICAL/RISK_NEUTRAL, conditioning. Propensity event must equal mechanism; joint outcome conditioning must be POTENTIAL_OWN_CONTRACT.}
\item{X}{Finite already-transformed selection-feature matrix; an n-by-zero matrix fits an intercept-only selection rate.}
\item{selected}{Numeric zero/one indicator of the specified selection mechanism, or of outcome observation for the joint likelihood.}
\item{feature_order}{Distinct feature names in exact matrix column order.}
\item{mechanism}{decision, takeup, funding or outcome_observation. Supply only rows eligible for this mechanism; for example, takeup is conditional on an offer.}
\item{folds}{List of records with nonempty unique zero-based train and validation indices, in both languages. Entities are disjoint and all training labels must strictly precede validation decisions. Validation rows cannot repeat.}
\item{row_metadata}{Aligned records with entity_id, as_of, features_known_at and label_known_at. Features must be known at decision time.}
\item{training}{Common artifact training metadata: known_at, split_id, data_fingerprint and optional data_origin. Propensity labels cannot exceed the cutoff. Joint data fingerprint is computed from the actual designs and labels.}
\item{model_id,model_version}{Nonempty frozen model identifiers.}
\item{observed}{Aligned numeric zero/one indicator that the actual own-contract target is observed.}
\item{propensities}{For IPW, a vector or rows-by-mechanisms numeric matrix of sequential conditional selection probabilities. For AIPW, aligned OOF probabilities. Positive retained target mass requires positive joint probability.}
\item{target_weights}{Nonnegative finite population masses, default one; total mass must be positive. AIPW warmup rows require zero target weight.}
\item{stabilization}{Optional vector with one marginal rate in (0,1] per mechanism. Its product is a common weight multiplier, not an identified target total.}
\item{trim}{Optional inclusive joint-propensity support c(lower, upper), lower strictly positive. Excluding mass explicitly changes the estimand; probabilities are not silently winsorized.}
\item{baseline_pd}{Aligned frozen baseline probabilities in [0,1]. Retain the originating model separately. No refit or pseudo-label validation occurs.}
\item{application_ids}{Unique nonempty application identifiers in input order.}
\item{augmentation_weight}{Nonnegative scalar hypothetical mass per unobserved applicant, divided into good/bad training weights.}
\item{bands}{Unique frozen band identifiers.}
\item{accepted_defaults,accepted_counts,rejected_counts}{Aligned integer band counts. Accepted counts must be positive, accepted defaults bounded by accepted counts, reject counts nonnegative. Unsupported bands require explicit treatment before this function.}
\item{log_odds_shift}{Scalar or per-band finite assumed log-odds shift of reject risk relative to observed band risk. Zero/one baseline rates remain at their boundaries; no hidden smoothing prior.}
\item{uniforms}{Optional list of explicit per-band uniforms in [0,1), exactly one per rejected record. Produces sampled hypothetical labels; otherwise fractional masses. No RNG is called.}
\item{X_outcome,X_selection}{Aligned finite numeric matrices for latent outcome and selection probit indices. Intercepts are fitted. Shared named columns must contain identical values.}
\item{outcome_features,selection_features}{Distinct column names for the respective designs.}
\item{rho_grid}{Optional nonempty unique correlations in [-.95,.95] for fixed-assumption likelihood profiles. NULL requests estimated correlation and requires an exclusion.}
\item{selected_rho}{Explicit selected member of rho_grid; required in fixed-correlation mode. No best scenario is selected as validation.}
\item{exclusion}{For estimated correlation, a list with features (selection-only columns) and substantive justification. Columns must add independent rank beyond the outcome design. Validity remains user asserted.}
\item{outcome_predictions}{Aligned OOF outcome-nuisance probabilities for AIPW.}
\item{fold_lineage}{List with rows under row_metadata contract, folds, and nuisance_prediction_origin equal to OUT_OF_FOLD for both supplied nuisance predictions. Positive target-weight rows require an OOF prediction. Externally supplied prediction provenance remains a caller declaration.}
\item{clusters}{Optional dependence-group IDs; defaults to metadata entity IDs. Repeated entities cannot be split across clusters. At least two positive-mass independent clusters are required.}
\item{strata}{Optional aligned textual stratum names, default one all-applicant stratum.}
\item{risk_bounds}{Optional named list of assumed c(lower,upper) missing-risk bounds for existing strata. Unspecified strata use c(0,1). Can encode an explicitly asserted monotonic-risk restriction.}
\item{log_odds_shifts}{Unique finite sensitivity-grid shifts applied only to missing potential outcomes. Observed outcomes are held fixed.}
\item{threshold}{Optional [0,1] population-risk threshold for crossing brackets between adjacent grid points; no unverified interpolation.}
\item{candidates}{Frozen records with model_id, model_hash, training_known_at and explicit training_entities list.}
\item{evaluation_rows}{Unique application_id records with entity_id, as_of, known_at and label_origin. OBSERVED_OWN_CONTRACT requires exact target, funded=TRUE, mature=TRUE and binary outcome. AUTHORIZED_EXPLORATION additionally requires local authorization_reference. SYNTHETIC_TRUTH requires exact target and synthetic data_origin. EXTERNAL_CONTRACT remains a separate proxy scope. HYPOTHETICAL/PSEUDO/UNOBSERVED rows never validate.}
\item{outcome_comparability}{Optional explicit target and transport_assumption for external-contract proxy evidence. Even when declared, the scope remains assumed external transport, not directly observed own-contract risk.}
\item{context}{Optional common calculation context.}
}
\details{
The panel preserves offered-but-not-taken-up, taken-up-but-not-funded, immature funded and censored funded strata. The caller constructs maturity/exit-policy targets before joining; partial default-free histories must not be supplied as mature nondefaults.

Propensity fitting calls the native binary GLM primitive, with independent forward/entity folds. Warmup OOF predictions remain NULL, never in-sample replacements. The final frozen model uses the declared training sample. Deterministic separation fails explicitly.

IPW is observed target mass divided by the product of sequential conditional propensities. Support restriction is reported with retained/excluded mass and Kish effective sample size. Conditional MAR and positivity are assumptions, not conclusions of fitting.

The joint binary likelihood is Phi(-b) for unselected rows, Phi2(a,b;rho) for selected defaults and Phi2(-a,b;-rho) for selected nondefaults. Native deterministic 64-node Plackett Gauss-Legendre quadrature and analytic scores drive bounded L-BFGS optimization, initialized by genuine separate probit fits. No continuous-outcome Heckman correction is substituted. Correlation/coefficient boundaries, tiny likelihood cells at or below 1e-14, separation and failed convergence are not accepted fits. Estimated correlation requires explicit exclusion assumptions; fixed-correlation covariance conditions on the supplied correlation. Observed-information covariance assumes IID observations. Hessian conditioning and weak identification are reported. Generic predict_pd returns unconditional Phi(X beta) for the exact potential-own-contract target and union feature order: outcome columns then selection-only columns.

AIPW averages m+S/e*(Y-m). Cluster influence totals give an empirical sandwich standard error and untruncated normal 95 percent interval. Adequate nuisance convergence and independent clusters are required. Out-of-domain estimates are reported rather than clipped. Double robustness concerns a population mean under MAR, not individual reject PD or general MNAR identification.

Worst-case binary bounds use observed default mass and unobserved mass. Optional restrictions remain assumptions. Log-odds sensitivity reports observed versus assumed components; correlation sensitivity is available through the joint model's fixed-rho profiles. Pseudo outcomes never become evaluation truth. Evaluation-only SplitPlans exclude training-entity or temporal contamination and expose distinct actual, external-proxy and synthetic-engineering scopes. No lending actions or pseudo-label performance rankings occur.
}
\value{CalcResult with method-specific panel, weights, scenarios, mean/influence uncertainty, partial-identification bounds, sensitivity surface or evaluation-only SplitPlan. Fit functions return FitResult with a common portable ModelArtifact and likelihood/OOF diagnostics. Hypothetical labels are marked validation-ineligible; all-synthetic evaluation carries synthetic provenance.}
\references{
Ehrhardt et al., Reject Inference Methods in Credit Scoring. \url{https://adimajo.github.io/assets/publications/rejectInference.pdf}

Binary endogenous-probit sample selection documentation. \url{https://search.r-project.org/CRAN/refmans/endogenous/html/sampselect.html}

Robins, Rotnitzky and Zhao (1994), Estimation of Regression Coefficients When Some Regressors Are Not Always Observed. \doi{10.1080/01621459.1994.10476818}
}
