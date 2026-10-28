\name{hybrid}
\alias{combine_pd_signals}
\alias{fit_stacked_pd}
\alias{evaluate_model_graph}
\alias{decompose_hybrid_prediction}
\title{Compatible PD Pooling, Held-Out Stacking and Frozen Model Graphs}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Combines aligned probability estimates, fits an actual held-out penalized logistic meta-model, executes a typed frozen graph and explains its result relative to explicit baseline inputs.}
\usage{
combine_pd_signals(pd_signals, weights, combination_scale,
  combination_contract, context = NULL)
fit_stacked_pd(out_of_fold_base_predictions, outcomes,
    base_model_ids,
  split_plan, meta_model_spec, target, training, model_id,
    model_version,
  weights = NULL, context = NULL)
evaluate_model_graph(model_graph, input_data, parameter_snapshots,
    context)
decompose_hybrid_prediction(hybrid_result, baseline_inputs,
  decomposition_scale = "native", context = NULL)
}
\arguments{
\item{pd_signals}{Records with signal/model IDs, model version, ordered row IDs, PD vector, target, observation and knowledge dates. All signals must align exactly.}
\item{weights}{Nonnegative global pool weights, row-specific regime weights for mixtures, or optional aligned meta-training frequency weights for stacking.}
\item{combination_scale}{Linear, log_odds or mixture.}
\item{combination_contract}{Interpretation estimator_pool, log_odds_pool or latent_regime_mixture, plus reference. Mixture requires ordered regime IDs and conditional_regime_probabilities TRUE.}
\item{context}{Calculation context. Graph execution requires explicit dated context; decomposition preserves the original context.}
\item{out_of_fold_base_predictions}{Row-by-base probability matrix; missing predictions allowed only outside used held-out rows.}
\item{outcomes}{Aligned outcomes. Used training and held-out labels must be mature, known and binary. Unused outer test labels are not inspected.}
\item{base_model_ids}{Ordered distinct base IDs.}
\item{split_plan}{Calendar-clamp horizon rule, rows with IDs/entity/group/role and observation, label, coverage and prediction dates; forward folds with training/validation IDs, fit date and base-specific provenance.}
\item{meta_model_spec}{Input scale probability or log_odds and actual fit_penalized_glm arguments: penalty, lambda path, selected lambda, intercept and scaling, optional elastic-net ratio and solver controls.}
\item{target}{Default event, positive horizon, days/months/years unit, physical or risk-neutral measure and conditioning.}
\item{training}{Training data fingerprint, split ID and knowledge date; optional data origin.}
\item{model_id,model_version}{Explicit fitted artifact identity.}
\item{model_graph}{Graph ID/version, nodes and output node. Each node has ID, operation, named input ports, parameters and output contract.}
\item{input_data}{Named input snapshots with value, contract and optional already-applied effect IDs.}
\item{parameter_snapshots}{Named frozen model/calibrator artifacts.}
\item{hybrid_result}{Sealed evaluated graph, including input/parameter snapshots and trace.}
\item{baseline_inputs}{Exactly matching input snapshots with only values changed.}
\item{decomposition_scale}{Native output scale or log_odds for interior PD outputs.}
}
\details{
Pools normalize nonnegative weights. Log-odds pooling preserves finite-weight zero/one limits, rejects simultaneous positive-weight zero and one, and never clips. Mixtures use supplied row-specific conditional regime probabilities; they are not event unions. Mixture within- and between-regime Bernoulli variance sum to p(1-p). Dispersion is not uncertainty.

Stacking validates forward-time, entity and group separation. Every base lineage declares model ID/version, target, held-out origin, reference, training fingerprint, known-at and fit/preprocessing/selection row IDs. These must lie inside fold training rows. Base fits precede validation forecasts; known prediction dates cannot exceed forecasts. Used labels must be known and full horizons mature even for early known defaults. Negative labels require declared complete-through coverage. Date horizons use integer days or whole months, clamping month-end dates. Outer development/calibration/test entities and groups are disjoint. Only held-out development rows train the meta-model. Declared lineage is checked, not authenticated. Penalties are caller selected; no test-based selection occurs.

The meta-model uses actual native penalized binomial likelihood. Positive-weight constant bases are removed and frozen; all-constant models fit a real intercept and prediction flags deviations from the ignored constant training values. Log-odds inputs must be interior. No post-selection uncertainty is claimed. Use predict_pd with base-model IDs as feature order on the returned fitted artifact.

Graph contracts contain unit, target, ordered row IDs, as_of and known_at. Units are design, probability, loss_fraction and money. Designs add feature_order; money adds currency. Vectors exactly match rows without recycling. Supported operations are input (input_id), predict_pd (design port, snapshot_id), apply_calibrator (pd port, snapshot_id, base_model_version, optional groups), combine_pd_signals (ordered named signal ports and pooling parameters), log_odds_shift (pd port, aligned finite shifts, effect ID/reference), and expected_loss (pd/lgd/ead ports and loss assumption/reference). Frozen parameters must be known by the cutoff. New fitting, external state access, unknown operations/parameters and graph cycles are rejected. Financial feedback networks require their domain solver before insertion as typed inputs.

Expected loss requires performing PD, default-conditional LGD/EAD and matching event/horizon/measure/rows/dates, nonnegative monetary EAD and explicit deterministic_conditional_amounts or conditional_independence_EAD_LGD assumption. Raw economic LGD may exceed one or be negative. This product is not the expectation of generally dependent random exposure and severity. Repeated declared effects in ancestry or across loss legs are rejected. Identical computation content reuses a local cache. All snapshots, node results, effects and cache keys are retained with a content seal.

Decomposition verifies the seal and replays nodes with relative tolerance 1e-12 plus absolute 1e-14 for native numerical differences, with exact contracts and trace relationships. Each contribution changes one whole input from the common baseline, holding others at baseline. Final equals baseline plus those changes plus an explicit interaction remainder. These are neither causal effects nor Shapley values. Local native deltas for same-contract single-parent steps expose calibration/adjustment effects; they must not be summed across branches. Log-odds decomposition requires interior final probabilities in every scenario. Content hashes do not authenticate source evidence.
}
\value{A CalcResult with pooled values and diagnostics; a FitResult for stacking; a sealed graph CalcResult with replayable lineage; or baseline, input contributions, interaction remainder, local steps and exact reconciliation diagnostics.}
\references{Held-out stacked-generalization contract: \url{https://sklearn.org/1.7/modules/generated/sklearn.ensemble.StackingClassifier.html}. Pooling and decomposition implement the explicitly stated algebraic definitions.}
