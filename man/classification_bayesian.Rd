\name{classification_bayesian}
\alias{fit_bayesian_pd}
\title{Proper-prior Bayesian logit and probit posterior simulation}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Native multi-chain Metropolis estimation using explicit shared innovations, frozen posterior draws, uncertainty and actual chain diagnostics.}
\usage{
fit_bayesian_pd(X_train, y_train, feature_order, link, prior_mean,
  prior_sd, sampler_spec, group_spec = NULL, credible_level = .95,
  target, training, model_id, model_version, context = NULL)
}
\arguments{
\item{X_train,y_train}{Finite numeric matrix and observed numeric zero/one labels. Single-class training samples are permitted under proper priors.}
\item{feature_order}{Distinct ordered feature names. Features are already prepared; this estimator does not learn scaling.}
\item{link}{logit or probit.}
\item{prior_mean,prior_sd}{Finite normal prior means and positive finite SDs for intercept followed by slopes.}
\item{sampler_spec}{List with initial_positions (chain by parameter matrix), lower positive-diagonal proposal_cholesky, normal_innovations (chain by step by parameter array or nested lists), uniforms (chain by step matrix, values strictly in (0,1)), integer warmup, positive thin, and innovation_contract=INDEPENDENT_STANDARD_NORMAL_AND_UNIFORM. At least two chains and eight retained draws required.}
\item{group_spec}{Optional list of distinct observed levels, per-row labels and positive prior_sd for zero-mean group intercepts. The SD is fixed, not inferred. Unknown prediction groups are rejected.}
\item{credible_level}{Interior probability for equal-tail latent-PD credible intervals.}
\item{target}{Full event/horizon/units/probability-measure/conditioning contract.}
\item{training}{Training fingerprint, split_id and ISO known_at information cutoff. Caller supplies mature labels and valid temporal/group splits.}
\item{model_id,model_version,context}{Immutable model identifiers and optional calculation context.}
}
\details{
The posterior combines the Bernoulli likelihood with proper independent normal priors. Symmetric random-walk Metropolis uses caller-supplied independent Gaussian and uniform draws; no proposal adaptation or hidden random seed is used. Retained posterior draws, proposal, initial positions and innovation fingerprint are frozen for audit and prediction. Group intercepts implement partial pooling conditional on their fixed proper prior SD. Parameter order is intercept, slopes, then declared group levels.

Diagnostics include chain acceptance rates, rank-normalized and folded split-Rhat, Geyer initial-positive/monotone paired-sequence ESS for bulk/tails, and raw-quantity mean ESS. ESS is capped by actual draw count. Splitting uses equal first/last halves; an odd middle draw is omitted. Constant chains report unavailable Rhat and zero ESS. DIAGNOSTICS_PASSED requires every coefficient Rhat<=1.01 and bulk/tail ESS>=400. Otherwise a visible failed/insufficient status accompanies the draws; no convergence claim is made. Finite-chain diagnostics cannot prove convergence.

predict_pd averages probabilities over posterior draws, rather than applying the link to mean coefficients. Diagnostics contain equal-tail PD intervals, latent-PD posterior SD, mean-PD Monte Carlo standard error, prediction-specific chain diagnostics and Bernoulli predictive variance. Intervals and MCSE require representative stationary posterior draws, so inspect convergence status before use. These are intervals for conditional PD, not fractional outcome prediction intervals. For grouped fits prediction X is list(X=numeric_matrix,groups=labels); all groups must have been trained.

Prior sensitivity, especially with few defaults, requires refitting explicit substantive alternative priors. No heldout data select a prior implicitly. Model-based posterior uncertainty is conditional on likelihood, priors and independent-row assumptions; it does not establish empirical calibration or independence of repeated borrower forecasts.
}
\value{FitResult with a data-only ModelArtifact containing all retained posterior draws and chain diagnostics. Use predict_pd with the exact frozen target and features.}
\references{Vehtari et al. (2021), Rank-normalization, folding, and localization: An improved R-hat for assessing convergence of MCMC. Stan reference manual, Posterior Analysis and Effective Sample Size.}
\seealso{\code{predict_pd},\code{predict_model_score},\code{serialize_fitted_model}}
