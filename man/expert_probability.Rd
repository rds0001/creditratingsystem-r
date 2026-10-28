\name{expert_probability}
\alias{fit_elicited_beta_prior}
\alias{update_expert_beta_binomial}
\alias{pool_expert_probability_distributions}
\alias{update_odds_from_evidence}
\alias{check_expert_preference_consistency}
\alias{assess_expert_rating_agreement}
\alias{compile_discrete_bayesian_network}
\alias{infer_discrete_credit_network}
\title{Documented Expert Probabilities, Agreement and Exact Finite Networks}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Fits elicited proper Beta priors, updates compatible observations, pools distributions, applies joint likelihood evidence, diagnoses judgments, and performs bounded exact Bayesian-network variable elimination.}
\usage{
fit_elicited_beta_prior(elicitation, elicitation_contract,
  max_cdf_error = .01, context = NULL)
update_expert_beta_binomial(prior, defaults, exposures,
  observation_contract, predictive_exposures = 0,
  credible_level = .95, context = NULL)
pool_expert_probability_distributions(components, weights,
  support, rule, exclusion_reasons = NULL, context = NULL)
update_odds_from_evidence(prior_pd, evidence_groups,
  dependence_contract, previous_evidence_ids = NULL, context = NULL)
check_expert_preference_consistency(comparisons, items,
  incomplete_policy, tolerance = 1e-8, context = NULL)
assess_expert_rating_agreement(first_ratings, second_ratings,
  ordered_grades, rater_pair, weighting, missing_policy,
  bootstrap_indices = NULL, confidence_level = .95, context = NULL)
compile_discrete_bayesian_network(nodes, network_contract,
  elimination_order = NULL, max_factor_cells = 1000000,
    context = NULL)
infer_discrete_credit_network(compiled_network, evidence,
  query_node = NULL, context = NULL)
}
\arguments{
\item{elicitation}{List with method \code{moments}, interior mean and exactly variance or concentration; or method \code{quantiles}, increasing interior quantiles/probabilities and positive weights.}
\item{elicitation_contract}{Population ID, reference, known-at ISO date, distinct expert/evidence IDs and explicit default target.}
\item{max_cdf_error}{Maximum acceptable quantile CDF residual, strictly between zero and one.}
\item{context}{Optional calculation context, consistent with any declared default target.}
\item{prior}{Sealed Beta artifact returned by elicitation or a previous posterior.}
\item{defaults, exposures}{Integer compatible counts, zero to one million, defaults no larger than exposures.}
\item{observation_contract}{Matching target/population, new evidence IDs, knowledge cutoff, and explicit true conditional-iid and outcome-complete flags.}
\item{predictive_exposures}{Future integer size, zero to 100000, for Beta-binomial predictive probabilities.}
\item{credible_level}{Interior probability for equal-tail posterior credible intervals.}
\item{components}{Matrix whose rows are normalized masses or constant bin densities on common support.}
\item{weights}{Nonnegative opinion weights with positive total; normalized internally.}
\item{support}{List with kind \code{discrete} and distinct ordered string values, or kind \code{histogram} and increasing numeric breaks.}
\item{rule}{\code{linear} or \code{logarithmic}.}
\item{exclusion_reasons}{Named reasons for every zero-weight component, using zero-based index strings.}
\item{prior_pd}{Prior probability, including zero or one.}
\item{evidence_groups}{List of unique group IDs, distinct evidence IDs, finite positive joint likelihood ratio and reference.}
\item{dependence_contract}{With multiple groups, explicit conditional independence under both default and survival, and a supporting reference.}
\item{previous_evidence_ids}{Previously applied evidence, forbidden from reuse.}
\item{comparisons}{Positive reciprocal square comparison matrix. Missing pairs must be missing symmetrically. Diagonal entries are one.}
\item{items}{Ordered distinct item labels.}
\item{incomplete_policy}{\code{error} or \code{connected_components}; disconnected priorities have no identified common scale.}
\item{tolerance}{Positive log-ratio consistency tolerance, not a universal regulatory threshold.}
\item{first_ratings, second_ratings}{Aligned grade vectors or lists; NULL/NA represents missingness.}
\item{ordered_grades}{Complete ordered grade labels, including unused categories.}
\item{rater_pair}{Two distinct rater IDs.}
\item{weighting}{\code{unweighted}, \code{linear} or \code{quadratic} similarity weights.}
\item{missing_policy}{\code{error} or \code{pairwise_complete}.}
\item{bootstrap_indices}{Optional matrix of zero-based paired-complete resamples, at least two rows, each retaining sample size.}
\item{confidence_level}{Interior probability for paired bootstrap percentile intervals.}
\item{nodes}{Lists with node ID, states, parents, origin (ELICITED/ESTIMATED/FIXED), reference, known-at date and complete conditional probability table.}
\item{network_contract}{Network ID/version, population, as-of/known-at dates, default node/state and explicit default target.}
\item{elimination_order}{Every node exactly once; default reverse lexical topological order.}
\item{max_factor_cells}{Positive maximum CPT or intermediate factor size, at most ten million.}
\item{compiled_network}{Sealed compiled finite network artifact.}
\item{evidence}{Named observed node states; an empty list means no evidence.}
\item{query_node}{Queried node, defaulting to the declared default node.}
}
\details{
Targets contain event \code{default}, positive horizon, time unit (days/months/years), PHYSICAL or RISK_NEUTRAL measure, and conditioning. Elicitation and observations retain distinct evidence. Moment priors use alpha=mean*concentration and beta=(1-mean)*concentration. Quantile fitting minimizes weighted squared CDF residuals in log shapes bounded by [-18,18], using native nlminb from starts (1,1), (2,18), (18,2), with residual, boundary and local-Jacobian identification checks. Prior concentration is not an empirical count.

Conjugate updates add defaults to alpha and nondefaults to beta. Posterior mean, median, credible interval and prior weight are separate. Future count probabilities integrate binomial probabilities over the posterior Beta. Existing evidence IDs cannot be reused; current prior availability cannot be later than observations. Complete conditionally iid sampling is a supplied assumption, not an inferred property.

Linear pools average normalized densities. Logarithmic pools exponentiate weighted log densities and normalize on declared finite support. Disjoint positive-weight support is an error. Histogram densities are constant in bins; cumulative output is at right endpoints. Opinion correlation is not independent evidence.

Likelihood updates add joint log likelihood ratios. Distinct groups require conditional independence under both outcomes. Zero/one priors remain dogmatic under finite positive likelihood ratios.

Preference priorities solve log least squares under a zero-sum log-priority constraint in each connected component. Pair and triangle residuals and DFS cycle witnesses diagnose rather than repair inconsistency. Uncertainty is judgment disagreement only; no sampling interval is asserted.

Weighted kappa compares observed agreement with independent empirical rater marginals. Expected agreement within 1e-15 of one is degenerate. Optional paired percentile bootstrap assumes independent cases unless caller supplies a valid cluster resampling design. Any degenerate replicate suppresses the interval instead of being silently discarded. Agreement does not establish predictive validity.

Networks require every parent-state combination once, nonnegative CPT rows normalized within 1e-12, an acyclic graph and dated parameter provenance. Exact log-domain factor multiplication and elimination marginalize absent evidence. Querying an observed node still checks evidence likelihood. Impossible evidence errors; positive likelihood underflow is separately flagged with finite log likelihood. Every intermediate obeys the frozen size limit. These are observational probability networks, not automatically causal models.
}
\value{A CalcResult whose value holds the sealed distribution/network, posterior/count distribution, pool, evidence contributions, preference diagnostics, agreement matrix/kappa, or exact queried probabilities and likelihood. Hashes identify frozen content, not authenticated authorship.}
\references{
SHELF: \url{https://shelf.sites.sheffield.ac.uk/}.
R Beta distribution: \url{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Beta.html}.
De Carvalho et al., logarithmic pooling: \url{https://arxiv.org/abs/1502.04206v2}.
Crawford and Williams (1985), A note on the analysis of subjective judgment matrices, Journal of Mathematical Psychology 29, 387--405.
Revelle, weighted kappa: \url{https://personality-project.org/r/psych/help/kappa.html}.
Scutari, exact inference: \url{https://www.bnlearn.com/about/teaching/eng-apm-inference.pdf}.
}
\examples{
target <- list(event="default", horizon=1, time_unit="years",
  probability_measure="PHYSICAL", conditioning="performing")
contract <- list(population_id="synthetic", reference="synthetic exercise",
  known_at="2024-01-01", expert_ids=list("one"),
  evidence_ids=list("elicitation-one"), target=target)
prior <- fit_elicited_beta_prior(
  list(method="moments", mean=.1, concentration=20), contract)
prior$value$alpha
}
