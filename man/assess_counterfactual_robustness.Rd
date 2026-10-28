\name{assess_counterfactual_robustness}
\alias{assess_counterfactual_robustness}
\title{Shared Finite Counterfactual Robustness Scenarios}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Evaluate candidate feasibility and target success across declared model and measurement scenarios, retaining every failed variant in the denominator.}
\usage{assess_counterfactual_robustness(candidates, record, model_variants,
  scenarios, feasibility_contract, target, candidate_ids,
    nominal_variant_id,
  context = NULL)}
\arguments{
\item{candidates}{Finite candidate matrix or row lists in frozen feature order.}
\item{record}{Numeric baseline vector.}
\item{model_variants}{Named list of predictor and model_contract entries sharing feature order, scale and target. Policy targets also require policy_evaluator and policy_contract.}
\item{scenarios}{Distinct scenario_id, model_variant_id, additive delta vector and nonnegative weight records. At least one weight is positive.}
\item{feasibility_contract}{Declared structural feasibility contract as for ICE.}
\item{target}{Versioned MODEL_OUTPUT boundary/interval or POLICY_ACTION target as for finite counterfactual search.}
\item{candidate_ids}{Distinct candidate identifiers.}
\item{nominal_variant_id}{Named variant used for the separately reported unperturbed nominal evaluation.}
\item{context}{Optional common calculation context.}
}
\details{The same additive measurement perturbation applies to baseline and all candidates. Immutable features mean no action, not no measurement uncertainty. Constraints are checked again. Every supplied scenario remains in the normalized denominator, including failed evaluations. A batch callback failure makes that entire scenario unevaluable; no selected rows are silently retried. Feasibility bounds assign unknown feasibility to failure and success respectively. Worst/best target margins are NULL if any margin is unavailable; policy targets have no numeric margin. All-scenario success includes zero-weight scenarios, while weighted fractions follow declared mass. Feature ranges and failure co-occurrence are descriptive, not causal attribution. Finite scenario fractions are not real intervention probabilities or continuous worst-case guarantees.}
\value{CalcResult containing nominal and scenario candidate results, conservative success fractions, feasibility bounds, missing-evaluation mass, margins, perturbation summaries and contract hashes.}
\seealso{\code{compute_credit_ice}, \code{find_feasible_credit_counterfactual}}
