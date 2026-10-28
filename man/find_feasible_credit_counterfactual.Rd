\name{find_feasible_credit_counterfactual}
\alias{find_feasible_credit_counterfactual}
\title{Finite-Domain Feasible Credit Counterfactual Search}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Enumerate an explicit candidate domain under structural constraints and declared costs, preserving search limits and failed candidates.}
\usage{find_feasible_credit_counterfactual(predictor, record, candidates,
  model_contract, feasibility_contract, cost_contract, target,
    candidate_ids,
  max_candidates = 10000, policy_evaluator = NULL,
    policy_contract = NULL,
  context = NULL)}
\arguments{
\item{predictor}{Frozen local function producing one scalar prediction per row.}
\item{record}{Numeric baseline vector in model feature order.}
\item{candidates}{Finite candidate matrix or row lists in model feature order.}
\item{model_contract}{Frozen model identity, feature order, output scale and target as for ICE.}
\item{feasibility_contract}{Explicit bounds, immutable/actionable partition, integer, one-hot, linear and derived constraints as for ICE.}
\item{cost_contract}{cost_id, version, metric, feature_weights and feature_scales. Both named maps cover every feature. Weights are nonnegative with positive total, scales positive. Metric is WEIGHTED_L1, WEIGHTED_L2 or WEIGHTED_CHANGE_COUNT.}
\item{target}{target_id, version and kind. MODEL_OUTPUT specifies operator <=, >= or INTERVAL and scalar value or two closed endpoints. POLICY_ACTION specifies an actions list.}
\item{candidate_ids}{Distinct candidate identifiers.}
\item{max_candidates}{Positive integer examination budget, including infeasible candidates.}
\item{policy_evaluator}{Optional full-policy replay function required for policy-action targets; follows the ICE binding contract.}
\item{policy_contract}{Frozen policy hash and fixed-source bindings as for ICE.}
\item{context}{Optional common calculation context.}
}
\details{For changes divided by feature scales, L1 sums weighted absolute changes, L2 takes the square root of weighted squared changes, and change-count sums weights for exactly changed features. Candidates are visited by cost then ID. Infeasible candidates are reported without prediction. Every evaluated failure remains visible. A complete finite search reports FEASIBLE_SOLUTION or NO_FEASIBLE_SOLUTION. SEARCH_LIMIT remains the status whenever candidates remain unevaluated, even if a feasible witness exists. Optimality concerns only the supplied finite domain and declared costs, never continuous or causal global optimality. Policy targets replay full gates rather than equating low risk to approval. No real credit action is executed.}
\value{CalcResult with search status, best witness, all evaluated/successful candidates, unevaluated IDs, domain scope, baseline and contract hashes.}
\seealso{\code{compute_credit_ice}}
