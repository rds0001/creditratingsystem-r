\name{decision_policy_backtest}
\alias{decision_policy_backtest}
\title{Historical Decision-Policy Backtest}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Audit frozen policy decisions, actual offers and funding stages, matched booked outcomes, declared scenario constraints and paired policy sensitivity.}
\usage{
decision_policy_backtest(evaluated_grid, grid_inputs,
    historical_cases,
  design, reference_candidate_id, uncertainty = "none",
  cluster_resamples = NULL, confidence_level = .95,
  tolerance = 1e-10, context = NULL)
}
\arguments{
\item{evaluated_grid}{Sealed DecisionGrid from evaluate_decision_grid.}
\item{grid_inputs}{Exact original named arguments to evaluate_decision_grid, without context.}
\item{historical_cases}{Dated validation records with case and historical candidate identifiers, sealed historical decision, actual offer/take-up stages and their knowledge date, nullable offered limit and booked principal, optional known complete application costs.}
\item{design}{Physical target, currency, split, data fingerprint and cutoff matching the frozen grid.}
\item{reference_candidate_id}{Reference policy for paired scenario differences.}
\item{uncertainty}{None or cluster_bootstrap.}
\item{cluster_resamples}{Supplied whole-cluster draws.}
\item{confidence_level}{Interior percentile interval level.}
\item{tolerance}{Nonnegative absolute/relative numerical reconciliation tolerance.}
\item{context}{Optional compatible calculation context.}
}
\value{CalcResult with ValidationResult, factual and scenario metrics, paired contrasts and cluster uncertainty, historical audits, constraints and counterfactual applicability.}
\details{Evaluation profile target_contract_version 1.1 requires a complete target with performance_origin first_draw_at and event default or first_default. The validation design repeats policy_target_contract_version 1.1 and performance_origin, preserving the exact original target. Missing version or 1.0 remains default-only; unknown versions are rejected. Observed decision stages do not imply observed economic outcomes.
No lending action or fit is executed. Actual funding requires take-up and an offer. Whole offer hashes bind terms, so changed price/limit scenarios do not inherit factual credit labels. Unfunded outcomes are unknown. Offered limits require frozen offer contract_terms.limit. Mature matched-funded risk is separate from assumed offer economics. Schema 1.1 approval applicability and additional gates are replayed by the real evaluator. Declared scenario violations and historical inconsistencies block reconciliation. Agreement alone does not approve a policy. Full-policy causal value remains unidentified; this validator does not assert an off-policy identification design.}
\references{Ehrhardt et al., Reject Inference Methods in Credit Scoring. \url{https://adimajo.github.io/assets/publications/rejectInference.pdf}}

