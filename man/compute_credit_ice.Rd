\name{compute_credit_ice}
\alias{compute_credit_ice}
\title{Feasible Ordered Local Credit Prediction Paths}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Evaluate an explicit finite ICE path while retaining infeasibility, nonmonotonicity, scalar crossings, optional rating classes and full-policy replays.}
\usage{compute_credit_ice(predictor, record, grid, model_contract,
  feasibility_contract, varying_features, thresholds = NULL,
  rating_scale = NULL, policy_evaluator = NULL,
    policy_contract = NULL,
  context = NULL)}
\arguments{
\item{predictor}{Frozen local function returning one prediction per numeric input row.}
\item{record}{Numeric baseline feature vector.}
\item{grid}{Ordered numeric matrix or list of rows in varying_features order.}
\item{model_contract}{Frozen attribution model contract with ordered features, output scale and explicit target.}
\item{feasibility_contract}{contract_id, version, finite bounds for all features, actionable_features and immutable_features partition, integer_features, one_hot_groups, linear_constraints and derived_relations. Linear records have constraint_id, coefficients, operator and rhs. Derived records have constraint_id, output_feature, two input_features, and product or ratio operation. Empty lists are explicit.}
\item{varying_features}{Ordered names of grid columns.}
\item{thresholds}{Optional records with threshold_id, value and operator <= or >=.}
\item{rating_scale}{Optional versioned rating scale for probability output only.}
\item{policy_evaluator}{Optional local function(record, prediction) returning decision (sealed DecisionArtifact) and explained_input_hash of the list containing record and prediction. Record is a plain ordered list.}
\item{policy_contract}{policy_hash, source_mode REPLAY_FULL_POLICY and fixed_source_hashes named list identifying any unchanged risk, affordability, eligibility or operations inputs.}
\item{context}{Optional common calculation context.}
}
\details{Bounds, immutable features, integers and one-hot identities are checked before prediction. Linear and derived identities use relative tolerance 1e-10; zero-denominator ratios fail. Infeasible points remain NULL with findings and no predictor or policy invocation. Gaps are never interpolated. Turns are along the supplied ordered path and do not imply a sorted feature axis or a unique continuous threshold. Rating classes use map_pd_to_grade; policy decisions require a separately bound full-policy replay. Content binding is a caller assertion, not authentication. Feasibility is declared structural feasibility, not proof of empirical support or real intervention success. No actual credit action is executed.}
\value{CalcResult with grid records, predictions, findings, threshold states, adjacent changes, turns, optional grades and sealed decisions, and contract hashes.}
\seealso{\code{map_pd_to_grade}, \code{apply_credit_decision_policy}}
