\name{decisioning_policy_evaluation}
\alias{evaluate_decision_grid}
\alias{select_constrained_policy}
\alias{assess_policy_robustness}
\title{Held-Out Policy Evaluation, Constrained Selection and Robustness}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Replays frozen local policies, separates supplied prospective offer
economics from observed matched-funded evidence, and checks finite shared
perturbations without inferring causal policy effects.}
\usage{
evaluate_decision_grid(policy_candidates, cases, candidate_inputs,
  observed_outcomes, evaluation_profile, evaluation_cutoff,
    context = NULL)
select_constrained_policy(evaluated_grid, context = NULL)
assess_policy_robustness(policy, baseline_cases,
    perturbation_scenarios,
  robustness_profile, as_of, context = NULL)
}
\arguments{
\item{policy_candidates}{Records candidate_id, raw frozen DecisionPolicy value
in policy, and offer_rule_reference. Each complete candidate must match its
predeclared content hash.}
\item{cases}{Records case_id, decision_date, nonnegative weight, split_id,
prediction_reference, excluded_from_model_training=TRUE and
excluded_from_calibration=TRUE. Total weight must be positive.}
\item{candidate_inputs}{Complete candidate/case grid with candidate_id, case_id,
separate dated risk, affordability, eligibility and operations objects,
explicit nullable offer, and nonapproval_costs for REFER, DECLINE and BLOCKED.}
\item{observed_outcomes}{Records case_id, funded, complete, and explicit nullable
booked_offer_hash, funded_at, funding_known_at, outcome_end, known_at, defaulted,
economic_loss and net_profit. Funding and outcome knowledge are separate.}
\item{evaluation_profile}{Versioned profile_id, version, known_at, currency,
registered_at, outcome_access_date, evaluation_role, prediction_design, split_id,
data_fingerprint, target, candidate_hashes, objective, constraints, tie_break,
tolerance, loss_bounds and profit_bounds. See Details.}
\item{evaluation_cutoff}{ISO economic evidence cutoff.}
\item{evaluated_grid}{Bound DecisionGrid value or CalcResult.}
\item{policy}{Frozen DecisionPolicy value or CalcResult.}
\item{baseline_cases}{Records case_id, decision_date, weight, four source objects,
nullable offer_terms, utility_reference, utility_by_action and feasible_by_action.}
\item{perturbation_scenarios}{Records scenario_id, weight, perturbation_reference
and complete cases tables preserving baseline IDs, dates and case weights.}
\item{robustness_profile}{Versioned common profile plus target, scenario_design
(BOUNDED_GRID, BOOTSTRAP_REPLAY or STRESS_SET), generation_reference,
interpretation=PERTURBATION_NOT_CAUSAL and tolerance.}
\item{as_of}{ISO robustness evaluation date.}
\item{context}{Optional compatible physical target/currency calculation context.}
}
\details{Evaluation profile target_contract_version 1.1 requires a complete target with performance_origin first_draw_at and event default or first_default. The validation design repeats policy_target_contract_version 1.1 and performance_origin, preserving the exact original target. Missing version or 1.0 remains default-only; unknown versions are rejected. Observed decision stages do not imply observed economic outcomes.

Evaluation roles are VALIDATION, OOF_VALIDATION or TEST; prediction design is
HELD_OUT or OUT_OF_FOLD. OOF_VALIDATION requires OUT_OF_FOLD. Per-case model and
calibration exclusion and pre-registration are caller assertions, not an
independent audit. Register candidates, objective and constraints before declared
outcome access. Policies use the same physical default target with integer day
horizon. Historical policy knowledge cutoff equals the case decision date.

Approved cases need versioned offer_id, version, known_at, currency, target,
risk_conditioning=CONDITIONAL_ON_THIS_ACCEPTED_OFFER, assumption_reference,
support_status, and nullable takeup_probability, conditional_booked_expected_loss,
conditional_booked_npv, ead and offer_cost. SUPPORTED assumptions require finite
values; positive-weight unsupported approved offers make monetary metrics null.
Per-applicant NPV is takeup times book NPV minus offer cost; book NPV already
includes losses and funding. EL is a separate metric, not subtracted again.
Nonapproval costs are explicit. All metrics use the same total applicant weight.

Unfunded rows cannot supply fabricated defaults, losses or profits. Funded outcome
horizons begin at actual funding and equal the target horizon. Funding knowledge
cannot precede funding. Complete labels need outcome knowledge no earlier than
horizon end; incomplete labels remain null. Observed results require recommended
approval, exact booked-offer hash match, funding known by cutoff and, for mature
statistics, complete outcomes known by cutoff. Changed assumptions or versions
are deliberately not treated as equivalent offers. Missing funding, unfunded,
different offers and incomplete outcomes have distinct statuses. Rejected cases
are never counted as nondefaults. Loss/profit bounds cover only missing outcomes
inside the known matched-funded cohort, not unknown funding or causal policy
value. Full candidate causal value remains null.

Selection accepts only VALIDATION/OOF_VALIDATION. TEST grids may be evaluated but
cannot select policy. The objective is expected_npv_per_application and tie break
candidate_id. Each constraint has distinct constraint_id, metric, operator
(le/ge) and threshold. Metrics include approval_rate, refer_rate, decline_rate,
blocked_rate, expected_loss_per_application, expected_npv_per_application and
expected_funded_ead_per_application. Slacks and binding flags are recomputed.
Unidentified candidates are infeasible. Objective/constraint-direction Pareto
domination is checked before utility/ID ties. Empty feasible sets return
INFEASIBLE, not an arbitrary candidate. Full candidate and policy bindings are
verified; the selected policy is returned unchanged. Final assessment requires
an untouched test period.

Robustness replays one fixed policy on a finite shared scenario set. Scenario
weights are nonnegative and sum to one. Each case utility/feasibility map explicitly
covers APPROVE, REFER, DECLINE and BLOCKED, with nullable finite monetary utility
and nullable Boolean feasibility. Approved cases require offer_id, version,
amount and annual_rate terms. These supplied terms are not computed by the rule
policy. Action flips, changed input sources, both-approved amount/rate changes,
feasibility and regret remain separate. A changed source is associated with a
flip, not causal attribution.

Regret compares only explicitly feasible actions with known utilities. Unknown
positive-weight case/scenario comparisons propagate null worst-case regret.
Known infeasible action rates are accompanied by unknown-dimension findings.
Worst-case mean regret ranges only over supplied positive-weight scenarios.
Bootstrap validity and dependence are caller responsibilities; no automatic
confidence interval, counterfactual identification or continuous uncertainty
bound is asserted. No financial action is executed.
}
\value{CalcResult with bound DecisionGrid, PolicySelection or PolicyRobustness,
input and artifact hashes, complete rule traces, separate assumption/outcome
metrics, feasibility reports and explicit unidentified quantities.}
\references{\url{https://scikit-learn.org/stable/modules/classification_threshold.html}
separates probability fitting and threshold selection and warns against tuning
on the same training observations; reviewed 2026-10-13. Local contracts and
economic algorithms here are library-specific.}

