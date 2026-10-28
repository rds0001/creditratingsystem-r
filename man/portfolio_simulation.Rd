\name{portfolio_simulation}
\alias{prepare_credit_portfolio}
\alias{simulate_credit_portfolio}
\alias{simulate_portfolio_migrations}
\alias{stress_portfolio_factor_structure}
\title{Shared Default Groups, Portfolio Migration and Paired Dependence Stress}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Native portfolio preparation, factor or supplied-copula default paths, once-only terminal guarantee protection, physical rating migrations and paired stress comparisons. Defaults are shared across all positions in a declared default-equivalence group.}
\usage{
prepare_credit_portfolio(positions, entity_graph,
    default_link_policy,
  portfolio_contract, guarantee_allocations = NULL,
  netting_contracts = NULL, context = NULL)
simulate_credit_portfolio(prepared_portfolio, group_pds,
    dependence_contract,
  shared_innovations, conditional_loss_paths, levels = c(.95, .99),
  context = NULL)
simulate_portfolio_migrations(initial_grades, transition_matrices,
  shared_uniforms, migration_contract, valuation_contract = NULL,
  context = NULL)
stress_portfolio_factor_structure(prepared_portfolio, group_pds,
  dependence_contract, shared_innovations, conditional_loss_paths,
  stress_cases, levels = c(.95, .99), context = NULL)
}
\arguments{
\item{positions}{List of exposure-lot records: position_id, claim_id, facility_id, entity_id, currency, ead, facility_ead, sector, region, exposure_basis GROSS/NETTED, collateral_treatment NONE/ALREADY_IN_CONDITIONAL_LGD and optional netting_contract_id. Position and claim IDs are unique; all lots of a facility must conserve its common facility_ead.}
\item{entity_graph}{Sealed relationship graph from build_entity_graph, with the same as_of and known_at cutoffs as the portfolio.}
\item{default_link_policy}{Explicit default-equivalence policy consumed by assign_default_groups. No member PD is averaged or copied. Unresolved equivalence conflicts cannot be simulated.}
\item{portfolio_contract}{Plain JSON-compatible list containing portfolio_id, version, currency, ISO as_of/known_at, positive horizon, time_unit days/months/years, probability_measure PHYSICAL, conditioning initially_performing, event first_default. Guarantee use additionally requires guarantee_loss_rule FIRST_LOSS_CAP_IF_GUARANTOR_SURVIVES_HORIZON or PRO_RATA_EAD_CAPPED_IF_GUARANTOR_SURVIVES_HORIZON. Calendar horizons use end-of-month clamping.}
\item{guarantee_allocations}{Optional list of sealed simultaneous allocations from allocate_guarantee_coverage. Allocation cutoffs/currency and terminal claim due dates must match. Each guarantee capacity is consumed in only one allocation artifact. Claim owner and guarantor default group must be known.}
\item{netting_contracts}{Optional records containing netting_contract_id, facility_ids, currency, legally_effective TRUE, evidence_reference, valid_from, valid_until and known_at. Netted EAD is already net of the evidenced agreement; this routine performs no second offset. Unused contracts are rejected.}
\item{prepared_portfolio}{Sealed output of prepare_credit_portfolio, or its CalcResult. Tampered positions, protection or target metadata fail the portfolio hash.}
\item{group_pds}{Named list with exactly one probability in [0,1] per frozen default_group_id. These are physical first-default probabilities at the explicit portfolio horizon.}
\item{dependence_contract}{Plain JSON-compatible list with kind and ordered group_ids. SUPPLIED_DEPENDENT_UNIFORMS consumes group_uniforms. GAUSSIAN_FACTORS additionally declares ordered factor_ids, factor_correlation and group-by-factor loadings. The matrix is validated without substantive repair and systematic latent variance cannot exceed one.}
\item{shared_innovations}{Plain JSON-compatible list with ordered scenario_ids, group_ids, weights and sampling_design IID/FIXED_WEIGHTED_SCENARIOS/DEPENDENT_OR_QMC/EXACT_STATES. Supply scenario-by-group group_uniforms in [0,1], or scenario-by-factor factor_normals and scenario-by-group group_normals. Nested arrays are explicit lists for portable hashing. Gaussian common/residual coordinates must be independent standard normals under the declared sampling law.}
\item{conditional_loss_paths}{Plain JSON-compatible list binding ordered scenario_ids and position_ids to driver_hash of the dependence contract plus shared innovations, scenario-by-position ead and raw lgd, and guarantee_treatment NOT_YET_APPLIED. EAD is nonnegative; signed or above-one conditional LGD is retained. Arrays use explicit nested lists for portable hashing.}
\item{levels}{Distinct VaR/expected-shortfall levels in [0,1]. Discrete boundary mass and level-one conventions follow credit_loss_distribution.}
\item{initial_grades}{Named list with one known state for every migration group.}
\item{transition_matrices}{Square homogeneous physical transition matrix, or period-by-origin-state-by-destination-state numeric array/list according to matrix_mode. Rows must sum to one within numerical tolerance; only that rounding normalization is applied.}
\item{shared_uniforms}{Plain JSON-compatible list containing scenario_ids, group_ids, scenario-by-period-by-group values in [0,1), and nonnegative weights. Draws must satisfy the declared sequential conditional-uniform law. Same-period group draws may be correlated. Nested arrays are explicit lists for portable hashing.}
\item{migration_contract}{contract_id, version, ordered group_ids and states, default_state, logical allow_cure, probability_measure PHYSICAL, increasing positive time_grid, time_unit, matrix_mode HOMOGENEOUS/TIME_VARYING and innovation_law CONDITIONALLY_UNIFORM_GIVEN_PRIOR_STATES. Default is absorbing unless cure is explicitly allowed.}
\item{valuation_contract}{Optional plain list with valuation_id, version, currency, valuation_basis SUPPLIED_FROZEN_MARKED_VALUES, matching group_ids/states, group initial_values, group-by-state terminal_values_by_grade and scenario-by-group cash_distributions. Physical migration probabilities are not converted into pricing probabilities.}
\item{stress_cases}{List of distinct case_id records with dependence_contract, group_pds, conditional_loss_paths and recovery_response FIXED_PATHS/REPLACED_CONDITIONAL_PATHS. Fixed paths require unchanged EAD/LGD and identities. Every case reuses the same supplied random coordinates and has a recomputed driver_hash.}
\item{context}{Optional common calculation context consistent with currency, horizon, time unit, measure and conditioning where applicable.}
}
\details{
The prepared artifact binds facility lots, default groups and reservations with a content hash. Complete legal evidence, facility totals and default-link policy are supplied by the caller; they are not legally authenticated. Collateral recovery belongs in the conditional LGD path and is not reapplied.

For Gaussian factors, common normals are transformed by the declared PSD correlation matrix; systematic loadings plus independent residual normals produce unit-variance latent assets. Asset correlation is distinct from default-indicator correlation. Supplied dependent uniforms can instead come from the granular copula methods. Each group produces one default event that applies to all its facilities. EAD and LGD paths are bound to the same scenario/driver contract, preserving supplied dependence with recovery and exposure.

Gross loss is default indicator times EAD times raw LGD. Guarantees pay only positive remaining loss when the borrower defaults and the guarantor survives the terminal horizon. First-loss protection pays up to its reserved monetary cap. Pro-rata protection first scales positive gross loss by the covered-EAD fraction capped at one, then applies monetary and remaining-loss caps. These are explicit terminal-horizon payoff assumptions, not a regulatory capital substitution formula or a model of interim guarantee settlement.

Migration paths retain the initial grade, every period grade and first-default time. First-default time remains fixed after an explicitly allowed cure. The output separately reports weighted empirical terminal frequencies and products of the declared transition matrices. Arbitrary temporal dependence of supplied uniforms does not establish the Markov law. Optional marked loss equals supplied initial value minus supplied terminal mark minus cash distributions; signed valuation gains remain present.

Dependence stress preserves random coordinates and labels whether conditional recovery/exposure paths are fixed or replaced. Monetary comparisons are paired finite differences. Identical marginal PD parameters need not imply identical finite-sample default frequencies after a factor-structure change. No hidden PD uplift, covariance repair, collateral recovery, additional netting or risk-neutral conversion is performed.
}
\value{CalcResult containing a sealed prepared portfolio, joint default/monetary loss paths with weighted risk distribution, grade and first-default paths with optional marked values, or baseline/stress results with paired changes.}
\seealso{\code{assign_default_groups}, \code{allocate_guarantee_coverage}, \code{credit_loss_distribution}, \code{incremental_credit_risk}}
