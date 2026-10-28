\name{decisioning_offer_economics}
\alias{compute_limit_frontier}
\alias{compute_risk_based_price_grid}
\alias{evaluate_treatment_candidates}
\title{Local Limit, Quote and Treatment Scenario Economics}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Enumerates supplied local offers and action-conditioned cashflows
with explicit risk, affordability and eligibility assumptions. No decision is
executed and no causal price or treatment effect is estimated.}
\usage{
compute_limit_frontier(offer_candidates, constraint_profile,
  as_of, context = NULL)
compute_risk_based_price_grid(quote_candidates, pricing_profile,
  as_of, context = NULL)
evaluate_treatment_candidates(treatment_candidates,
    treatment_profile,
  as_of, context = NULL)
}
\arguments{
\item{offer_candidates}{Records with offer_id, version, known_at, currency,
subject_id, amount, integer tenor_days, assumption_reference, target, pd, lgd,
ead, risk_conditioning=CONDITIONAL_ON_THIS_OFFER, risk_reference,
affordability_status, explicit nullable affordability_maximum_amount and
eligibility_status. One subject and distinct offer IDs required.}
\item{constraint_profile}{Versioned profile with profile_id, version, known_at,
currency, target, maximum_amount, maximum_ead, maximum_expected_loss,
maximum_concentration, positive concentration_denominator,
existing_group_exposure and nonnegative tolerance.}
\item{quote_candidates}{Versioned offer records as above, annual_rate, target,
risk_conditioning=CONDITIONAL_ON_THIS_ACCEPTED_QUOTE, risk_reference,
takeup_probability, takeup_reference, upfront_fee, origination_cost,
offer_cost and periods. Each period specifies payment_date, scheduled_principal,
conditional_default_probability, conditional_prepayment_probability, lgd,
recovery_date, payment_discount_factor, recovery_discount_factor,
servicing_cost and prepayment_fee.}
\item{pricing_profile}{Versioned common profile plus default_timing=BEFORE_DUE,
prepayment_timing=AFTER_COUPON, funding_treatment=DISCOUNT_CURVE_ONLY,
selection_assumption=QUOTE_CONDITIONED_INPUTS_NOT_CAUSAL.}
\item{treatment_candidates}{Records with treatment_id, version, known_at,
subject_id, currency, action, prediction_reference, support_status,
affordability_status, eligibility_status, uptake_probability, offer_cost,
accepted_upfront_cost, rejected_present_value and cashflows. Each cashflow has
cashflow_id, date, expected_recovery, expected_cost and discount_factor.}
\item{treatment_profile}{Versioned common profile plus subject_id, baseline_id,
baseline_present_value, identification=CONDITIONAL_SCENARIO_NOT_CAUSAL and
allowed_actions.}
\item{as_of}{ISO decision and present-value anchor date. All profiles and
candidate assumptions must already be known.}
\item{context}{Optional calculation context, compatible currency and physical
probability measure.}
}
\details{
Targets specify physical default, positive integer horizon, time_unit=days and
explicit conditioning. Limit candidates share one comparison target, although
tenors may differ. Price targets span the entire quote tenor and end on its last
payment. Offer-conditioned risk is supplied, never copied implicitly from a
baseline borrower PD.

Limit EL is PD times conditional economic LGD times EAD. Concentration is existing
group exposure plus candidate EAD divided by the declared fixed post-decision
denominator. Numerical tolerance applies in the units of each comparison.
Slacks, violations and binding flags remain separate from affordability and
eligibility gates. Unknown affordability caps are infeasible. Feasible offers
are enumerated without monotone-risk assumptions. Same-tenor domination means
at least as much amount, no more EL and one strict improvement. No hidden
cross-tenor utility or interpolation is used.

Pricing starts survival at one. Default and prepayment probabilities are mutually
exclusive conditional on opening survival; masses are survival times their
respective hazards. Surviving principal follows the supplied full amortization
schedule. Fixed simple ACT/365F coupons use opening principal. Default before
due misses coupon and exposes opening principal; recovery is principal times
one minus LGD at the supplied later date. LGD describes undiscounted net recovery
at that date, and may be signed. An already discounted LGD would double discount.
Prepayment after coupon returns all opening principal plus its fee. Continuing
accounts pay coupon and scheduled principal. Servicing cost applies to all
opening survivors. Full surviving repayment is required; no residual value is
assumed. Same-date funding discount factors must agree within a quote.

Booked NPV subtracts initial amount and origination cost, adds upfront fee and
discounted expected cashflows. Funding enters only through discount factors.
Reported EL is already reflected in recovery cashflows and must not be subtracted
again. Per-offer NPV is takeup times booked NPV minus offer cost, paid once
regardless of uptake. No demand elasticity or unique break-even rate is inferred.

Treatment expected recovery/cost cashflows are already event-probability-weighted
within acceptance. Accepted value subtracts accepted upfront cost; total expected
value mixes uptake and explicit rejected value and subtracts offer cost once.
The baseline is an independent comparator. Unsupported predictions give null
values and cannot be ranked. Supported but unaffordable treatments retain their
value and infeasibility reason. Allowed supported feasible treatments rank by
expected value, ties by ID. Historical selection and missing counterfactuals
are not repaired by this conditional scenario comparison.
}
\value{CalcResult with SHA-256-bound LimitFrontier, PriceGrid or
TreatmentEvaluation, candidate fingerprints, monetary components, separate
feasibility reasons and declared assumptions.}
