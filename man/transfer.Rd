\name{transfer_events}
\alias{assess_transfer_applicability}
\alias{transfer_event_default_probability}
\alias{combine_default_transfer}
\alias{transfer_loss_components}
\title{Explicit Payment Restrictions and Transfer Default Risk}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Match dated payment restrictions, map events to payment default, combine default events, and integrate exclusive state losses.}
\usage{
assess_transfer_applicability(payments, event_policy, as_of,
    known_at,
  horizon_end, context = NULL)
transfer_event_default_probability(transfer_event_probability,
  event_to_default_probability, mitigation_contract, event_id,
    target,
  context = NULL)
combine_default_transfer(pre_transfer_risk, transfer_risk,
    dependence,
  joint_probability = NULL, context = NULL)
transfer_loss_components(combined_risk, conditional_losses,
    currency,
  applied_loss_effects, context = NULL)
}
\arguments{
\item{payments}{Payment rows with IDs, due and knowledge dates, source and settlement currencies, payer/payee and route jurisdictions.}
\item{event_policy}{Versioned named policy with explicit event types, jurisdictions, currencies, validity intervals, knowledge dates and evidenced exemptions.}
\item{as_of, known_at, horizon_end}{ISO dates for the inclusive payment window and information cutoff. Event validity ends exclusively.}
\item{transfer_event_probability}{Scalar event probability for the declared exposure target.}
\item{event_to_default_probability}{Conditional unmitigated default probability given the event.}
\item{mitigation_contract}{Named list with effect_id, success_probability_given_event_and_unmitigated_failure, conditioning equal to event_and_unmitigated_failure, and assumption_reference.}
\item{event_id}{Explicit event identifier.}
\item{target}{Named exposure_id, positive horizon, time_unit (days/months/years), probability_measure (PHYSICAL/RISK_NEUTRAL), and conditioning.}
\item{pre_transfer_risk}{Named stage PRE_TRANSFER, pd, target and applied_effects.}
\item{transfer_risk}{Transfer-default result with a compatible target.}
\item{dependence}{Explicit or independent, in lowercase.}
\item{joint_probability}{Intersection probability within the Frechet bounds; required for explicit dependence and absent for independence.}
\item{combined_risk}{Combined event-union result with stage TRANSFER_ADJUSTED.}
\item{conditional_losses}{Named monetary losses for neither, default_only, transfer_only and joint. Each is a joint conditional expectation, with discounting and recovery assumptions supplied by the caller.}
\item{currency}{Reporting currency.}
\item{applied_loss_effects}{Unique economic effect IDs; cannot repeat PD-level effects. Empty list explicitly means none.}
\item{context}{Optional calculation context.}
}
\details{Transfer default probability equals event probability times conditional failure times one minus conditional mitigation success. With base PD p, transfer PD q and intersection j, combined PD is p+q-j. Each exclusive state contributes exactly once to loss. Equal currencies do not exempt payment freezes. Missing applicability information is reported explicitly. Applicability screens the route at the payment due date; separate timed payment legs require separate rows. A knowledge cutoff later than as_of creates a retrospective view; historical backtests require the original available knowledge cutoff. No legal inference, automatic grade translation or joint loss averaging is performed. The caller supplies probabilities for identical targets and consistently identifies economic effects.}
\value{A CalcResult with applicability findings, a probability decomposition, an exclusive state partition, or expected monetary loss and state contributions.}
