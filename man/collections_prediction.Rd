\name{predict_cure_and_redefault}
\alias{predict_cure_and_redefault}
\title{Ordered Cure and Subsequent Re-Default Projections}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Reuse a frozen Aalen--Johansen model to distinguish reversible state occupancy, sustained cure and first default after cure.}
\usage{
predict_cure_and_redefault(current_episode, model,
    action_conditions,
  horizon_grid, state_recoveries, target, currency,
  extrapolation = "error", context = NULL)
}
\arguments{
\item{current_episode}{Named list: episode_id, state, model_time, known_at and logical ever_default.}
\item{model}{Frozen multistate model with a collections_contract declaring cure_states, default_states, supported_actions, supported_initial_states and action_interpretation equal to observational_conditioning.}
\item{action_conditions}{Named list with action_id and assumption_reference.}
\item{horizon_grid}{Increasing nonnegative elapsed times from model_time, in the model target's units.}
\item{state_recoveries}{Horizon-by-state matrix of conditional period cashflow expectations, in model state order. These must not be cumulative recovery totals.}
\item{target}{Exact frozen physical event target.}
\item{currency}{Explicit cashflow currency.}
\item{extrapolation}{Default error rejects unsupported times and actions. Explicit flat continuation labels unsupported reference-model projections.}
\item{context}{Optional calculation context.}
}
\value{A CalcResult with state occupancy, sustained-cure probability, ordered cure-then-redefault probability, expected undiscounted recovery and support diagnostics.}
\details{Cure states must already encode the specified probation rule in training data. The augmented projection remembers cure and then marks the first subsequent default permanently. Historical default is never erased. Initial default occupancy does not count as re-default. Model training must be known by the episode cutoff. Action conditioning is observational and does not identify a causal treatment effect. Unsupported flat projections do not change model parameters.}
