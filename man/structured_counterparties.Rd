\name{stress_pool_counterparties}
\alias{stress_pool_counterparties}
\title{Counterparty netting, collateral and replacement cash stress}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Applies shared physical default to distinct existing monetary claims and supplied signed replacement marks with dated recoveries and no duplicate cash claims.}
\usage{
stress_pool_counterparties(baseline_cashflows, exposure_contracts,
  netting_sets, exposure_paths, collateral_paths, settlement_terms,
  scenario_contract, party_mapping, party_default_paths,
  shared_innovations, as_of, context = NULL)
}
\arguments{
\item{baseline_cashflows}{Scenario/cashflow_id/date/amount rows for existing incoming claims.}
\item{exposure_contracts}{Unique exposure/counterparty/netting identities, role/basis and disjoint linked cashflow IDs. Swap replacement marks cannot duplicate baseline cash.}
\item{netting_sets}{Explicit enforceability, unique received-transferable collateral contract and snapshot/default timing. Unenforceable sets are singletons.}
\item{exposure_paths}{Complete scenario/exposure/date signed_replacement_mark grid; zero added marks on cash-linked claims.}
\item{collateral_paths}{Complete scenario/set/date collateral values, haircuts and known_at dates no later than the snapshot.}
\item{settlement_terms}{Scenario/counterparty estate recovery fraction/capacity, recovery/collateral/replacement delays and additional replacement cost.}
\item{scenario_contract}{Versioned physical weighted scenario and risk-date contract.}
\item{party_mapping, party_default_paths}{Unique legal party/group mapping and complete conditional default hazard grid.}
\item{shared_innovations}{Exactly used scenario/date/channel uniforms in [0,1).}
\item{as_of}{Origin ISO date.}
\item{context}{Optional compatible physical calendar-day horizon context.}
}
\details{Default occurs after the known collateral snapshot and before same-day cash. Each unpaid linked cashflow is removed once. Signed replacement marks generate one replacement outlay/inflow, then net positive closeout receives collateral and residual estate recovery; negative closeout is paid once. Estate capacity and additional replacement cost apply once per counterparty across sets. Settlement dates outside the horizon are rejected.

Baseline less adjusted cash reconciles to credit loss plus additional cost. Role loss attribution is proportional to positive gross claims, an allocation convention after netting. Posted collateral is outside this received-collateral method. Legal enforceability and derivative prices are externally supplied, not estimated.}
\value{CalcResult with bound PoolCounterpartyStress containing adjusted cashflows, removed monetary IDs, source contracts, shared events, loss attribution and conservation residuals.}
