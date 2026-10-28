\name{simulate_trade_receivables}
\alias{simulate_trade_receivables}
\title{Invoice cashflows with shared debtor, seller and servicer events}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Separates debtor credit loss, commercial dilution, capped seller recourse and aggregate operational losses on observed static invoice claims.}
\usage{
simulate_trade_receivables(invoice_pool, scenario_contract,
    party_mapping,
  party_default_paths, invoice_scenarios, seller_terms,
    servicer_terms,
  shared_innovations, context = NULL)
}
\arguments{
\item{invoice_pool}{Bound static CreditAssetPool of zero-interest bullet invoices.}
\item{scenario_contract}{Versioned physical scenario IDs, weights, dates, currency, sampling design and conditioning.}
\item{party_mapping}{Unique party_id to default_group_id mapping across every debtor, seller and servicer.}
\item{party_default_paths}{Complete scenario/default-group/date rows with conditional_pd and default_channel.}
\item{invoice_scenarios}{Scenario/invoice actual payment date, dilution date/fraction and debtor recovery date/fraction/cost.}
\item{seller_terms}{Scenario/seller settlement, performing and default-estate capacities, default recovery fraction and credit-only or credit-and-dilution recourse scope.}
\item{servicer_terms}{Scenario/servicer remittance date, commingling recovery fraction/capacity and replacement cost.}
\item{shared_innovations}{Exactly used scenario/date/channel uniforms in [0,1).}
\item{context}{Optional compatible physical target and actual calendar-day horizon.}
}
\details{Actual delayed payment stays exposed until payment, with default before same-day payment. Commercial dilution precedes that event and remains distinct; full dilution creates no workout cost. Seller recourse settles after debtor recovery and aggregate capacity is allocated once across eligible invoices. Seller cash goes directly to investors.

Before-failure collections held by a servicer share one capped commingling claim. Collections on or after failure redirect directly. Replacement cost is charged once. Same legal parties share their default group across roles. All cash dates must be within the explicit horizon. Principal less credit, dilution, commingling and costs reconciles to investor cash. Legal enforceability, future originations and early-settlement discounts are not inferred.}
\value{CalcResult with bound TradeReceivablePaths, source-separated dated cash, shared events, invoice/seller/servicer attribution and scenario conservation residuals.}
