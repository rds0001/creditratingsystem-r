\name{simulate_asset_pool}
\alias{simulate_asset_pool}
\title{Dated asset pool cashflows with shared default events}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Evolves initially performing contractual claims under supplied physical scenarios, including competing prepayment, dilution, dated recovery and financed reinvestment.}
\usage{
simulate_asset_pool(asset_pool, scenario_contract, obligor_paths,
  asset_paths, shared_innovations, context = NULL)
}
\arguments{
\item{asset_pool}{Bound CreditAssetPool from build_credit_asset_pool.}
\item{scenario_contract}{Versioned scenario IDs, normalized weights, increasing ISO dates, currency, physical measure, conditioning, sampling design, explicit event ordering and principal-only reinvestment convention.}
\item{obligor_paths}{Complete scenario/default-group/date rows with conditional_pd and default_channel.}
\item{asset_paths}{Complete scenario/asset/date rows with prepayment probability/channel, dilution and recovery fractions, recovery delay days, recovery and servicing costs, interest multiplier, overdue_days and disputed flag.}
\item{shared_innovations}{Unique scenario/date/channel rows with uniform in [0,1). Referenced channels must be complete and unused channels are rejected.}
\item{context}{Optional result context matching currency, physical measure, conditioning and actual calendar-day horizon.}
}
\details{Default is absorbing and shared across each declared debtor group. Dilution proportionately reduces the remaining schedule. Default precedes coupon; otherwise coupon precedes prepayment or scheduled principal. Gross recovery and workout cost are booked once on their actual future date. Extra recovery dates add cash-only rows, not hazard draws.

Reinvestment purchases already observed warehouse claims after their purchase-date cashflows, using retained principal only. Dated eligibility, balance and cash constraints apply. Failed purchases are not retried; remaining cash is released at the exclusive revolving endpoint. Intermediate legal transfer-out requires sale-price mechanics and is rejected. Unpaid assets and recoveries remain explicit at the horizon; modeled credit loss is distinct from received recovery cash. Future originations and initial workout states are outside this engine.}
\value{CalcResult containing a hashed AssetPoolPaths artifact with cashflows, asset events, shared group events, acquisitions, ending states and source bindings.}
