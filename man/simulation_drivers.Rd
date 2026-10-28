\name{simulation_drivers}
\alias{simulate_operating_cashflows}
\alias{simulate_asset_values}
\title{Explicit Operating Cashflows and Physical Asset Paths}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Translate supplied quantity, price, cost and market scenarios into
reconciled operating cashflows and physical asset values.}
\usage{
simulate_operating_cashflows(driver_paths, operating_contract,
    context = NULL)
simulate_asset_values(asset_parameters, market_paths,
    context = NULL)
}
\arguments{
\item{driver_paths}{Records with scenario_id, period_start, period_end, currency,
base_quantity, base_unit_price, base_unit_variable_cost, base_fixed_cost,
volume_multiplier, price_multiplier, variable_cost_multiplier,
fixed_cost_multiplier, working_capital_revenue_ratio, cash_taxes, capex,
asset_disposals and other_operating_cash. Periods are contiguous, monetary
amounts are period totals. Taxes and other operating cash may be signed.}
\item{operating_contract}{List with contract_id, version, currency, period_unit
(day/month/quarter/year), quantity_unit, initial_working_capital and logical
include_asset_disposals_in_cfads.}
\item{asset_parameters}{List with asset_id, currency, valuation_date, initial_value,
salvage_value, initial_age_years, economic_life_years, depreciation_model
(straight_line/geometric_to_salvage), idiosyncratic_volatility,
probability_measure (PHYSICAL) and positive year_basis_days. Geometric ageing also
requires nonnegative annual_depreciation_rate.}
\item{market_paths}{Records with scenario_id, date, nonnegative market_index and
innovation. Dates are after valuation. Indices are relative levels with initial
value one. Innovations are supplied standard-normal draws with declared joint
dependence; no random draws are generated implicitly.}
\item{context}{Optional compatible calculation context.}
}
\details{Revenue is base quantity times volume multiplier times base price times
price multiplier. Variable cost scales with quantity; fixed cost has a separate
multiplier. Working capital is the declared fraction of period revenue.
CFADS subtracts variable and fixed costs, cash taxes, capex and working-capital
change, and adds other operating cash and explicitly included disposals.
Reserves, funding and debt service are not operating revenues. No universal
sector formula, tax-loss rule or interest tax shield is inferred.

Asset ageing applies to initial value less salvage, linearly over remaining life
or geometrically with continuous depreciation. Geometric ageing approaches salvage
asymptotically; economic life remains diagnostic. Market level and a lognormal
idiosyncratic factor multiply the ageing baseline. Log increments are
\eqn{-\sigma^2 dt/2 + \sigma\sqrt{dt}z}. Unit expected idiosyncratic growth requires
the declared innovation law, not arbitrary stress draws. Salvage is not a guaranteed
price floor. Physical values are distinct from liquidation proceeds, costs and delay.}
\value{A CalcResult containing operating periods with the full CFADS bridge, or
asset paths with ageing, market and idiosyncratic contributions, plus frozen
contract/parameter hashes and input fingerprints.}
