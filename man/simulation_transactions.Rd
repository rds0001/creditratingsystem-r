\name{simulate_transactions}
\alias{simulate_transactions}
\title{Execute Frozen Transaction Cash Pools on Shared Scenarios}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Combines explicit operating cash flows, loan payments, assets,
counterparty interruptions, first-default assessment and recoveries for disjoint
complete obligor cash pools. No random numbers are generated internally.}
\usage{
simulate_transactions(transaction_recipes, scenario_cube,
  entity_innovations, parameter_snapshot, context = NULL)
}
\arguments{
\item{transaction_recipes}{List of records with \code{transaction_id},
\code{version}, \code{subject_id}, \code{parameter_id}, \code{calendar},
\code{parameter_hash} and \code{cube_hash}. Hashes bind the exact canonical
parameter object and cube. Each calendar record has \code{start_date},
\code{end_date}, \code{payment_date} and \code{scenario_time}.}
\item{scenario_cube}{Physical ScenarioCube or its CalcResult, with
\code{schema_version="1.0"}, \code{kind="ScenarioCube"}, axes
\code{c("scenario","time","factor")}, distinct \code{scenario_ids} and
\code{factor_ids}, positive increasing \code{time_grid}, aligned nested
\code{values}, normalized nonnegative \code{weights}, \code{units},
\code{origin}, \code{frequency} and \code{sampling_contract}. Frequency is
monthly, quarterly or annual and times are consecutive year fractions of
that frequency. Calendar endpoints advance the origin by the corresponding
number of months, preserving end of month. The sampling law is
FORECAST_CONDITIONAL, STATIONARY_GAUSSIAN or DECLARED_CYCLE_MIXTURE, with an
explicit conditioning scope.}
\item{entity_innovations}{Unique records \code{scenario_id}, \code{date},
\code{channel_id}, finite \code{value}. Asset shocks and counterparty/default
or cure uniforms consume declared channels. Channels may be intentionally
shared by different drivers; missing and unused records are rejected.}
\item{parameter_snapshot}{Versioned object \code{snapshot_id}, \code{version},
\code{parameters}, mapping each parameter ID to a cash-pool specification.
Required fields are \code{currency}, \code{reserve_id}, \code{initial_reserve},
\code{reserve_targets}, \code{loans}, \code{operating_contract},
\code{operating_base}, \code{operating_bindings}, \code{assets},
\code{default_policy}, \code{materiality}, \code{external_evidence},
\code{recovery}, \code{sampling_design}, \code{legal_pooling},
\code{arrears_interest} and \code{payment_allocation}. See Details.}
\item{context}{Optional standard calculation context. Currency and physical
probability measure must agree with the declared inputs.}
}
\details{
Each loan specifies \code{terms}, unique liquidity \code{priority} and
\code{deficit_cost_share}; optional \code{base_rate_fixings} and
\code{recovery_priority} pass to the granular debt/recovery methods.
Operating base records use \code{simulate_operating_cashflows}; multiplier
bindings identify \code{factor_id}, matching \code{unit}, and transformation
\code{level} or \code{exp}. Going-concern asset disposals are rejected since
this recipe does not implement disposal-driven collateral removal.
Assets specify \code{parameters} for \code{simulate_asset_values},
\code{market_binding}, \code{innovation_channel}, and recovery
\code{delay_days}, \code{realization_fraction}, \code{cost},
\code{annual_discount_rate}. Recovery uses effective annual discounting over
actual delay days divided by 365, anchored at the shared default date.

The legal pooling declaration must be FULL_OBLIGOR_CROSS_COLLATERALIZED.
Arrears interest is NO_ADDITIONAL_ARREARS_INTEREST and payment allocation is
INTEREST_BEFORE_PRINCIPAL. Contractually scheduled nonnegative coupons accrue;
actual payments reduce unpaid interest first, then principal. Actual exposure
is distinct from the scheduled closing balance. Negative operating cash is
an operating deficit with liquidity priority before debt. Reserve cash is
consumed only once and any remaining reserve joins recovery only once.

Materiality specifies nonnegative \code{absolute_threshold},
\code{relative_threshold} in [0,1], \code{comparison="gt_both"} and
\code{continuous_between_grid_dates=TRUE}. The own material-arrears clock
starts at the first material grid endpoint, advancing between consecutive
material endpoints. All payments must be represented by the grid; continuity
is a supplied assumption. Complete independent outside-obligation evidence
is required on every scenario/date, including origin, with explicit assessed
no-indicator rows. The default policy must have obligor assessment scope.
Borrower first default is assessed after due-date payments, so payments at
that endpoint reduce exposure before recovery. The economic dual test cannot
veto independent unlikely-to-pay or material-past-due signals.

Optional \code{counterparties} specifies \code{paths}, \code{contracts},
\code{cure_draws} and
\code{baseline_receipts="INCLUDED_ONCE_IN_OPERATING_CFADS"}. Path rows name
an innovation channel at their period end; cure rows name an innovation
channel and innovation date. Granular counterparty cash changes enter the
operating bridge once. Shared counterparties must have identical default
probability/innovation paths across recipes. Their default probabilities
are never added directly to borrower default probability.

The recovery object supplies \code{policy} for
\code{simulate_recovery_waterfall} and
\code{reserve_recovery="REMAINING_RESERVE_AT_DEFAULT"}.
Transactions, obligors, monetary operating contracts, loan facilities,
collateral and reserves cannot be duplicated across pools. Common macro
factors and innovation channels can be shared deliberately.

Going-concern ledgers stop at the first detected default, whose actual
exposure, assets and reserve are then frozen for recovery. Initial defaults
are retained as paths but excluded from initially-performing risk estimates.
IID_UNWEIGHTED sampling requires uniform cube weights; weighted/dependent
sampling does not acquire an IID binomial confidence interval. The supplied
forecast/stationary/cycle-mixture conditioning is preserved, not inferred
from an average. This scoped composition is not automatic legal pooling or
regulatory approval, and does not infer missing outside-default evidence.
}
\value{CalcResult containing transaction bundles with frozen recipe and
parameter hashes, scenario paths and actual first-default exposure,
default assessments, recovery allocations, risk estimates and optional
counterparty paths. Top-level cube, parameter-snapshot and innovation hashes
bind lineage.}
\seealso{\code{build_debt_schedule}, \code{apply_cash_waterfall},
\code{simulate_operating_cashflows}, \code{simulate_asset_values},
\code{simulate_contract_counterparties}, \code{detect_cashflow_default},
\code{simulate_recovery_waterfall}, \code{estimate_simulation_risk}}
