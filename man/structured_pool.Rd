\name{structured_credit_pool_foundations}
\alias{build_credit_asset_pool}
\alias{tranche_loss_from_pool}
\alias{receivables_borrowing_base}
\title{Dated Structured Pools, Static Tranche Loss and Borrowing Bases}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Preserves contractual asset identity, dated ownership and eligibility;
computes static attachment/detachment losses and explicitly capped invoice
borrowing bases. No legal eligibility or operational credit action is inferred.}
\usage{
build_credit_asset_pool(asset_contracts, pool_assignments,
    eligibility_profile,
  pool_id, as_of, information_cutoff, context = NULL)
tranche_loss_from_pool(pool_loss, attachment, detachment,
    pool_notional,
  loss_unit = "fraction", context = NULL)
receivables_borrowing_base(invoices, borrowing_profile,
  as_of, information_cutoff, context = NULL)
}
\arguments{
\item{asset_contracts}{One snapshot per asset_id, with asset_class, nullable
invoice_id, obligor_id, default_group_id, seller_id, servicer_id, currency,
origination_date, observed_at, known_at, balance_date, principal_balance,
schedule, overdue_days, disputed, dilution_reserve, recourse and
collateral_allocations. Schedule rows have date, principal_due and interest_due;
collateral rows have collateral_id and allocation_fraction.}
\item{pool_assignments}{Records assignment_id, pool_id, asset_id, known_at,
effective_from, nullable effective_until, share, purchase_price and
purchase_priority. Intervals are half-open; simultaneous shares across all
supplied pools cannot exceed one for any asset.}
\item{eligibility_profile}{Versioned profile_id, version, known_at, currency,
product_kind, pool_mode, nullable revolving_until, allowed_asset_classes,
maximum_dpd, exclude_disputed and eligible_recourse.}
\item{pool_id}{Target pool identity.}
\item{as_of}{ISO pool or invoice snapshot date. Current balances must match it.}
\item{information_cutoff}{ISO knowledge cutoff no later than as_of. Profile must
already be known; later-known assets/assignments are excluded.}
\item{pool_loss}{Nonnegative scalar or vector of pool losses in the declared unit.}
\item{attachment,detachment}{Fractions satisfying zero <= attachment < detachment
<= one.}
\item{pool_notional}{Positive reference pool notional.}
\item{loss_unit}{fraction or amount. Amount is normalized by pool notional.}
\item{invoices}{Records invoice_id, debtor_id, debtor_group_id, seller_id,
servicer_id, currency, issue_date, due_date, balance_date, known_at, outstanding,
disputed, dilution_reserve and recourse.}
\item{borrowing_profile}{Versioned common profile plus maximum_overdue_days,
exclude_disputed, allowed_recourse, advance_rates (NONE/SELLER),
default_group_cap, group_caps, additional_reserve,
reserve_scope=ADDITIONAL_NOT_IN_INVOICE_DILUTION and coverage_status
(COMPLETE, PARTIAL or UNKNOWN).}
\item{context}{Optional currency/physical-measure-compatible calculation context.}
}
\details{
Supported product kinds and claim classes are ABS (CONSUMER_LOAN, AUTO_LOAN,
LEASE), RMBS (RESIDENTIAL_MORTGAGE), CMBS (COMMERCIAL_MORTGAGE), CLO
(CORPORATE_LOAN), MORTGAGE_COVER (RESIDENTIAL_MORTGAGE, COMMERCIAL_MORTGAGE),
PUBLIC_SECTOR_COVER (PUBLIC_SECTOR_CLAIM) and TRADE_RECEIVABLES (INVOICE).
The profile may narrow compatible classes. Incompatible routes are rejected.
Recourse is NONE or SELLER. These classifications do not certify legal product
eligibility or establish borrower/default probabilities.

Pool mode STATIC disallows a reinvestment end. REVOLVING needs an exclusive future
end. Only current eligible assignments enter reference principal. Eligible future
assignments strictly before reinvestment end form separate warehouse inventory,
not current collateral. This inventory consists of pre-existing observed assets,
not future originations silently introduced into a historical pool.

Origination <= observation <= knowledge and balance_date <= observation are
required. Current pool balances must be dated exactly as_of. Stale balances are
not rolled forward by pretending scheduled payments were made. Duplicate assets
and invoices are rejected. An obligor cannot be split among default groups.
Principal schedules are increasing, nonnegative and reconcile fully to observed
balance; balloons/bullets are explicit. There is no invented refinancing or
residual. Collateral allocation fractions summed across visible assets cannot
exceed one; this is an allocated-collateral contract, not implicit multiple whole
claims with unknown seniority. Assignment share checks use every supplied pool
and half-open interval boundaries. Exclusions remain explicit in the artifact.

Static tranche loss fraction is min(max(L-A,0),D-A)/(D-A). Tranche notional equals
pool notional times D-A, so L=.15, A=.1, D=.2, pool100 yields tranche notional10,
loss5 and tranche fraction.5. Excess pool losses from costs can exceed pool
notional, but tranche loss is capped at contractual tranche notional. Invalid or
nonrepresentable normalizations fail. Static capital loss is not a timed-payment
default event, PD or replacement for a dynamic waterfall.

Borrowing-base eligibility excludes late-known/stale currency, aging, disputed
and disallowed-recourse invoices according to profile. Invoice aging is the
nonnegative date difference after due date. Eligible outstanding first subtracts
invoice dilution reserves. Concentration caps use a fixed denominator equal to
total eligible net faces before caps, allocated proportionally within debtor
group. They do not solve a retained-pool endogenous concentration constraint.
One debtor cannot evade a cap by using several group labels.

Apply recourse-specific advance rates after caps, then subtract the separate
nonoverlapping reserve and floor at zero. Net groupG invoices80+100 and groupH100
under a50 percent cap retain140+100; at80 percent advance and reserve5 the base
is187. Seller recourse is not risk-free. PARTIAL/UNKNOWN coverage reports a known
subtotal with null total borrowing base. No actual account limit is changed.
}
\value{CalcResult with bound CreditAssetPool or ReceivablesBorrowingBase, or a
static tranche loss table. Artifacts retain identity, dates, exclusions, input
and content bindings; static output reports pool versus tranche normalization.}
\references{\href{https://www.bis.org/publications/securitisations-tranching-concentrates-uncertainty}{Securitisations: tranching concentrates uncertainty}
illustrates attachment/detachment and nonlinear concentration of risk-parameter
uncertainty; reviewed 2026-10-13. No regulatory capital algorithm is claimed.}
