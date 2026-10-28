\name{simulate_recovery_waterfall}
\alias{simulate_recovery_waterfall}
\title{Dated Recovery Allocation on Shared Default Events}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Allocates supplied ultimate collateral proceeds and costs to legal claims with shared default identities and explicit discount anchors.}
\usage{simulate_recovery_waterfall(default_events, claims,
    realization_paths,
  recovery_policy, context = NULL)}
\arguments{
\item{default_events}{Records with scenario_id, default_event_id, subject_id and default_date; one first event per subject and scenario.}
\item{claims}{Records identifying scenario, event, subject, claim, pool and currency, with ead_at_default, legal_balance, priority and deficit_cost_share. A claim belongs to one pool.}
\item{realization_paths}{Records identifying scenario, event, pool, currency, realization and collateral, with date, gross_value, realization_fraction, cost, discount_factor and discount_anchor_date. Each collateral identity is consumed once per scenario.}
\item{recovery_policy}{Versioned policy with currency, LEGAL_CAP or UNCAPPED_PRO_RATA allocation, PROCEEDS_FIRST_EXPLICIT_DEFICIT_SHARES cost rule and ULTIMATE_COMPLETE completion declaration.}
\item{context}{Optional matching calculation context.}}
\value{CalcResult with claim-level economic loss and LGD, dated allocation ledger, pool conservation totals and input fingerprint.}
\details{Legal caps persist across realization dates. Ascending priorities allocate first, equal ranks pro rata to remaining balance. Uncapped participation requires one rank and explicit positive allocation weights. Proceeds pay costs once; only deficits are funded by creditor shares. Net recovery is discounted to the shared default date. EAD is distinct from legal balance. Raw LGD may be negative or exceed one and is undefined at zero EAD. Completeness and legal entitlements are caller-supplied evidence; incomplete workouts must not be represented as ultimate losses.}
