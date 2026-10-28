\name{validation_retail}
\alias{validate_retail_cohort}
\alias{roll_rate_backtest}
\title{Retail Cohort and Exact Monthly Transition Validation}
\usage{
validate_retail_cohort(application_targets, account_panel,
 behavioral_targets, target_profile, split_memberships,
 source_lineage, design, context = NULL)
roll_rate_backtest(forecasts, account_panel, states, state_contract,
 design, weighting, uncertainty, cluster_resamples = NULL,
 confidence_level = 0.95, context = NULL)
}
\arguments{
\item{application_targets}{Existing operational application-target rows, preserving approved/accepted/booked/funded distinctions.}
\item{account_panel}{Existing account-month snapshots for cohort audit; observed dated state endpoints with explicit risk/default episode IDs for transitions.}
\item{behavioral_targets}{Existing account/date behavioral targets and follow-up/maturity fields.}
\item{target_profile}{Frozen profile ID/version/reference/known_at; default definition/unit/group rule, distinct event concepts, first-default/exit conventions, application-day and behavioral-month horizons, product risk-set rules and split isolation keys.}
\item{split_memberships}{Customer/role rows with all declared isolation-group identifiers.}
\item{source_lineage}{Source ID/hash/kind and observed/available dates per application:id or account:id@date prediction key.}
\item{design}{Frozen validation identities, target and evaluation cutoff.}
\item{context}{Optional matching calculation context.}
\item{forecasts}{Dated frozen transition matrices with common validation fields, account and origin state, risk/default episode IDs and optional opening_balance.}
\item{states}{Ordered state names.}
\item{state_contract}{Versioned known state scale, default/cure/exit/absorbing roles, extended with chargeoff_states subset of default states.}
\item{weighting}{count or opening_balance.}
\item{uncertainty}{none or cluster_bootstrap.}
\item{cluster_resamples}{Actual shared sampling-cluster draws.}
\item{confidence_level}{Interior confidence level.}
}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Audits supplied retail population/target tables without rebuilding targets, and scores exact-horizon state transitions using shared migration validation.}
\details{Cohort audit reports impossible selection flows, outcomes on unfunded contracts, unsupported nondefaults, missing default provenance, calendar/MOB and risk-set conflicts, missing targets, group split overlap and historical feature lookahead. Product-qualified active revolving zero balances remain at risk. No-hit/thin-file/pseudolabel origins are not observed outcomes. Early defaults remain visible but do not bypass mature-performance coverage. Household isolation does not imply shared default. Scope-qualified PASS/FAIL is a supplied-contract audit, not empirical or regulatory approval.

Transition knowledge cannot precede actual state observation; observed endpoints must be exact whole-month horizons except declared absorption. Lagged feature estimates are not observed endpoint states. The historically known origin must match the forecast. Later known revisions use the evaluation cutoff. Missing months never become self-transitions or shorten the horizon. Default-to-cure requires a new risk episode; re-default requires a new default episode. Continuous default cannot silently change episode IDs; absorption departures are violations. Violating paths are excluded with explicit diagnostics. Multiclass Brier/log loss and shared bootstrap use rating_migration_backtest. Calibration conditions on valid observed successors; no informative-missingness correction is inferred.}
\value{CalcResult containing ValidationResult metrics, coverage, violations or transition/episode traces, frozen contracts and fingerprints.}
