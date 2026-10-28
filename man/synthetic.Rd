\name{synthetic}
\alias{generate_macro_reference_world}
\alias{generate_obligor_panel}
\alias{generate_facility_workouts}
\alias{generate_retail_reference_world}
\alias{generate_asset_pool_reference_world}
\alias{generate_system_event_reference_world}
\alias{generate_challenge_cases}
\alias{validate_synthetic_world}
\title{Offline Synthetic Credit Worlds with Shared Actual Innovations}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Native R generators and audits for linked macro, wholesale, facility,
retail, asset-pool and system-event worlds. Hidden DGP truth is segregated from
observations, frozen predictions, decisions and observed outcomes.}
\usage{
generate_macro_reference_world(specification, innovations,
  output_directory, chunk_rows = 1000L, memory_budget_mb = 256L)
generate_obligor_panel(macro_world, specification, innovations,
  output_directory, chunk_rows = 1000L, memory_budget_mb = 256L)
generate_facility_workouts(obligor_world, specification,
    innovations,
  output_directory, chunk_rows = 1000L, memory_budget_mb = 256L)
generate_retail_reference_world(macro_world, specification,
    innovations,
  output_directory, chunk_rows = 1000L, memory_budget_mb = 256L)
generate_asset_pool_reference_world(worlds, specification,
    innovations,
  output_directory, chunk_rows = 1000L, memory_budget_mb = 256L)
generate_system_event_reference_world(worlds, specification,
    innovations,
  output_directory, chunk_rows = 1000L, memory_budget_mb = 256L)
generate_challenge_cases(case_ids = NULL, context = NULL)
validate_synthetic_world(world, check_hashes = TRUE, context = NULL)
}
\arguments{
\item{specification}{Named list with \code{tier} (tiny, standard, stress),
\code{dgp_family} (well_specified, misspecified), ISO \code{start_date} and
\code{observation_cutoff}. Retail additionally requires
\code{selection_mechanism}: MAR, MNAR or missing_overlap. All other DGP coefficients
are fixed engineering parameters documented in the world, not estimated defaults.}
\item{innovations}{Directory containing a versioned manifest and actual uint32
little-endian stream files. Each stream declares relative filename, two-dimensional
shape and SHA256. Uniforms equal (uint32 + 0.5) / 2^32; paired Box--Muller transforms
produce normal innovations. Python and R read the same bytes, not matching seeds.}
\item{output_directory}{Absent or empty caller-selected directory. All output is
written there; there is no implicit installed-resource write. Existing nonempty output
is rejected. An interrupted directory can be inspected; use a fresh directory for
a new run.}
\item{chunk_rows}{Integer from 1 to 50,000. Table processing and output are bounded;
scenario paths are streamed individually. Changing this does not change draws.}
\item{memory_budget_mb}{Explicit planning budget, at least 32 MiB. A stream/index
estimate is checked before output creation. This is not an operating-system process
memory limiter. No silent subsampling occurs when declared capacity is insufficient.}
\item{macro_world}{Generated macro result, its value, or directory. Source tier,
start and observation cutoff must agree with the requested output.}
\item{obligor_world}{Generated obligor result, its value, or directory; its observed
default events seed linked facility workouts.}
\item{worlds}{List of generated worlds or directories. Pool composition uses actual
funded accounts/facilities. System events use existing obligors, accounts,
facilities and pools. Source identities and dates are preserved.}
\item{world}{Generated result, result value, or directory to audit.}
\item{check_hashes}{Logical scalar. FALSE skips file digest checks only; semantic,
temporal, foreign-key and economic checks remain active.}
\item{case_ids}{NULL for all constructed cases, or unique case identifiers.
Identifiers are complete_separation, zero_default, spiegelhalter_half,
singular_covariance, cyclic_support, transfer_extremes, censored_target,
raw_lgd_bounds, cohort_100 and future_feature.}
\item{context}{Optional standard calculation context for challenge/audit results.}
}
\value{A CalcResult. Generators return a value with directory and manifest.
Challenge generation returns cases, constructed flag and independent-oracle origin.
Audit returns passed, issues, row counts, realized events and descriptive
design-versus-realized diagnostics. No audit result implies real-bank validation.}
\details{
Five directories separate observable_inputs, latent_truth, frozen_predictions,
decision_history and observed_outcomes. Tables are gzip JSONL. The macro scenario
cube is float64 little endian in path/month/factor order and starts after the final
history date. All file paths in manifests are relative. Shared innovations are
validated before generation; no RNG, network, Python bridge, implicit fit or
outcome-driven reselection is used by these functions.

Wholesale tiny/standard/stress comprise 64/3000/20000 obligors over 3/10/15 forecast
years; macro history has 36/240/360 months. Scenario dimensions are 512x24x8,
16384x60x8 and 131072x120x8. Standard includes 750 distinct groups. Retail application
capacities are 320/50000/500000 over 12/36/84 origination months; household counts
are 160/20000/200000 and persons 240/30000/300000. Funded account capacities are
160/24000/240000 with 24/60/120 account months. Pools have 2/12/100 pools and
80/30000/100000 source-asset capacity, four tranches each. Capacities do not force
acceptance, survivor counts, default rates or recoveries.

Group/household modulo-10 partitions provide 60/20/20 entity train/calibration/test
splits. Forward time partitions are also 60/20/20; mismatched cells are excluded.
Annual financial knowledge lags observation by 90 days. Default reporting lags
14 days; recovery reporting lags seven days. Macro releases have initial/revised
lags of 30/90 days. Late bureau corrections cannot enter original application
predictions. Immature unresolved labels remain NULL, and unfunded applicants never
receive own-loan observed labels. Early closure is a competing nondefault.

Balance sheets, earnings and CFADS reconcile. Observable structural equity and
volatility are generated using the European-call equations from latent asset
values. Independent noisy external opinions, expiring overrides, scoped support,
operating leases/charters/offtake and independent foreign-currency transfer
sensitivity are separate tables. Zero/low-default strata are explicit.

Facility conversion, EAD and recovery share borrower-risk dependence. Gross
recoveries/costs are dated at 90, 360 and 900 days; incomplete workouts remain open.
Raw discounted LGD is not clipped. Constructed LGD -0.2 and 1.2 fixtures are excluded
from stochastic cohort frequencies.

Retail includes mortgages, installment/auto loans, cards, overdrafts, BNPL,
small-business credit and leasing; net-income observations, co-borrowers, bureau
tradelines and transaction transfer/reversal links are explicit. MCAR/MAR/MNAR
missingness, MAR/MNAR/no-overlap acceptance, take-up, changing policy and funding
are separated. Frozen demonstration coefficients use observable data with declared
fallbacks, while latent frailty affects only the hidden DGP and MNAR mechanism.
Monthly cash ledgers reconcile opening balance plus draws and accrued interest
minus payments and recoveries to closing balance. Floating mortgage resets,
balloons, missed payments, active undrawn balances, cure/redefault episodes,
competing exits and collection actions are dated. Collection actions are a fixed
engineering process, not causal treatment-effect estimates.

Pool servicing copies actual source ledgers; contractual future schedules are a
separate no-default/no-prepayment projection. Original assigned principal is not
current EAD. Forward additions and final aggregate-notional knowledge dates are
explicit. System-event checkpoints follow an independently authored closed-form
schedule with duplicates, late corrections, failed batches, guarantee expiry and
model switch; replay is not used to compute expected checkpoints.

All tiny worlds and full wholesale standard worlds with actual innovations are
installed under extdata/synthetic_tiny and extdata/synthetic_standard. The native
extdata/synthetic_innovations.R script can export additional complete tier streams
offline: Rscript SCRIPT DIR standard --retail. Its SHA256-counter construction
matches the Python authoring script byte for byte and differs intentionally from
the bundled fixed PCG64 reference experiment. Large tiers require sufficient disk
space and explicit run budgets; no data are downloaded.

System-event worlds include model, policy and calibration version registries and
complete source snapshots with audited content hashes. Checkpoints are
demonstration fixtures, not fitted models. Servicing includes original lifecycle
events. A separately constructed open-workout fixture is excluded from stochastic
default counts. Exact snapshot hashes are checked against their own sources;
independent floating-point calculations may differ by one representable unit.

Audit checks hashes, counts, columns, local identities/foreign keys, date ordering,
hidden-truth leakage, funded-only labels, splits, balance/cash/limit identities,
pool uniqueness and tranche totals. Conditional hazard sums and realized events
are descriptive; dependent observations are not treated as independent binomial
samples. Synthetic engineering evidence cannot establish actual portfolio
predictive performance, regulatory compliance or causal identification.
}
\references{
Merton, R. C. (1974), On the Pricing of Corporate Debt: The Risk Structure of
Interest Rates. \url{https://dspace.mit.edu/entities/publication/5189ad6f-77e1-410e-8b71-29347b2ceb71}.
NIST, Exact Binomial Confidence Limits.
\url{https://www.itl.nist.gov/div898/software/dataplot/refman2/auxillar/exacbici.htm}.
}
\examples{
cases <- generate_challenge_cases(c("zero_default", "raw_lgd_bounds"))
cases$value$cases
root <- system.file("extdata", "syn_t", package = "creditratingsystem")
if (nzchar(root)) validate_synthetic_world(file.path(root, "retail"))$value$passed
}
