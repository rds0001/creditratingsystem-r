# creditratingsystem

A native R library for granular credit risk modelling with local user data.

See [getting started](inst/doc/getting_started.txt) for local installation, typed
CSV input and a workflow for fitting and validating models with your own data.

Version 0.1.0; local software and synthetic reference acceptance completed.
Available methods include train-only preprocessing and monotone binning,
scorecards, binary GLMs, frozen PD calibration, Merton and first-passage models,
Gaussian-factor PIT/TTC conversion, observed long-run PD, and discrete and
continuous-time transition curves. Survival includes Cox, AFT, competing risks,
Fine-Gray and multistate estimates; collections projects ordered cure and redefault.
Dated workout recovery and exposure schedules retain economic loss and raw CCF. Joint loss paths preserve default, LGD,
exposure and discount dependence; portfolio tails use fractional boundary mass.

Consumer methods cover application snapshots, affordability, credit-policy
rules, dated account panels, behavioral predictions, roll rates and vintage
performance. Collections methods preserve open episodes, probation and
post-cure censoring, reconstruct pre-action evidence and compare repayment
cashflows. Model registries, role histories and pinned batch recipes support
historical replay. Monitoring reports cohort maturity, missing outcomes,
feature/calibration drift and explicit challenger/group comparisons.

The installed retail example connects training, separate calibration, application,
booking, behavioral prediction, monitoring, collections and historical replay.
Its tiny synthetic cohort deliberately reports insufficient monitoring evidence.
Assumptions, applicable models and numerical conventions are documented per method.

```r
library(creditratingsystem)
expected_loss(0.02, 0.4, 1000)$value  # 8
```

Each calculation returns its value, units, method identity, context, provenance,
diagnostics and limitations. Probabilities use decimal fractions. User-supplied
rating scales and parameters remain explicit. Calculations do not download data
or send user information to external services.

Classification, LGD/EAD and selection-bias estimators are implemented, alongside
cashflow, structured-credit and market-implied methods. Product examples retain
their own dated contracts, outcomes and applicability limits.
Synthetic evidence does not establish performance on a bank's own portfolio.

Author: RiskDataScience GmbH. Responsible person: Dr. Dimitrios Geromichalos.
Contact: riskdatascience@web.de.

Licensed under the GNU Affero General Public License, version 3 only
(`AGPL-3.0-only`); see [license text](inst/licenses/AGPL-3.txt).

Bundled synthetic data include six connected tiny reference worlds and complete
worked examples. Native offline generators support larger standard and stress
worlds. Observable inputs, latent truth, frozen predictions, decisions and
outcomes are stored separately. Use
`system.file("extdata", "syn_t", package="creditratingsystem")` for the complete
tiny worlds. The large `inst/extdata/syn_s` reference set in the development
workspace is supplementary data and is excluded from CRAN source archives.
See the synthetic function documentation for tier sizes and generation contracts.

Large example JSON resources are gzip-compressed while retaining their stable
`.json` resource names. R text connections and `jsonlite::fromJSON(path)` detect
this compression automatically. Other tools should use a gzip-aware reader.

## Test runtime

Normal `R CMD check` uses small analytical cases and real fits, including a short
observation-window generation of all six synthetic worlds. Set
`CREDITRATINGSYSTEM_EXTENDED_TESTS=true` for the additional full-data and expensive
statistical acceptance tests. Run these in separately selected CI jobs; stress
tier generation is an explicit offline task. Use one numerical backend thread
for checks and record elapsed times. This switch is explicit on every host.

The [CRAN repository policy](https://cran.r-project.org/web/packages/policies.html)
asks for short checks and allows optional long-running tests while retaining
feature coverage in ordinary checks. Examples should demonstrate use in a few
seconds. The package is still under development; these test tiers are not a claim
of CRAN acceptance.
