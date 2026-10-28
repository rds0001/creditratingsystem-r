\name{simulate_contract_counterparties}
\alias{simulate_contract_counterparties}
\title{Shared Counterparty Defaults and Contract Cashflow Interruptions}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Simulate first counterparty events from supplied conditional default
probabilities and uniforms, then apply dated contract cure and replacement rules.}
\usage{simulate_contract_counterparties(counterparty_paths, contracts,
  cure_uniforms, currency, context = NULL)}
\arguments{
\item{counterparty_paths}{Contiguous period records with scenario_id,
counterparty_id, period_start, period_end, conditional_pd, uniform in [0,1),
and probability_measure (PHYSICAL). Initially performing counterparties share one first
event across all their contracts. Uniforms must have the appropriate conditional
law and declared dependence.}
\item{contracts}{Unique contract records with contract_id, counterparty_id,
currency, default_payment_timing (BEFORE_DUE), cure_probability, integer
cure_delay_days, logical replacement_enabled, integer replacement_delay_days,
nonnegative replacement_payment_fraction, and cashflows (date, nonnegative amount).
Disabled replacement requires zero delay and fraction. Payment dates must belong
to the counterparty assessment grid, including its initial date.}
\item{cure_uniforms}{Records with scenario_id, contract_id and uniform in [0,1).
Each scenario/contract combination must appear exactly once.}
\item{currency}{Reporting currency shared by all contracts.}
\item{context}{Optional compatible physical-probability calculation context.}}
\details{An event occurs at the first period end with uniform less than its
survival-conditional PD, before a payment due at that endpoint. Previously paid
cash remains paid. Payments are interrupted during the declared cure delay.
Successful contractual cure restores future scheduled payments but does not
invent recovery of missed payments. If cure fails, replacement starts after
cure delay plus replacement delay and pays the declared fraction. The replacement
is assumed performing over the remaining horizon. An instant cure/replacement
can preserve a payment despite the recorded counterparty event.
Counterparty defaults affect operating receipts; they must not also be counted
as borrower defaults. Borrower debt-service, UTP and arrears tests remain separate.}
\value{A CalcResult containing counterparty_events, contract_events and individual
cashflows with scheduled/received amounts, changes, payment fractions and state,
plus input fingerprints.}
\seealso{\code{detect_cashflow_default}, \code{simulate_operating_cashflows}}
