\name{detect_cashflow_default}
\alias{detect_cashflow_default}
\title{Cashflow Double Test and Independent Default Triggers}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Evaluate simultaneous debt-service and value-coverage breaches,
independent unlikeliness-to-pay and material-arrears triggers, and absorbing first default.}
\usage{detect_cashflow_default(path_records, default_policy, currency,
    context = NULL)}
\arguments{
\item{path_records}{Record lists containing scenario_id, entity_id, facility_id,
date, currency, cfads, debt_service_due, debt_outstanding, asset_value,
materiality_met, material_days_past_due, liquidity_buffer_available and utp_indicators. Monetary values use
one reporting currency. CFADS and debt service cover the same period; debt and
asset values share a date and valuation perimeter. Empty UTP indicators represent
an assessed absence of other signals, never unknown evidence.}
\item{default_policy}{Versioned list with policy_id, version, basis
\code{BASEL_CRE36_REFERENCE}, assessment_level (obligor or facility),
dscr_threshold, ltv_threshold, dscr_comparison (lt/le), ltv_comparison (gt/ge),
past_due_days_threshold, past_due_comparison (gt), past_due_rule_reference,
materiality_rule_reference, utp_rule_reference, logical dual_test_as_utp,
economic_joint_timing (SAME_DATE), default_state (ABSORBING_FIRST_DEFAULT).
Non-90 day thresholds through 180 require supervisory_exception_eligibility
(RETAIL_OR_PSE_APPROVED); facility scope requires facility_scope_eligibility
(RETAIL_APPROVED). These declarations do not establish legal eligibility.}
\item{currency}{Reporting currency string, consistent across rows and context.}
\item{context}{Optional calculation context.}}
\details{The economic double test requires simultaneous DSCR and LTV breaches
on the same facility and date. A declared policy may use it as a UTP indicator only while a debt-service shortfall
remains after explicitly available liquidity. Buffers must be legally and
operationally available by the payment date, net of prior claims; hypothetical
collateral proceeds or uncommitted refinancing do not qualify. They never cancel
other independently established UTP signals.
Regulatory-reference default is UTP OR material arrears exceeding the applicable
day threshold. Adequate value never cancels either trigger. Zero debt service
has no DSCR breach; positive debt and zero assets have an LTV breach.
Supplied evidence must cover the assessment subject, including relevant obligations
outside the simulated facility. Material-arrears counters follow the referenced
materiality rule and must advance by calendar time or reset within an interval.
Obligor assessment requires a common facility grid. Default remains absorbing;
cure and redefault require separate episode logic. Detection dates are grid
endpoints, with the preceding nondefault endpoint reported. Initial defaults are
left censored and excluded from initially performing PD risk denominators.}
\value{A CalcResult containing policy and hash, facility_tests, assessments and
first_default_events, including initial_default, last_nondefault_date,
observed_through and default_time_status.}
\references{Basel Framework, CRE36.68--36.75:
\url{https://www.bis.org/committees/bcbs/basel-framework/standard/cre?allChapters=true&chapter=36}.}
