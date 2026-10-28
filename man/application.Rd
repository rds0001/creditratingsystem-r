\name{application_risk}
\alias{build_application_snapshot}
\alias{assemble_joint_application}
\alias{build_application_targets}
\alias{predict_application_risk}
\alias{compare_offer_risk}
\title{Application evidence, selection-aware targets and conditional risk}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Freeze dated applicant evidence, retain joint legal roles, construct
funded first-default targets and apply explicit conditional models locally.}
\usage{
build_application_snapshot(applications, feature_records,
    term_events = NULL,
                           required_features = NULL)
assemble_joint_application(application_id, parties, income_items,
                           obligation_items, currency)
build_application_targets(applications, decisions, bookings,
    default_events,
                          exits, horizon_days, evaluation_cutoff,
                          performance_origin = "first_draw_at")
predict_application_risk(snapshot, model, target, calibrator = NULL,
    offset = NULL)
compare_offer_risk(snapshot, offers, model, target, support_bounds,
                   calibrator = NULL, lgd = NULL, offset = NULL)
}
\arguments{
\item{applications}{Rows with unique application_id, customer_id, product_id,
population_id, application_at, information_cutoff, and requested_terms named list.}
\item{feature_records}{Rows with application_id, feature, value (possibly NULL),
observed_at, known_at and source_id.}
\item{term_events}{Optional rows with application_id, stage (offered/accepted/booked),
terms named list, effective_at and known_at.}
\item{required_features}{Optional names whose missing evidence is reported.}
\item{application_id}{String application identifier.}
\item{parties}{Rows with party_id, role (borrower/co_borrower/guarantor),
liability_share in [0,1], optional household_id.}
\item{income_items, obligation_items}{Rows with economic_id, party_id,
amount_monthly and currency. Amount is the total shared economic amount.}
\item{currency}{Explicit common currency for supplied monthly amounts.}
\item{decisions}{Rows with decision_id, application_id, status
(approved/rejected/pending/withdrawn), decided_at, known_at; optional accepted_at
requires accepted_known_at.}
\item{bookings}{Rows with unique application_id and facility_id, booked_at,
known_at; optional first_draw_at requires first_draw_known_at. Optional followup_end
and outcome_available_at govern follow-up completeness.}
\item{default_events}{Rows with event_id, facility_id, default_at, known_at.}
\item{exits}{Rows with facility_id, exit_at, known_at; early repayment is censoring.}
\item{horizon_days}{Positive integer calendar-day outcome horizon.}
\item{evaluation_cutoff}{ISO date of known events and outcomes.}
\item{performance_origin}{Explicit first_draw_at or booked_at target origin.}
\item{snapshot}{One frozen signed snapshot from build_application_snapshot.}
\item{model}{Frozen ModelArtifact or FitResult with application_contract metadata.}
\item{target}{Complete fitted target, including performance_origin; physical PD.}
\item{calibrator}{Optional calibration artifact matching target and model version.}
\item{offset}{Optional scalar link-scale offset matching fitted classifier.}
\item{offers}{Rows with unique offer_id and terms named list.}
\item{support_bounds}{Named list mapping every modelled term to [lower,upper].}
\item{lgd}{Optional fixed LGD in [0,1], used with offered amount as fixed EAD.}
}
\value{CalcResult. Snapshots, targets and offer results return lists of row lists;
joint application and risk prediction return named records. IDs are strings,
dates ISO YYYY-MM-DD. Missing values remain NULL.}
\details{
Snapshots select feature revisions with observed_at and known_at no later than
information_cutoff, then latest observation and knowledge dates. Ties fail.
Source timestamps and requested/offered/accepted/booked terms stay separate.
Snapshots have a shared canonical hash; subsequent inference rejects changed
content. Objects remain mutable in memory but are copied and integrity checked.

Joint items are deduplicated within income and debt tables by economic_id.
Conflicting shared amounts fail. Guarantor resources do not become borrower
income. Original liability roles and shares remain; shares need not sum to one.
Budget amounts are not multiplied by liability shares. No individual PDs are
averaged, and no household identity is inferred. Caller supplies already normalized,
recognized monthly amounts in a common currency.

Targets preserve rejected, approved-not-taken-up, accepted-not-booked,
booked-not-drawn and funded stages. Known bookings require approval and acceptance;
chronology and separate knowledge dates are checked. Information cutoff cannot
follow evaluation cutoff. Acceptance attached to a nonapproved decision fails. A target is first default
in (origin, origin+horizon], using funded-facility events. Unfunded applications,
early exits, incomplete follow-up and pending ascertainment have missing labels.
A complete nondefault window yields zero; default at horizon end yields one.
Information cutoff remains separate from performance origin. Calendar maturity is
reported separately from label_observed. Booking-origin targets explicitly treat
booked facilities as the funded risk set even before first draw. One booked facility
per application is supported; split multi-facility records explicitly upstream.

Prediction requires model$application_contract with product_ids, population_ids,
performance_origin, term_stage and term_features. The last maps term field to
fitted feature name and may be an explicitly named empty list. Term-feature names
must not also occur among customer features. Product, population and complete
target must match; missing/nonfinite model inputs fail. The shared classifier
currently supports binary GLM artifacts with identity preprocessing. Optional
calibration validates target/version; isotonic additionally declares
metadata$input_scale = "probability". Output contains PD, unadjusted PD, model and
snapshot identities, target and feature coverage. Risk is separate from
creditworthiness budgets and approval decisions.

Offer comparisons require an offered-term conditional model and every scenario
term to appear in its feature map. All customer features stay fixed. Marginal
support bounds flag extrapolation but cannot establish joint support or causal
effects. Optional expected loss is PD times fixed LGD times offered amount;
requested_terms$currency is then mandatory and returned. No dynamic EAD, take-up,
automatic policy or causal claim is implied.
}
\examples{
a <- list(list(application_id = "A", customer_id = "C", product_id = "consumer",
               population_id = "new", application_at = "2024-01-01",
               information_cutoff = "2024-01-01", requested_terms = list(amount = 1000)))
f <- list(list(application_id = "A", feature = "income", value = 3000,
               observed_at = "2023-12-01", known_at = "2023-12-02", source_id = "payroll"))
build_application_snapshot(a, f, required_features = "income")$value
}
