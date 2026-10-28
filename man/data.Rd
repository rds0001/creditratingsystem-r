\name{credit_data}
\alias{read_local_dataset}
\alias{validate_credit_dataset}
\alias{join_as_of}
\alias{build_observation_windows}
\alias{build_recovery_episodes}
\alias{make_grouped_temporal_splits}
\title{Local typed credit data and dated observation preparation}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Native offline CSV/JSON import, relational checks, bitemporal joins,
first-default windows, workout linkage and group-isolated chronological splits.}
\usage{
read_local_dataset(path, schema, encoding = "utf-8",
    delimiter = ",",
                   decimal_mark = ".")
validate_credit_dataset(tables, contract_version = "1.0")
join_as_of(left, right, entity_keys = "entity_id", as_of = "as_of",
           known_at = "known_at", observed_at = "observed_at",
           available_at = "available_at", release_id = "release_id",
           suffix = "_right")
build_observation_windows(ratings, default_events, exits,
    horizon_days,
                          evaluation_cutoff,
    entity_key = "entity_id")
build_recovery_episodes(default_events, recovery_cashflows,
    cures = NULL,
                        reopening_days = 90,
    evaluation_cutoff = NULL, closures = NULL)
make_grouped_temporal_splits(panel, train_end, calibration_end,
    test_end,
                            embargo_days = 0,
    entity_key = "entity_id",
                            group_key = "group_id",
    time_key = "observed_at",
                            outcome_end_key = "outcome_end")
}
\arguments{
\item{path}{Local CSV or JSON path; URIs and UNC paths are rejected.}
\item{schema}{Named list with version \code{"1.0"}, named \code{columns}, optional
\code{primary_key} and \code{allow_empty}. Each column declares type
string/integer/number/boolean/date, optional nullable, and unit for numeric types.}
\item{encoding, delimiter, decimal_mark}{Explicit text encoding, CSV separator,
and decimal mark (point or comma). No grouping separators.}
\item{tables}{Named lists or data.frames: obligors, observations, facilities,
defaults and recoveries; required parent tables must be present.}
\item{contract_version}{Supported relational contract version, \code{"1.0"}.}
\item{left, right}{Lists of row lists or data.frames with keys and date columns.}
\item{entity_keys}{One or more matching entity columns.}
\item{as_of, known_at, observed_at, available_at, release_id, suffix}{Column
names and right-column collision suffix. Release IDs must be unique.}
\item{ratings}{Rows with observation_id, entity_id, as_of, optional
performance_start, followup_end, funded and outcome_available_at (complete
outcome ascertainment availability date).}
\item{default_events}{For observation windows: event_id, entity_id, default_at,
known_at. For recovery linkage: episode_id, entity_id, default_at, known_at,
ead and currency.}
\item{exits}{Rows with entity_id, exit_at and known_at. Empty list is allowed.}
\item{horizon_days}{Positive integer performance horizon in calendar days.}
\item{evaluation_cutoff}{ISO knowledge cutoff date. NULL for recovery means all
provided history.}
\item{entity_key, group_key, time_key, outcome_end_key}{Column names for identity,
optional connected group, observation time and complete outcome-window end.}
\item{recovery_cashflows}{Rows with cashflow_id, episode_id, cashflow_at, known_at,
amount, kind (recovery/cost), currency. Positive costs are subtracted.}
\item{cures}{Optional rows with episode_id, cure_at, known_at, optional closed_at with required closed_known_at on or after closure.}
\item{closures}{Optional legal terminal rows with unique closure_id and episode_id,
closed_at, known_at on or after closure, and closure_reason SETTLEMENT, WRITE_OFF
or LEGAL_TERMINATION. These close a workout without asserting borrower cure.
Only known terminals apply. Conflicting terminals for a merged workout, including
legacy cure closure, fail; closure cannot precede any default in that workout.
Post-closure cashflows fail. Output retains closure_id and closure_reason.}
\item{reopening_days}{Nonnegative integer days after cure during which re-default
belongs to the same workout; an uncured workout is also continued.}
\item{panel}{Rows with unique observation_id, string entity ID, observation and
outcome-end dates; optional string group ID.}
\item{train_end, calibration_end, test_end}{Strictly increasing ISO end dates.}
\item{embargo_days}{Nonnegative calendar days around split boundaries.}
}
\value{A CalcResult with a list of named rows in \code{$value}, or a named report.
All dates are ISO YYYY-MM-DD strings. Missing outputs remain NULL. Import
provenance includes SHA-256 of original bytes. Validation returns valid,
violations (one-based row numbers) and row_counts. Recovery returns episodes and
cashflows. Splitting returns assignments, counts and group_overlap.}
\details{
Imports require exact columns, finite numeric values, explicit numeric units,
true/false boolean literals, and JSON-safe integers. Integer text contains signed
digits only; IDs larger than 9007199254740991 are strings. Empty files require
allow_empty. Duplicate JSON object keys are rejected. Null or empty text is missing only in nullable columns.

Relational tables have keys obligors(entity_id), observations(entity_id,
observed_at), facilities(facility_id), defaults(episode_id), recoveries(cashflow_id).
Observations require available_at; facilities require entity_id and nonnegative
ead; defaults require entity_id and default_at; recoveries require episode_id,
cashflow_at, nonnegative amount and kind. Entity foreign keys reference obligors;
recoveries reference defaults. Duplicate/missing keys, invalid dates, negative
amounts and recovery before default are reported rather than dropped.

The as-of join selects latest observed_at not after as_of, then latest available_at
not after known_at. Same-date ties raise DATA_AMBIGUOUS_JOIN. Unmatched rows remain
with NULL right fields. This is a fact/revision join, not an interval join.

First-default windows use (origin, origin+horizon] and known events only. Prior
default excludes a row. An observed in-window event yields one, complete follow-up
without default yields zero, and censored or unfunded rows have missing targets.
A future outcome_available_at produces outcome_pending and a missing target
unless default is already observed; calendar maturity alone is insufficient.
Default wins a same-day exit tie. A rejected application never becomes a good
loan label. Complete event reporting through followup_end is a caller assumption;
reporting gaps require shortening follow-up. Cure/re-default targets are separate.

Workout reopening uses the preceding default's known cure date. Original EAD
is not added across repeats. Unique cashflows remain attached to dated defaults
and exactly one workout, with calendar days since first default. Currencies must
match. Explicit closure with known closed_known_at alone closes a workout; cure and data-end do not. A
cashflow after closure fails pending revised closure or explicit reopening.
Amounts are undiscounted, and no LGD or discount rate is inferred.

Temporal splits connect all entity/group links transitively and reserve every
component for its latest partition, including future rows beyond test end.
Earlier rows are excluded. Training/calibration outcomes plus embargo end by
their upper boundary; calibration/test observations are strictly after the lower
boundary plus embargo. Test outcomes end by test_end. Empty partitions are
reported. This evaluates unseen connected entities, not repeat-customer backtests.
All routines are deterministic in-memory preparation, not empirical certification.
}
\examples{
x <- list(list(entity_id = "A", as_of = "2024-02-01", known_at = "2024-02-01"))
y <- list(list(entity_id = "A", observed_at = "2024-01-01",
               available_at = "2024-01-05", release_id = "v1", score = 2))
join_as_of(x, y)$value
}
