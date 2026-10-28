\name{expert_qualitative}
\alias{compile_qualitative_rubric}
\alias{evaluate_qualitative_evidence}
\title{Frozen Qualitative Rubrics and Dated Structured Evidence}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Compiles explicit categorical or numeric qualitative criteria and evaluates known, valid structured facts with evidence and conflict traces. No narrative inference or automatic conversion of points to PD occurs.}
\usage{
compile_qualitative_rubric(criteria, rubric_contract, score_policy,
  interactions = NULL, rating_limits = NULL, context = NULL)
evaluate_qualitative_evidence(compiled_rubric, observations,
    evidence,
  as_of, known_at, conflicts = NULL, context = NULL)
}
\arguments{
\item{criteria}{Criterion lists with ID, kind, nonnegative weight, evidence requirements, missing/non-applicable policies and distinct scored levels.}
\item{rubric_contract}{Rubric ID/version, population, reference, known-at date, score direction higher-is-worse, ordered grades and explicit default target.}
\item{score_policy}{Ordered finite score floor/cap, minimum weighted evidence coverage, and unresolved-interaction policy refer or apply-adverse.}
\item{interactions}{Optional IDs, AND conditions mapping criteria to allowed levels, nonnegative adverse point deltas and references.}
\item{rating_limits}{Optional IDs, AND level conditions, best/worst allowed grades and references.}
\item{context}{Optional calculation context consistent with the rubric target.}
\item{compiled_rubric}{Sealed rubric returned by compilation.}
\item{observations}{Dated records with observation/criterion IDs, value, non-applicable Boolean, observation time, knowledge/validity fields and evidence IDs.}
\item{evidence}{Dated evidence with ID/type, source ID, declared independence group, content fingerprint and VERIFIED/UNVERIFIED/REJECTED status.}
\item{as_of,known_at}{ISO observation and knowledge cutoff dates.}
\item{conflicts}{Optional dated conflict records linking observations of one criterion, OPEN/RESOLVED status, reference and selected resolution observation when resolved.}
}
\details{
Criteria weights are normalized once at compilation and never renormalized because evidence disappears. Categorical levels must exactly partition declared categories. Numeric levels must be finite contiguous disjoint intervals, left closed/right open except the last upper endpoint, with points respecting any declared increasing or decreasing risk order. Higher points always indicate greater risk.

Each criterion declares required evidence types and a positive minimum number of independent source groups. Independence is a supplied assertion, not inferred from document counts. Missing policy is refer or worst. Non-applicable policy is separately refer, worst or fixed; fixed points must be within the criterion range and require valid supporting evidence. Unknown facts never become good or average observations.

Only verified, known and valid evidence supports facts. Validity intervals are half open. Observation time cannot exceed its knowledge time. Unknown/future/expired records are explicitly excluded. Multiple active identical facts can combine evidence; differing facts always refer until an explicit known valid resolution. Open conflicts, incompatible resolutions and unavailable resolution selections remain unresolved.

Weighted criterion points are summed, nonnegative adverse interaction deltas added and declared floor/cap applied. Unknown contributions produce an interval and no point estimate. Unknown interactions either refer or conservatively apply the adverse delta. Missing/assumed and non-applicable records do not receive invented levels. Active rating limits intersect; inconsistent or unresolved limits refer. Points are not mapped automatically to PD or grade.

Evidence coverage uses original normalized criterion weights. A conservative numerical score may coexist with REFER when evidence coverage or judgment remains unresolved; that score does not authorize a credit action. Content hashes record exact rubric and sorted input-record content, not author authentication.
}
\value{A CalcResult containing the sealed rubric, or score and interval, readiness/referral status, evidence coverage, criterion and interaction contributions, rating limits, conflicts, record exclusions, input hashes and declared cutoffs/target.}
\references{SHELF structured elicitation background: \url{https://shelf.sites.sheffield.ac.uk/}. These functions implement explicit local contracts and do not claim an elicitation workshop occurred.}
