\name{rank_credit_reason_codes}
\alias{rank_credit_reason_codes}
\title{Consolidate Stored Model and Policy Reasons}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Deterministic same-scale contribution grouping with separately reproduced sealed policy evidence.}
\usage{rank_credit_reason_codes(contributions, reference_basis,
    reason_dictionary,
  policy_decision = NULL, max_reasons = NULL, context = NULL)}
\arguments{
\item{contributions}{Records with unique feature_id and finite scalar contribution.}
\item{reference_basis}{attribution_id, model_id, model_version, reference_id, explained_quantity, output_scale (probability, log_odds, score, currency, linear_predictor), baseline, prediction and adverse_direction HIGHER or LOWER. Contributions must reconstruct prediction minus baseline.}
\item{reason_dictionary}{dictionary_id, version, model_reasons and policy_reasons. Every entry has distinct reason_code, description and nonnegative integer priority. Model entries name nonoverlapping known feature_ids. Policy entries bind rule_ids.}
\item{policy_decision}{Optional sealed DecisionArtifact. Matched rules are reproduced, not rerun.}
\item{max_reasons}{Optional positive integer model reason limit; omitted reasons remain in the output.}
\item{context}{Optional calculation context.}
}
\details{Signed transformed-feature contributions are summed within a reason before testing adverse direction. Non-adverse groups are suppressed with backing evidence retained. Model reasons rank by descending adverse net magnitude, then priority and code. Policy reasons rank separately by priority and code, retaining matched rule traces and selected status. Their units are never added to model effects. Unmapped contributions and absolute contribution coverage are explicit. Content hashes detect alteration but do not authenticate evidence. Reasons are descriptive, not causal effects or automatically legally sufficient adverse-action notices.}
\value{CalcResult with ranked model reasons, separate policy reasons, suppressed/omitted groups, unmapped contributions, coverage and provenance hashes.}
