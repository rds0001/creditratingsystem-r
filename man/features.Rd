\name{features}
\alias{fit_preprocessor}
\alias{transform_features}
\alias{build_financial_ratios}
\alias{build_distributed_lags}
\alias{fit_monotone_bins}
\alias{compute_woe_iv}
\title{Frozen training transformations and auditable financial features}
\usage{
fit_preprocessor(train_features, feature_schema, target, training,
  model_id, model_version, context = NULL)
transform_features(features, fitted_preprocessor, context = NULL)
build_financial_ratios(financial_statements, ratio_spec,
    context = NULL)
build_distributed_lags(series, observations, lag_spec, frequency,
  gap_policy = "null", context = NULL)
fit_monotone_bins(x_train, y_train, feature_name, direction,
    min_bin_count,
  max_prebins, target, training, model_id, model_version,
    context = NULL)
compute_woe_iv(bin_counts, smoothing, woe_sign, context = NULL)
}
\arguments{
\item{train_features, features}{Nonempty list of named row records or data frame.}
\item{feature_schema}{Ordered specifications with name and type numeric/categorical. Numeric: imputation error/mean/median/constant, scaling none/standard/robust, optional fill_value and missing_indicator. Categorical: missing_policy error/separate/mode, unknown_policy error/separate, optional drop_reference training category.}
\item{target, training}{Common ModelArtifact target and training metadata. Training contains data_fingerprint, split_id, known_at and optional data_origin.}
\item{model_id, model_version}{Explicit model identity and version.}
\item{context}{Optional calculation context.}
\item{fitted_preprocessor}{FitResult or ModelArtifact from preprocessing or monotone binning.}
\item{financial_statements}{Rows entity_id, as_of, period_start, period_end, currency, values and units. Values/units are named lists. Period end equals as_of.}
\item{ratio_spec}{Rows name, numerator and denominator named field coefficients, denominator_policy positive/nonzero, output_unit dimensionless.}
\item{series}{Records entity_id, feature, observed_at, available_at, known_at, value. Dates ISO with observed <= available <= known.}
\item{observations}{Query records entity_id, as_of, known_at.}
\item{lag_spec}{Rows name, feature, nonnegative integer lag, operation level/difference/relative_change.}
\item{frequency}{days, months or years.}
\item{gap_policy}{null or error for unavailable lag/current value or zero relative-change base.}
\item{x_train}{Numeric observations with missing values allowed.}
\item{y_train}{Complete observed binary default labels.}
\item{feature_name}{Name of the binned feature.}
\item{direction}{increasing or decreasing observed bin risks.}
\item{min_bin_count, max_prebins}{Positive integer minimum nonmissing support and maximum initial approximate count bins.}
\item{bin_counts}{Rows with unique bin_id, defaults and nondefaults as nonnegative integer counts.}
\item{smoothing}{Nonnegative additive class-conditional smoothing. Positive smoothing needed for zero class cells; both classes must exist before smoothing.}
\item{woe_sign}{nondefault_over_default or default_over_nondefault.}}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Estimates preprocessing and supervised binning only from declared training rows, applies frozen artifacts, and constructs units-aware ratios and exact-calendar vintage-aware lags.}
\details{Standard scaling uses population standard deviation after imputation; robust scaling uses median and type-7 interquartile range. Zero scale becomes one and is reported. Missing and unknown categories have distinct declared handling; categorical mode ties are lexical. One-hot output names and lineage are frozen. Constant columns are retained and require explicit downstream model treatment.

Monotone bins preserve ties, build approximate count prebins, merge small bins with the nearest-risk neighbor (left on ties), then pool adjacent monotonicity violations. This is not claimed globally optimal. Missing has a separate bin exempt from support and monotonicity. Right-closed boundaries use observed maxima; the final NULL upper boundary has no upper limit. Transformation returns bin IDs, not WOE. WOE uses class-conditional smoothed shares; IV is sign-invariant and equals the sum of contributions.

Ratios require aligned statement dates and matching input units, with explicit missing/invalid-denominator statuses instead of infinities. Lags select actual calendar dates and the latest available vintage at the query information cutoff. Month/year lags clamp month ends. Future revisions cannot alter historical-cutoff replay.}
\value{Fits return FitResult with ModelArtifact. Transform returns CalcResult containing design_matrix, feature_order and lineage with coverage diagnostics. Ratios and lags return row records with values, statuses and eligibility. WOE returns bin shares, weights of evidence, IV contributions and total IV.}
\references{Pool-adjacent-violators principle: \url{https://docs.scipy.org/doc/scipy/reference/generated/scipy.optimize.isotonic_regression.html}. All support, unit and vintage policies are explicitly declared library contracts.}
