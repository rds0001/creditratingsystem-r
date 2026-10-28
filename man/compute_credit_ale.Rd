\name{compute_credit_ale}
\alias{compute_credit_ale}
\title{Supported First and Purified Second-Order Accumulated Local Effects}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Weighted finite-bin local differences with explicit interpolation, empirical centering and unsupported-cell reporting.}
\usage{compute_credit_ale(predictor, X, model_contract, selected_features,
  bins, feature_types, weights = NULL, min_bin_count = 1,
    context = NULL)}
\arguments{
\item{predictor}{Frozen local prediction function taking a numeric row matrix.}
\item{X}{Reference feature matrix or row lists in frozen feature order.}
\item{model_contract}{Frozen model identity, feature order, output scale and target.}
\item{selected_features}{One or two distinct selected feature names.}
\item{bins}{Named list of finite strictly increasing edges for each selected feature, covering every reference value.}
\item{feature_types}{Named type declarations; selected features must be continuous.}
\item{weights}{Optional nonnegative reference weights with positive total.}
\item{min_bin_count}{Positive minimum count of positive-weight rows in every required bin/cell.}
\item{context}{Optional common calculation context.}
}
\details{Bins are left-closed/right-open except the final upper endpoint. First-order weighted upper-minus-lower local differences are accumulated, linearly interpolated within bins and centered over the original weighted reference. Second-order four-corner mixed differences are double-accumulated; both first-order accumulated components are removed, followed by weighted empirical centering of the bilinear surface. The purification components are returned. Empty or insufficient cells yield NON_ESTIMABLE_SUPPORT with NULL global curves, while supported local differences remain available. No gaps are filled with zero effects or interpolated across. Numeric categories require a separate methodology and are rejected unless explicitly declared continuous. Counts and correlation do not establish off-manifold boundary support or causal effects. The exact finite-bin interpolation convention is part of the method contract.}
\value{CalcResult with centered boundary grid and reference effects when supported, local differences, counts, masses, unsupported cell indices, purification components, correlations and hashes.}
\references{Apley and Zhu, Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models. \url{https://arxiv.org/abs/1612.08468}.}
