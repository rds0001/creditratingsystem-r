\name{credit_dependence}
\alias{validate_dependence_matrix}
\alias{fit_copula}
\alias{simulate_copula}
\alias{joint_default_probability}
\alias{make_shared_innovations}
\title{Explicit Dependence Models and Addressed Innovations}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Validate dependence matrices, fit Gaussian, t, positive Clayton and Gumbel copulas, transform shared draws, calculate default intersections and write portable innovation files.}
\usage{
validate_dependence_matrix(matrix, variable_ids, matrix_kind,
  repair_policy = "none", tolerance = 1e-10, context = NULL)
fit_copula(observations, variable_ids, family, input_kind,
    tie_policy,
  correlation_repair = "none", df = NULL, df_bounds = c(1, 100),
  target, training, model_id, model_version, context = NULL)
simulate_copula(dependence_model, independent_uniforms,
    variable_ids,
  auxiliary_uniforms = NULL, context = NULL)
joint_default_probability(p_a, p_b, event_order,
  dependence_model = NULL, joint_probability = NULL, context = NULL)
make_shared_innovations(scenario_ids, time_grid, factor_ids,
    stream_spec,
  seed, output_directory, chunk_rows = 1024, context = NULL)
}
\arguments{
\item{matrix}{Finite square matrix in the declared variable order.}
\item{variable_ids}{Distinct ordered variable names.}
\item{matrix_kind}{correlation or covariance.}
\item{repair_policy, correlation_repair}{none preserves the input; spectral explicitly projects eigenvalues and rescales correlations.}
\item{tolerance}{Relative matrix validation tolerance in (0,1).}
\item{observations}{Aligned numeric training matrix, at least three rows and two columns.}
\item{family}{gaussian, t, clayton or gumbel.}
\item{input_kind}{observations for average ranks/(N+1), or uniforms already strictly inside (0,1).}
\item{tie_policy}{error rejects ties; average explicitly labels a continuous approximation.}
\item{df}{Optional fixed positive t degrees of freedom. NULL estimates it conditionally on rank correlation.}
\item{df_bounds}{Positive increasing bounds for t profile likelihood. Boundary estimates are explicitly marked.}
\item{target, training}{Model target and training provenance, including fingerprint, split ID and knowledge date.}
\item{model_id, model_version}{Frozen model identifiers.}
\item{dependence_model}{A copula FitResult or frozen model artifact.}
\item{independent_uniforms}{Open-uniform matrix, one column per frozen variable.}
\item{auxiliary_uniforms}{One uniform column for t/positive Clayton, two for nonindependent Gumbel; absent otherwise.}
\item{p_a, p_b}{Scalar default probabilities.}
\item{event_order}{Two event IDs in the bivariate frozen model order.}
\item{joint_probability}{Explicit intersection; supply this or a model, not both.}
\item{scenario_ids, factor_ids}{Unique address components; reordering scenarios preserves addressed values.}
\item{time_grid}{Nonnegative strictly increasing numeric times.}
\item{stream_spec}{Lists with stream_id and distribution uniform or normal.}
\item{seed}{Nonempty string included in every SHA256 address.}
\item{output_directory}{Absent or empty local directory for float64 little-endian files and manifest.}
\item{chunk_rows}{Positive processing batch size; cannot alter addressed draws.}
\item{context}{Optional calculation context.}
}
\details{Fitting uses Kendall tau inversion. The t family additionally profiles the joint copula likelihood over degrees of freedom with rank correlation fixed. Clayton and Gumbel fits are exchangeable and support nonnegative average tau. Pairwise composite likelihood diagnostics do not imply joint maximum likelihood or an uncertainty estimate. Shared frailty and scale draws are explicit. Defaults are lower-tail uniform events; latent correlation is not default correlation, and zero t correlation does not imply independence. Matrix repairs are disclosed and are not claimed nearest-correlation solutions. Addressed uniforms use the first32 SHA256 bits at cell midpoints; normal draws use two lanes and Box-Muller cosine. Uniform files match bitwise across languages; normal transforms may differ by floating-point rounding.}
\value{CalcResult for matrix validation, simulated uniforms, scalar intersection or innovation manifest; FitResult with a declarative frozen copula model for estimation.}
\references{Yan (2007), Enjoy the Joy of Copulas, Journal of Statistical Software 21(4), doi:10.18637/jss.v021.i04. Native copula package fitCopula documentation.}
