\name{validation_numerics}
\alias{simulation_convergence}
\alias{sensitivity_analysis}
\title{Simulation Precision and Evaluated Frozen Model Sensitivity}
\usage{
simulation_convergence(runs, shared_innovations, sampling_contract,
 precision, design, context = NULL)
sensitivity_analysis(model, baseline, perturbations, method,
 shared_samples, design, context = NULL)
}
\arguments{
\item{runs}{Path results with replicate_id, time_step, sample_size, ordered path_ids, values and canonical innovation_fingerprint.}
\item{shared_innovations}{Named replicates with ordered path_ids and path-by-innovation values.}
\item{sampling_contract}{iid_paths_with_independent_replicates sampling_unit, mean/bernoulli_probability metric, reference, finest_time_step and time_unit; optional analytic_reference value/source.}
\item{precision}{confidence_level, mc_half_width and grid_difference_tolerance.}
\item{design}{Frozen validation identities, target and evaluation cutoff.}
\item{context}{Optional matching calculation context.}
\item{model}{Evaluator descriptor: pd_model kind with fitted_model, or model_graph kind with model_graph, input_data, parameter_snapshots, execution_context and zero-based output_row.}
\item{baseline}{Numeric coordinate vector.}
\item{perturbations}{Named coordinates with location, lower, upper and step. PD coordinates use feature location and full trained order. Graph coordinates use input/parameter location and a mixed string/integer path; numeric indices are zero based in both languages.}
\item{method}{finite_difference or variance_based.}
\item{shared_samples}{NULL for local derivatives; global list of A/B matrices and distribution_contract with independent_inputs TRUE and reference.}
}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Separates genuine sampling uncertainty from paired grid changes and evaluates frozen models under explicit numerical input or graph-parameter interventions.}
\details{Simulation levels require exact shared path prefixes and declared fingerprint matches, at least two paths, nested step multiples and all declared replicates at every level. Fingerprints do not authenticate external simulator execution. Means use sample standard deviation over square root of path count; normal mean intervals are asymptotic. Bernoulli Wilson intervals retain positive upper uncertainty at zero defaults. Paired coarse-minus-fine differences use the same paths. Threshold flags are diagnostic and cannot bound continuous-time discretization bias. Intervals are unadjusted.

Sensitivity evaluates actual predict_pd or evaluate_model_graph outputs. Graph interventions may change numeric input value leaves or declared node parameter leaves, preserving topology/contracts/frozen fitted snapshots. Baselines must equal those leaves. Central or one-sided second-order differences respect bounds and report step-halving differences; elasticity is undefined at zero output. Unrepresentable steps error. Shared A/B designs generate one- and two-coordinate hybrids for Jansen first, total and pair interaction indices. Pooled population variance is the denominator. Correlated coordinate declarations are rejected, zero output variance is non-estimable, finite-sample indices are not clipped and global uncertainty is explicitly not estimated. Independence is declared, not authenticated; neither numerical sensitivity nor variance decomposition is causal.}
\value{CalcResult with ValidationResult metrics, explicit diagnostics, evaluated coordinate traces or innovation nesting, fingerprints and DIAGNOSTIC_ONLY decision.}
\references{Jansen (1999), Computer Physics Communications 117, 35--43, doi:10.1016/S0010-4655(98)00154-4. NIST/SEMATECH e-Handbook, section 7.2.4.1, Wilson confidence intervals.}
