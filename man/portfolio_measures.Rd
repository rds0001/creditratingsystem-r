\name{credit_loss_distribution}
\alias{credit_loss_distribution}
\alias{allocate_expected_shortfall}
\alias{incremental_credit_risk}
\alias{credit_concentration_measures}
\alias{asymptotic_factor_loss}
\title{Portfolio Loss Distribution, Tail Allocation and Concentration}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Weighted discrete loss quantiles and exact upper-mass expected shortfall, paired scenario changes, concentration and a homogeneous Gaussian factor benchmark.}
\usage{
credit_loss_distribution(loss_paths, weights, scenario_ids,
  levels = c(.95, .99), currency, context = NULL)
allocate_expected_shortfall(position_loss_paths, weights,
    scenario_ids,
  position_ids, level, currency, groups = NULL, context = NULL)
incremental_credit_risk(base_loss_paths, change_loss_paths, weights,
  scenario_ids, levels = c(.95, .99), currency, change_id,
    context = NULL)
credit_concentration_measures(positions, weight_field, group_fields,
  weight_unit, context = NULL)
asymptotic_factor_loss(pd, lgd, asset_correlation,
    systematic_factor,
  context = NULL)
}
\arguments{
\item{loss_paths, base_loss_paths, change_loss_paths}{Finite signed monetary paths; changes are paired with base paths on identical scenarios.}
\item{weights}{Aligned nonnegative scenario weights with positive total; normalized internally.}
\item{scenario_ids, position_ids}{Distinct nonempty identifiers in input order.}
\item{levels, level}{Tail probability levels in [0,1]; level one uses the maximum-loss limit.}
\item{currency}{Explicit reporting currency matching context.}
\item{context}{Optional calculation context.}
\item{position_loss_paths}{Scenario by position monetary loss matrix.}
\item{groups}{Optional named list assigning exactly one group to each position.}
\item{change_id}{Explicit identifier for the paired portfolio change.}
\item{positions}{Records with unique position_id, nonnegative declared weight and group fields.}
\item{weight_field, group_fields, weight_unit}{Weight column, grouping columns and explicit exposure/risk unit.}
\item{pd, lgd, asset_correlation}{Homogeneous scalar unconditional physical PD, fixed raw LGD and latent asset correlation in [0,1).}
\item{systematic_factor}{Finite standard-normal factor values; negative is adverse.}
}
\value{CalcResult with the requested distribution, allocations, paired changes, concentration dimensions or conditional Gaussian benchmark.}
\details{VaR uses the left inverse CDF. ES selects exactly the upper 1-alpha mass, allocating partial boundary atoms proportionally; level zero is the mean and level one the maximum-loss limit. Signed hedge allocations reconcile to ES. These are supplied-distribution calculations with no inferred confidence intervals. HHI and entropy are descriptive, not regulatory limits. The factor benchmark assumes infinitely granular identical exposures; latent asset correlation is not default correlation. Its unconditional factor law does not establish empirical TTC calibration.}
\references{Rockafellar and Uryasev, Conditional Value-at-Risk for General Loss Distributions. \url{https://sites.math.washington.edu/~rtr/papers/rtr187-CVaR2.pdf}}
