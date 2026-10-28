\name{importance_sample_defaults}
\alias{importance_sample_defaults}
\title{Gaussian Importance Sampling for First Default and Loss}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Evaluates a local deterministic default evaluator under a complete Gaussian mean-shift proposal with ordinary density-ratio estimates.}
\usage{importance_sample_defaults(standard_normal_innovations,
    proposal_shift,
  evaluator, importance_contract, context = NULL)}
\arguments{
\item{standard_normal_innovations}{Finite trial by independent standard-normal coordinate matrix.}
\item{proposal_shift}{Finite shift vector covering every coordinate.}
\item{evaluator}{Local function returning aligned defaulted and economic_loss vectors, or a CalcResult containing them. Nondefaults have zero default loss. All randomness must enter via the innovation matrix.}
\item{importance_contract}{Versioned contract identifying evaluator, subject, currency, measure, horizon, time unit, initially-performing first-default conditioning, INDEPENDENT_STANDARD_NORMAL base law, MEAN_SHIFT proposal, IID or DEPENDENT_OR_QMC design, and both deterministic/all-randomness declarations.}
\item{context}{Optional matching calculation context.}}
\value{CalcResult containing ordinary PD and loss estimates, likelihood ratios and contributions, separate self-normalized PD, weight concentration and applicable IID sample standard errors.}
\details{For x=z+mu the likelihood ratio is exp(-mu'z-mu'mu/2). Freeze the proposal independently of evaluated IID draws. The evaluator defines default, not the sampling transformation. Ordinary finite-sample PD estimates can exceed one and are not clipped. Self-normalized estimates are generally biased. Zero proposal defaults provide no exact target-PD upper bound. QMC/dependent designs have no IID standard error. Finite second moments are required but not established for arbitrary caller loss functions. Density-ratio numerical underflow and overflow are rejected.}
\references{Art B. Owen, Monte Carlo theory, methods and examples, Chapter 9. \url{https://artowen.su.domains/mc/Ch-var-is.pdf}}
