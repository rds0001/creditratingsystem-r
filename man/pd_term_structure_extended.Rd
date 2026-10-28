\name{pd_term_structure_extended}
\alias{conditional_pd_from_cumulative}
\alias{hazard_to_pd}
\alias{estimate_transition_matrix}
\alias{project_transition_matrix}
\title{Default curve inversion, annual hazards and rating migration}
\usage{
conditional_pd_from_cumulative(cumulative_pd, context = NULL)
hazard_to_pd(hazard, interval_lengths, context = NULL)
estimate_transition_matrix(ratings, observation_windows, states,
    default_state,
  estimator, knowledge_date, observation_scheme, exits = NULL,
  cohort_period_days = NULL, days_per_year = 365.25,
  empty_row_policy = "error", context = NULL)
project_transition_matrix(matrix, horizon, matrix_kind, states,
    default_state,
  step_length = NULL, time_unit = "years", context = NULL)
}
\arguments{
\item{cumulative_pd}{Nondecreasing finite probability vector.}
\item{hazard}{Nonnegative annual hazard vector.}
\item{interval_lengths}{Positive year fractions, one per hazard.}
\item{ratings}{Record list or data frame with entity_id, at, known_at, state. Dates are ISO calendar dates.}
\item{observation_windows}{Records entity_id, start, end, complete_until, known_at. One window per entity; completeness must be known by cutoff.}
\item{states}{Ordered unique state labels.}
\item{default_state}{Absorbing default label among states.}
\item{estimator}{cohort or duration.}
\item{knowledge_date}{ISO date information cutoff.}
\item{observation_scheme}{exact_event_times or state_carried_until_change. Duration requires exact_event_times.}
\item{exits}{Optional records entity_id, at, known_at. Exits censor follow-up; same-day rating changes are included before censoring.}
\item{cohort_period_days}{Required positive integer common window duration for cohort estimation.}
\item{days_per_year}{Positive calendar-day conversion basis for annual exposure and cohort step length.}
\item{empty_row_policy}{error, or explicit identity assumption for unidentified nondefault rows.}
\item{matrix}{Square origin-row/destination-column transition matrix or annual generator.}
\item{horizon}{Nonnegative projection horizon in years.}
\item{matrix_kind}{transition or generator.}
\item{step_length}{Positive years per discrete transition matrix; absent for generator.}
\item{time_unit}{years.}
\item{context}{Optional consistent calculation context.}}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Separates conditional, cumulative and marginal default probabilities, and discrete migration from continuous-time intensity models.}
\details{Conditional periods after complete default have NULL values and an undefined mask. Annual hazards integrate over positive year fractions using stable exponential arithmetic.

Cohort estimates divide complete-case endpoint counts by origin counts. Censored records are excluded unless default was already observed; informative censoring can bias this estimator. Duration estimates divide all observed off-diagonal event counts by originating exposure time and set diagonal intensities to negative row totals. This is a homogeneous Markov maximum-likelihood estimator only when all actual state-change times are observed. Periodic panel observations require a different likelihood. State at window start is the last known rating at or before start; later events occur on (start,end]. Future-known records are ignored; duplicate rating dates require upstream resolution. Default is absorbing and observed cures are rejected. Unknown rows error unless no migration is explicitly assumed and reported.

Discrete projection uses an integer number of periods and repeated matrix multiplication. Generator projection uses native Matrix matrix exponential. Inputs must have valid stochastic or generator rows and exactly absorbing default. Projection assumes time-homogeneous Markov dynamics; no generator is inferred from a transition matrix.}
\value{CalcResult. Curve inversion returns conditional_pd and undefined_after_full_default. Hazard conversion returns conditional_pd, cumulative_pd, survival and marginal_pd. Estimation returns matrix, matrix_kind, counts and exposure_years, with exclusions and unidentified rows in diagnostics. Projection returns matrix, pd_by_origin and survival_by_origin.}
\references{Jackson (2011), Multi-State Models for Panel Data: The msm Package for R, \doi{10.18637/jss.v038.i08}. See also \code{Matrix::expm}.}
