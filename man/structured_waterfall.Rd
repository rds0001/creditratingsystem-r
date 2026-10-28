\name{run_tranche_waterfall}
\alias{run_tranche_waterfall}
\title{Contractual tranche cash accounts and payment priorities}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Allocates actual pool cash to senior costs, interest, liquidity, reserves and tranche principal with explicit triggers and terminal identification.}
\usage{
run_tranche_waterfall(asset_pool_paths, structure_profile,
    context = NULL)
}
\arguments{
\item{asset_pool_paths}{Bound AssetPoolPaths from simulate_asset_pool.}
\item{structure_profile}{Versioned structure and currency; payment_dates; tranches with identity, unique seniority, notional, legal_maturity and dated coupon_fractions; dated additional fees; reserve and liquidity limits; principal priority and diversion rules; OC/IC thresholds and protected seniority; terminal_policy.}
\item{context}{Optional context consistent with physical scenario currency, conditioning and actual calendar-day horizon.}
}
\details{Pool interest and principal/recovery enter separate accounts. Pool workout and servicing costs and nonoverlapping additional deal fees rank first. Tranche coupon and prior arrears follow seniority. Reserve and repayable liquidity provide explicit support. Liquidity repayment precedes reserve refill and tranche principal; it can use principal even where direct principal-to-interest diversion is prohibited. Remaining interest is diverted to principal or equity as configured. Outstanding interest arrears block all principal distributions.

OC uses performing principal, pending net modeled recovery and actual cash over protected outstanding debt. IC uses interest after fees over protected coupon and arrears. A breach forces sequential principal and may authorize principal-to-interest diversion. Ratios with zero denominators are null and do not trigger.

REQUIRE_COMPLETE requires full pool runoff, releases final reserve, disallows new terminal liquidity and records ultimate residual principal write-down. KEEP_OPEN returns null ultimate loss. Timely-interest and legal-maturity failures persist after subsequent cure. No interest on arrears or post-maturity penalty claim is invented. Initial reserve is separately funded support.}
\value{CalcResult containing a hashed TrancheWaterfallPaths artifact with dated tranche cashflows, account conservation residuals, persistent payment-failure indicators and ending principal loss identification.}
