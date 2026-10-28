\name{covered_bond_dual_recourse}
\alias{covered_bond_dual_recourse}
\title{Capped issuer, cover-pool and residual-estate recovery}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Allocates covered-bond payments under externally supplied legal, segregation, maturity-extension and recovery conventions.}
\usage{
covered_bond_dual_recourse(cover_pool, cover_pool_paths,
    bond_contract,
  issuer_paths, context = NULL)
}
\arguments{
\item{cover_pool}{Bound mortgage or public-sector CreditAssetPool.}
\item{cover_pool_paths}{Matching complete-runoff AssetPoolPaths with physical shared scenarios.}
\item{bond_contract}{Versioned identity, currency, legal_profile_id, principal, original and optional extended maturity, dated original/extension coupons, extension rule, segregation/estate conventions and shared innovation hash.}
\item{issuer_paths}{Scenario rows with nullable issuer_default_date, dated issuer_capacity, extension_triggered, estate_payment_date, estate_recovery_fraction and estate_capacity.}
\item{context}{Optional compatible physical currency/conditioning and final calendar-day settlement horizon.}
}
\details{Cover collections accumulate net of pool costs in a segregated account. Before issuer default, issuer capacity pays due claims; after default, cover cash pays. Default precedes same-day issuer payments. Interest ranks before principal. Declared extension requires issuer default by original maturity and shifts principal while activating only explicitly supplied extra coupons.

Residual estate settlement follows complete cover runoff and every possible maturity date. Recovery is capped by remaining claim and estate capacity, preventing double recovery. Original-date deferral, effective-maturity shortfall, timely-interest failure and ultimate loss remain separate. Legal enforceability and national insolvency rules are external assumptions. No penalty interest or automatic recovery uplift is inferred.}
\value{CalcResult with a bound CoveredBondRecoursePaths artifact, source-separated cashflows, account reconciliation and scenario ending shortfalls.}
\references{\url{https://finance.ec.europa.eu/banking/banking-regulation/covered-bonds_en}}
