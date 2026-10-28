\name{reconcile_card_authorizations}
\alias{reconcile_card_authorizations}
\title{Reconcile Dated Card Authorizations and Settlements}
\usage{
reconcile_card_authorizations(authorizations, events, policy, as_of,
  known_at, currency, context = NULL)
}
\arguments{
\item{authorizations}{Rows with authorization_id, account_id, approved_at,
known_at, sequence, currency, amount and expires_at.}
\item{events}{Rows with event_id, authorization_id, account_id, occurred_at,
known_at, sequence, currency, amount and kind. SETTLEMENT requires Boolean
 authorization_complete; REVERSAL requires reverses_settlement_id;
CANCELLATION has zero amount.}
\item{policy}{Named list with policy_id, version, known_at,
expiry_boundary equal to EXCLUSIVE, logical late_settlement_allowed,
authorization_conversion_factor in [0,1], and positive minor_unit.}
\item{as_of}{ISO observation date.}
\item{known_at}{ISO knowledge cutoff.}
\item{currency}{Explicit common currency.}
\item{context}{Optional calculation context matching dates and currency.}
}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Replaces settled reservations by booked money, preserving event
identity and separate knowledge time. No total account exposure is inferred.}
\details{A partial capture of 80 from a reservation of 200 leaves booked
exposure 80 and pending authorization 120. Complete capture releases the
unused reservation. Reversal reduces booked exposure without reopening the
hold. Expiry is exclusive; late settlement needs explicit policy permission.
Cancellation closes a hold without refunding a purchase. Overcapture and
reversal above the original remaining settlement are rejected.

Amounts are reconciled in integer minor units, with an explicit maximum of
2\verb{^}40 units for each amount and account aggregate. Binary representation
error tolerance in units is max(1e-9, min(1e-3, 8*2\verb{^}(-52)*abs(amount/minor_unit))).
This is not a business rounding rule. The scenario conversion factor is supplied,
not an estimated CCF, and can produce fractional minor-unit scenario exposures.
Only supplied reservations and their booked charges are covered; account
payments and installment conversions require a separate cash ledger.}
\value{A CalcResult with authorization and account results, ordered event
ledger, excluded records, explicit policy and input/output fingerprints.}
