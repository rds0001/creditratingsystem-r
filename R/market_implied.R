.cr_mkt_records <- function(rows, name, id_key) {
    if (!is.list(rows) || !length(rows) || any(!vapply(rows, .cr_object, logical(1)))) 
        .cr_error("INVALID_INPUT", paste(name, "requires records"))
    ids <- vapply(rows, function(r) .cr_mkt_text(r[[id_key]], id_key), character(1))
    if (anyDuplicated(ids)) 
        .cr_error("DUPLICATE_ID", paste(name, "has duplicate IDs"))
    rows
}

build_credit_quote_snapshot <- function(quotes, instruments, discount_curve, snapshot_contract, context = NULL) {
    d <- .cr_mkt_curve(discount_curve)
    c <- snapshot_contract
    as_of <- .cr_date(c$as_of, "as_of")
    known <- .cr_date(c$known_at, "known_at")
    if (known > as_of || d$known_at > known || !identical(d$as_of, c$as_of) || !identical(d$currency, 
        c$currency)) 
        .cr_error("CONTEXT_MISMATCH", "Snapshot and discount curve conflict")
    max_age <- .cr_mkt_scalar(c$maximum_age_days, "maximum_age_days", 0)
    if (max_age != floor(max_age) || !isTRUE(c$stale_policy %in% c("EXCLUDE", "KEEP_FLAGGED")) || !isTRUE(c$bid_ask_policy %in% 
        c("REQUIRED", "OPTIONAL"))) 
        .cr_error("INVALID_INPUT", "Explicit quote age/stale/bid-ask policies required")
    ins <- .cr_mkt_records(instruments, "instruments", "instrument_id")
    names(ins) <- vapply(ins, function(r) r$instrument_id, character(1))
    rows <- .cr_mkt_records(quotes, "quotes", "quote_id")
    rows <- rows[order(vapply(rows, function(r) r$quote_id, character(1)))]
    scales <- list(CDS_SPREAD = list(BASIS_POINTS = 1e-04, FRACTION = 1), CDS_UPFRONT = list(PERCENT_NOTIONAL = 0.01, 
        FRACTION = 1), BOND_CLEAN_PRICE = list(PER_100_PAR = 1))
    output <- findings <- list()
    groups <- list()
    for (q in rows) {
        instrument <- ins[[q$instrument_id]]
        if (is.null(instrument)) 
            .cr_error("UNKNOWN_ID", "Quote instrument absent")
        for (key in c("issuer_id", "seniority", "currency")) .cr_mkt_text(instrument[[key]], key)
        kind <- .cr_mkt_text(q$quote_type, "quote_type")
        unit <- .cr_mkt_text(q$unit, "unit")
        factor <- scales[[kind]][[unit]]
        if (is.null(factor)) 
            .cr_error("INVALID_UNIT", "Quote type and units incompatible")
        if (!identical(instrument$instrument_type, if (kind == "BOND_CLEAN_PRICE") 
            "BOND"
        else "CDS")) 
            .cr_error("INVALID_QUOTE", "Instrument type mismatch")
        observed <- .cr_date(q$observed_at, "observed_at")
        available <- .cr_date(q$known_at, "known_at")
        if (available < observed) 
            .cr_error("INVALID_DATE", "Quote knowledge precedes observation")
        reasons <- character()
        if (observed > as_of || available > known) 
            reasons <- c(reasons, "FUTURE_INFORMATION")
        age <- as.numeric(as.Date(as_of) - as.Date(observed))
        if (age > max_age) 
            reasons <- c(reasons, "STALE")
        if (instrument$currency != c$currency) 
            reasons <- c(reasons, "CURRENCY_MISMATCH")
        mid <- .cr_mkt_scalar(q$value, "quote.value") * factor
        if (kind %in% c("CDS_SPREAD", "BOND_CLEAN_PRICE") && mid < 0) 
            .cr_error("INVALID_QUOTE", "Negative spread or price")
        bid <- q$bid
        ask <- q$ask
        if (xor(is.null(bid), is.null(ask)) || (is.null(bid) && c$bid_ask_policy == "REQUIRED")) 
            .cr_error("INVALID_QUOTE", "Complete bid/ask pair required")
        if (!is.null(bid)) {
            bid <- .cr_mkt_scalar(bid, "bid") * factor
            ask <- .cr_mkt_scalar(ask, "ask") * factor
            if (!(bid <= mid && mid <= ask)) 
                reasons <- c(reasons, "CROSSED_OR_OUTSIDE_BID_ASK")
        }
        eligible <- !any(reasons != "STALE" | c$stale_policy == "EXCLUDE")
        output[[length(output) + 1L]] <- list(quote_id = q$quote_id, instrument_id = q$instrument_id, 
            issuer_id = instrument$issuer_id, seniority = instrument$seniority, currency = instrument$currency, 
            quote_type = kind, value = mid, bid = bid, ask = ask, normalized_unit = if (kind == "BOND_CLEAN_PRICE") "PER_100_PAR" else "FRACTION", 
            observed_at = q$observed_at, known_at = q$known_at, age_days = age, eligible = eligible, 
            findings = as.list(reasons), instrument = instrument)
        if (length(reasons)) 
            findings[[length(findings) + 1L]] <- list(quote_id = q$quote_id, findings = as.list(reasons))
        groups[[instrument$issuer_id]] <- unique(c(groups[[instrument$issuer_id]], instrument$seniority))
    }
    vintage_groups <- split(seq_along(output), vapply(output, function(r) .cr_hash(list(r$instrument_id, 
        r$quote_type, r$observed_at, r$known_at)), character(1)))
    for (group in vintage_groups) if (length(unique(vapply(output[group], function(r) r$value, numeric(1)))) > 
        1) 
        for (i in group) {
            output[[i]]$eligible <- FALSE
            output[[i]]$findings <- c(output[[i]]$findings, list("CONFLICTING_SAME_VINTAGE_QUOTES"))
        }
    findings <- lapply(Filter(function(r) length(r$findings) > 0, output), function(r) list(quote_id = r$quote_id, 
        findings = r$findings))
    mixed <- list()
    for (k in sort(names(groups))) if (length(groups[[k]]) > 1) 
        mixed[[length(mixed) + 1L]] <- list(issuer_id = k, seniorities = as.list(sort(groups[[k]])))
    value <- list(schema_version = "1.0", kind = "CreditQuoteSnapshot", as_of = c$as_of, known_at = c$known_at, 
        currency = c$currency, quotes = output, findings = findings, cross_seniority_groups = mixed, 
        discount_curve = d, snapshot_contract = c, information_clock = "CALENDAR_DATE")
    value$snapshot_hash <- .cr_hash(value)
    .cr_mkt_out("build_credit_quote_snapshot", value, context)
}

.cr_mkt_hazard <- function(discount, knots, rates) {
    h <- discount[c("as_of", "known_at", "currency", "time_basis", "extrapolation")]
    h$knots <- as.list(knots)
    h$rates <- as.list(rates)
    h$probability_measure <- "RISK_NEUTRAL"
    h
}

.cr_mkt_cal_quotes <- function(quotes, discount, kind) {
    rows <- .cr_mkt_records(quotes, "calibration quotes", "quote_id")
    for (row in rows) {
        if (!identical(row$eligible, TRUE)) 
            .cr_error("INELIGIBLE_QUOTE", "Only eligible quotes may calibrate")
        observed <- .cr_date(row$observed_at, "observed_at")
        known <- .cr_date(row$known_at, "known_at")
        if (observed > known || known > discount$as_of) 
            .cr_error("LOOKAHEAD_BIAS", "Quote knowledge exceeds valuation date")
        ins <- row$instrument
        if (!.cr_object(ins) || !identical(ins$instrument_type, kind)) 
            .cr_error("INVALID_QUOTE", "Full instrument contract required")
        if (!identical(ins$as_of, discount$as_of) || !identical(ins$currency, discount$currency)) 
            .cr_error("CONTEXT_MISMATCH", "Instrument and curve conflict")
        for (key in c("issuer_id", "seniority")) .cr_mkt_text(ins[[key]], key)
        if (!identical(row$normalized_unit, if (kind == "BOND") 
            "PER_100_PAR"
        else "FRACTION")) 
            .cr_error("INVALID_UNIT", "Normalized quote units required")
        .cr_mkt_scalar(row$value, "quote value")
    }
    for (key in c("issuer_id", "seniority")) if (length(unique(vapply(rows, function(r) r$instrument[[key]], 
        character(1)))) != 1) 
        .cr_error("INCOMPATIBLE_QUOTES", "Cannot pool issuer or seniority")
    rows
}

.cr_mkt_seal <- function(value) {
    value$calibration_hash <- .cr_hash(value)
    value
}

.cr_mkt_read_fit <- function(value) {
    if (!.cr_object(value)) 
        .cr_error("INVALID_ARTIFACT", "Calibration required")
    hash <- value$calibration_hash
    value$calibration_hash <- NULL
    if (!identical(hash, .cr_hash(value))) 
        .cr_error("INTEGRITY_ERROR", "Calibration changed after fitting")
    if (!identical(value$kind, "MarketCreditCalibration") || !identical(value$probability_measure, "RISK_NEUTRAL")) 
        .cr_error("INVALID_ARTIFACT", "Expected market calibration")
    value
}

bootstrap_cds_hazard_curve <- function(quotes, discount_curve, calibration_contract, context = NULL) {
    d <- .cr_mkt_curve(discount_curve)
    c <- calibration_contract
    rows <- .cr_mkt_cal_quotes(quotes, d, "CDS")
    recovery <- .cr_mkt_scalar(c$recovery, "recovery", 0, 1)
    upper <- .cr_mkt_scalar(c$maximum_hazard, "maximum_hazard", 0)
    tol <- .cr_mkt_scalar(c$price_tolerance, "price_tolerance", 0)
    if (upper <= 0 || tol <= 0) 
        .cr_error("INVALID_INPUT", "Positive solver bounds and tolerance required")
    knots <- vapply(rows, function(r) .cr_mkt_time(r$instrument$maturity, d$as_of), numeric(1))
    o <- order(knots)
    knots <- knots[o]
    rows <- rows[o]
    if (any(knots <= 0) || any(diff(knots) <= 0)) 
        .cr_error("INVALID_QUOTE", "Distinct increasing maturities required")
    rates <- residuals <- numeric()
    solver <- list()
    feasible <- TRUE
    for (j in seq_along(rows)) {
        row <- rows[[j]]
        instrument <- row$instrument
        instrument$recovery <- recovery
        if (identical(row$quote_type, "CDS_SPREAD")) {
            instrument$coupon <- row$value
            instrument$upfront <- 0
            if (!isTRUE(instrument$accrued_premium == 0)) 
                .cr_error("INVALID_QUOTE", "Spread bootstrap needs zero accrued premium")
        }
        else if (identical(row$quote_type, "CDS_UPFRONT")) 
            instrument$upfront <- row$value
        else .cr_error("INVALID_QUOTE", "CDS quote required")
        objective <- function(rate) cds_expected_cashflow_legs(.cr_mkt_hazard(d, knots[seq_len(j)], c(rates, 
            rate)), d, instrument)$value$upfront_residual_pv
        lo <- objective(0)
        hi <- objective(upper)
        if (abs(lo) <= tol) {
            rate <- 0
            iterations <- 0
        }
        else if (abs(hi) <= tol) {
            rate <- upper
            iterations <- 0
        }
        else if (lo * hi > 0) {
            feasible <- FALSE
            solver[[length(solver) + 1L]] <- list(quote_id = row$quote_id, status = "NO_NONNEGATIVE_BRACKET", 
                residual_at_zero = lo, residual_at_upper = hi)
            break
        }
        else {
            root <- stats::uniroot(objective, c(0, upper), tol = 1e-13)
            rate <- root$root
            iterations <- root$iter
        }
        residual <- objective(rate)
        rates <- c(rates, rate)
        residuals <- c(residuals, residual)
        solver[[length(solver) + 1L]] <- list(quote_id = row$quote_id, status = if (rate %in% c(0, upper)) "BOUNDARY" else "INTERIOR", 
            iterations = as.integer(iterations), residual_at_zero = lo, residual_at_upper = hi)
    }
    if (any(abs(residuals) > tol)) {
        feasible <- FALSE
        for (i in seq_along(residuals)) if (abs(residuals[i]) > tol) 
            solver[[i]]$status <- "RESIDUAL_TOLERANCE_NOT_MET"
    }
    h <- if (length(rates)) 
        .cr_mkt_hazard(d, knots[seq_along(rates)], rates)
    else NULL
    if (!is.null(h)) 
        h$known_at <- max(d$known_at, vapply(rows, function(r) r$known_at, character(1)))
    sensitivities <- list()
    if (feasible) 
        for (j in seq_along(rows)) {
            row <- rows[[j]]
            ins <- row$instrument
            ins$recovery <- recovery
            if (row$quote_type == "CDS_SPREAD") {
                ins$coupon <- row$value
                ins$upfront <- 0
            }
            else ins$upfront <- row$value
            pv <- function(rr, rec) {
                cc <- ins
                cc$recovery <- rec
                cds_expected_cashflow_legs(.cr_mkt_hazard(d, knots, rr), d, cc)$value$upfront_residual_pv
            }
            step <- 1e-06
            minus <- max(0, rates[j] - step)
            plus <- min(upper, rates[j] + step)
            r0 <- r1 <- rates
            r0[j] <- minus
            r1[j] <- plus
            derivative <- (pv(r1, recovery) - pv(r0, recovery))/(plus - minus)
            a <- max(0, recovery - step)
            b <- min(1, recovery + step)
            dr <- (pv(rates, b) - pv(rates, a))/(b - a)
            legs <- cds_expected_cashflow_legs(h, d, ins)$value
            dq <- if (row$quote_type == "CDS_SPREAD") 
                -legs$risky_annuity
            else -.cr_mkt_discount(d, .cr_mkt_time(ins$settlement_date, d$as_of))
            sensitivities[[j]] <- list(quote_id = row$quote_id, hazard_price_derivative = derivative, 
                local_hazard_per_quote = if (abs(derivative) > 1e-14) -dq/derivative else NULL, local_hazard_per_recovery = if (abs(derivative) > 
                  1e-14) -dr/derivative else NULL, scope = "LOCAL_IMPLICIT_DERIVATIVE_PRECEDING_HAZARDS_FIXED")
        }
    value <- .cr_mkt_seal(list(schema_version = "1.0", kind = "MarketCreditCalibration", instrument_type = "CDS", 
        probability_measure = "RISK_NEUTRAL", known_at = max(d$known_at, vapply(rows, function(r) r$known_at, 
            character(1))), feasible = feasible, hazard_curve = h, issuer_id = rows[[1]]$instrument$issuer_id, 
        seniority = rows[[1]]$instrument$seniority, discount_curve = d, quotes = rows, calibration_contract = c, 
        residuals = as.list(residuals), solver = solver, sensitivities = sensitivities, survival = if (is.null(h)) list() else lapply(h$knots, 
            function(t) .cr_mkt_discount(h, t)), assumptions = list(recovery = "FIXED", intensity = "PIECEWISE_CONSTANT", 
            probability_measure = "RISK_NEUTRAL")))
    value$calibration_hash <- NULL
    value["curve_sensitivities"] <- list(if (feasible) .cr_mkt_cds_sensitivity(value) else NULL)
    .cr_mkt_out("bootstrap_cds_hazard_curve", .cr_mkt_seal(value), context)
}

.cr_mkt_jacobian <- function(fn, x, lower, upper) {
    columns <- lapply(seq_along(x), function(j) {
        step <- 1e-05 * max(1, abs(x[j]))
        a <- max(lower[j], x[j] - step)
        b <- min(upper[j], x[j] + step)
        xx <- yy <- x
        xx[j] <- a
        yy[j] <- b
        (fn(yy) - fn(xx))/(b - a)
    })
    do.call(cbind, columns)
}

.cr_mkt_rank <- function(jac) {
    if(any(!is.finite(jac))) .cr_error("NUMERICAL_FAILURE", "Nonfinite calibration Jacobian")
    scale <- sqrt(colSums(jac^2))
    standardized <- sweep(jac, 2, ifelse(scale > 0, scale, 1), "/")
    singular <- svd(standardized, nu = 0, nv = 0)$d
    rank <- sum(singular > max(1e-10, if (length(singular)) singular[1] * 1e-08 else 0))
    full <- rank == ncol(jac)
    list(rank = as.integer(rank), parameter_count = ncol(jac), column_norms = as.list(scale), scaled_singular_values = as.list(singular), 
        full_column_rank = full, scaled_condition_number = if (full) singular[1]/tail(singular, 1) else NULL, 
        rank_relative_tolerance = 1e-08)
}

.cr_mkt_matrix_list <- function(x) lapply(seq_len(nrow(x)), function(i) as.list(x[i, ]))

calibrate_bond_hazard_curve <- function(quotes, discount_curve, calibration_contract, context = NULL) {
    d <- .cr_mkt_curve(discount_curve)
    c <- calibration_contract
    rows <- .cr_mkt_cal_quotes(quotes, d, "BOND")
    knots <- .cr_mkt_vec(c$hazard_knots, "hazard_knots")
    if (any(knots <= 0) || any(diff(knots) <= 0)) 
        .cr_error("INVALID_CURVE", "Increasing positive knots required")
    initial <- .cr_mkt_vec(c$initial_hazards, "initial_hazards")
    upper <- .cr_mkt_scalar(c$maximum_hazard, "maximum_hazard", 0)
    recovery <- .cr_mkt_scalar(c$recovery, "recovery", 0, 1)
    liquidity <- .cr_mkt_scalar(c$liquidity_spread, "liquidity_spread")
    if (!identical(c$weight_interpretation, "KNOWN_GAUSSIAN_PRICE_STANDARD_ERRORS")) 
        .cr_error("INVALID_INPUT", "Explicit Gaussian measurement-error assumptions required")
    if (length(initial) != length(knots) || upper <= 0 || any(initial < 0) || any(initial > upper)) 
        .cr_error("INVALID_INPUT", "Initial hazards must match bounds and knots")
    max_eval <- .cr_mkt_scalar(c$max_evaluations, "max_evaluations", 1)
    if (max_eval != floor(max_eval)) 
        .cr_error("INVALID_INPUT", "Integer max_evaluations required")
    se <- vapply(rows, function(r) .cr_mkt_scalar(r$price_standard_error, "price_standard_error", 0), 
        numeric(1))
    if (any(se <= 0)) 
        .cr_error("INVALID_WEIGHTS", "Positive price errors required")
    if (any(!vapply(rows, function(r) identical(r$quote_type, "BOND_CLEAN_PRICE"), logical(1)))) 
        .cr_error("INVALID_QUOTE", "Bond clean prices required")
    observed <- vapply(rows, function(r) r$value, numeric(1))
    prices <- function(rates) {
        h <- .cr_mkt_hazard(d, knots, rates)
        vapply(rows, function(row) {
            ins <- row$instrument
            ins$recovery <- recovery
            ins$liquidity_spread <- liquidity
            risky_bond_expected_cashflows(h, d, ins)$value$clean_price_per_100
        }, numeric(1))
    }
    residual <- function(rates) (prices(rates) - observed)/se
    lower <- rep(0, length(knots))
    uppers <- rep(upper, length(knots))
    objective <- function(rates) sum(residual(rates)^2)
    gradient <- function(rates) as.numeric(2 * crossprod(.cr_mkt_jacobian(residual, rates, lower, uppers), 
        residual(rates)))
    fit <- stats::optim(initial, objective, gradient, method = "L-BFGS-B", lower = lower, upper = uppers, 
        control = list(maxit = as.integer(max_eval), factr = 10000, pgtol = 1e-08))
    if (fit$convergence != 0 || any(!is.finite(fit$par))) 
        .cr_error("NON_CONVERGENCE", "Bond hazard fit failed", list(solver_status = fit$convergence))
    rates <- fit$par
    jac <- .cr_mkt_jacobian(residual, rates, lower, uppers)
    rank <- .cr_mkt_rank(jac)
    boundary <- rates < 1e-07 | upper - rates < 1e-07
    intervals <- list()
    covariance <- NULL
    if (rank$full_column_rank && !any(boundary)) {
        cov <- solve(crossprod(jac))
        covariance <- .cr_mkt_matrix_list(cov)
        sd <- sqrt(diag(cov))
        intervals <- lapply(seq_along(rates), function(j) list(parameter_index = j - 1L, lower = rates[j] - 
            1.95996398454005 * sd[j], upper = rates[j] + 1.95996398454005 * sd[j], scope = "UNTRUNCATED_LOCAL_WALD_CONDITIONAL_ON_RECOVERY_LIQUIDITY_AND_KNOWN_PRICE_ERRORS"))
    }
    h <- .cr_mkt_hazard(d, knots, rates)
    h$known_at <- max(d$known_at, vapply(rows, function(r) r$known_at, character(1)))
    g <- gradient(rates)/2
    g[rates <= 1e-08 & g > 0] <- 0
    g[rates >= upper - 1e-08 & g < 0] <- 0
    value <- .cr_mkt_seal(list(schema_version = "1.0", kind = "MarketCreditCalibration", instrument_type = "BOND", 
        probability_measure = "RISK_NEUTRAL", known_at = h$known_at, feasible = TRUE, hazard_curve = h, 
        issuer_id = rows[[1]]$instrument$issuer_id, seniority = rows[[1]]$instrument$seniority, discount_curve = d, 
        quotes = rows, calibration_contract = c, residuals = as.list(prices(rates) - observed), weighted_residuals = as.list(residual(rates)), 
        objective = objective(rates), fitted_prices = as.list(prices(rates)), jacobian = .cr_mkt_matrix_list(jac), 
        identifiability = rank, boundary_parameters = as.list(which(boundary) - 1L), parameter_intervals = intervals, 
        covariance = covariance, interval_status = if (length(intervals)) "AVAILABLE_LOCAL_APPROXIMATION" else "UNAVAILABLE_BOUNDARY_OR_RANK_DEFICIENCY", 
        solver = list(converged = TRUE, evaluations = unname(fit$counts[["function"]]), optimality = max(abs(g))), 
        survival = lapply(as.list(knots), function(t) .cr_mkt_discount(h, t)), assumptions = list(recovery = "FIXED", 
            liquidity_spread = "FIXED", price_errors = "KNOWN_GAUSSIAN", probability_measure = "RISK_NEUTRAL")))
    .cr_mkt_out("calibrate_bond_hazard_curve", value, context)
}

.cr_mkt_predicted_quotes <- function(fit, hazards, recovery, liquidity) {
    d <- fit$discount_curve
    h <- .cr_mkt_hazard(d, unlist(fit$hazard_curve$knots), hazards)
    vapply(fit$quotes, function(row) {
        ins <- row$instrument
        ins$recovery <- recovery
        if (fit$instrument_type == "BOND") {
            ins$liquidity_spread <- liquidity
            risky_bond_expected_cashflows(h, d, ins)$value$clean_price_per_100
        }
        else cds_expected_cashflow_legs(h, d, ins)$value[[if (row$quote_type == "CDS_SPREAD") 
            "par_spread"
        else "fair_upfront"]]
    }, numeric(1))
}

diagnose_market_credit_identifiability <- function(calibration, alternative_assumptions, diagnostic_contract, 
    context = NULL) {
    fit <- .cr_mkt_read_fit(calibration)
    c <- diagnostic_contract
    if (!isTRUE(fit$feasible)) 
        .cr_error("INFEASIBLE_CALIBRATION", "Feasible reference fit required")
    scenarios <- .cr_mkt_records(alternative_assumptions, "alternative_assumptions", "scenario_id")
    tolerance <- .cr_mkt_scalar(c$equivalence_tolerance, "equivalence_tolerance", 0)
    quote_scales <- .cr_mkt_vec(c$quote_standard_errors, "quote_standard_errors")
    if (length(quote_scales) != length(fit$quotes) || any(quote_scales <= 0)) 
        .cr_error("INVALID_WEIGHTS", "Positive quote-error scales required")
    reference <- vapply(fit$quotes, function(r) r$value, numeric(1))
    base <- fit$calibration_contract
    rates <- unlist(fit$hazard_curve$rates)
    recovery <- base$recovery
    liquidity <- if (is.null(base$liquidity_spread)) 
        0
    else base$liquidity_spread
    include_liquidity <- fit$instrument_type == "BOND"
    params <- c(rates, recovery, if (include_liquidity) liquidity else numeric())
    rb <- unlist(c$recovery_bounds)
    lb <- unlist(c$liquidity_bounds)
    if (length(rb) != 2 || !all(is.finite(rb)) || rb[1] < 0 || rb[1] >= rb[2] || rb[2] > 1 || recovery < 
        rb[1] || recovery > rb[2]) 
        .cr_error("INVALID_INPUT", "Recovery bounds must contain reference")
    if (include_liquidity && (length(lb) != 2 || !all(is.finite(lb)) || lb[1] >= lb[2] || liquidity < 
        lb[1] || liquidity > lb[2])) 
        .cr_error("INVALID_INPUT", "Liquidity bounds must contain reference")
    lower <- c(rep(0, length(rates)), rb[1], if (include_liquidity) lb[1] else numeric())
    upper <- c(rep(base$maximum_hazard, length(rates)), rb[2], if (include_liquidity) lb[2] else numeric())
    independent <- c$independent_recovery_observation
    if (!is.null(independent)) {
        .cr_mkt_scalar(independent$value, "independent recovery", 0, 1)
        if (.cr_mkt_scalar(independent$standard_error, "independent recovery standard error", 0) <= 0) 
            .cr_error("INVALID_INPUT", "Positive recovery error required")
        .cr_mkt_text(independent$evidence_id, "recovery evidence_id")
        if (.cr_date(independent$known_at, "recovery known_at") > fit$known_at) 
            .cr_error("LOOKAHEAD_BIAS", "Future recovery evidence")
    }
    response <- function(p) {
        values <- (.cr_mkt_predicted_quotes(fit, p[seq_along(rates)], p[length(rates) + 1L], if (include_liquidity) 
            tail(p, 1)
        else 0) - reference)/quote_scales
        if (!is.null(independent)) 
            values <- c(values, (p[length(rates) + 1L] - independent$value)/independent$standard_error)
        values
    }
    jac <- .cr_mkt_jacobian(response, params, lower, upper)
    rank <- .cr_mkt_rank(jac)
    base_objective <- sum(response(params)^2)
    profiles <- list()
    for (scenario in scenarios) {
        rr <- .cr_mkt_scalar(scenario$recovery, "scenario recovery", rb[1], rb[2])
        ll <- .cr_mkt_scalar(if (is.null(scenario$liquidity_spread)) 
            0
        else scenario$liquidity_spread, "scenario liquidity", if (include_liquidity) 
            lb[1]
        else 0, if (include_liquidity) 
            lb[2]
        else 0)
        cc <- base
        cc$recovery <- rr
        if (include_liquidity) 
            cc$liquidity_spread <- ll
        alternative <- tryCatch(if (include_liquidity)
            calibrate_bond_hazard_curve(fit$quotes, fit$discount_curve, cc)$value
        else bootstrap_cds_hazard_curve(fit$quotes, fit$discount_curve, cc)$value,
            creditratingsystem_error=function(e) e)
        if(inherits(alternative,"creditratingsystem_error")) {
            if(!alternative$code %in% c("NON_CONVERGENCE","NUMERICAL_FAILURE")) stop(alternative)
            profiles[[length(profiles)+1L]]<-list(scenario_id=scenario$scenario_id,recovery=rr,
                liquidity_spread=ll,feasible=FALSE,calibration_hash=NULL,hazards=NULL,
                objective=NULL,near_equivalent=FALSE,failure_code=alternative$code)
            next
        }
        row <- list(scenario_id = scenario$scenario_id, recovery = rr, liquidity_spread = ll, feasible = alternative$feasible, 
            calibration_hash = alternative$calibration_hash)
        if (alternative$feasible) {
            pp <- c(unlist(alternative$hazard_curve$rates), rr, if (include_liquidity) ll else numeric())
            obj <- sum(response(pp)^2)
            row <- c(row, list(hazards = alternative$hazard_curve$rates, objective = obj, near_equivalent = obj <= 
                base_objective + tolerance))
        }
        else row <- c(row, list(hazards = NULL, objective = NULL, near_equivalent = FALSE))
        profiles[[length(profiles) + 1L]] <- row
    }
    value <- list(reference_calibration_hash = calibration$calibration_hash, joint_identifiability = rank, 
        parameter_order = as.list(c(paste0("hazard_", seq_along(rates) - 1L), "recovery", if (include_liquidity) "liquidity_spread" else character())), 
        jacobian = .cr_mkt_matrix_list(jac), reference_objective = base_objective, profiles = profiles, 
        equivalent_scenario_ids = as.list(vapply(Filter(function(r) r$near_equivalent, profiles), function(r) r$scenario_id, 
            character(1))), recovery_evidence = if (is.null(independent)) "ASSUMPTION_ONLY" else "INDEPENDENT_OBSERVATION", 
        scope = "FINITE_DECLARED_SCENARIO_PROFILE_NOT_GLOBAL_IDENTIFIED_SET", diagnostic_contract = c)
    .cr_mkt_out("diagnose_market_credit_identifiability", value, context)
}

compare_cds_bond_credit_basis <- function(cds_calibration, bond_calibration, comparison_contract, context = NULL) {
    cds <- .cr_mkt_read_fit(cds_calibration)
    bond <- .cr_mkt_read_fit(bond_calibration)
    c <- comparison_contract
    if (!identical(cds$instrument_type, "CDS") || !identical(bond$instrument_type, "BOND") || !isTRUE(cds$feasible) || 
        !isTRUE(bond$feasible)) 
        .cr_error("INVALID_ARTIFACT", "Feasible CDS and bond calibrations required")
    findings <- character()
    for (key in c("issuer_id", "seniority")) if (cds[[key]] != bond[[key]]) 
        findings <- c(findings, paste0(toupper(key), "_MISMATCH"))
    for (key in c("as_of", "currency", "known_at")) if (cds$discount_curve[[key]] != bond$discount_curve[[key]]) 
        findings <- c(findings, paste0(toupper(key), "_MISMATCH"))
    if (.cr_hash(cds$discount_curve) != .cr_hash(bond$discount_curve)) 
        findings <- c(findings, "DISCOUNT_CONVENTION_MISMATCH")
    recovery <- .cr_mkt_scalar(c$recovery, "comparison recovery", 0, 1)
    if (cds$calibration_contract$recovery != recovery || bond$calibration_contract$recovery != recovery) 
        findings <- c(findings, "RECOVERY_ASSUMPTION_MISMATCH")
    if (!isTRUE(bond$calibration_contract$liquidity_spread == c$bond_liquidity_spread)) 
        findings <- c(findings, "LIQUIDITY_ASSUMPTION_MISMATCH")
    if (!identical(c$funding_treatment, "NO_ADDITIONAL_FUNDING_WEDGE")) 
        .cr_error("UNSUPPORTED_CONVENTION", "No additional funding convention required")
    maximum_age <- .cr_mkt_scalar(c$maximum_quote_age_days, "maximum_quote_age_days", 0)
    if (maximum_age != floor(maximum_age)) 
        .cr_error("INVALID_INPUT", "Integer maximum quote age required")
    for (name in c("CDS", "BOND")) {
        fit <- if (name == "CDS") 
            cds
        else bond
        if (any(vapply(fit$quotes, function(q) as.numeric(as.Date(fit$discount_curve$as_of) - as.Date(q$observed_at)) > 
            maximum_age, logical(1)))) 
            findings <- c(findings, paste0(name, "_STALE_QUOTES"))
    }
    ins <- c$comparison_cds_contract
    if (!.cr_object(ins)) 
        .cr_error("INVALID_INPUT", "Comparison CDS contract required")
    if (!identical(ins$issuer_id, cds$issuer_id) || !identical(ins$seniority, cds$seniority)) 
        findings <- c(findings, "COMPARISON_INSTRUMENT_MISMATCH")
    if (!isTRUE(ins$recovery == recovery)) 
        findings <- c(findings, "COMPARISON_RECOVERY_MISMATCH")
    maturity <- .cr_mkt_time(ins$maturity, cds$discount_curve$as_of)
    if (maturity > min(tail(unlist(cds$hazard_curve$knots), 1), tail(unlist(bond$hazard_curve$knots), 
        1))) 
        findings <- c(findings, "UNMATCHED_MATURITY_EXTRAPOLATION")
    value <- list(comparable = !length(findings), findings = as.list(findings), cds_calibration_hash = cds_calibration$calibration_hash, 
        bond_calibration_hash = bond_calibration$calibration_hash, probability_measure = "RISK_NEUTRAL", 
        basis_sign = "CDS_PAR_SPREAD_MINUS_BOND_IMPLIED_EQUIVALENT_CDS_PAR_SPREAD", interpretation = "CONVENTION_CONDITIONAL_MARKET_BASIS_NOT_PURE_DEFAULT_SIGNAL", 
        comparison_contract = c)
    if (length(findings)) 
        value <- c(value, list(cds_par_spread = NULL, bond_implied_par_spread = NULL, basis = NULL, basis_basis_points = NULL, 
            survival_difference = NULL))
    else {
        a <- cds_expected_cashflow_legs(cds$hazard_curve, cds$discount_curve, ins)$value
        b <- cds_expected_cashflow_legs(bond$hazard_curve, bond$discount_curve, ins)$value
        basis <- a$par_spread - b$par_spread
        value <- c(value, list(cds_par_spread = a$par_spread, bond_implied_par_spread = b$par_spread, 
            basis = basis, basis_basis_points = 10000 * basis, survival_difference = a$maturity_survival - 
                b$maturity_survival))
    }
    .cr_mkt_out("compare_cds_bond_credit_basis", value, context)
}

.cr_mkt_cds_sensitivity <- function(fit) {
    rates <- unlist(fit$hazard_curve$rates)
    c <- fit$calibration_contract
    recovery <- c$recovery
    upper <- c$maximum_hazard
    fn <- function(p) .cr_mkt_predicted_quotes(fit, p, recovery, 0)
    jac <- .cr_mkt_jacobian(fn, rates, rep(0, length(rates)), rep(upper, length(rates)))
    rank <- .cr_mkt_rank(jac)
    a <- max(0, recovery - 1e-06)
    b <- min(1, recovery + 1e-06)
    dr <- (.cr_mkt_predicted_quotes(fit, rates, b, 0) - .cr_mkt_predicted_quotes(fit, rates, a, 0))/(b - 
        a)
    boundary <- any(rates <= 1e-10 | rates >= upper - 1e-10)
    available <- rank$full_column_rank && !boundary
    list(quote_order = lapply(fit$quotes, function(r) r$quote_id), hazard_order = as.list(seq_along(rates) - 
        1L), quote_hazard_jacobian = .cr_mkt_matrix_list(jac), identifiability = rank, hazard_per_quote = if (available) .cr_mkt_matrix_list(solve(jac, 
        diag(length(rates)))) else NULL, hazard_per_recovery = if (available) as.list(as.numeric(solve(jac, 
        -dr))) else NULL, status = if (available) "INTERIOR_IMPLICIT_FULL_CURVE" else "UNAVAILABLE_BOUNDARY_OR_RANK_DEFICIENCY")
}

