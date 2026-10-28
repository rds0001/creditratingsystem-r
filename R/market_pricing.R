.cr_mkt_vec <- function(x, name) {
    if (is.list(x)) 
        for (v in x) {
            vv <- .cr_numeric(v, name, vector_only = TRUE)
            if (length(vv) != 1L) 
                .cr_error("INVALID_INPUT", paste(name, "requires scalar array elements"))
        }
    .cr_numeric(unlist(x), name, vector_only = TRUE)
}

.cr_mkt_scalar <- function(x, name, lower = -Inf, upper = Inf) {
    x <- .cr_numeric(x, name)
    if (length(x) != 1L || x < lower || x > upper) 
        .cr_error("INVALID_INPUT", paste(name, "violates scalar bounds"))
    as.numeric(x)
}

.cr_mkt_text <- function(x, name) {
    .cr_string(x, name, "INVALID_INPUT")
    x
}

.cr_mkt_curve <- function(curve, hazard = FALSE) {
    if (!.cr_object(curve)) 
        .cr_error("INVALID_CURVE", "Curve contract required")
    c <- curve
    a <- .cr_date(c$as_of, "curve.as_of")
    k <- .cr_date(c$known_at, "curve.known_at")
    if (k > a) 
        .cr_error("LOOKAHEAD_BIAS", "Curve information later than valuation")
    .cr_mkt_text(c$currency, "currency")
    if (!identical(c$time_basis, "ACT/365F") || !identical(c$extrapolation, "FLAT_LAST_RATE")) 
        .cr_error("INVALID_CURVE", "ACT/365F and FLAT_LAST_RATE required")
    if (hazard && !identical(c$probability_measure, "RISK_NEUTRAL")) 
        .cr_error("MEASURE_MISMATCH", "Market hazard must be risk neutral")
    knots <- .cr_mkt_vec(c$knots, "knots")
    rates <- .cr_mkt_vec(c$rates, "rates")
    if (length(knots) != length(rates) || any(knots <= 0) || any(diff(knots) <= 0) || (hazard && any(rates < 
        0))) 
        .cr_error("INVALID_CURVE", "Increasing interval ends and admissible rates required")
    c$knots <- as.list(knots)
    c$rates <- as.list(rates)
    c
}

.cr_mkt_rate <- function(c, t) c$rates[[min(findInterval(t, unlist(c$knots)) + 1L, length(c$rates))]]

.cr_mkt_integrated <- function(c, t) {
    if (t < 0) 
        .cr_error("INVALID_DATE", "Curve evaluation before origin")
    result <- 0
    start <- 0
    for (i in seq_along(c$knots)) {
        end <- c$knots[[i]]
        result <- result + c$rates[[i]] * max(0, min(t, end) - start)
        if (t <= end) 
            return(result)
        start <- end
    }
    result + c$rates[[length(c$rates)]] * (t - start)
}

.cr_mkt_discount <- function(c, t, spread = 0) {
    exponent <- -.cr_mkt_integrated(c, t) - spread * t
    if (abs(exponent) > 700) 
        .cr_error("NUMERICAL_FAILURE", "Discount or survival exponent exceeds range")
    exp(exponent)
}

.cr_mkt_moments <- function(q, length) {
    z <- q * length
    if (!is.finite(z) || -z > 700) .cr_error("NUMERICAL_FAILURE", "Cashflow integration exponent exceeds stable range")
    if (abs(z) < 1e-04) {
        k <- 0:11
        a <- sum((-z)^k/factorial(k)/(k + 1)) * length
        b <- sum((-z)^k/factorial(k)/(k + 2)) * length^2
    }
    else {
        e <- exp(-z)
        a <- -expm1(-z)/q
        b <- (1 - (1 + z) * e)/(q * q)
    }
    c(a, b)
}

.cr_mkt_integral <- function(h, d, start, end, spread = 0, hazard_multiplier = 1, default_density = TRUE, 
    linear_origin = NULL, linear_scale = 1) {
    points <- sort(unique(c(start, end, unlist(h$knots), unlist(d$knots))))
    points <- points[points >= start & points <= end]
    total <- 0
    if (length(points) < 2) 
        return(0)
    for (i in seq_len(length(points) - 1L)) {
        a <- points[i]
        b <- points[i + 1L]
        lam <- .cr_mkt_rate(h, (a + b)/2)
        r <- .cr_mkt_rate(d, (a + b)/2)
        mm <- .cr_mkt_moments(r + spread + hazard_multiplier * lam, b - a)
        exponent <- -.cr_mkt_integrated(d, a) - spread * a - hazard_multiplier * .cr_mkt_integrated(h, 
            a)
        if (abs(exponent) > 700) 
            .cr_error("NUMERICAL_FAILURE", "Integral exponent exceeds range")
        value <- if (is.null(linear_origin)) 
            mm[1]
        else linear_scale * ((a - linear_origin) * mm[1] + mm[2])
        total <- total + exp(exponent) * (if (default_density) 
            lam
        else 1) * value
    }
    if (!is.finite(total)) 
        .cr_error("NUMERICAL_FAILURE", "Nonfinite expected cashflow")
    total
}

.cr_mkt_time <- function(date, origin) as.numeric(as.Date(.cr_date(date, "cashflow date")) - as.Date(.cr_date(origin, 
    "as_of")))/365

.cr_mkt_setup <- function(hazard_curve, discount_curve, contract) {
    h <- .cr_mkt_curve(hazard_curve, TRUE)
    d <- .cr_mkt_curve(discount_curve)
    if (!.cr_object(contract)) 
        .cr_error("INVALID_INPUT", "Instrument contract required")
    for (key in c("as_of", "currency")) if (!identical(h[[key]], d[[key]]) || !identical(contract[[key]], 
        d[[key]])) 
        .cr_error("CONTEXT_MISMATCH", paste("Instrument and curve", key, "differ"))
    if (!identical(contract$probability_measure, "RISK_NEUTRAL")) 
        .cr_error("MEASURE_MISMATCH", "Risk-neutral contract required")
    if (!identical(contract$default_settlement, "AT_DEFAULT")) 
        .cr_error("UNSUPPORTED_CONVENTION", "Recovery must settle at default")
    list(h = h, d = d, c = contract)
}

.cr_mkt_out <- function(method, value, context = NULL) {
    ctx <- .cr_result(NULL, "market_implied.context", context = context)$context
    if (!is.null(ctx$probability_measure) && !identical(ctx$probability_measure, "RISK_NEUTRAL")) 
        .cr_error("MEASURE_MISMATCH", "Market pricing context must be risk neutral")
    valuation <- if (is.null(value$discount_curve)) 
        value
    else value$discount_curve
    for (ck in c("as_of", "currency")) {
        vk <- if (ck == "as_of") 
            "as_of"
        else "currency"
        if (!is.null(ctx[[ck]]) && !is.null(valuation[[vk]]) && !identical(ctx[[ck]], valuation[[vk]])) 
            .cr_error("CONTEXT_MISMATCH", paste("Context conflicts with", vk))
    }
    known <- if (is.null(value$known_at)) 
        valuation$known_at
    else value$known_at
    if (!is.null(ctx$known_at) && !is.null(known) && ctx$known_at < known) 
        .cr_error("LOOKAHEAD_BIAS", "Context knowledge precedes market vintage")
    .cr_result(value, paste0("market_implied.", method), context = context, provenance = if (method %in% 
        c("bootstrap_cds_hazard_curve", "calibrate_bond_hazard_curve")) 
        list(parameter_origin = "ESTIMATED")
    else list(), limitations = "Deterministic continuous-default valuation with supplied schedules; no ISDA-equivalence claim. Risk-neutral PD is not physical PD.")
}

cds_expected_cashflow_legs <- function(hazard_curve, discount_curve, cds_contract, context = NULL) {
    z <- .cr_mkt_setup(hazard_curve, discount_curve, cds_contract)
    h <- z$h
    d <- z$d
    c <- z$c
    recovery <- .cr_mkt_scalar(c$recovery, "recovery", 0, 1)
    coupon <- .cr_mkt_scalar(c$coupon, "coupon", 0)
    start <- .cr_mkt_time(c$protection_start, c$as_of)
    end <- .cr_mkt_time(c$maturity, c$as_of)
    if (start < 0 || end <= start) 
        .cr_error("INVALID_DATE", "Positive future protection interval required")
    settlement <- .cr_mkt_time(c$settlement_date, c$as_of)
    if (settlement < 0 || settlement > end) 
        .cr_error("INVALID_DATE", "Settlement outside protection interval")
    accrued <- .cr_mkt_scalar(c$accrued_premium, "accrued_premium")
    upfront <- .cr_mkt_scalar(c$upfront, "upfront")
    accrual <- 0
    if (identical(c$premium_mode, "CONTINUOUS")) {
        premium <- .cr_mkt_integral(h, d, start, end, default_density = FALSE)
        if (accrued != 0) 
            .cr_error("INVALID_INPUT", "Continuous premium has no discrete accrued settlement")
    }
    else if (identical(c$premium_mode, "SCHEDULED")) {
        rows <- c$schedule
        if (!is.list(rows) || !length(rows)) 
            .cr_error("INVALID_SCHEDULE", "Coupon schedule required")
        if (!isTRUE(c$day_count %in% c("ACT/360", "ACT/365F")) || !is.logical(c$accrual_on_default) || 
            length(c$accrual_on_default) != 1 || is.na(c$accrual_on_default)) 
            .cr_error("INVALID_INPUT", "Explicit day count and accrual flag required")
        premium <- 0
        previous <- NULL
        for (row in rows) {
            a <- .cr_mkt_time(row$accrual_start, c$as_of)
            b <- .cr_mkt_time(row$accrual_end, c$as_of)
            p <- .cr_mkt_time(row$payment_date, c$as_of)
            if (b <= a || b <= start || p < b || p < 0 || (!is.null(previous) && abs(previous - a) > 
                1e-12)) 
                .cr_error("INVALID_SCHEDULE", "Contiguous accrual with valid payment required")
            if ((is.null(previous) && a > start) || b > end + 1e-12) 
                .cr_error("INVALID_SCHEDULE", "Accrual must cover protection interval")
            previous <- b
            scale <- 365/(if (c$day_count == "ACT/360") 
                360
            else 365)
            premium <- premium + (b - a) * scale * .cr_mkt_discount(d, p) * .cr_mkt_discount(h, b)
            if (c$accrual_on_default) 
                accrual <- accrual + .cr_mkt_integral(h, d, max(start, a), b, linear_origin = a, linear_scale = scale)
        }
        if (abs(previous - end) > 1e-12) 
            .cr_error("INVALID_SCHEDULE", "Last coupon end must equal maturity")
    }
    else .cr_error("INVALID_INPUT", "Premium mode must be CONTINUOUS or SCHEDULED")
    protection <- (1 - recovery) * .cr_mkt_integral(h, d, start, end)
    annuity <- premium + accrual
    settle_df <- .cr_mkt_discount(d, settlement)
    fair_upfront <- (protection - coupon * annuity)/settle_df - accrued
    value <- list(as_of = c$as_of, known_at = max(h$known_at, d$known_at), currency = c$currency, probability_measure = "RISK_NEUTRAL", 
        premium_annuity = premium, accrual_on_default_annuity = accrual, risky_annuity = annuity, premium_pv = coupon * 
            premium, accrual_on_default_pv = coupon * accrual, protection_pv = protection, settlement_accrued_pv = accrued * 
            settle_df, par_spread = if (annuity > 0) protection/annuity else NULL, fair_upfront = fair_upfront, 
        upfront_residual_pv = (fair_upfront - upfront) * settle_df, maturity_survival = .cr_mkt_discount(h, 
            end), pricing_contract_hash = .cr_hash(c))
    .cr_mkt_out("cds_expected_cashflow_legs", value, context)
}

risky_bond_expected_cashflows <- function(hazard_curve, discount_curve, bond_contract, context = NULL) {
    z <- .cr_mkt_setup(hazard_curve, discount_curve, bond_contract)
    h <- z$h
    d <- z$d
    c <- z$c
    recovery <- .cr_mkt_scalar(c$recovery, "recovery", 0, 1)
    liquidity <- .cr_mkt_scalar(c$liquidity_spread, "liquidity_spread")
    convention <- c$recovery_convention
    if (!isTRUE(convention %in% c("PAR", "TREASURY", "MARKET_VALUE"))) 
        .cr_error("UNSUPPORTED_CONVENTION", "Explicit recovery convention required")
    if (!isTRUE(c$cashflow_assumption %in% c("CONTRACTUAL_NONOPTIONAL", "EXOGENOUS_OPTION_SCENARIO"))) 
        .cr_error("UNSUPPORTED_CONVENTION", "Explicit optional cashflow assumption required")
    if (c$cashflow_assumption == "EXOGENOUS_OPTION_SCENARIO") 
        .cr_mkt_text(c$option_scenario_id, "option_scenario_id")
    rows <- c$schedule
    if (!is.list(rows) || !length(rows)) 
        .cr_error("INVALID_SCHEDULE", "Bond schedule required")
    times <- coupons <- principals <- numeric()
    for (row in rows) {
        t <- .cr_mkt_time(row$payment_date, c$as_of)
        if (t <= 0 || (length(times) > 0 && t <= tail(times, 1))) 
            .cr_error("INVALID_SCHEDULE", "Strictly increasing future payments required")
        times <- c(times, t)
        coupons <- c(coupons, .cr_mkt_scalar(row$coupon_amount, "coupon_amount", 0))
        principals <- c(principals, .cr_mkt_scalar(row$principal_amount, "principal_amount", 0))
    }
    face <- .cr_mkt_scalar(c$outstanding_principal, "outstanding_principal", 0)
    if (face <= 0 || abs(sum(principals) - face) > 1e-10 * max(1, face)) 
        .cr_error("INVALID_SCHEDULE", "Amortization must conserve principal")
    accrued <- .cr_mkt_scalar(c$accrued_interest, "accrued_interest", 0)
    if (!identical(c$settlement_date, c$as_of)) 
        .cr_error("UNSUPPORTED_CONVENTION", "Same-day bond settlement required")
    price <- function(spread) {
        disc <- vapply(times, function(t) .cr_mkt_discount(d, t, spread) * .cr_mkt_discount(h, t), numeric(1))
        cp <- sum(coupons * disc)
        pp <- sum(principals * disc)
        if (convention == "PAR") {
            outstanding <- face
            previous <- 0
            rp <- 0
            for (i in seq_along(times)) {
                rp <- rp + recovery * outstanding * .cr_mkt_integral(h, d, previous, times[i], spread = spread)
                outstanding <- outstanding - principals[i]
                previous <- times[i]
            }
        }
        else if (convention == "MARKET_VALUE") {
            total <- sum((coupons + principals) * vapply(times, function(t) exp(-.cr_mkt_integrated(d, 
                t) - spread * t - (1 - recovery) * .cr_mkt_integrated(h, t)), numeric(1)))
            rp <- total - cp - pp
        }
        else {
            zero <- d
            zero$rates <- as.list(rep(0, length(d$rates)))
            rp <- sum((coupons + principals) * vapply(times, function(t) recovery * .cr_mkt_discount(d, 
                t) * .cr_mkt_integral(h, zero, 0, t, spread = spread), numeric(1)))
        }
        c(cp, pp, rp)
    }
    prices <- price(liquidity)
    cp <- prices[1]
    pp <- prices[2]
    rp <- prices[3]
    zero_liquidity <- sum(price(0))
    risk_free <- sum((coupons + principals) * vapply(times, function(t) .cr_mkt_discount(d, t), numeric(1)))
    dirty <- sum(prices)
    value <- list(as_of = c$as_of, known_at = max(h$known_at, d$known_at), currency = c$currency, probability_measure = "RISK_NEUTRAL", 
        recovery_convention = convention, coupon_pv = cp, principal_pv = pp, recovery_pv = rp, dirty_price = dirty, 
        clean_price = dirty - accrued, clean_price_per_100 = 100 * (dirty - accrued)/face, risk_free_price = risk_free, 
        credit_only_price = zero_liquidity, credit_price_effect = zero_liquidity - risk_free, liquidity_price_effect = dirty - 
            zero_liquidity, maturity_survival = .cr_mkt_discount(h, tail(times, 1)), pricing_contract_hash = .cr_hash(c), 
        liquidity_treatment = "EXOGENOUS_DISCOUNT_WEDGE_ALL_DEFAULT_AND_PROMISED_CASHFLOWS")
    if (any(!is.finite(unlist(value[vapply(value, is.numeric, logical(1))])))) 
        .cr_error("NUMERICAL_FAILURE", "Nonfinite bond price")
    .cr_mkt_out("risky_bond_expected_cashflows", value, context)
}

