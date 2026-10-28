# Native explicit calibration and leakage-controlled selection estimators.
.cr_cm_scalar <- function(x, name, lower = -Inf, upper = Inf) {
    x <- .cr_numeric(x, name)
    if (length(x) != 1L || x < lower || x > upper) 
        .cr_error("INVALID_INPUT", paste(name, "outside scalar domain"))
    as.numeric(x)
}

.cr_cm_out <- function(value, name, context = NULL, diagnostics = NULL, data_origin = NULL) {
    provenance <- list(parameter_origin = "ESTIMATED")
    if (!is.null(data_origin)) 
        provenance$data_origin <- data_origin
    .cr_result(value, paste0("calibration.", name), context = context, diagnostics = diagnostics, provenance = provenance, 
        limitations = "Estimation uncertainty does not establish external validity or regulatory acceptance.")
}

.cr_cm_artifact <- function(kind, params, meta, inputs, diagnostics, context) {
    method <- switch(kind, beta = "calibrate_beta", hierarchical = "fit_hierarchical_calibration", lgd_level = "calibrate_lgd_level")
    a <- list(schema_version = "1.0", artifact_type = "CalibrationArtifact", calibration_type = kind, 
        method_id = paste0("calibration.", method), parameter_version = "1.0", input_kind = if (kind == 
            "lgd_level") "loss" else "probability", parameters = params, target = meta$target, metadata = meta, 
        calibration_input_fingerprint = .cr_hash(inputs), diagnostics = diagnostics)
    a$artifact_hash <- .cr_hash(a)
    .cr_result(a, paste0("calibration.", method), "calibration_artifact", .cr_cal_context(context, meta$target), 
        diagnostics = diagnostics, provenance = list(data_origin = meta$data_origin, parameter_origin = "ESTIMATED"))
}

calibrate_beta <- function(base_pd, outcomes, weights = NULL, metadata, monotonic = TRUE, boundary_policy = "reject", 
    epsilon = 1e-12, context = NULL) {
    meta <- .cr_cal_metadata(metadata, TRUE)
    d <- .cr_cal_data(base_pd, weights, outcomes, probability = TRUE)
    p <- d$x
    w <- d$w
    y <- d$y
    if (!is.logical(monotonic) || length(monotonic) != 1L || is.na(monotonic) || !boundary_policy %in% 
        c("reject", "clip")) 
        .cr_error("INVALID_INPUT", "Declare monotonic boolean and boundary policy")
    eps <- .cr_cm_scalar(epsilon, "epsilon", 1e-15, 0.01)
    if (boundary_policy == "reject" && any(p == 0 | p == 1)) 
        .cr_error("INVALID_PROBABILITY", "Beta calibration requires interior probabilities")
    q <- if (boundary_policy == "clip") 
        pmin(1 - eps, pmax(eps, p))
    else p
    X <- cbind(1, log(q), -log1p(-q))
    if (qr(X[w > 0, , drop = FALSE])$rank < 3L) 
        .cr_error("NOT_IDENTIFIABLE", "Rank-three beta design required")
    if (length(unique(y[w > 0])) < 2L) 
        .cr_error("SEPARATION", "Both classes required")
    active <- X[w > 0, , drop = FALSE]
    signed <- sweep(active, 2, apply(abs(active), 2, max), "/") * (2 * y[w > 0] - 1)
    if (monotonic) {
        A <- cbind(signed[, 1], -signed[, 1], signed[, 2:3, drop = FALSE])
        sep <- lpSolve::lp("max", colMeans(A), rbind(A, diag(4)), c(rep(">=", nrow(A)), rep("<=", 4)), 
            c(rep(0, nrow(A)), rep(1, 4)))
        if (sep$status != 0L) 
            .cr_error("NUMERICAL_FAILURE", "Beta separation certificate failed")
        if (sep$objval > 1e-08) 
            .cr_error("SEPARATION", "Finite constrained beta MLE unavailable")
    }
    else .cr_glm_separation(active, y[w > 0])
    softplus <- function(z) pmax(z, 0) + log1p(exp(-abs(z)))
    fn <- function(b) sum(w * (softplus(drop(X %*% b)) - y * drop(X %*% b)))
    gr <- function(b) drop(crossprod(X, w * (stats::plogis(drop(X %*% b)) - y)))
    fit <- stats::optim(c(0, 1, 1), fn, gr, method = "L-BFGS-B", lower = if (monotonic) 
        c(-Inf, 0, 0)
    else rep(-Inf, 3), control = list(factr = 1, pgtol = 1e-09, maxit = 2000))
    if (fit$convergence != 0L || max(abs(fit$par)) > 100) 
        .cr_error("NONCONVERGENCE", "Beta MLE did not converge")
    b <- fit$par
    f <- stats::plogis(drop(X %*% b))
    H <- crossprod(X, X * (w * f * (1 - f)))
    if (qr(H)$rank < 3L) 
        .cr_error("NOT_IDENTIFIABLE", "Singular calibration information")
    params <- list(intercept = b[1], a = b[2], b = b[3], monotonic = monotonic, boundary_policy = boundary_policy, 
        epsilon = eps)
    diag <- list(log_likelihood = -fn(b), information = lapply(seq_len(3), function(i) as.list(H[i, ])), 
        conditional_covariance = lapply(seq_len(3), function(i) as.list(solve(H)[i, ])), boundary_active = monotonic && 
            min(b[-1]) < 1e-07, uncertainty_note = "Interior inverse-information approximation; active constraints require boundary-aware inference", 
        clipped_count = sum(q != p))
    .cr_cm_artifact("beta", params, meta, list(p = as.list(p), w = as.list(w), y = as.list(y)), diag, 
        context)
}

.cr_cm_pava <- function(values, mass) {
    blocks <- list()
    for (i in seq_along(values)) {
        blocks[[length(blocks) + 1L]] <- c(i, i, mass[i], values[i] * mass[i])
        while (length(blocks) > 1L) {
            k <- length(blocks)
            a <- blocks[[k - 1L]]
            b <- blocks[[k]]
            if (a[4]/a[3] <= b[4]/b[3]) 
                break
            blocks <- blocks[-c(k - 1L, k)]
            blocks[[length(blocks) + 1L]] <- c(a[1], b[2], a[3] + b[3], a[4] + b[4])
        }
    }
    fit <- numeric(length(values))
    membership <- integer(length(values))
    for (j in seq_along(blocks)) {
        b <- blocks[[j]]
        ix <- b[1]:b[2]
        fit[ix] <- b[4]/b[3]
        membership[ix] <- j - 1L
    }
    list(fitted = fit, membership = membership, blocks = blocks)
}

calibrate_grade_pd <- function(counts, defaults, grades = NULL, prior_alpha = 0, prior_beta = 0, monotonic = TRUE, 
    target_default_rate = NULL, default_grade = NULL, confidence = 0.95, target_weights = NULL, context = NULL) {
    n <- .cr_numeric(counts, "counts", vector_only = TRUE)
    d <- .cr_numeric(defaults, "defaults", vector_only = TRUE)
    if (length(n) != length(d) || !length(n) || any(n <= 0 | d < 0 | d > n | n != floor(n) | d != floor(d))) 
        .cr_error("INVALID_COUNTS", "Require integer 0<=defaults<=positive counts")
    names <- if (is.null(grades)) 
        as.character(seq_along(n) - 1L)
    else unlist(grades, use.names = FALSE)
    if (length(names) != length(n) || !is.character(names) || any(!nzchar(names)) || anyDuplicated(names)) 
        .cr_error("INVALID_INPUT", "Unique ordered grade names required")
    a <- .cr_cm_scalar(prior_alpha, "prior_alpha", 0)
    b <- .cr_cm_scalar(prior_beta, "prior_beta", 0)
    conf <- .cr_cm_scalar(confidence, "confidence", 1e-08, 1 - 1e-08)
    if (!is.logical(monotonic) || length(monotonic) != 1L || is.na(monotonic)) 
        .cr_error("INVALID_INPUT", "monotonic boolean required")
    mask <- rep(TRUE, length(n))
    if (!is.null(default_grade)) {
        j <- match(default_grade, names)
        if (is.na(j)) 
            .cr_error("INVALID_INPUT", "Unknown default grade")
        if (d[j] != n[j]) 
            .cr_error("INVALID_COUNTS", "Default grade must contain only defaults")
        mask[j] <- FALSE
    }
    if (!any(mask)) 
        .cr_error("EMPTY_INPUT", "Performing grade required")
    tw <- if (is.null(target_weights)) 
        n
    else .cr_numeric(target_weights, "target_weights", vector_only = TRUE)
    if (length(tw) != length(n) || any(tw < 0) || !is.finite(sum(tw)) || sum(tw[mask]) <= 0) 
        .cr_error("INVALID_WEIGHTS", "Nonnegative target weights with positive performing total required")
    means <- (d[mask] + a)/(n[mask] + a + b)
    mass <- n[mask] + a + b
    z <- if (monotonic) 
        .cr_cm_pava(means, mass)
    else list(fitted = means, membership = seq_along(means) - 1L, blocks = lapply(seq_along(means), function(i) c(i, 
        i, mass[i], means[i] * mass[i])))
    fitted <- z$fitted
    before <- fitted
    shift <- 0
    if (!is.null(target_default_rate)) {
        t <- .cr_cm_scalar(target_default_rate, "target_default_rate", 0, 1)
        positive <- fitted > 0 & fitted < 1
        lo <- sum(tw[mask][fitted == 1])/sum(tw[mask])
        hi <- sum(tw[mask][fitted > 0])/sum(tw[mask])
        if (!(lo < t && t < hi)) 
            .cr_error("INFEASIBLE_TARGET", "Target cannot preserve grade boundaries")
        f <- function(s) {
            q <- fitted
            q[positive] <- stats::plogis(stats::qlogis(fitted[positive]) + s)
            sum(tw[mask] * q)/sum(tw[mask]) - t
        }
        shift <- stats::uniroot(f, c(-1000, 1000), tol = 1e-12)$root
        fitted[positive] <- stats::plogis(stats::qlogis(fitted[positive]) + shift)
    }
    result <- list()
    k <- 0L
    for (i in seq_along(n)) {
        if (!mask[i]) {
            result[[i]] <- list(grade = names[i], pd = 1, pool = NULL, count = n[i], defaults = d[i], 
                lower = 1, upper = 1, default_grade = TRUE)
            next
        }
        k <- k + 1L
        bl <- z$blocks[[z$membership[k] + 1L]]
        ix <- which(mask)[bl[1]:bl[2]]
        nn <- sum(n[ix])
        dd <- sum(d[ix])
        aa <- dd + a * length(ix)
        bb <- nn - dd + b * length(ix)
        lower <- if (aa == 0) 
            0
        else stats::qbeta((1 - conf)/2, aa, bb + as.numeric(a + b == 0))
        upper <- if (bb == 0) 
            1
        else stats::qbeta((1 + conf)/2, aa + as.numeric(a + b == 0), bb)
        result[[i]] <- list(grade = names[i], pd = fitted[k], pool = z$membership[k], count = n[i], defaults = d[i], 
            lower = lower, upper = upper, default_grade = FALSE)
    }
    .cr_cm_out(list(grades = result, intercept_shift = shift, achieved_mean_pd = sum(tw[mask] * fitted)/sum(tw[mask]), 
        pre_target_mean_pd = sum(tw[mask] * before)/sum(tw[mask]), interval_scope = "Pre-target pooled intervals, conditional on selected pools; no simultaneous/post-selection guarantee", 
        target_weighting = if (is.null(target_weights)) "counts" else "user_supplied", interval_method = if (a + 
            b > 0) "beta_posterior" else "exact_binomial"), "calibrate_grade_pd", context)
}

low_default_bound <- function(defaults, counts, confidence = 0.95, dependence = "independent", correlation = 0, 
    pooling = "none", integration_tolerance = 1e-08, context = NULL) {
    d <- .cr_numeric(defaults, "defaults")
    n <- .cr_numeric(counts, "counts")
    conf <- .cr_cm_scalar(confidence, "confidence", 1e-08, 1 - 1e-08)
    rho <- .cr_cm_scalar(correlation, "correlation", 0, 0.999)
    tol <- .cr_cm_scalar(integration_tolerance, "integration_tolerance", 1e-12, 0.001)
    if (length(d) != length(n) || !length(d) || any(n <= 0 | d < 0 | d > n | n != floor(n) | d != floor(d))) 
        .cr_error("INVALID_COUNTS", "Integer counts/defaults required")
    if (!dependence %in% c("independent", "gaussian_one_factor") || !pooling %in% c("none", "ordered_tail")) 
        .cr_error("INVALID_INPUT", "Unknown dependence/pooling mode")
    if (dependence == "independent" && rho != 0) 
        .cr_error("INVALID_INPUT", "Independent mode requires zero correlation")
    if (pooling == "ordered_tail") {
        d <- rev(cumsum(rev(d)))
        n <- rev(cumsum(rev(n)))
    }
    rows <- list()
    for (j in seq_along(d)) {
        dd <- d[j]
        nn <- n[j]
        worst <- 0
        evaluations <- 0
        cdf <- function(p) {
            evaluations <<- evaluations + 1L
            if (p <= 0) 
                return(1)
            if (p >= 1) 
                return(as.numeric(dd == nn))
            if (rho == 0) 
                return(stats::pbinom(dd, nn, p))
            z <- stats::integrate(function(z) stats::pbinom(dd, nn, stats::pnorm((stats::qnorm(p) - sqrt(rho) * z)/sqrt(1 - rho))) * 
                stats::dnorm(z), -10, 10, abs.tol = tol, rel.tol = tol, subdivisions = 200)
            worst <<- max(worst, z$abs.error)
            z$value
        }
        bound <- if (dd == nn) 
            1
        else stats::uniroot(function(p) cdf(p) - (1 - conf), c(0, 1), tol = 1e-12)$root
        cdfvalue <- cdf(bound)
        rows[[j]] <- list(defaults = dd, count = nn, upper_pd = bound, cdf_at_bound = cdfvalue, integration_error = worst, 
            evaluations = evaluations)
    }
    if (pooling == "ordered_tail") {
        bound <- 0
        for (j in seq_along(rows)) {
            bound <- max(bound, rows[[j]]$upper_pd)
            rows[[j]]$ordered_upper_pd <- bound
        }
    }
    .cr_cm_out(list(bounds = rows, confidence = conf, dependence = dependence, correlation = rho, pooling = pooling, 
        assumption = "Conditional independent Bernoulli given one shared standard normal factor; declared correlation is fixed, not estimated", 
        confidence_scope = "Model-conditional pointwise bounds; unknown dependence and multiple grades lack universal simultaneous coverage"), 
        "low_default_bound", context)
}

fit_hierarchical_calibration <- function(base_pd, outcomes, groups, weights = NULL, metadata, variance_bounds = c(1e-08, 
    25), context = NULL) {
    meta <- .cr_cal_metadata(metadata, TRUE)
    d <- .cr_cal_data(base_pd, weights, outcomes, probability = TRUE, open_domain = TRUE)
    p <- d$x
    w <- d$w
    y <- d$y
    g <- unlist(groups, use.names = FALSE)
    if (length(g) != length(p) || !is.character(g) || any(!nzchar(g))) 
        .cr_error("INVALID_INPUT", "Nonempty group IDs required")
    levels <- sort(unique(g), method = "radix")
    ix <- lapply(levels, function(v) which(g == v))
    if (length(levels) < 2L || any(vapply(ix, function(k) sum(w[k]) == 0, logical(1))) || length(unique(y[w > 
        0])) < 2L) 
        .cr_error("NOT_IDENTIFIABLE", "Two positive groups and both classes required")
    bounds <- .cr_numeric(variance_bounds, "variance_bounds", vector_only = TRUE)
    if (length(bounds) != 2L || !(0 < bounds[1] && bounds[1] < bounds[2] && bounds[2] <= 100)) 
        .cr_error("INVALID_INPUT", "Positive ordered variance bounds <=100 required")
    offset <- stats::qlogis(p)
    softplus <- function(z) pmax(z, 0) + log1p(exp(-abs(z)))
    modes <- function(a, v) {
        ans <- list()
        total <- 0
        gradient <- numeric(2)
        for (j in seq_along(ix)) {
            k <- ix[[j]]
            f <- function(b) sum(w[k] * (y[k] - stats::plogis(offset[k] + a + b))) - b/v
            bound <- max(100, sum(w[k]) * v)
            b <- stats::uniroot(f, c(-bound, bound), tol = 1e-11)$root
            q <- stats::plogis(offset[k] + a + b)
            h <- sum(w[k] * q * (1 - q)) + 1/v
            ll <- sum(w[k] * (y[k] * (offset[k] + a + b) - softplus(offset[k] + a + b))) - b * b/(2 * 
                v) - 0.5 * log(v * h)
            total <- total + ll
            ans[[j]] <- c(b, 1/h)
            third <- sum(w[k] * q * (1 - q) * (1 - 2 * q))
            gradient[1] <- gradient[1] + b/v - 0.5 * third/(v * h * h)
            gradient[2] <- gradient[2] + b * b/(2 * v) - 0.5 - 0.5 * third * b/(v * h * h) + 0.5/(v * 
                h)
        }
        list(total = total, posterior = ans, gradient = gradient)
    }
    fit <- stats::optim(c(0, log(min(bounds[2], max(bounds[1], 0.1)))), function(x) -modes(x[1], exp(x[2]))$total, 
        gr = function(x) -modes(x[1], exp(x[2]))$gradient, method = "L-BFGS-B", lower = c(-30, log(bounds[1])), 
        upper = c(30, log(bounds[2])), control = list(factr = 1, pgtol = 1e-10, maxit = 1000))
    if (fit$convergence != 0L || abs(fit$par[1]) >= 29.99) 
        .cr_error("NONCONVERGENCE", "Hierarchical Laplace fit did not converge")
    a <- fit$par[1]
    v <- exp(fit$par[2])
    z <- modes(a, v)
    effects <- lapply(seq_along(levels), function(j) list(group = levels[j], mode = z$posterior[[j]][1], 
        conditional_variance = z$posterior[[j]][2], shrinkage_information_fraction = 1 - z$posterior[[j]][2]/v))
    params <- list(intercept = a, variance = v, groups = effects, unknown_group = "logistic_normal_marginal", 
        known_group = "conditional_mode", quadrature_points = 40)
    .cr_cm_artifact("hierarchical", params, meta, list(p = as.list(p), w = as.list(w), y = as.list(y), 
        groups = as.list(g)), list(laplace_log_likelihood = z$total, gradient = as.list(z$gradient), 
        variance_at_boundary = v < bounds[1] * 1.01 || v > bounds[2] * 0.99, variance_bounds = as.list(bounds), 
        uncertainty_note = "Conditional group inverse curvature; hyperparameter and approximation uncertainty not included"), 
        context)
}

.cr_cm_loss_metadata <- function(metadata) {
    if (!is.list(metadata) || !is.character(metadata$weight_type) || length(metadata$weight_type) != 1L || !metadata$weight_type %in% c("frequency", "exposure")) 
        .cr_error("INVALID_WEIGHT_TYPE", "Declared frequency/exposure loss weights required")
    kind <- metadata$weight_type
    metadata$weight_type <- "frequency"
    meta <- .cr_cal_metadata(metadata, FALSE)
    meta$weight_type <- kind
    meta
}

calibrate_lgd_level <- function(base_lgd, target_lgd, weights = NULL, metadata, transformation = "additive", 
    context = NULL) {
    meta <- .cr_cm_loss_metadata(metadata)
    x <- .cr_numeric(base_lgd, "base_lgd", vector_only = TRUE)
    w <- if (is.null(weights)) 
        rep(1, length(x))
    else .cr_numeric(weights, "weights", vector_only = TRUE)
    target <- .cr_cm_scalar(target_lgd, "target_lgd")
    if (length(w) != length(x) || any(w < 0) || !is.finite(sum(w)) || sum(w) <= 0 || (meta$weight_type == 
        "frequency" && any(w != floor(w)))) 
        .cr_error("INVALID_WEIGHTS", "Valid declared frequency/exposure weights required")
    if (!transformation %in% c("additive", "multiplicative", "logit")) 
        .cr_error("INVALID_INPUT", "Unknown loss transformation")
    if (transformation == "additive") {
        parameter <- target - sum(w * x)/sum(w)
        adjusted <- x + parameter
    }
    else if (transformation == "multiplicative") {
        mean <- sum(w * x)/sum(w)
        if (any(x < 0) || mean <= 0 || target < 0) 
            .cr_error("INFEASIBLE_TARGET", "Positive base mean and nonnegative losses/target required")
        parameter <- target/mean
        adjusted <- x * parameter
    }
    else {
        .cr_probability(x, "open")
        if (!(0 < target && target < 1)) 
            .cr_error("INFEASIBLE_TARGET", "Interior logit target required")
        parameter <- stats::uniroot(function(z) sum(w * stats::plogis(stats::qlogis(x) + z))/sum(w) - target, c(-1000, 1000), 
            tol = 1e-12)$root
        adjusted <- stats::plogis(stats::qlogis(x) + parameter)
    }
    params <- list(transformation = transformation, parameter = parameter, target_lgd = target, boundary_policy = if (transformation == 
        "additive") "raw" else if (transformation == "multiplicative") "nonnegative" else "reject")
    .cr_cm_artifact("lgd_level", params, meta, list(x = as.list(x), w = as.list(w)), list(achieved_mean = sum(w * 
        adjusted)/sum(w), adjusted_lgd = as.list(adjusted), weighting = meta$weight_type), context)
}

estimate_margin_of_conservatism <- function(base_estimate, components, confidence = 0.95, correlation = NULL, 
    already_included = character(), bounds = NULL, context = NULL) {
    base <- .cr_cm_scalar(base_estimate, "base_estimate")
    conf <- .cr_cm_scalar(confidence, "confidence", 0.5, 1 - 1e-08)
    if (!is.list(components)) 
        .cr_error("INVALID_INPUT", "List of uncertainty components required")
    included <- unlist(already_included, use.names = FALSE)
    if (anyDuplicated(included)) 
        .cr_error("DUPLICATE_UNCERTAINTY", "Duplicate already-included source")
    ids <- character()
    bias <- sd <- numeric()
    detail <- list()
    for (row in components) {
        if (!is.list(row) || !is.character(row$source_id) || length(row$source_id) != 1L || !nzchar(row$source_id) || 
            !is.character(row$category) || length(row$category) != 1L || !row$category %in% c("data", "method", "general")) 
            .cr_error("INVALID_INPUT", "Source and category required")
        sid <- row$source_id
        if (sid %in% ids) 
            .cr_error("DUPLICATE_UNCERTAINTY", "One source cannot count twice")
        ids <- c(ids, sid)
        b <- .cr_cm_scalar(row$adverse_bias, "adverse_bias", 0)
        s <- .cr_cm_scalar(row$standard_error, "standard_error", 0)
        use <- !sid %in% included
        bias <- c(bias, if (use) b else 0)
        sd <- c(sd, if (use) s else 0)
        detail[[length(detail) + 1L]] <- list(source_id = sid, category = row$category, included = use, 
            bias = b, standard_error = s)
    }
    m <- length(ids)
    C <- if (is.null(correlation)) 
        diag(m)
    else as.matrix(.cr_numeric(correlation, "correlation"))
    if (!identical(dim(C), c(m, m)) || (m > 0 && (max(abs(C - t(C))) > 1e-12 || max(abs(diag(C) - 1)) > 
        1e-12 || min(eigen(C, symmetric = TRUE, only.values = TRUE)$values) < -1e-10))) 
        .cr_error("INVALID_CORRELATION", "Symmetric PSD correlation required")
    uncertainty <- if (m) 
        stats::qnorm(conf) * sqrt(max(0, drop(t(sd) %*% C %*% sd)))
    else 0
    moc <- sum(bias) + uncertainty
    raw <- base + moc
    adjusted <- raw
    if (!is.null(bounds)) {
        limit <- .cr_numeric(bounds, "bounds", vector_only = TRUE)
        if (length(limit) != 2L || limit[1] >= limit[2]) 
            .cr_error("INVALID_INPUT", "Ordered bounds required")
        adjusted <- min(limit[2], max(limit[1], raw))
    }
    .cr_cm_out(list(components = detail, confidence = conf, correlation = lapply(seq_len(m), function(i) as.list(C[i, 
        ])), bias_margin = sum(bias), uncertainty_margin = uncertainty, margin = moc, unbounded_adjusted_estimate = raw, 
        adjusted_estimate = adjusted, clipping_adjustment = adjusted - raw, assumption = "Declared joint normal estimation errors; one-sided quantile; no universal regulatory default"), 
        "estimate_margin_of_conservatism", context)
}

population_reweighting <- function(sample_features, target_distribution, method = "poststratification", 
    cap = NULL, outcome_dependent = FALSE, context = NULL) {
    if (!is.logical(outcome_dependent) || length(outcome_dependent) != 1L || is.na(outcome_dependent)) 
        .cr_error("INVALID_INPUT", "Declare outcome_dependent boolean")
    if (method == "poststratification") {
        groups <- unlist(sample_features, use.names = FALSE)
        if (!length(groups) || !is.character(groups) || any(!nzchar(groups)) || !is.list(target_distribution) || 
            is.null(names(target_distribution))) 
            .cr_error("INVALID_INPUT", "Sample stratum strings and target proportions object required")
        target <- lapply(target_distribution, function(v) .cr_cm_scalar(v, "target proportion", 0, 1))
        if (abs(sum(unlist(target)) - 1) > 1e-12) 
            .cr_error("INVALID_INPUT", "Target proportions must sum to one")
        missing <- names(target)[vapply(target, function(v) v > 0, logical(1)) & !names(target) %in% 
            groups]
        if (length(missing)) 
            .cr_error("NO_COMMON_SUPPORT", "Positive target strata absent from sample", list(strata = as.list(missing)))
        freq <- table(groups)/length(groups)
        raw <- vapply(groups, function(g) if (is.null(target[[g]])) 
            0
        else target[[g]]/freq[[g]], numeric(1))
    }
    else if (method == "membership_odds") {
        q <- .cr_probability(sample_features, "open", vector_only = TRUE)
        if (!is.list(target_distribution)) 
            .cr_error("INVALID_INPUT", "Membership priors required")
        ps <- .cr_cm_scalar(target_distribution$sample_prior, "sample_prior", 1e-12, 1 - 1e-12)
        pt <- .cr_cm_scalar(target_distribution$target_prior, "target_prior", 1e-12, 1 - 1e-12)
        if (abs(ps + pt - 1) > 1e-12) 
            .cr_error("INVALID_INPUT", "Membership priors must sum to one")
        raw <- q/(1 - q) * ps/pt
        groups <- NULL
        target <- NULL
    }
    else .cr_error("INVALID_INPUT", "Unknown weighting method")
    if (!is.null(cap)) 
        cap <- .cr_cm_scalar(cap, "cap", 1e-12)
    clipped <- if (is.null(cap)) 
        raw
    else pmin(raw, cap)
    if (any(!is.finite(raw)) || !is.finite(sum(clipped)) || sum(clipped) <= 0) 
        .cr_error("NO_COMMON_SUPPORT", "No positive weights")
    w <- clipped * length(clipped)/sum(clipped)
    realized <- if (is.null(groups)) 
        NULL
    else stats::setNames(lapply(sort(unique(groups), method = "radix"), function(g) sum(w[groups == g])/sum(w)), 
        sort(unique(groups), method = "radix"))
    .cr_cm_out(list(weights = as.list(unname(w)), raw_weights = as.list(unname(raw)), effective_sample_size = sum(w)^2/sum(w * 
        w), clipped_count = sum(clipped != raw), cap_before_normalization = cap, max_final_weight = max(w), 
        realized_distribution = realized, target_distribution = target, outcome_dependent_sampling = outcome_dependent, 
        normalization = "Mean one after cap; final maximum can exceed pre-normalization cap", support_note = "Overlap is explicit by stratum; supplied membership propensities require externally assessed covariate overlap"), 
        "population_reweighting", context)
}

.cr_cm_estimator <- function(value) {
    if (!is.character(value) || length(value) != 1L || !toupper(value) %in% c("OLS", "WLS", "GLM")) 
        .cr_error("INVALID_INPUT", "Unknown estimator")
    toupper(value)
}

.cr_cm_xy <- function(X, y, feature_order, weights = NULL, estimator = "OLS") {
    x <- .cr_numeric(X, "X")
    y <- .cr_numeric(y, "outcomes", vector_only = TRUE)
    features <- unlist(feature_order, use.names = FALSE)
    if (!is.matrix(x) || nrow(x) != length(y) || !length(y) || ncol(x) != length(features) || anyDuplicated(features) || 
        !is.character(features) || any(!nzchar(features))) 
        .cr_error("INVALID_DESIGN", "Matrix and unique feature order required")
    w <- if (is.null(weights)) 
        rep(1, length(y))
    else .cr_numeric(weights, "weights", vector_only = TRUE)
    if (length(w) != length(y) || any(w <= 0) || !is.finite(sum(w))) 
        .cr_error("INVALID_WEIGHTS", "Strictly positive weights required")
    if (!estimator %in% c("OLS", "WLS", "GLM")) 
        .cr_error("INVALID_INPUT", "Unknown estimator")
    if (estimator == "OLS" && any(w != 1)) 
        .cr_error("INVALID_WEIGHTS", "OLS uses unit weights")
    if (estimator == "GLM" && (any(!y %in% c(0, 1)) || any(w != floor(w)))) 
        .cr_error("INVALID_OUTCOME", "Binary GLM outcomes and frequency weights required")
    list(x = x, y = y, w = w, features = features)
}

.cr_cm_fit <- function(x, y, w, features, estimator, penalty = 0, alpha = 0, standardize = FALSE) {
    center <- if (standardize) 
        colSums(x * w)/sum(w)
    else rep(0, ncol(x))
    scale <- if (standardize) 
        sqrt(colSums(sweep(x, 2, center, "-")^2 * w)/sum(w))
    else rep(1, ncol(x))
    scale[scale <= 1e-12] <- 1
    D <- cbind(1, sweep(sweep(x, 2, center, "-"), 2, scale, "/"))
    n <- length(y)
    if (penalty == 0 && qr(D * sqrt(w))$rank < ncol(D)) 
        .cr_error("RANK_DEFICIENT", "Selected design is singular")
    if (estimator == "GLM" && length(unique(y)) < 2L) 
        .cr_error("SEPARATION", "Both GLM classes required")
    if (estimator == "GLM" && penalty == 0) 
        .cr_glm_separation(D, y)
    b <- numeric(ncol(D))
    b[1] <- if (estimator == "GLM") 
        stats::qlogis(sum(w * y)/sum(w))
    else sum(w * y)/sum(w)
    if (estimator != "GLM" && alpha == 0) 
        b <- drop(solve(crossprod(D, D * w) + diag(c(0, rep(penalty * (1 - alpha) * sum(w), length(b) - 
            1L)), nrow = length(b)), crossprod(D, w * y)))
    else {
        L <- max(eigen(crossprod(D, D * w)/sum(w), symmetric = TRUE, only.values = TRUE)$values) * (if (estimator == 
            "GLM") 
            0.25
        else 1) + penalty * (1 - alpha)
        converged <- FALSE
        for (it in seq_len(50000L)) {
            eta <- drop(D %*% b)
            pred <- if (estimator == "GLM") 
                stats::plogis(eta)
            else eta
            grad <- drop(crossprod(D, w * (pred - y)))/sum(w)
            grad[-1] <- grad[-1] + penalty * (1 - alpha) * b[-1]
            q <- b - grad/L
            q[-1] <- sign(q[-1]) * pmax(0, abs(q[-1]) - penalty * alpha/L)
            if (max(abs(q - b)) < 1e-10) {
                b <- q
                converged <- TRUE
                break
            }
            b <- q
        }
        if (!converged) 
            .cr_error("NONCONVERGENCE", "Penalized candidate did not converge")
    }
    eta <- drop(D %*% b)
    if (estimator == "GLM") {
        ll <- sum(w * (y * eta - pmax(eta, 0) - log1p(exp(-abs(eta)))))
        k <- length(b)
        variance <- NULL
    }
    else {
        rss <- sum(w * (y - eta)^2)
        variance <- rss/n
        if (variance <= 1e-20) 
            .cr_error("DEGENERATE_VARIANCE", "Zero residual variance")
        ll <- -0.5 * n * (log(2 * pi) + 1 + log(variance)) + 0.5 * sum(log(w))
        k <- length(b) + 1L
    }
    model <- list(schema_version = "1.0", artifact_type = "SelectionModel", estimator = estimator, link = if (estimator == 
        "GLM") "logit" else "identity", feature_order = as.list(features), intercept = b[1], coefficients = as.list(b[-1]), 
        center = as.list(center), scale = as.list(scale), penalty = penalty, elastic_net_alpha = alpha, 
        residual_variance = variance, log_likelihood = ll, parameter_count = k, row_count = n, weight_sum = sum(w))
    model$training <- list(role = "training_only", known_at = NULL, cutoff_status = "not_supplied", data_fingerprint = .cr_hash(list(X = lapply(seq_len(nrow(x)), 
        function(i) as.list(x[i, ])), y = as.list(y), weights = as.list(w))))
    model$diagnostic_hash <- .cr_hash(list(log_likelihood = ll, parameter_count = k, row_count = n, weight_sum = sum(w)))
    model$artifact_hash <- .cr_hash(model)
    model
}

.cr_cm_predict <- function(model, x) {
    coef <- unlist(model$coefficients, use.names = FALSE)
    center <- unlist(model$center, use.names = FALSE)
    scale <- unlist(model$scale, use.names = FALSE)
    eta <- model$intercept + if (length(coef)) 
        drop(sweep(sweep(x, 2, center, "-"), 2, scale, "/") %*% coef)
    else rep(0, nrow(x))
    if (model$estimator == "GLM") 
        stats::plogis(eta)
    else eta
}

fit_stepwise <- function(X, outcomes, feature_order, candidate_groups, mandatory_terms = character(), 
    hierarchy = NULL, direction = "bidirectional", criterion = "AIC", estimator = "GLM", weights = NULL, 
    max_steps = 100, lr_alpha = 0.05, training = NULL, context = NULL) {
    estimator <- .cr_cm_estimator(estimator)
    d <- .cr_cm_xy(X, outcomes, feature_order, weights, estimator)
    # Positional numeric design: bank-supplied dimnames are not serialized data.
    x <- unname(d$x)
    y <- d$y
    w <- d$w
    features <- d$features
    if (!is.list(candidate_groups) || !length(candidate_groups) || is.null(names(candidate_groups)) || anyDuplicated(names(candidate_groups))) 
        .cr_error("INVALID_INPUT", "Named candidate groups required")
    groups <- lapply(candidate_groups, unlist, use.names = FALSE)
    names <- sort(names(groups), method = "radix")
    mandatory <- unlist(mandatory_terms, use.names = FALSE)
    parents <- if (is.null(hierarchy)) 
        list()
    else lapply(hierarchy, unlist, use.names = FALSE)
    for (g in names) if (!nzchar(g) || !length(groups[[g]]) || anyDuplicated(groups[[g]]) || any(!groups[[g]] %in% 
        features)) 
        .cr_error("INVALID_INPUT", "Groups require unique known features")
    if (anyDuplicated(unlist(groups, use.names = FALSE))) 
        .cr_error("INVALID_INPUT", "Candidate groups must not overlap")
    if (any(!mandatory %in% names) || any(!names(parents) %in% names)) 
        .cr_error("INVALID_INPUT", "Invalid mandatory/hierarchy terms")
    for (g in names(parents)) if (any(!parents[[g]] %in% names) || g %in% parents[[g]]) 
        .cr_error("INVALID_INPUT", "Invalid hierarchy")
    valid <- function(s) all(vapply(s, function(g) all(parents[[g]] %in% s), logical(1)))
    for (g in names) {
        seen <- character()
        stack <- parents[[g]]
        while (length(stack)) {
            h <- stack[1]
            stack <- stack[-1]
            if (h == g) 
                .cr_error("INVALID_INPUT", "Cyclic interaction hierarchy")
            if (!h %in% seen) {
                seen <- c(seen, h)
                stack <- c(stack, parents[[h]])
            }
        }
    }
    if (!valid(mandatory)) 
        .cr_error("INVALID_INPUT", "Mandatory terms require hierarchy parents")
    if (!is.character(criterion) || length(criterion) != 1L) 
        .cr_error("INVALID_INPUT", "String criterion required")
    criterion <- toupper(criterion)
    if (!is.character(direction) || length(direction) != 1L || !direction %in% c("forward", "backward", "bidirectional") || !criterion %in% c("AIC", "BIC", 
        "LR")) 
        .cr_error("INVALID_INPUT", "Unknown direction/criterion")
    steps <- .cr_cm_scalar(max_steps, "max_steps", 1, 10000)
    if (steps != floor(steps)) 
        .cr_error("INVALID_INPUT", "Integer max_steps required")
    sig <- .cr_cm_scalar(lr_alpha, "lr_alpha", 1e-08, 1 - 1e-08)
    selected <- if (direction == "backward") 
        names
    else mandatory
    cache <- new.env(parent = emptyenv())
    evidence <- list()
    key <- function(s) .cr_hash(as.list(sort(s, method = "radix")))
    fit <- function(s) {
        k <- key(s)
        if (!exists(k, cache, inherits = FALSE)) {
            feats <- features[features %in% unlist(groups[s], use.names = FALSE)]
            assign(k, .cr_cm_fit(x[, match(feats, features), drop = FALSE], y, w, feats, estimator), 
                cache)
        }
        get(k, cache, inherits = FALSE)
    }
    score <- function(m) -2 * m$log_likelihood + (if (criterion == "AIC") 
        2
    else log(if (estimator == "GLM") sum(w) else length(y))) * m$parameter_count
    current <- fit(selected)
    path <- list(list(step = 0, groups = as.list(sort(selected, method = "radix")), log_likelihood = current$log_likelihood, 
        criterion_value = if (criterion == "LR") NULL else score(current)))
    visited <- key(selected)
    reason <- "no_improvement"
    for (step in seq_len(steps)) {
        candidates <- list()
        moves <- list()
        if (direction != "backward") 
            for (g in setdiff(names, selected)) moves[[length(moves) + 1L]] <- c("add", g)
        if (direction != "forward") 
            for (g in setdiff(intersect(names, selected), mandatory)) moves[[length(moves) + 1L]] <- c("remove", 
                g)
        for (move in moves) {
            action <- move[1]
            g <- move[2]
            trial <- if (action == "add") 
                union(selected, g)
            else setdiff(selected, g)
            if (!valid(trial) || key(trial) %in% visited) 
                next
            m <- tryCatch(fit(trial), error = identity)
            if (inherits(m, "error")) {
                evidence[[length(evidence) + 1L]] <- list(step = step, action = action, group = g, failure = if (is.null(m$code)) conditionMessage(m) else m$code)
                next
            }
            df <- abs(m$parameter_count - current$parameter_count)
            stat <- max(0, 2 * abs(m$log_likelihood - current$log_likelihood))
            p <- stats::pchisq(stat, df, lower.tail = FALSE)
            improvement <- if (criterion != "LR") 
                score(current) - score(m)
            else if (action == "add" && p < sig) 
                -log(max(p, 1e-300))
            else if (action == "remove" && p > sig) 
                log(p/sig)
            else -1
            row <- list(step = step, action = action, group = g, groups = as.list(sort(trial, method = "radix")), 
                log_likelihood = m$log_likelihood, criterion_value = if (criterion == "LR") NULL else score(m), 
                lr_statistic = stat, lr_df = df, lr_p = p, improvement = improvement, failure = NULL)
            evidence[[length(evidence) + 1L]] <- row
            if (improvement > 1e-10) 
                candidates[[length(candidates) + 1L]] <- list(improvement = improvement, action = action, 
                  group = g, trial = trial, model = m, row = row)
        }
        if (!length(candidates)) 
            break
        best <- max(vapply(candidates, function(z) z$improvement, numeric(1)))
        choices <- Filter(function(z) best - z$improvement <= 1e-10, candidates)
        choice <- choices[[order(vapply(choices, function(z) paste(z$action, z$group), character(1)), 
            method = "radix")[1]]]
        selected <- choice$trial
        current <- choice$model
        visited <- c(visited, key(selected))
        path[[length(path) + 1L]] <- choice$row
        if (step == steps) 
            reason <- "maximum_steps"
    }
    current <- .cr_cm_stamp(current, training)
    .cr_cm_out(list(selected_model = current, selected_groups = as.list(sort(selected, method = "radix")), 
        path = path, candidate_evidence = evidence, stopping_reason = reason, criterion = criterion, 
        direction = direction, fixed_sample_fingerprint = .cr_hash(list(X = lapply(seq_len(nrow(x)), 
            function(i) as.list(x[i, ])), y = as.list(y), w = as.list(w))), inference_note = "Selection path is adaptive; selected-model fixed-model standard errors are not post-selection inference"), 
        "fit_stepwise", context)
}

.cr_cm_split <- function(plan, n) {
    if (!is.list(plan) || !is.list(plan$rows) || length(plan$rows) != n || !is.list(plan$folds) || !length(plan$folds)) 
        .cr_error("INVALID_SPLIT", "Rows and nonempty folds required")
    rows <- plan$rows
    ids <- character()
    for (r in rows) {
        if (!is.list(r) || !is.character(r$entity_id) || length(r$entity_id) != 1L || !nzchar(r$entity_id) || 
            !is.character(r$role) || length(r$role) != 1L || !r$role %in% c("development", "calibration", "test")) 
            .cr_error("INVALID_SPLIT", "Entity and row role required")
        .cr_date(r$as_of, "as_of")
        .cr_date(r$label_known_at, "label_known_at")
        if (r$label_known_at < r$as_of) 
            .cr_error("INVALID_SPLIT", "Label cannot predate observation")
        ids <- c(ids, r$entity_id)
    }
    indices <- function(value) {
        v <- unlist(value, use.names = FALSE)
        if (!is.numeric(v) || !length(v) || any(!is.finite(v) | v != floor(v) | v < 0 | v >= n) || anyDuplicated(v)) 
            .cr_error("INVALID_SPLIT", "Unique zero-based integer indices required")
        as.integer(v) + 1L
    }
    folds <- list()
    for (fold in plan$folds) {
        if (!is.list(fold)) 
            .cr_error("INVALID_SPLIT", "Fold object required")
        tr <- indices(fold$train)
        va <- indices(fold$validation)
        if (length(intersect(tr, va)) || length(intersect(ids[tr], ids[va]))) 
            .cr_error("LEAKAGE", "Fold entities overlap")
        if (any(vapply(rows[c(tr, va)], function(r) r$role != "development", logical(1)))) 
            .cr_error("LEAKAGE", "Selection uses development only")
        if (max(vapply(rows[tr], function(r) r$label_known_at, character(1))) >= min(vapply(rows[va], 
            function(r) r$as_of, character(1)))) 
            .cr_error("LEAKAGE", "Training labels must mature before validation")
        folds[[length(folds) + 1L]] <- list(train = tr, validation = va)
    }
    roles <- stats::setNames(lapply(c("development", "calibration", "test"), function(role) which(vapply(rows, 
        function(r) r$role == role, logical(1)))), c("development", "calibration", "test"))
    for (i in 1:2) for (j in (i + 1L):3L) {
        a <- roles[[i]]
        b <- roles[[j]]
        if (length(intersect(ids[a], ids[b]))) 
            .cr_error("LEAKAGE", "Stage entities overlap")
        if (length(a) && length(b) && max(vapply(rows[a], function(r) r$label_known_at, character(1))) >= 
            min(vapply(rows[b], function(r) r$as_of, character(1)))) 
            .cr_error("LEAKAGE", "Earlier labels not mature before later stage")
    }
    list(folds = folds, roles = roles)
}

.cr_cm_loss <- function(y, p, w, scoring) {
    if (scoring %in% c("mse", "brier")) 
        return(sum(w * (y - p)^2)/sum(w))
    if (scoring == "log_loss") {
        p <- pmin(1 - 1e-15, pmax(1e-15, p))
        return(-sum(w * (y * log(p) + (1 - y) * log1p(-p)))/sum(w))
    }
    .cr_error("INVALID_INPUT", "Unknown scoring rule")
}

fit_regularization_path <- function(X, outcomes, feature_order, penalty_grid, split_plan, estimator = "GLM", 
    elastic_net_alpha = 0, scoring = "log_loss", weights = NULL, context = NULL) {
    estimator <- .cr_cm_estimator(estimator)
    d <- .cr_cm_xy(X, outcomes, feature_order, weights, estimator)
    x <- d$x
    y <- d$y
    w <- d$w
    features <- d$features
    sp <- .cr_cm_split(split_plan, length(y))
    grid <- .cr_numeric(penalty_grid, "penalty_grid", vector_only = TRUE)
    alpha <- .cr_cm_scalar(elastic_net_alpha, "elastic_net_alpha", 0, 1)
    if (!length(grid) || any(grid < 0) || anyDuplicated(grid)) 
        .cr_error("INVALID_INPUT", "Unique nonnegative penalties required")
    if (!scoring %in% (if (estimator == "GLM") 
        c("log_loss", "brier")
    else "mse")) 
        .cr_error("INVALID_INPUT", "Scoring must match family")
    results <- list()
    for (penalty in grid) {
        records <- list()
        for (j in seq_along(sp$folds)) {
            tr <- sp$folds[[j]]$train
            va <- sp$folds[[j]]$validation
            z <- tryCatch({
                m <- .cr_cm_fit(x[tr, , drop = FALSE], y[tr], w[tr], features, estimator, penalty, alpha, 
                  TRUE)
                value <- .cr_cm_loss(y[va], .cr_cm_predict(m, x[va, , drop = FALSE]), w[va], scoring)
                list(fold = j - 1L, score = value, failure = NULL, training_center = m$center, training_scale = m$scale)
            }, error = function(e) list(fold = j - 1L, score = NULL, failure = if (is.null(e$code)) conditionMessage(e) else e$code))
            records[[j]] <- z
        }
        score <- if (all(vapply(records, function(r) is.null(r$failure), logical(1)))) 
            mean(vapply(records, function(r) r$score, numeric(1)))
        else NULL
        results[[length(results) + 1L]] <- list(penalty = penalty, mean_score = score, folds = records)
    }
    valid <- Filter(function(r) !is.null(r$mean_score), results)
    if (!length(valid)) 
        .cr_error("ALL_CANDIDATES_FAILED", "No penalty completed every fold", list(path = results))
    selected <- valid[[order(vapply(valid, function(r) r$mean_score, numeric(1)), -vapply(valid, function(r) r$penalty, 
        numeric(1)))[1]]]
    ix <- sp$roles$development
    model <- .cr_cm_fit(x[ix, , drop = FALSE], y[ix], w[ix], features, estimator, selected$penalty, alpha, 
        TRUE)
    model <- .cr_cm_stamp(model, list(known_at = max(vapply(split_plan$rows[ix], function(r) r$label_known_at, 
        character(1))), role = "development_only"))
    .cr_cm_out(list(selected_penalty = selected$penalty, selected_model = model, path = results, selection_rule = "Mean fold loss; exact ties prefer stronger penalty", 
        untouched_rows = as.list(c(sp$roles$calibration, sp$roles$test) - 1L), split_fingerprint = .cr_hash(split_plan)), 
        "fit_regularization_path", context)
}

resample_fit_uncertainty <- function(X, outcomes, feature_order, resampling_indices, resampling_units, 
    unit_kind, estimator = "GLM", weights = NULL, confidence = 0.95, prediction_X = NULL, selection_spec = NULL, 
    context = NULL) {
    estimator <- .cr_cm_estimator(estimator)
    d <- .cr_cm_xy(X, outcomes, feature_order, weights, estimator)
    x <- d$x
    y <- d$y
    w <- d$w
    features <- d$features
    units <- unlist(resampling_units, use.names = FALSE)
    conf <- .cr_cm_scalar(confidence, "confidence", 1e-08, 1 - 1e-08)
    if (!unit_kind %in% c("cluster", "time_block") || length(units) != length(y) || !is.character(units) || 
        any(!nzchar(units))) 
        .cr_error("INVALID_RESAMPLING", "Whole-cluster/time-block units required")
    if (!is.list(resampling_indices) || !length(resampling_indices)) 
        .cr_error("INVALID_RESAMPLING", "Shared replicate index arrays required")
    px <- if (is.null(prediction_X)) 
        x
    else .cr_numeric(prediction_X, "prediction_X")
    if (!is.matrix(px) || ncol(px) != ncol(x)) 
        .cr_error("INVALID_DESIGN", "Prediction columns mismatch")
    results <- predictions <- coefficients <- list()
    for (j in seq_along(resampling_indices)) {
        idx <- unlist(resampling_indices[[j]], use.names = FALSE)
        if (!is.numeric(idx) || !length(idx) || any(!is.finite(idx) | idx != floor(idx) | idx < 0 | idx >= 
            length(y))) 
            .cr_error("INVALID_RESAMPLING", "Invalid zero-based index")
        idx <- as.integer(idx) + 1L
        freq <- tabulate(idx, nbins = length(y))
        for (unit in unique(units)) if (length(unique(freq[units == unit])) != 1L) 
            .cr_error("INVALID_RESAMPLING", "Every selected unit must be repeated in full")
        z <- tryCatch({
            m <- if (is.null(selection_spec)) 
                .cr_cm_fit(x[idx, , drop = FALSE], y[idx], w[idx], features, estimator)
            else do.call(fit_stepwise, c(list(X = x[idx, , drop = FALSE], outcomes = y[idx], feature_order = features, 
                weights = w[idx], estimator = estimator), selection_spec))$value$selected_model
            cols <- match(unlist(m$feature_order, use.names = FALSE), features)
            p <- .cr_cm_predict(m, px[, cols, drop = FALSE])
            beta <- numeric(ncol(x) + 1L)
            beta[1] <- m$intercept
            beta[cols + 1L] <- unlist(m$coefficients, use.names = FALSE)
            list(model = m, p = p, beta = beta)
        }, error = identity)
        if (inherits(z, "error")) 
            results[[j]] <- list(replicate = j - 1L, model = NULL, failure = if (is.null(z$code)) conditionMessage(z) else z$code)
        else {
            predictions[[length(predictions) + 1L]] <- z$p
            coefficients[[length(coefficients) + 1L]] <- z$beta
            results[[j]] <- list(replicate = j - 1L, model = z$model, failure = NULL)
        }
    }
    effective <- length(predictions)
    q <- c((1 - conf)/2, (1 + conf)/2)
    intervals <- function(z) {
        m <- do.call(rbind, z)
        lapply(seq_len(ncol(m)), function(i) as.list(unname(stats::quantile(m[, i], q, type = 7))))
    }
    .cr_cm_out(list(replicates = results, effective_replicates = effective, failed_replicates = length(results) - 
        effective, coefficient_intervals = if (effective >= 2L) intervals(coefficients) else NULL, prediction_intervals = if (effective >= 
        2L) intervals(predictions) else NULL, confidence = conf, unit_kind = unit_kind, selection_repeated = !is.null(selection_spec), 
        interval_note = "Percentile intervals conditional on successful replicates; failure proportion must be reviewed", 
        indices_fingerprint = .cr_hash(resampling_indices)), "resample_fit_uncertainty", context)
}

fit_model_selection_protocol <- function(X, outcomes, feature_order, candidates, split_plan, metadata, 
    search_budget, scoring = "log_loss", context = NULL) {
    d <- .cr_cm_xy(X, outcomes, feature_order, estimator = "GLM")
    x <- d$x
    y <- d$y
    features <- d$features
    sp <- .cr_cm_split(split_plan, length(y))
    meta <- .cr_cal_metadata(metadata, TRUE)
    if (!length(sp$roles$calibration) || !length(sp$roles$test)) 
        .cr_error("INVALID_SPLIT", "Separate calibration/test required")
    calibration_rows <- split_plan$rows[sp$roles$calibration]
    if (meta$calibration$prediction_origin != "HOLDOUT" || min(vapply(calibration_rows, function(r) r$as_of, 
        character(1))) < meta$calibration$start || max(vapply(calibration_rows, function(r) r$as_of, 
        character(1))) > meta$calibration$end || max(vapply(calibration_rows, function(r) r$label_known_at, 
        character(1))) > meta$calibration$known_at) 
        .cr_error("INVALID_CALIBRATION_METADATA", "Metadata must cover actual independent calibration rows and label maturity")
    budget <- .cr_cm_scalar(search_budget, "search_budget", 1, 1e+06)
    if (!is.list(candidates) || !length(candidates) || length(candidates) > budget || budget != floor(budget)) 
        .cr_error("INVALID_INPUT", "Budget must cover all candidates")
    evidence <- list()
    best <- NULL
    names <- character()
    for (candidate in candidates) {
        if (!is.list(candidate) || !is.character(candidate$id) || length(candidate$id) != 1L || !nzchar(candidate$id) || 
            candidate$id %in% names) 
            .cr_error("INVALID_INPUT", "Unique candidate IDs required")
        names <- c(names, candidate$id)
        z <- tryCatch({
            columns <- if (is.null(candidate$features)) 
                features
            else unlist(candidate$features, use.names = FALSE)
            if (anyDuplicated(columns) || any(!columns %in% features)) 
                .cr_error("INVALID_INPUT", "Invalid feature subset")
            indices <- match(columns, features)
            result <- fit_regularization_path(x[, indices, drop = FALSE], y, columns, if (is.null(candidate$penalty_grid)) 
                0
            else unlist(candidate$penalty_grid, use.names = FALSE), split_plan, elastic_net_alpha = if (is.null(candidate$elastic_net_alpha)) 
                0
            else candidate$elastic_net_alpha, scoring = scoring)$value
            score <- Filter(function(row) row$penalty == result$selected_penalty, result$path)[[1]]$mean_score
            list(score = score, id = candidate$id, model = result$selected_model, indices = indices, 
                path = result$path)
        }, error = identity)
        if (inherits(z, "error")) 
            evidence[[length(evidence) + 1L]] <- list(candidate_id = candidate$id, score = NULL, failure = if (is.null(z$code)) conditionMessage(z) else z$code)
        else {
            evidence[[length(evidence) + 1L]] <- list(candidate_id = candidate$id, score = z$score, failure = NULL, 
                path = z$path)
            if (is.null(best) || z$score < best$score || (z$score == best$score && identical(sort(c(z$id, 
                best$id), method = "radix")[1], z$id))) 
                best <- z
        }
    }
    if (is.null(best)) 
        .cr_error("ALL_CANDIDATES_FAILED", "Every candidate failed", list(evidence = evidence))
    ix <- sp$roles$calibration
    p <- .cr_cm_predict(best$model, x[ix, best$indices, drop = FALSE])
    cal <- calibrate_logistic(p, y[ix], metadata = meta, tolerance = 1e-08)$value
    frozen <- .cr_cm_freeze(best$model, best$id, meta, split_plan)
    .cr_cm_out(list(selected_candidate = best$id, selected_model = best$model, selected_frozen_model = frozen, 
        calibration_artifact = cal, candidate_evidence = evidence, untouched_test_indices = as.list(sp$roles$test - 
            1L), test_used_for_selection = FALSE, test_metrics = NULL, split_fingerprint = .cr_hash(split_plan), 
        protocol = "Development inner grouped temporal CV; final development refit; independent logistic calibration; no test predictions or labels used in optimization"), 
        "fit_model_selection_protocol", context, data_origin = meta$data_origin)
}

.cr_cm_apply <- function(calibrator, values, target, base_model_version, groups = NULL, context = NULL) {
    a <- if (inherits(calibrator, "creditratingsystem_result")) 
        calibrator$value
    else calibrator
    if (is.list(a) && !is.null(a$value) && is.null(a$artifact_type)) 
        a <- a$value
    if (!is.list(a)) 
        .cr_error("INVALID_CALIBRATOR", "Calibration artifact required")
    digest <- a$artifact_hash
    a$artifact_hash <- NULL
    if (!identical(digest, .cr_hash(a))) 
        .cr_error("INTEGRITY_ERROR", "Calibration artifact differs from hash")
    kind <- a$calibration_type
    method <- switch(kind, beta = "calibrate_beta", hierarchical = "fit_hierarchical_calibration", lgd_level = "calibrate_lgd_level", 
        NULL)
    if (is.null(method) || !identical(a$schema_version, "1.0") || !identical(a$artifact_type, "CalibrationArtifact") || 
        !identical(a$parameter_version, "1.0") || !identical(a$method_id, paste0("calibration.", method)) || 
        !identical(a$input_kind, if (kind == "lgd_level") 
            "loss"
        else "probability") || !is.character(a$calibration_input_fingerprint) || nchar(a$calibration_input_fingerprint) != 
        64) 
        .cr_error("INVALID_CALIBRATOR", "Invalid extended calibration schema")
    meta <- if (kind == "lgd_level") 
        .cr_cm_loss_metadata(a$metadata)
    else .cr_cal_metadata(a$metadata, TRUE)
    if (.cr_hash(a$target) != .cr_hash(meta$target) || !is.list(a$parameters)) 
        .cr_error("INVALID_CALIBRATOR", "Invalid target/parameters")
    if (.cr_hash(.cr_cal_target(target)) != .cr_hash(a$target)) 
        .cr_error("INCOMPATIBLE_TARGET", "Calibration target differs")
    if (!identical(base_model_version, meta$base_model_version)) 
        .cr_error("INCOMPATIBLE_MODEL_VERSION", "Calibration model differs")
    q <- a$parameters
    trace <- list(calibration_hash = digest, calibration_type = kind)
    if (kind == "lgd_level") {
        x <- .cr_numeric(values, "values", allow_empty = TRUE, vector_only = TRUE)
        parameter <- .cr_cm_scalar(q$parameter, "parameter")
        mode <- q$transformation
        .cr_cm_scalar(q$target_lgd, "target_lgd")
        if (!mode %in% c("additive", "multiplicative", "logit") || !identical(q$boundary_policy, switch(mode, 
            additive = "raw", multiplicative = "nonnegative", logit = "reject"))) 
            .cr_error("INVALID_CALIBRATOR", "Invalid loss transformation")
        if (mode == "additive") 
            v <- x + parameter
        else if (mode == "multiplicative") {
            if (parameter < 0 || any(x < 0)) 
                .cr_error("INVALID_CALIBRATOR", "Nonnegative multiplicative domain required")
            v <- x * parameter
        }
        else {
            .cr_probability(x, "open")
            v <- stats::plogis(stats::qlogis(x) + parameter)
        }
    }
    else if (kind == "beta") {
        p <- .cr_probability(values, vector_only = TRUE)
        eps <- .cr_cm_scalar(q$epsilon, "epsilon", 1e-15, 0.01)
        b0 <- .cr_cm_scalar(q$intercept, "intercept")
        b1 <- .cr_cm_scalar(q$a, "a")
        b2 <- .cr_cm_scalar(q$b, "b")
        if (!is.logical(q$monotonic) || length(q$monotonic) != 1L || is.na(q$monotonic) || (q$monotonic && 
            min(b1, b2) < 0) || !q$boundary_policy %in% c("reject", "clip")) 
            .cr_error("INVALID_CALIBRATOR", "Invalid beta parameters")
        if (q$boundary_policy == "reject" && any(p == 0 | p == 1)) 
            .cr_error("INVALID_PROBABILITY", "Interior beta input required")
        x <- if (q$boundary_policy == "clip") 
            pmin(1 - eps, pmax(eps, p))
        else p
        v <- stats::plogis(b0 + b1 * log(x) - b2 * log1p(-x))
        trace$clipped_count <- sum(x != p)
    }
    else {
        p <- .cr_probability(values, "open", vector_only = TRUE)
        b0 <- .cr_cm_scalar(q$intercept, "intercept")
        variance <- .cr_cm_scalar(q$variance, "variance", 1e-12, 100)
        if (!identical(q$unknown_group, "logistic_normal_marginal") || !identical(q$known_group, "conditional_mode") || 
            q$quadrature_points != 40 || !is.list(q$groups)) 
            .cr_error("INVALID_CALIBRATOR", "Invalid hierarchical policy")
        effects <- list()
        for (row in q$groups) {
            if (!is.list(row) || !is.character(row$group) || length(row$group) != 1L || !is.null(effects[[row$group]])) 
                .cr_error("INVALID_CALIBRATOR", "Invalid group effects")
            effects[[row$group]] <- .cr_cm_scalar(row$mode, "group mode")
            .cr_cm_scalar(row$conditional_variance, "conditional_variance", 0, variance * (1 + 1e-10))
        }
        groups <- unlist(groups, use.names = FALSE)
        if (length(groups) != length(p) || !is.character(groups) || any(!nzchar(groups))) 
            .cr_error("INVALID_INPUT", "Group IDs required")
        J <- matrix(0, 40, 40)
        for (i in 1:39) {
            J[i, i + 1] <- sqrt(i)
            J[i + 1, i] <- sqrt(i)
        }
        e <- eigen(J, symmetric = TRUE)
        nodes <- e$values
        mass <- e$vectors[1, ]^2
        v <- numeric(length(p))
        unknown <- 0L
        for (i in seq_along(p)) {
            eta <- stats::qlogis(p[i]) + b0
            if (!is.null(effects[[groups[i]]])) 
                v[i] <- stats::plogis(eta + effects[[groups[i]]])
            else {
                v[i] <- sum(mass * stats::plogis(eta + sqrt(variance) * nodes))
                unknown <- unknown + 1L
            }
        }
        trace$unknown_group_count <- unknown
    }
    .cr_result(v, "calibration.apply_calibrator", if (kind == "lgd_level") 
        "loss_fraction"
    else "probability", .cr_cal_context(context, a$target), diagnostics = trace, provenance = list(parameter_origin = "ESTIMATED"))
}

.cr_cm_stamp <- function(model, training) {
    model$artifact_hash <- NULL
    if (!is.null(training)) {
        if (!is.list(training)) 
            .cr_error("INVALID_TRAINING", "Training object required")
        role <- if (is.null(training$role)) 
            "training_only"
        else training$role
        if (!role %in% c("training_only", "development_only")) 
            .cr_error("INVALID_TRAINING", "Training-only role required")
        model$training$known_at <- .cr_date(training$known_at, "training.known_at")
        model$training$cutoff_status <- "declared"
        model$training$role <- role
    }
    model$artifact_hash <- .cr_hash(model)
    model
}

.cr_cm_freeze <- function(model, name, metadata, split_plan) {
    frozen <- list(schema_version = "1.0", model_id = name, model_version = metadata$base_model_version, 
        method_id = "calibration.fit_model_selection_protocol", feature_order = model$feature_order, 
        parameters = list(selection_model = model), preprocessing = list(transformation = "stored_training_center_scale"), 
        target = metadata$target, training = list(data_fingerprint = model$training$data_fingerprint, 
            split_id = paste0("development:", metadata$calibration$split_id), known_at = model$training$known_at, 
            role = "development_only"), diagnostics = list(selection_model_hash = model$artifact_hash, 
            diagnostic_hash = model$diagnostic_hash, split_fingerprint = .cr_hash(split_plan)), provenance = list(data_origin = metadata$data_origin, 
            parameter_origin = "ESTIMATED"))
    frozen$artifact_hash <- .cr_hash(frozen)
    frozen
}

.cr_cm_predict_pd <- function(fitted_model, X, feature_order, expected_target, context = NULL) {
    model <- .cr_model(fitted_model)
    raw <- model
    digest <- raw$artifact_hash
    raw$artifact_hash <- NULL
    if (!identical(digest, .cr_hash(raw))) 
        .cr_error("INTEGRITY_ERROR", "Frozen selection classifier hash mismatch")
    if (!identical(model$method_id, "calibration.fit_model_selection_protocol") || !identical(model$preprocessing, 
        list(transformation = "stored_training_center_scale"))) 
        .cr_error("INVALID_MODEL", "Invalid selection classifier schema")
    if (!identical(unlist(feature_order, use.names = FALSE), unlist(model$feature_order, use.names = FALSE))) 
        .cr_error("FEATURE_MISMATCH", "Feature order differs")
    if (.cr_hash(.cr_cal_target(expected_target)) != .cr_hash(model$target)) 
        .cr_error("TARGET_MISMATCH", "Target differs")
    inner <- model$parameters$selection_model
    if (!is.list(inner)) 
        .cr_error("INVALID_MODEL", "Missing selection parameters")
    body <- inner
    h <- body$artifact_hash
    body$artifact_hash <- NULL
    if (!identical(h, .cr_hash(body))) 
        .cr_error("INTEGRITY_ERROR", "Inner selection hash mismatch")
    if (!identical(inner$artifact_type, "SelectionModel") || !identical(inner$estimator, "GLM") || !identical(inner$link, 
        "logit") || .cr_hash(inner$feature_order) != .cr_hash(model$feature_order)) 
        .cr_error("INVALID_MODEL", "Invalid Bernoulli selection schema")
    size <- length(model$feature_order)
    coef <- .cr_numeric(unlist(inner$coefficients, use.names = FALSE), "coefficients", allow_empty = TRUE, 
        vector_only = TRUE)
    center <- .cr_numeric(unlist(inner$center, use.names = FALSE), "center", allow_empty = TRUE, vector_only = TRUE)
    scale <- .cr_numeric(unlist(inner$scale, use.names = FALSE), "scale", allow_empty = TRUE, vector_only = TRUE)
    .cr_cm_scalar(inner$intercept, "intercept")
    if (any(c(length(coef), length(center), length(scale)) != size) || any(scale <= 0)) 
        .cr_error("INVALID_MODEL", "Invalid coefficients/scaling")
    x <- .cr_numeric(X, "X", allow_empty = TRUE)
    if (!is.matrix(x) || ncol(x) != size) 
        .cr_error("FEATURE_MISMATCH", "Prediction matrix differs")
    values <- .cr_cm_predict(inner, x)
    if (any(!is.finite(values))) 
        .cr_error("NUMERICAL_FAILURE", "Prediction overflow")
    .cr_result(values, "classification.predict_pd", "probability", .cr_cal_context(context, model$target), 
        diagnostics = list(model_id = model$model_id, model_version = model$model_version, prediction_family = "selection_protocol"), 
        provenance = model$provenance)
}

