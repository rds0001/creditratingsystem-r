.cr_sb_out <- function(method, value, context = NULL, diagnostics = list(), synthetic = FALSE) {
    .cr_result(value, paste0("selection_bias.", method), context = context, diagnostics = diagnostics, 
        provenance = if (synthetic) 
            list(data_origin = "SYNTHETIC")
        else list(), limitations = "Unobserved own-contract outcomes are not identified without the stated assumptions. Hypothetical labels are not validation evidence.")
}

.cr_sb_vec <- function(value, n, name, probability = FALSE) {
    x <- if (probability) 
        .cr_probability(value, boundary_policy = "closed", vector_only = TRUE)
    else .cr_numeric(value, name, vector_only = TRUE)
    if (length(x) != n) 
        .cr_error("INCOMPATIBLE_SHAPE", paste(name, "must have one value per row"))
    x
}

.cr_sb_sample <- function(outcomes, observed, target_weights = NULL) {
    s <- .cr_numeric(observed, "observed", vector_only = TRUE)
    n <- length(s)
    if (!n || any(!s %in% c(0, 1))) 
        .cr_error("INVALID_SELECTION", "Nonempty numeric binary observed indicator required")
    if (length(outcomes) != n) 
        .cr_error("INCOMPATIBLE_SHAPE", "Outcomes and observed must match")
    y <- numeric(n)
    for (i in seq_len(n)) {
        v <- outcomes[[i]]
        if (s[i] == 0) {
            if (!is.null(v) && !(is.numeric(v) && length(v) == 1L && is.na(v))) 
                .cr_error("UNOBSERVED_OUTCOME", "Unobserved own-contract outcomes must be missing")
        }
        else {
            z <- .cr_numeric(v, "outcome")
            if (length(z) != 1L || !z %in% c(0, 1)) 
                .cr_error("INVALID_OUTCOME", "Observed binary outcome required")
            y[i] <- z
        }
    }
    w <- if (is.null(target_weights)) 
        rep(1, n)
    else .cr_sb_vec(target_weights, n, "target_weights")
    if (any(w < 0) || !is.finite(sum(w)) || sum(w) <= 0) 
        .cr_error("INVALID_WEIGHTS", "Positive finite target mass required")
    list(y = y, s = s, w = w)
}

.cr_sb_str <- function(value, name) {
    .cr_string(value, name, "INVALID_INPUT")
    value
}

.cr_sb_rows <- function(rows, name) {
    if (!is.list(rows) || any(!vapply(rows, .cr_object, logical(1)))) 
        .cr_error("INVALID_INPUT", paste(name, "must be records"))
    ids <- vapply(rows, function(r) .cr_sb_str(r$application_id, "application_id"), character(1))
    if (anyDuplicated(ids)) 
        .cr_error("DUPLICATE_ID", paste(name, "has duplicate application IDs"))
    stats::setNames(rows, ids)
}

build_selection_dataset <- function(applications, decisions, bookings, outcomes, as_of, target_population, 
    target, context = NULL) {
    cutoff <- .cr_date(as_of, "as_of")
    .cr_sb_str(target_population, "target_population")
    target <- .cr_cal_target(target)
    a <- .cr_sb_rows(applications, "applications")
    d <- .cr_sb_rows(decisions, "decisions")
    b <- .cr_sb_rows(bookings, "bookings")
    o <- .cr_sb_rows(outcomes, "outcomes")
    if (!length(a)) 
        .cr_error("EMPTY_INPUT", "Applications required")
    if (length(setdiff(c(names(d), names(b), names(o)), names(a)))) 
        .cr_error("UNKNOWN_ID", "Stage records require submitted application")
    panel <- list()
    for (key in names(a)) {
        app <- a[[key]]
        submitted <- .cr_date(app$submitted_at, "submitted_at")
        entity <- .cr_sb_str(app$entity_id, "entity_id")
        if (submitted > cutoff) 
            .cr_error("FUTURE_INPUT", "Application later than audit cutoff")
        stage <- list()
        last <- submitted
        tables <- list(decision = d, booking = b, outcome = o)
        for (nm in names(tables)) {
            row <- tables[[nm]][[key]]
            if (is.null(row)) 
                next
            event <- .cr_date(row$as_of, paste0(nm, ".as_of"))
            known <- .cr_date(row$known_at, paste0(nm, ".known_at"))
            if (known < event || event < last) 
                .cr_error("INVALID_DATE", "Stage dates must be causal and ordered")
            if (known > cutoff) 
                next
            stage[[nm]] <- row
            last <- event
        }
        dec <- stage$decision
        book <- stage$booking
        out <- stage$outcome
        if (!is.null(dec)) {
            if (length(dec$decision) != 1L || !dec$decision %in% c("offered", "rejected", "withdrawn", 
                "not_decisionable")) 
                .cr_error("INVALID_SELECTION", "Unknown decision")
            .cr_sb_str(dec$policy_version, "policy_version")
        }
        offered <- !is.null(dec) && dec$decision == "offered"
        if (!is.null(book) && (!offered || !is.logical(book$taken_up) || length(book$taken_up) != 1L || 
            is.na(book$taken_up) || !is.logical(book$funded) || length(book$funded) != 1L || is.na(book$funded) || 
            (book$funded && !book$taken_up))) 
            .cr_error("INVALID_SELECTION", "Booking requires offer and funding requires uptake")
        funded <- !is.null(book) && book$funded
        observed <- FALSE
        y <- NULL
        external <- NULL
        if (!is.null(out)) {
            if (identical(out$outcome_scope, "external_contract")) 
                external <- out
            else if (identical(out$outcome_scope, "own_contract")) {
                if (!funded) 
                  .cr_error("UNOBSERVED_OUTCOME", "Own-contract outcome requires funding")
                if (!identical(.cr_hash(out$target), .cr_hash(target))) 
                  .cr_error("TARGET_MISMATCH", "Outcome target differs")
                if (length(out$status) != 1L || !out$status %in% c("observed", "immature", "censored")) 
                  .cr_error("INVALID_OUTCOME", "Declare outcome observation status")
                observed <- out$status == "observed"
                if (observed) {
                  y <- .cr_numeric(out$outcome, "outcome")
                  if (length(y) != 1L || !y %in% c(0, 1)) 
                    .cr_error("INVALID_OUTCOME", "Binary outcome required")
                }
                else if (!is.null(out$outcome)) 
                  .cr_error("UNOBSERVED_OUTCOME", "Unobserved target must remain missing")
            }
            else .cr_error("INVALID_OUTCOME", "Declare own_contract or external_contract")
        }
        stratum <- if (observed) 
            "funded_observed"
        else if (funded) 
            paste0("funded_", if (!is.null(out) && identical(out$outcome_scope, "own_contract")) 
                out$status
            else "immature")
        else if (!is.null(book) && book$taken_up) 
            "taken_up_not_funded"
        else if (offered) 
            "offered_not_taken_up"
        else if (!is.null(dec)) 
            dec$decision
        else "pending_decision"
        panel[[length(panel) + 1L]] <- list(application_id = key, entity_id = entity, submitted_at = submitted, 
            decisionable = !is.null(dec) && dec$decision != "not_decisionable", offered = offered, taken_up = !is.null(book) && 
                book$taken_up, funded = funded, observed = as.integer(observed), outcome = y, stratum = stratum, 
            policy_version = if (is.null(dec)) NULL else dec$policy_version, external_outcome = external, 
            outcome_scope = "own_contract", validation_eligible = observed)
    }
    strata <- vapply(panel, `[[`, character(1), "stratum")
    counts <- as.list(table(strata))
    .cr_sb_out("build_selection_dataset", list(rows = panel, target = target, target_population = target_population, 
        as_of = cutoff, counts = counts), context)
}

.cr_sb_folds <- function(folds, row_metadata, n) {
    if (length(row_metadata) != n || !length(folds)) 
        .cr_error("INVALID_SPLIT", "Dated row metadata and folds required")
    meta <- lapply(row_metadata, function(r) {
        entity <- .cr_sb_str(r$entity_id, "entity_id")
        date <- .cr_date(r$as_of, "as_of")
        known <- .cr_date(r$label_known_at, "label_known_at")
        features <- .cr_date(r$features_known_at, "features_known_at")
        if (features > date || known < date) 
            .cr_error("LEAKAGE", "Features must be available at decision; labels cannot predate it")
        list(entity = entity, date = date, known = known)
    })
    seen <- integer()
    plans <- list()
    for (fold in folds) {
        tr <- .cr_numeric(unlist(fold$train), "train", vector_only = TRUE)
        te <- .cr_numeric(unlist(fold$validation), "validation", vector_only = TRUE)
        for (v in list(tr, te)) if (!length(v) || any(v != floor(v)) || any(v < 0 | v >= n) || anyDuplicated(v)) 
            .cr_error("INVALID_SPLIT", "Nonempty unique zero-based indices required")
        tr <- as.integer(tr) + 1L
        te <- as.integer(te) + 1L
        if (length(intersect(tr, te)) || length(intersect(seen, te))) 
            .cr_error("LEAKAGE", "Training/evaluation must be disjoint and evaluations unique")
        if (length(intersect(vapply(meta[tr], `[[`, character(1), "entity"), vapply(meta[te], `[[`, character(1), 
            "entity")))) 
            .cr_error("LEAKAGE", "Entities overlap training/evaluation")
        if (max(vapply(meta[tr], `[[`, character(1), "known")) >= min(vapply(meta[te], `[[`, character(1), 
            "date"))) 
            .cr_error("LEAKAGE", "Training labels must precede evaluation dates")
        seen <- c(seen, te)
        plans[[length(plans) + 1L]] <- list(train = tr, validation = te)
    }
    plans
}

fit_selection_propensity <- function(X, selected, feature_order, mechanism, folds, row_metadata, target, 
    training, model_id, model_version, context = NULL) {
    if (length(mechanism) != 1L || !mechanism %in% c("decision", "takeup", "funding", "outcome_observation")) 
        .cr_error("INVALID_SELECTION", "Declare separate selection mechanism")
    x <- .cr_glm_design(X, feature_order, TRUE)$x
    s <- .cr_sb_vec(selected, nrow(x), "selected")
    if (any(!s %in% c(0, 1))) 
        .cr_error("INVALID_SELECTION", "Binary mechanism outcomes required")
    if (!identical(target$event, mechanism)) 
        .cr_error("TARGET_MISMATCH", "Target event must equal selection mechanism")
    plans <- .cr_sb_folds(folds, row_metadata, nrow(x))
    cutoff <- .cr_date(training$known_at, "training.known_at")
    if (any(vapply(row_metadata, `[[`, character(1), "label_known_at") > cutoff)) 
        .cr_error("LEAKAGE", "Training cutoff precedes labels")
    full <- fit_binary_glm(x, s, link = "logit", feature_order = feature_order, fit_intercept = TRUE, 
        target = target, training = training, model_id = model_id, model_version = model_version, context = context)
    predictions <- rep(list(NULL), nrow(x))
    lineage <- list()
    for (plan in plans) {
        tr <- plan$train
        te <- plan$validation
        ft <- training
        ft$known_at <- max(vapply(row_metadata[tr], `[[`, character(1), "label_known_at"))
        ft$data_fingerprint <- .cr_hash(list(X = lapply(seq_along(tr), function(i) as.list(unname(x[tr[i], 
            ]))), selected = as.list(s[tr])))
        fit <- fit_binary_glm(x[tr, , drop = FALSE], s[tr], link = "logit", feature_order = feature_order, 
            fit_intercept = TRUE, target = target, training = ft, model_id = model_id, model_version = model_version, 
            context = context)
        p <- predict_pd(fit, x[te, , drop = FALSE], feature_order = feature_order, expected_target = target)$value
        for (i in seq_along(te)) predictions[[te[i]]] <- p[i]
        lineage[[length(lineage) + 1L]] <- list(train = as.list(tr - 1L), validation = as.list(te - 1L), 
            model_hash = .cr_hash(fit$model))
    }
    diag <- full$diagnostics
    diag$mechanism <- mechanism
    diag$out_of_fold_propensity <- predictions
    diag$folds <- lineage
    diag$row_metadata <- row_metadata
    diag$warmup_indices <- as.list(which(vapply(predictions, is.null, logical(1))) - 1L)
    diag$positivity_min <- min(unlist(predictions))
    diag$selection_assumptions <- list("Conditional exchangeability is not established by a propensity fit.", 
        "The input rows must be the eligible risk set for this mechanism.")
    model <- full$model
    model$diagnostics <- diag
    .cr_fit_result(model, "selection_bias.fit_selection_propensity", diag, context)
}

selection_ipw_weights <- function(observed, propensities, target_weights = NULL, stabilization = NULL, 
    trim = NULL, context = NULL) {
    s <- .cr_numeric(observed, "observed", vector_only = TRUE)
    n <- length(s)
    if (!n || any(!s %in% c(0, 1))) 
        .cr_error("INVALID_SELECTION", "Binary observed indicator required")
    p <- .cr_numeric(propensities, "propensities")
    if (is.null(dim(p))) 
        p <- matrix(p, ncol = 1L)
    if (length(dim(p)) != 2L || nrow(p) != n || ncol(p) == 0L || any(p < 0 | p > 1)) 
        .cr_error("INVALID_PROPENSITY", "Rows by sequential mechanisms probabilities required")
    e <- apply(p, 1L, prod)
    w <- if (is.null(target_weights)) 
        rep(1, n)
    else .cr_sb_vec(target_weights, n, "target_weights")
    if (any(w < 0) || sum(w) <= 0 || !is.finite(sum(w))) 
        .cr_error("INVALID_WEIGHTS", "Positive target mass required")
    keep <- rep(TRUE, n)
    if (!is.null(trim)) {
        t <- .cr_numeric(trim, "trim", vector_only = TRUE)
        if (length(t) != 2L || !(0 < t[1] && t[1] <= t[2] && t[2] <= 1)) 
            .cr_error("INVALID_TRIM", "Inclusive support lower > 0 required")
        keep <- e >= t[1] & e <= t[2]
    }
    if (any(e == 0 & w > 0 & keep)) 
        .cr_error("POSITIVITY_FAILURE", "Zero probability target rows are not identified")
    if (any(s == 1 & e == 0)) 
        .cr_error("INVALID_PROPENSITY", "Observed event cannot have zero selection probability")
    factor <- 1
    if (!is.null(stabilization)) {
        f <- .cr_probability(stabilization, boundary_policy = "closed", vector_only = TRUE)
        if (any(f <= 0)) .cr_error("INVALID_PROPENSITY", "Stabilization rates must lie in (0,1]")
        if (length(f) != ncol(p)) 
            .cr_error("INCOMPATIBLE_SHAPE", "One marginal stabilization rate per mechanism")
        factor <- prod(f)
    }
    ipw <- numeric(n)
    eligible <- s == 1 & keep
    ipw[eligible] <- w[eligible] * factor/e[eligible]
    if (!any(ipw > 0)) 
        .cr_error("EMPTY_SUPPORT", "No observed target mass remains")
    .cr_sb_out("selection_ipw_weights", list(weights = as.list(ipw), joint_propensity = as.list(e), included = as.list(keep), 
        effective_sample_size = sum(ipw)^2/sum(ipw^2), excluded_target_mass = sum(w[!keep]), retained_target_mass = sum(w[keep]), 
        estimand_changed = any(!keep & w > 0), stabilization_factor = factor, assumptions = list("Sequential conditional MAR at every mechanism", 
            "Positivity in the retained target population"), estimand = "retained_target_population"), 
        context)
}

fuzzy_reject_augmentation <- function(outcomes, observed, baseline_pd, application_ids, augmentation_weight = 1, 
    context = NULL) {
    dat <- .cr_sb_sample(outcomes, observed)
    y <- dat$y
    s <- dat$s
    p <- .cr_sb_vec(baseline_pd, length(s), "baseline_pd", TRUE)
    if (length(application_ids) != length(s) || anyDuplicated(application_ids)) 
        .cr_error("DUPLICATE_ID", "Unique application IDs required")
    mass <- .cr_numeric(augmentation_weight, "augmentation_weight")
    if (length(mass) != 1L || mass < 0) 
        .cr_error("INVALID_WEIGHTS", "Nonnegative scalar augmentation mass required")
    rows <- list()
    for (i in seq_along(s)) {
        key <- .cr_sb_str(application_ids[[i]], "application_id")
        labels <- if (s[i] == 1) 
            list(c(y[i], 1))
        else list(c(0, mass * (1 - p[i])), c(1, mass * p[i]))
        for (pair in labels) rows[[length(rows) + 1L]] <- list(application_id = key, outcome = pair[1], 
            weight = pair[2], label_origin = if (s[i] == 1) "OBSERVED" else "HYPOTHETICAL", validation_eligible = s[i] == 
                1, baseline_pd = p[i])
    }
    .cr_sb_out("fuzzy_reject_augmentation", list(rows = rows, role = "TRAINING_ONLY", assumption = "Missing applicant PD equals the frozen baseline PD; not identified from observed labels."), 
        context)
}

parcel_rejected_applications <- function(bands, accepted_defaults, accepted_counts, rejected_counts, 
    log_odds_shift = 0, uniforms = NULL, context = NULL) {
    n <- length(bands)
    if (!n || anyDuplicated(bands)) 
        .cr_error("INVALID_BANDS", "Distinct frozen band names required")
    d <- .cr_sb_vec(accepted_defaults, n, "accepted_defaults")
    a <- .cr_sb_vec(accepted_counts, n, "accepted_counts")
    r <- .cr_sb_vec(rejected_counts, n, "rejected_counts")
    if (any(a <= 0) || any(d < 0 | d > a) || any(r < 0) || any(c(d, a, r) != floor(c(d, a, r)))) 
        .cr_error("INVALID_COUNTS", "Positive accepted counts and integer counts required")
    shift <- .cr_numeric(log_odds_shift, "log_odds_shift")
    if (length(shift) == 1L) 
        shift <- rep(shift, n)
    if (length(shift) != n) 
        .cr_error("INCOMPATIBLE_SHAPE", "Scalar or per-band shifts required")
    base <- d/a
    risk <- stats::plogis(stats::qlogis(base) + shift)
    labels <- NULL
    if (!is.null(uniforms)) {
        if (length(uniforms) != n) 
            .cr_error("INCOMPATIBLE_SHAPE", "One shared uniform vector per band")
        labels <- list()
        for (i in seq_len(n)) {
            u <- .cr_numeric(uniforms[[i]], "uniforms", allow_empty = TRUE, vector_only = TRUE)
            if (length(u) != r[i] || any(u < 0 | u >= 1)) 
                .cr_error("INVALID_DRAWS", "Exactly reject-count uniforms in [0,1) required")
            labels[[i]] <- as.list(as.integer(u < risk[i]))
        }
    }
    .cr_sb_out("parcel_rejected_applications", list(bands = as.list(bands), accepted_defaults = as.list(d), 
        accepted_counts = as.list(a), rejected_counts = as.list(r), baseline_bad_rate = as.list(base), 
        assumed_reject_bad_rate = as.list(risk), hypothetical_default_mass = as.list(r * risk), hypothetical_labels = labels, 
        log_odds_shift = as.list(shift), label_origin = "HYPOTHETICAL", validation_eligible = FALSE, 
        role = "TRAINING_SCENARIO"), context)
}

reject_inference_bounds <- function(outcomes, observed, target_weights = NULL, strata = NULL, risk_bounds = NULL, 
    context = NULL) {
    dat <- .cr_sb_sample(outcomes, observed, target_weights)
    y <- dat$y
    s <- dat$s
    w <- dat$w
    if (is.null(strata)) 
        strata <- rep("all", length(s))
    if (length(strata) != length(s) || !is.character(strata) || anyNA(strata) || any(!nzchar(strata))) 
        .cr_error("INVALID_STRATA", "One stratum name per row")
    keys <- sort(unique(strata))
    restrictions <- if (is.null(risk_bounds)) 
        list()
    else risk_bounds
    if (!is.list(restrictions) || length(setdiff(names(restrictions), keys))) 
        .cr_error("INVALID_BOUNDS", "Bounds must name existing strata")
    parts <- list()
    lower <- 0
    upper <- 0
    for (key in keys) {
        k <- strata == key
        known <- sum(w[k] * s[k] * y[k])
        missing <- sum(w[k] * (1 - s[k]))
        b <- restrictions[[key]]
        if (is.null(b)) 
            b <- c(0, 1)
        bounds <- .cr_numeric(b, "risk_bounds", vector_only = TRUE)
        if (length(bounds) != 2L || !(0 <= bounds[1] && bounds[1] <= bounds[2] && bounds[2] <= 1)) 
            .cr_error("INVALID_BOUNDS", "Require 0 <= lower <= upper <= 1")
        lo <- known + missing * bounds[1]
        hi <- known + missing * bounds[2]
        lower <- lower + lo
        upper <- upper + hi
        parts[[length(parts) + 1L]] <- list(stratum = key, observed_default_mass = known, unobserved_mass = missing, 
            assumed_missing_risk_bounds = as.list(bounds), lower_mass = lo, upper_mass = hi)
    }
    .cr_sb_out("reject_inference_bounds", list(lower = lower/sum(w), upper = upper/sum(w), strata = parts, 
        assumptions = c(list("Binary own-contract potential outcome"), if (length(restrictions)) list("User-specified missing-risk restrictions") else list()), 
        identification = if (lower != upper) "PARTIAL" else "POINT", target_mass = sum(w)), context)
}

reject_inference_sensitivity <- function(baseline_pd, observed, outcomes, log_odds_shifts, target_weights = NULL, 
    threshold = NULL, context = NULL) {
    dat <- .cr_sb_sample(outcomes, observed, target_weights)
    y <- dat$y
    s <- dat$s
    w <- dat$w
    p <- .cr_sb_vec(baseline_pd, length(s), "baseline_pd", TRUE)
    shifts <- .cr_numeric(log_odds_shifts, "log_odds_shifts", vector_only = TRUE)
    if (!length(shifts) || anyDuplicated(shifts)) 
        .cr_error("INVALID_GRID", "Unique explicit shifts required")
    if (!is.null(threshold)) {
        z <- .cr_probability(threshold, boundary_policy = "closed")
        if (length(z) != 1L) 
            .cr_error("INVALID_INPUT", "Scalar threshold required")
    }
    surface <- list()
    for (delta in sort(shifts)) {
        risk <- stats::plogis(stats::qlogis(p) + delta)
        contribution <- s * y + (1 - s) * risk
        surface[[length(surface) + 1L]] <- list(log_odds_shift = delta, population_pd = sum(w * contribution)/sum(w), 
            hypothetical_pd = as.list(risk))
    }
    crossings <- list()
    if (!is.null(threshold) && length(surface) > 1L) 
        for (i in seq_len(length(surface) - 1L)) if (surface[[i]]$population_pd <= threshold && threshold <= 
            surface[[i + 1L]]$population_pd) 
            crossings[[length(crossings) + 1L]] <- list(surface[[i]]$log_odds_shift, surface[[i + 1L]]$log_odds_shift)
    .cr_sb_out("reject_inference_sensitivity", list(surface = surface, crossover_brackets = crossings, 
        observed_default_mass = sum(w * s * y), unobserved_target_mass = sum(w * (1 - s)), identified_component = "Observed own-contract outcomes only", 
        assumed_component = "User-supplied log-odds shift for missing potential outcomes", baseline_boundary_count = sum(p == 
            0 | p == 1), validation_eligible = FALSE), context)
}

estimate_population_risk_aipw <- function(outcomes, observed, outcome_predictions, propensities, fold_lineage, 
    target_weights = NULL, clusters = NULL, context = NULL) {
    dat <- .cr_sb_sample(outcomes, observed, target_weights)
    y <- dat$y
    s <- dat$s
    w <- dat$w
    n <- length(s)
    m <- .cr_sb_vec(outcome_predictions, n, "outcome_predictions", TRUE)
    e <- .cr_sb_vec(propensities, n, "propensities", TRUE)
    if (!.cr_object(fold_lineage)) 
        .cr_error("INVALID_SPLIT", "Explicit cross-fit lineage required")
    plans <- .cr_sb_folds(fold_lineage$folds, fold_lineage$rows, n)
    covered <- unique(unlist(lapply(plans, `[[`, "validation")))
    if (any(w[setdiff(seq_len(n), covered)] > 0)) 
        .cr_error("LEAKAGE", "Positive target-weight rows require out-of-fold nuisance predictions")
    if (!identical(fold_lineage$nuisance_prediction_origin, "OUT_OF_FOLD")) 
        .cr_error("LEAKAGE", "Both nuisance predictions must be out-of-fold")
    if (any(e <= 0 & w > 0)) 
        .cr_error("POSITIVITY_FAILURE", "Positive target-weight propensities must exceed zero")
    correction <- numeric(n)
    selected <- s == 1 & w > 0
    correction[selected] <- (y[selected] - m[selected])/e[selected]
    pseudo <- m + correction
    estimate <- sum(w * pseudo)/sum(w)
    influence <- w * (pseudo - estimate)/sum(w)
    groups <- if (is.null(clusters)) 
        vapply(fold_lineage$rows, `[[`, character(1), "entity_id")
    else clusters
    if (length(groups) != n) 
        .cr_error("INCOMPATIBLE_SHAPE", "One cluster per row")
    for (entity in unique(vapply(fold_lineage$rows, `[[`, character(1), "entity_id"))) if (length(unique(as.character(groups[vapply(fold_lineage$rows, 
        `[[`, character(1), "entity_id") == entity]))) > 1L) 
        .cr_error("INVALID_CLUSTERS", "Repeated entities cannot be split across uncertainty clusters")
    keys <- sort(unique(as.character(groups[w > 0])))
    totals <- vapply(keys, function(key) sum(influence[as.character(groups) == key]), numeric(1))
    if (length(keys) < 2L) 
        .cr_error("INSUFFICIENT_CLUSTERS", "At least two independent positive-mass clusters required")
    variance <- length(keys)/(length(keys) - 1L) * sum(totals^2)
    se <- sqrt(variance)
    .cr_sb_out("estimate_population_risk_aipw", list(population_pd = estimate, augmented_contributions = as.list(pseudo), 
        influence = as.list(influence), standard_error = se, confidence_interval = as.list(estimate + 
            c(-1, 1) * 1.95996398454005 * se), cluster_count = length(keys), uncertainty = "CLUSTER_EMPIRICAL_INFLUENCE_NORMAL", 
        nuisance_lineage_hash = .cr_hash(fold_lineage), outside_probability_domain = estimate < 0 || 
            estimate > 1, assumptions = list("Conditional MAR", "Positivity", "Either outcome or selection nuisance correctly specified", 
            "Independent clusters and adequate nuisance convergence"), estimand = "Population mean, not individual reject PD"), 
        context)
}

prepare_reject_evaluation <- function(candidates, evaluation_rows, as_of, target, outcome_comparability = NULL, 
    context = NULL) {
    cutoff <- .cr_date(as_of, "as_of")
    target <- .cr_cal_target(target)
    if (!is.list(candidates) || !length(candidates)) 
        .cr_error("INVALID_INPUT", "Frozen candidate records required")
    for (c in candidates) {
        .cr_sb_str(c$model_id, "model_id")
        .cr_sb_str(c$model_hash, "model_hash")
        if (is.null(c$training_entities) || !is.list(c$training_entities) || any(!vapply(c$training_entities, 
            function(v) is.character(v) && length(v) == 1L && nzchar(v), logical(1)))) 
            .cr_error("INVALID_SPLIT", "Candidate training_entities must explicitly identify all training groups")
        if (.cr_date(c$training_known_at, "training_known_at") > cutoff) 
            .cr_error("LEAKAGE", "Candidate trained after cutoff")
    }
    rows <- .cr_sb_rows(evaluation_rows, "evaluation_rows")
    scopes <- list()
    flags <- list()
    for (key in names(rows)) {
        row <- rows[[key]]
        date <- .cr_date(row$as_of, "as_of")
        known <- .cr_date(row$known_at, "known_at")
        if (known < date || known > cutoff) 
            .cr_error("INVALID_DATE", "Evaluation label unavailable at cutoff")
        entity <- .cr_sb_str(row$entity_id, "entity_id")
        origin <- row$label_origin
        contaminated <- any(vapply(candidates, function(c) entity %in% c$training_entities || date <= 
            c$training_known_at, logical(1)))
        scope <- "NOT_IDENTIFIED"
        eligible <- FALSE
        same_target <- identical(.cr_hash(row$target), .cr_hash(target))
        if (identical(origin, "OBSERVED_OWN_CONTRACT")) {
            if (!same_target || !isTRUE(row$funded) || !isTRUE(row$mature)) 
                .cr_error("TARGET_MISMATCH", "Comparable mature funded own-contract outcomes required")
            scope <- "FUNDED_OBSERVED_TEST"
            eligible <- TRUE
        }
        else if (identical(origin, "AUTHORIZED_EXPLORATION")) {
            if (is.null(row$authorization_reference) || !nzchar(row$authorization_reference) || !same_target || 
                !isTRUE(row$funded) || !isTRUE(row$mature)) 
                .cr_error("INVALID_EVIDENCE", "Exploration needs authorization and observed comparable target")
            scope <- "OBSERVED_EXPLORATION"
            eligible <- TRUE
        }
        else if (identical(origin, "SYNTHETIC_TRUTH")) {
            if (!identical(row$data_origin, "SYNTHETIC") || !same_target) 
                .cr_error("INVALID_EVIDENCE", "Declare synthetic origin and target")
            scope <- "SYNTHETIC_ENGINEERING_ONLY"
            eligible <- TRUE
        }
        else if (identical(origin, "EXTERNAL_CONTRACT")) {
            if (is.null(outcome_comparability)) 
                flags[[length(flags) + 1L]] <- list(application_id = key, reason = "EXTERNAL_TARGET_NOT_TRANSPORTED")
            else {
                if (!.cr_object(outcome_comparability) || is.null(outcome_comparability$transport_assumption) || 
                  !nzchar(outcome_comparability$transport_assumption) || !identical(.cr_hash(outcome_comparability$target), 
                  .cr_hash(target))) 
                  .cr_error("INVALID_EVIDENCE", "Explicit transport assumption and target required")
                scope <- "EXTERNAL_PROXY_ASSUMED_TRANSPORT"
                eligible <- TRUE
            }
        }
        else if (length(origin) == 1L && origin %in% c("HYPOTHETICAL", "PSEUDO", "UNOBSERVED")) 
            flags[[length(flags) + 1L]] <- list(application_id = key, reason = "NO_OBSERVED_TRUTH")
        else .cr_error("INVALID_EVIDENCE", "Unknown label origin")
        if (eligible) {
            y <- .cr_numeric(row$outcome, "outcome")
            if (length(y) != 1L || !y %in% c(0, 1)) 
                .cr_error("INVALID_OUTCOME", "Binary label required")
        }
        if (contaminated) {
            flags[[length(flags) + 1L]] <- list(application_id = key, reason = "TRAIN_EVALUATION_CONTAMINATION")
            eligible <- FALSE
        }
        scopes[[length(scopes) + 1L]] <- list(application_id = key, entity_id = entity, evidence_scope = scope, 
            validation_eligible = eligible, label_origin = origin)
    }
    synthetic <- length(scopes) > 0L && all(vapply(scopes, function(r) r$evidence_scope == "SYNTHETIC_ENGINEERING_ONLY", 
        logical(1)))
    .cr_sb_out("prepare_reject_evaluation", list(rows = scopes, contamination_flags = flags, target = target, 
        as_of = cutoff, validation_identified = any(vapply(scopes, `[[`, logical(1), "validation_eligible")), 
        candidate_hashes = lapply(candidates, `[[`, "model_hash"), role = "EVALUATION_ONLY", split_plan = list(role = "EVALUATION_ONLY", 
            validation_indices = as.list(which(vapply(scopes, `[[`, logical(1), "validation_eligible")) - 
                1L), excluded_indices = as.list(which(!vapply(scopes, `[[`, logical(1), "validation_eligible")) - 
                1L), entity_disjoint = TRUE, cutoff = cutoff), no_lending_action = TRUE), context, synthetic = synthetic)
}

.cr_sb_gl <- local({
    n <- 64L
    i <- seq_len(n - 1L)
    off <- i/sqrt(4 * i * i - 1)
    jac <- matrix(0, n, n)
    jac[cbind(i, i + 1L)] <- off
    jac[cbind(i + 1L, i)] <- off
    e <- eigen(jac, symmetric = TRUE)
    order <- order(e$values)
    list(nodes = e$values[order], weights = 2 * e$vectors[1L, order]^2)
})

.cr_sb_bvn <- function(a, b, rho) {
    t <- rho * (.cr_sb_gl$nodes + 1)/2
    q <- 1 - t * t
    vapply(seq_along(a), function(i) {
        integrand <- exp(-(a[i]^2 - 2 * t * a[i] * b[i] + b[i]^2)/(2 * q))/(2 * pi * sqrt(q))
        value <- stats::pnorm(a[i]) * stats::pnorm(b[i]) + rho/2 * sum(integrand * .cr_sb_gl$weights)
        max(0, min(stats::pnorm(a[i]), stats::pnorm(b[i]), value))
    }, numeric(1))
}

.cr_sb_objective <- function(theta, d, z, s, y, fixed_rho) {
    k <- ncol(d)
    j <- ncol(z)
    rho <- if (is.null(fixed_rho)) 
        tail(theta, 1L)
    else fixed_rho
    a <- as.vector(d %*% theta[seq_len(k)])
    b <- as.vector(z %*% theta[k + seq_len(j)])
    chosen <- s == 1
    sign <- 2 * y[chosen] - 1
    u <- sign * a[chosen]
    v <- b[chosen]
    r <- sign * rho
    prob <- numeric(length(s))
    prob[!chosen] <- stats::pnorm(-b[!chosen])
    prob[chosen] <- vapply(seq_along(u), function(i) .cr_sb_bvn(u[i], v[i], r[i]), numeric(1))
    if (any(prob <= 1e-14)) 
        return(list(value = 1e+15 + sum(pmax(0, 1e-14 - prob)) * 1e+15, gradient = rep(0, length(theta))))
    den <- sqrt(1 - r * r)
    score_a <- sign * stats::dnorm(u) * stats::pnorm((v - r * u)/den)/prob[chosen]
    score_b <- numeric(length(s))
    score_b[chosen] <- stats::dnorm(v) * stats::pnorm((u - r * v)/den)/prob[chosen]
    score_b[!chosen] <- -stats::dnorm(b[!chosen])/prob[!chosen]
    grad <- c(as.vector(crossprod(d[chosen, , drop = FALSE], score_a)), as.vector(crossprod(z, score_b)))
    if (is.null(fixed_rho)) {
        density <- exp(-(u * u - 2 * r * u * v + v * v)/(2 * (1 - r * r)))/(2 * pi * den)
        grad <- c(grad, sum(sign * density/prob[chosen]))
    }
    list(value = -sum(log(prob)), gradient = -grad)
}

fit_joint_selection_model <- function(X_outcome, X_selection, selected, outcomes, outcome_features, selection_features, 
    target, training, model_id, model_version, rho_grid = NULL, selected_rho = NULL, exclusion = NULL, 
    context = NULL) {
    design <- .cr_glm_design(X_outcome, outcome_features, TRUE)
    x <- design$x
    d <- design$d
    design <- .cr_glm_design(X_selection, selection_features, TRUE)
    z0 <- design$x
    z <- design$d
    dat <- .cr_sb_sample(outcomes, selected)
    y <- dat$y
    s <- dat$s
    if (nrow(x) != length(s) || nrow(z) != length(s)) 
        .cr_error("INCOMPATIBLE_SHAPE", "Designs and selection must have equal rows")
    if (length(unique(s)) != 2L || length(unique(y[s == 1])) != 2L) 
        .cr_error("INSUFFICIENT_CLASS_SUPPORT", "Both selection and selected outcome classes required")
    for (name in intersect(outcome_features, selection_features)) if (!identical(x[, match(name, outcome_features)], 
        z0[, match(name, selection_features)])) 
        .cr_error("FEATURE_MISMATCH", "Shared feature names must have identical values")
    target <- .cr_cal_target(target)
    if (!identical(target$conditioning, "POTENTIAL_OWN_CONTRACT")) 
        .cr_error("TARGET_MISMATCH", "Joint target is unconditional potential own-contract PD")
    mode <- "FIXED_RHO_SENSITIVITY"
    if (is.null(rho_grid)) {
        if (!.cr_object(exclusion) || !length(exclusion$features) || is.null(exclusion$justification) || 
            !nzchar(exclusion$justification)) 
            .cr_error("IDENTIFICATION_REQUIRED", "Estimated rho requires exclusion and substantive justification")
        names <- unlist(exclusion$features)
        if (anyDuplicated(names) || !all(names %in% setdiff(selection_features, outcome_features))) 
            .cr_error("INVALID_EXCLUSION", "Exclusions must occur only in selection design")
        if (qr(cbind(d, z0[, match(names, selection_features), drop = FALSE]))$rank < ncol(d) + length(names)) 
            .cr_error("INVALID_EXCLUSION", "Exclusions redundant with outcome design")
        grid <- list(NULL)
        mode <- "ESTIMATED_RHO_ASSUMED_EXCLUSION"
        if (!is.null(selected_rho)) 
            .cr_error("INVALID_GRID", "selected_rho valid only for sensitivity grid")
    }
    else {
        vals <- .cr_numeric(rho_grid, "rho_grid", vector_only = TRUE)
        if (!length(vals) || anyDuplicated(vals) || any(abs(vals) > 0.95)) 
            .cr_error("INVALID_GRID", "Unique rho grid in [-0.95,0.95] required")
        grid <- as.list(sort(vals))
        if (is.null(selected_rho) || length(selected_rho) != 1L || !selected_rho %in% vals) 
            .cr_error("INVALID_GRID", "Explicit selected_rho must be a supplied scenario")
    }
    outcome_fit <- fit_binary_glm(x[s == 1, , drop = FALSE], y[s == 1], link = "probit", feature_order = outcome_features, 
        fit_intercept = TRUE, target = target, training = training, model_id = model_id, model_version = model_version)
    selection_fit <- fit_binary_glm(z0, s, link = "probit", feature_order = selection_features, fit_intercept = TRUE, 
        target = target, training = training, model_id = model_id, model_version = model_version)
    start <- c(unlist(outcome_fit$model$parameters$coefficients), unlist(selection_fit$model$parameters$coefficients))
    profiles <- list()
    fits <- list()
    for (q in seq_along(grid)) {
        rho <- grid[[q]]
        initial <- if (is.null(rho)) 
            c(start, 0)
        else start
        fn <- function(theta) .cr_sb_objective(theta, d, z, s, y, rho)$value
        gr <- function(theta) .cr_sb_objective(theta, d, z, s, y, rho)$gradient
        fit <- stats::optim(initial, fn, gr, method = "L-BFGS-B", lower = c(rep(-20, length(start)), 
            if (is.null(rho)) -0.95 else numeric()), upper = c(rep(20, length(start)), if (is.null(rho)) 0.95 else numeric()), 
            control = list(maxit = 1000, factr = 450, pgtol = 1e-07, lmm = 10))
        objective <- fn(fit$par)
        gradient <- gr(fit$par)
        if (fit$convergence != 0L || objective >= 1e+14 || max(abs(gradient)) > 0.002) 
            .cr_error("NONCONVERGENCE", "Bivariate probit likelihood did not converge", list(message = fit$message, 
                max_score = max(abs(gradient))))
        fitted_rho <- if (is.null(rho)) 
            tail(fit$par, 1L)
        else rho
        if (any(abs(fit$par[seq_along(start)]) > 19.999) || (is.null(rho) && abs(fitted_rho) > 0.94999)) 
            .cr_error("WEAK_IDENTIFICATION", "Optimum at coefficient/rho boundary")
        profiles[[q]] <- list(rho = fitted_rho, log_likelihood = -objective, model_population_pd = mean(stats::pnorm(as.vector(d %*% fit$par[seq_len(ncol(d))]))), risk_scope = "MODEL_ASSUMED_POTENTIAL_OWN_CONTRACT", outcome_coefficients = as.list(fit$par[seq_len(ncol(d))]), 
            selection_coefficients = as.list(fit$par[ncol(d) + seq_len(ncol(z))]), maximum_absolute_score = max(abs(gradient)), 
            iterations = unname(fit$counts[[1]]))
        fits[[q]] <- fit
    }
    index <- if (startsWith(mode, "ESTIMATED")) 
        1L
    else match(selected_rho, unlist(grid))
    chosen <- profiles[[index]]
    theta <- fits[[index]]$par
    h <- matrix(0, length(theta), length(theta))
    for (j in seq_along(theta)) {
        step <- 1e-04 * max(1, abs(theta[j]))
        plus <- theta
        minus <- theta
        plus[j] <- plus[j] + step
        minus[j] <- minus[j] - step
        h[, j] <- (.cr_sb_objective(plus, d, z, s, y, grid[[index]])$gradient - .cr_sb_objective(minus, 
            d, z, s, y, grid[[index]])$gradient)/(2 * step)
    }
    h <- (h + t(h))/2
    eigenvalues <- eigen(h, symmetric = TRUE, only.values = TRUE)$values
    condition <- if (min(eigenvalues) > 0) 
        max(eigenvalues)/min(eigenvalues)
    else NULL
    covariance <- if (min(eigenvalues) > 1e-08) 
        lapply(seq_len(nrow(h)), function(i) as.list(solve(h)[i, ]))
    else NULL
    a <- as.vector(d %*% unlist(chosen$outcome_coefficients))
    b <- as.vector(z %*% unlist(chosen$selection_coefficients))
    joint <- .cr_sb_bvn(a, b, chosen$rho)
    ps <- stats::pnorm(b)
    features <- c(outcome_features, setdiff(selection_features, outcome_features))
    diag <- list(mode = mode, profiles = profiles, selected_rho = chosen$rho, exclusion = exclusion, 
        hessian_condition_number = condition, weak_identification = is.null(condition) || condition > 
            1e+08, covariance = covariance, covariance_type = if (!is.null(rho_grid)) "OBSERVED_INFORMATION_IID_CONDITIONAL_ON_FIXED_RHO" else "OBSERVED_INFORMATION_IID", 
        quadrature = "PLACKETT_GAUSS_LEGENDRE_64", fitted_population_pd = as.list(stats::pnorm(a)), fitted_selection_probability = as.list(ps), 
        fitted_selected_pd = as.list(joint/ps), fitted_unselected_pd = as.list((stats::pnorm(a) - joint)/(1 - 
            ps)), assumptions = if (!is.null(exclusion)) list("Joint standard-normal latent disturbances", 
            "Correct outcome and selection indices", "Exclusion validity is user asserted, not established by likelihood") else list("Joint standard-normal latent disturbances", 
            "Correct indices", "User-fixed latent selection correlation"), role = "TRAINING_AND_ASSUMPTION_SENSITIVITY_ONLY")
    train <- training
    train$data_fingerprint <- .cr_hash(list(X_outcome = lapply(seq_len(nrow(x)), function(i) as.list(unname(x[i, 
        ]))), X_selection = lapply(seq_len(nrow(z0)), function(i) as.list(unname(z0[i, ]))), selected = as.list(s), 
        outcomes = lapply(seq_along(s), function(i) if (s[i] == 1) y[i] else NULL)))
    model <- .cr_model(list(schema_version = "1.0", model_id = model_id, model_version = model_version, 
        method_id = "selection_bias.fit_joint_selection_model", feature_order = as.list(features), parameters = list(outcome_features = as.list(outcome_features), 
            selection_features = as.list(selection_features), outcome_coefficients = chosen$outcome_coefficients, 
            selection_coefficients = chosen$selection_coefficients, rho = chosen$rho), preprocessing = list(transformation = "identity"), 
        target = target, training = train, diagnostics = diag, provenance = list(data_origin = if (is.null(training$data_origin)) "USER_SUPPLIED" else training$data_origin, 
            parameter_origin = "ESTIMATED")))
    .cr_fit_result(model, "selection_bias.fit_joint_selection_model", diag, context)
}

.cr_sb_predict_joint <- function(model, X, feature_order, expected_target, context = NULL) {
    model <- .cr_model(model)
    if (!identical(as.list(feature_order), model$feature_order)) 
        .cr_error("FEATURE_MISMATCH", "Feature order differs from frozen joint model")
    if (!identical(.cr_hash(expected_target), .cr_hash(model$target))) 
        .cr_error("TARGET_MISMATCH", "Expected target differs")
    x <- .cr_glm_design(X, feature_order, TRUE)$x
    p <- model$parameters
    if (!identical(model$preprocessing, list(transformation = "identity")) || !is.list(p$outcome_features) || 
        !all(unlist(p$outcome_features) %in% feature_order)) 
        .cr_error("INVALID_MODEL", "Invalid joint model feature mapping")
    rho <- .cr_numeric(p$rho, "rho")
    if (length(rho) != 1L || abs(rho) > 0.95 || anyDuplicated(unlist(p$outcome_features)) || !identical(model$target$conditioning, 
        "POTENTIAL_OWN_CONTRACT")) 
        .cr_error("INVALID_MODEL", "Invalid joint correlation, mapping or target")
    coef <- .cr_numeric(unlist(p$outcome_coefficients), "outcome_coefficients", vector_only = TRUE)
    if (length(coef) != length(p$outcome_features) + 1L) 
        .cr_error("INVALID_MODEL", "Outcome coefficient count differs")
    xx <- x[, match(unlist(p$outcome_features), feature_order), drop = FALSE]
    pd <- stats::pnorm(coef[1] + as.vector(xx %*% coef[-1]))
    .cr_result(pd, "classification.predict_pd", "probability", context, provenance = model$provenance, 
        diagnostics = list(prediction_family = "joint_binary_selection", conditioning = "POTENTIAL_OWN_CONTRACT", 
            rho = p$rho), limitations = "Potential own-contract risk depends on frozen selection and normality assumptions; it is not observed reject validation.")
}

