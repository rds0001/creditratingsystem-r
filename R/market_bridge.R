fit_market_to_physical_pd_bridge <- function(observations, feature_order, bridge_contract, folds, model_id, 
    model_version, context = NULL) {
    if (!is.list(observations) || !length(observations)) 
        .cr_error("INVALID_INPUT", "Observed dated market/default rows required")
    c <- bridge_contract
    target <- .cr_cal_target(c$target)
    if (!identical(target$probability_measure, "PHYSICAL") || !identical(target$conditioning, "initially_performing") || 
        !identical(target$event, "first_default")) 
        .cr_error("TARGET_MISMATCH", "Observed physical first-default target required")
    if (!identical(c$feature_probability_measure, "RISK_NEUTRAL") || !identical(c$outcome_origin, "OBSERVED_ISSUER_DEFAULT")) 
        .cr_error("MEASURE_MISMATCH", "Q features and observed issuer outcomes required")
    .cr_mkt_text(c$target_population, "target_population")
    if (!identical(c$population_selection, "MARKET_QUOTED_ISSUERS") || !identical(c$transformation, "IDENTITY_PREDECLARED_FEATURES")) 
        .cr_error("INVALID_INPUT", "Declare quoted population and predeclared features")
    feature_contracts <- c$market_feature_contracts
    if (!.cr_object(feature_contracts) || !setequal(names(feature_contracts), unlist(feature_order))) 
        .cr_error("FEATURE_MISMATCH", "Each feature needs a measure/horizon/definition contract")
    for (feature in unlist(feature_order)) {
        fc <- feature_contracts[[feature]]
        if (!.cr_object(fc) || !identical(fc$probability_measure, "RISK_NEUTRAL") || !isTRUE(fc$time_unit %in% 
            c("days", "months", "years"))) 
            .cr_error("MEASURE_MISMATCH", "Market feature measure and horizon units required")
        if (.cr_mkt_scalar(fc$horizon, "market feature horizon", 0) <= 0) 
            .cr_error("INVALID_INPUT", "Positive market feature horizon required")
        .cr_mkt_text(fc$definition, "market feature definition")
    }
    cutoff <- .cr_date(c$known_at, "known_at")
    ctx <- .cr_result(NULL, "market_implied.context", context = context)$context
    if (!is.null(ctx$probability_measure) && !identical(ctx$probability_measure, "PHYSICAL")) 
        .cr_error("MEASURE_MISMATCH", "Bridge output is physical")
    feature_order <- unlist(feature_order)
    ids <- character()
    metadata <- values <- list()
    outcomes <- numeric()
    for (row in observations) {
        ids <- c(ids, .cr_mkt_text(row$observation_id, "observation_id"))
        asof <- .cr_date(row$as_of, "as_of")
        end <- format(.hy_horizon_end(asof, target), "%Y-%m-%d")
        label <- .cr_date(row$label_known_at, "label_known_at")
        complete <- .cr_date(row$complete_through, "complete_through")
        if (complete < end || label < end || label > cutoff) 
            .cr_error("IMMATURE_OUTCOME", "Mature available target windows required")
        curve_date <- .cr_date(row$market_curve_as_of, "market_curve_as_of")
        curve_known <- .cr_date(row$market_curve_known_at, "market_curve_known_at")
        features_known <- .cr_date(row$features_known_at, "features_known_at")
        if (curve_known > curve_date || curve_date > asof || features_known > asof || features_known < 
            curve_known) 
            .cr_error("LOOKAHEAD_BIAS", "Future curve or features")
        .cr_mkt_text(row$market_curve_hash, "market_curve_hash")
        if (!identical(row$outcome_origin, "OBSERVED_ISSUER_DEFAULT") || !identical(row$initial_state, 
            "PERFORMING")) 
            .cr_error("INVALID_OUTCOME", "Observed initially-performing issuer required")
        y <- .cr_mkt_scalar(row$outcome, "outcome", 0, 1)
        if (!y %in% c(0, 1)) 
            .cr_error("INVALID_OUTCOME", "Binary outcome required")
        default <- row$default_date
        if (y == 1) {
            default <- .cr_date(default, "default_date")
            if (!(asof < default && default <= end)) 
                .cr_error("TARGET_MISMATCH", "Default outside horizon")
        }
        else if (!is.null(default) && .cr_date(default, "default_date") <= end) 
            .cr_error("TARGET_MISMATCH", "Nondefault conflicts with default date")
        if (!.cr_object(row$features) || !setequal(names(row$features), feature_order)) 
            .cr_error("FEATURE_MISMATCH", "Exactly declared feature values required")
        values[[length(values) + 1L]] <- vapply(row$features[feature_order], function(v) .cr_mkt_scalar(v, 
            "market feature"), numeric(1))
        outcomes <- c(outcomes, y)
        metadata[[length(metadata) + 1L]] <- list(entity_id = .cr_mkt_text(row$entity_id, "entity_id"), 
            as_of = asof, label_known_at = label, features_known_at = features_known)
    }
    if (anyDuplicated(ids)) 
        .cr_error("DUPLICATE_ID", "Duplicate observations")
    x <- .cr_glm_design(do.call(rbind, values), feature_order, TRUE)$x
    y <- outcomes
    plans <- .cr_sb_folds(folds, metadata, length(y))
    training <- list(data_fingerprint = .cr_hash(observations), split_id = .cr_mkt_text(c$split_id, "split_id"), 
        known_at = cutoff, population_selection = c$population_selection, target_population = c$target_population, 
        data_origin = if (is.null(c$data_origin)) "USER_SUPPLIED" else c$data_origin)
    fitone <- function(xx, yy, tt) fit_binary_glm(xx, yy, link = c$link, feature_order = feature_order, 
        fit_intercept = TRUE, target = target, training = tt, model_id = model_id, model_version = model_version, 
        context = context)
    full <- fitone(x, y, training)
    predictions <- rep(list(NULL), length(y))
    lineage <- list()
    for (plan in plans) {
        tr <- plan$train
        te <- plan$validation
        ft <- training
        ft$known_at <- max(vapply(metadata[tr], function(r) r$label_known_at, character(1)))
        ft$data_fingerprint <- .cr_hash(observations[tr])
        fitted <- fitone(x[tr, , drop = FALSE], y[tr], ft)
        p <- unlist(predict_pd(fitted, x[te, , drop = FALSE], feature_order = feature_order, expected_target = target)$value)
        for (j in seq_along(te)) predictions[te[j]] <- list(p[j])
        lineage[[length(lineage) + 1L]] <- list(train = as.list(tr - 1L), validation = as.list(te - 1L), 
            model_hash = .cr_hash(fitted$model), training_known_at = ft$known_at)
    }
    indices <- which(!vapply(predictions, is.null, logical(1)))
    p <- unlist(predictions[indices])
    yy <- y[indices]
    loss <- (yy - p)^2
    groups <- vapply(metadata[indices], function(r) r$entity_id, character(1))
    clusters <- split(loss, groups)
    influence <- vapply(clusters, function(v) (sum(v) - length(v) * mean(loss))/length(loss), numeric(1))
    cluster_se <- if (length(clusters) > 1) 
        sqrt(length(clusters)/(length(clusters) - 1) * sum(influence^2))
    else NULL
    diagnostics <- c(full$diagnostics, list(out_of_time_pd = predictions, warmup_indices = as.list(which(vapply(predictions, 
        is.null, logical(1))) - 1L), folds = lineage, observed_evaluation_count = length(indices), out_of_time_brier = mean(loss), 
        brier_cluster_standard_error = cluster_se, uncertainty = "ENTITY_CLUSTER_EMPIRICAL_INFLUENCE", 
        evaluation_scope = "OBSERVED_MATURE_MARKET_QUOTED_ISSUERS_ONLY", out_of_time_log_loss = -mean(yy * 
            log(p) + (1 - yy) * log1p(-p)), feature_probability_measure = "RISK_NEUTRAL", output_probability_measure = "PHYSICAL", 
        conversion_assumption = "HISTORICALLY_ESTIMATED_CONDITIONAL_ASSOCIATION_NOT_UNIVERSAL_MEASURE_CHANGE"))
    model <- full$model
    model$method_id <- "market_implied.fit_market_to_physical_pd_bridge"
    model$parameters$base_method_id <- "classification.fit_binary_glm"
    model$preprocessing$market_bridge_contract <- c
    model$diagnostics <- diagnostics
    model <- .cr_model(model)
    .cr_fit_result(model, model$method_id, diagnostics, context)
}

.cr_mkt_predict_bridge <- function(model, X, feature_order, expected_target, context = NULL) {
    model <- .cr_model(model)
    if (!identical(model$method_id, "market_implied.fit_market_to_physical_pd_bridge") || !identical(model$parameters$base_method_id, 
        "classification.fit_binary_glm")) 
        .cr_error("INVALID_ARTIFACT", "Expected historical physical bridge")
    c <- model$preprocessing$market_bridge_contract
    if (!identical(c$feature_probability_measure, "RISK_NEUTRAL") || !identical(model$target$probability_measure, 
        "PHYSICAL")) 
        .cr_error("MEASURE_MISMATCH", "Bridge measure changed")
    base <- model
    base$method_id <- "classification.fit_binary_glm"
    base$preprocessing <- list(transformation = "identity")
    predict_pd(base, X, feature_order = feature_order, expected_target = expected_target, context = context)
}

