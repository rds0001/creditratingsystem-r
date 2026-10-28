# Native frozen calibration; no Python runtime or external services.
.cr_cal_target <- function(target) {
  if (!.cr_object(target)) .cr_error("INVALID_CALIBRATION_METADATA", "target must be an object")
  .cr_canonical(target)
  for (nm in c("event", "conditioning"))
    .cr_string(target[[nm]], paste0("target.", nm), "INVALID_CALIBRATION_METADATA")
  if (length(target$probability_measure) != 1L ||
      !target$probability_measure %in% c("PHYSICAL", "RISK_NEUTRAL") ||
      length(target$time_unit) != 1L || !target$time_unit %in% c("days", "months", "years"))
    .cr_error("INVALID_CALIBRATION_METADATA", "Declare horizon unit and probability measure")
  h <- .cr_numeric(target$horizon, "target.horizon")
  if (length(h) != 1L || h <= 0) .cr_error("INVALID_CALIBRATION_METADATA", "Positive scalar horizon required")
  target$horizon <- as.double(h)
  target
}

.cr_cal_metadata <- function(metadata, empirical) {
  if (!.cr_object(metadata)) .cr_error("INVALID_CALIBRATION_METADATA", "Explicit calibration metadata required")
  .cr_canonical(metadata)
  metadata$target <- .cr_cal_target(metadata$target)
  for (nm in c("base_model_version", "population_definition", "outcome_policy"))
    .cr_string(metadata[[nm]], nm, "INVALID_CALIBRATION_METADATA")
  if (!identical(metadata$weight_type, "frequency"))
    .cr_error("INVALID_WEIGHT_TYPE", "Only frequency weights are supported")
  if (length(metadata$data_origin) != 1L || !metadata$data_origin %in% c("USER_SUPPLIED", "SYNTHETIC", "REFERENCE"))
    .cr_error("INVALID_CALIBRATION_METADATA", "Declare data_origin")
  sample <- metadata$calibration
  if (!.cr_object(sample)) .cr_error("INVALID_CALIBRATION_METADATA", "Calibration window and split required")
  for (nm in c("start", "end", "known_at")) sample[[nm]] <- .cr_date(sample[[nm]], nm)
  if (sample$start > sample$end || sample$end > sample$known_at)
    .cr_error("INVALID_CALIBRATION_METADATA", "Require start <= end <= known_at")
  .cr_string(sample$split_id, "split_id", "INVALID_CALIBRATION_METADATA")
  allowed <- if (empirical) c("HOLDOUT", "OUT_OF_FOLD") else c("HOLDOUT", "OUT_OF_FOLD", "EXTERNAL_TARGET")
  if (length(sample$prediction_origin) != 1L || !sample$prediction_origin %in% allowed)
    .cr_error("INVALID_CALIBRATION_METADATA", "Independent prediction origin required")
  metadata$calibration <- sample
  metadata
}

.cr_cal_data <- function(values, weights, outcomes = NULL, probability = FALSE, open_domain = FALSE) {
  x <- if (probability) .cr_probability(values, boundary_policy = if (open_domain) "open" else "closed",
                   allow_empty = FALSE, vector_only = TRUE) else .cr_numeric(values, "values", vector_only = TRUE)
  w <- if (is.null(weights)) rep(1, length(x)) else .cr_numeric(weights, "weights", vector_only = TRUE)
  if (length(w) != length(x)) .cr_error("INCOMPATIBLE_SHAPE", "Weights and values must match")
  if (any(w < 0 | w != floor(w)) || !any(w > 0) || !is.finite(sum(w)))
    .cr_error("INVALID_WEIGHTS", "Frequency weights must be nonnegative integers with positive finite total")
  y <- NULL
  if (!is.null(outcomes)) {
    y <- .cr_numeric(outcomes, "outcomes", vector_only = TRUE)
    if (length(y) != length(x)) .cr_error("INCOMPATIBLE_SHAPE", "Outcomes and values must match")
    if (any(!y %in% c(0, 1))) .cr_error("INVALID_OUTCOME", "Binary fully observed outcomes required")
  }
  list(x = x, w = w, y = y)
}

.cr_cal_controls <- function(tolerance, max_iterations) {
  tol <- .cr_numeric(tolerance, "tolerance")
  limit <- .cr_numeric(max_iterations, "max_iterations")
  if (length(tol) != 1L || tol <= 0 || tol >= 0.01 || length(limit) != 1L || limit < 1 || limit != floor(limit))
    .cr_error("INVALID_INPUT", "Positive tolerance below 0.01 and positive integer iterations required")
  list(tol = tol, limit = limit)
}

.cr_cal_context <- function(context, target) {
  if (inherits(context, "CalcResult")) {
    if (!identical(context$method_id, "core.build_calculation_context"))
      .cr_error("INVALID_CONTEXT", "Expected a calculation context result")
    context <- context$value
  }
  if (is.null(context)) return(NULL)
  if (!.cr_object(context)) .cr_error("INVALID_CONTEXT", "Named context object required")
  for (nm in c("horizon", "time_unit", "probability_measure", "conditioning"))
    if (!is.null(context[[nm]]) && !identical(.cr_hash(context[[nm]]), .cr_hash(target[[nm]])))
      .cr_error("INCOMPATIBLE_TARGET", paste("Context", nm, "differs from calibration target"))
  context
}

.cr_cal_artifact <- function(kind, parameters, metadata, data, diagnostics, context) {
  method <- paste0("calibration.calibrate_", kind)
  fingerprint <- .cr_hash(list(values = as.list(data$x), weights = as.list(data$w),
                              outcomes = if (is.null(data$y)) NULL else as.list(data$y)))
  artifact <- list(schema_version = "1.0", artifact_type = "CalibrationArtifact",
       calibration_type = kind, method_id = method, parameter_version = "1.0",
       input_kind = if (kind == "isotonic") "score" else "probability",
       parameters = parameters, target = metadata$target, metadata = metadata,
       calibration_input_fingerprint = fingerprint, diagnostics = diagnostics)
  artifact$artifact_hash <- .cr_hash(artifact)
  .cr_result(artifact, method, "calibration_artifact", .cr_cal_context(context, metadata$target), diagnostics = diagnostics,
      provenance = list(data_origin = metadata$data_origin, parameter_origin = "ESTIMATED"),
      limitations = c("Sample independence and outcome maturity are caller declarations; this fit does not verify them.",
                      "Calibration is not out-of-sample validation or model approval."))
}

calibrate_intercept <- function(base_pd, target_default_rate, weights = NULL,
                                metadata, context = NULL, tolerance = 1e-12, max_iterations = 200) {
  meta <- .cr_cal_metadata(metadata, FALSE)
  data <- .cr_cal_data(base_pd, weights, probability = TRUE)
  control <- .cr_cal_controls(tolerance, max_iterations)
  target <- .cr_probability(target_default_rate)
  if (length(target) != 1L) .cr_error("INVALID_INPUT", "Scalar target_default_rate required")
  p <- data$x; w <- data$w; wn <- w / sum(w)
  active <- p > 0 & p < 1 & w > 0
  lower <- sum(wn[p == 1]); upper <- sum(wn[p > 0])
  if (!any(active)) .cr_error("NOT_IDENTIFIABLE", "Interior positive-weight probabilities required")
  if (target <= lower || target >= upper)
    .cr_error("INFEASIBLE_TARGET", "Target cannot be attained by a finite shift preserving 0/1",
              list(lower_limit = lower, upper_limit = upper))
  scores <- stats::qlogis(p[active])
  residual <- function(shift) lower + sum(wn[active] * stats::plogis(scores + shift)) - target
  if (residual(0) == 0) {
    shift <- 0; iterations <- 0; bracket <- list(0, 0)
  } else {
    low <- -1; high <- 1
    while (residual(low) > 0 && abs(low) < 4096) low <- low * 2
    while (residual(high) < 0 && high < 4096) high <- high * 2
    fit <- tryCatch(stats::uniroot(residual, c(low, high), tol = control$tol,
                           maxiter = control$limit, check.conv = TRUE), error = identity)
    if (inherits(fit, "error")) .cr_error("NON_CONVERGENCE", "Cannot solve finite intercept shift")
    shift <- fit$root; iterations <- fit$iter; bracket <- list(low, high)
  }
  achieved <- target + residual(shift)
  if (abs(achieved-target) > control$tol) .cr_error("NON_CONVERGENCE", "Target reconciliation exceeds tolerance")
  diagnostics <- list(converged = TRUE, iterations = iterations, bracket = bracket,
        target_default_rate = target, achieved_mean_pd = achieved, residual = achieved-target,
        tolerance = control$tol, n_observations = length(p), weight_sum = sum(w),
        zero_weight_count = sum(w == 0), boundary_policy = "preserve", identifiable = TRUE)
  .cr_cal_artifact("intercept", list(intercept = shift, slope = 1, boundary_policy = "preserve"),
                   meta, data, diagnostics, context)
}

calibrate_logistic <- function(base_pd, outcomes, weights = NULL, metadata, context = NULL,
                               tolerance = 1e-10, max_iterations = 100) {
  meta <- .cr_cal_metadata(metadata, TRUE)
  data <- .cr_cal_data(base_pd, weights, outcomes, probability = TRUE, open_domain = TRUE)
  if (is.null(data$y)) .cr_error("MISSING_INPUT", "Observed outcomes required")
  control <- .cr_cal_controls(tolerance, max_iterations)
  keep <- data$w > 0
  x <- stats::qlogis(data$x[keep]); w <- data$w[keep]; y <- data$y[keep]
  if (length(unique(x)) < 2L) .cr_error("NOT_IDENTIFIABLE", "Constant logit cannot identify two parameters")
  if (length(unique(y)) < 2L) .cr_error("SEPARATION", "Both classes required for finite MLE")
  x0 <- x[y == 0]; x1 <- x[y == 1]
  if (max(x0) <= min(x1) || max(x1) <= min(x0))
    .cr_error("SEPARATION", "Complete or quasi separation has no finite MLE")
  center <- mean(x); scale <- max(abs(x-center))
  design <- cbind(1, (x-center)/scale)
  warnings <- character()
  fit <- tryCatch(withCallingHandlers(stats::glm.fit(design, y, weights = w, family = stats::binomial(),
      start = c(stats::qlogis(sum(w*y)/sum(w)), 0),
      control = stats::glm.control(epsilon = min(control$tol^2, 1e-12), maxit = control$limit)),
      warning = function(warning) {
        warnings <<- c(warnings, conditionMessage(warning)); invokeRestart("muffleWarning")
      }), error = identity)
  if (inherits(fit, "error") || any(!is.finite(fit$coefficients)))
    .cr_error("NON_CONVERGENCE", "Logistic calibration failed")
  probability <- stats::plogis(drop(design %*% fit$coefficients))
  gradient <- drop(crossprod(design, (w/sum(w))*(probability-y)))
  error <- max(abs(gradient))
  if (error > control$tol) .cr_error("NON_CONVERGENCE", "Logistic score equation did not converge",
                                    list(gradient_max = error, iterations = fit$iter))
  transform <- matrix(c(1, 0, -center/scale, 1/scale), nrow = 2L)
  beta <- drop(transform %*% fit$coefficients)
  information <- crossprod(design, (w*probability*(1-probability))*design)
  covariance <- tryCatch(transform %*% solve(information) %*% t(transform), error = identity)
  if (inherits(covariance, "error") || any(!is.finite(covariance)) || any(diag(covariance) <= 0))
    .cr_error("NOT_IDENTIFIABLE", "Calibration information is singular")
  eta <- drop(design %*% fit$coefficients)
  nll <- sum(w*(pmax(eta, 0)+log1p(exp(-abs(eta)))-y*eta))
  diagnostics <- list(converged = TRUE, iterations = fit$iter, gradient_max = error,
       solver_converged = isTRUE(fit$converged),
       negative_log_likelihood = nll, standard_errors = as.list(sqrt(diag(covariance))),
       covariance = lapply(seq_len(2), function(i) as.list(covariance[i, ])),
       uncertainty = "IID_FREQUENCY_FISHER", slope_nonpositive = beta[2] <= 0,
       n_observations = length(data$x), weight_sum = sum(data$w),
       zero_weight_count = sum(data$w == 0), boundary_policy = "reject", solver_warnings = as.list(warnings))
  .cr_cal_artifact("logistic", list(intercept = beta[1], slope = beta[2], boundary_policy = "reject"),
                   meta, data, diagnostics, context)
}

calibrate_isotonic <- function(scores, outcomes, weights = NULL, metadata,
                               increasing = TRUE, out_of_bounds = "error", context = NULL) {
  meta <- .cr_cal_metadata(metadata, TRUE)
  data <- .cr_cal_data(scores, weights, outcomes)
  if (is.null(data$y)) .cr_error("MISSING_INPUT", "Observed outcomes required")
  if (!is.logical(increasing) || length(increasing) != 1L || is.na(increasing) ||
      length(out_of_bounds) != 1L || is.na(out_of_bounds) || !out_of_bounds %in% c("error", "clip"))
    .cr_error("INVALID_INPUT", "Declare monotone direction and error/clip boundary policy")
  keep <- data$w > 0
  ord <- order(data$x[keep], method = "radix")
  xx <- data$x[keep][ord]; ww <- data$w[keep][ord]; yy <- data$y[keep][ord]
  knots <- sort(unique(xx))
  group <- match(xx, knots)
  mass <- as.numeric(rowsum(ww, group, reorder = FALSE))
  events <- as.numeric(rowsum(ww*yy, group, reorder = FALSE))
  storage <- matrix(0, nrow = length(knots), ncol = 4L)
  top <- 0L; direction <- if (increasing) 1 else -1
  for (i in seq_along(knots)) {
    top <- top+1L
    storage[top, ] <- c(i, i, mass[i], events[i])
    while (top > 1L) {
      left <- storage[top-1L, ]; right <- storage[top, ]
      if (direction*left[4]/left[3] <= direction*right[4]/right[3]) break
      top <- top-1L
      storage[top, ] <- c(left[1], right[2], left[3]+right[3], left[4]+right[4])
    }
  }
  blocks <- lapply(seq_len(top), function(i) storage[i, ])
  values <- numeric(length(knots))
  for (block in blocks) values[seq.int(block[1], block[2])] <- block[4]/block[3]
  parameters <- list(knots = as.list(knots), values = as.list(values), increasing = increasing,
                     out_of_bounds = out_of_bounds, interpolation = "left_constant")
  diagnostics <- list(n_observations = length(data$x), weight_sum = sum(data$w),
       zero_weight_count = sum(data$w == 0), n_unique_scores = length(knots), n_blocks = length(blocks),
       blocks = lapply(blocks, function(b) as.list(c(b[1]-1, b[2]-1, b[3], b[4]))),
       weighted_outcome_mean = sum(data$w*data$y)/sum(data$w),
       weighted_fitted_mean = sum(mass*values)/sum(data$w))
  .cr_cal_artifact("isotonic", parameters, meta, data, diagnostics, context)
}

.cr_cal_check <- function(calibrator) {
  if (inherits(calibrator, "CalcResult")) calibrator <- calibrator$value
  if (!.cr_object(calibrator)) .cr_error("INVALID_CALIBRATOR", "Expected CalibrationArtifact")
  if (!identical(calibrator$schema_version, "1.0"))
    .cr_error("UNSUPPORTED_SCHEMA_VERSION", "Unsupported calibration schema")
  if (!identical(calibrator$artifact_type, "CalibrationArtifact") ||
      length(calibrator$calibration_type) != 1L ||
      !calibrator$calibration_type %in% c("intercept", "logistic", "isotonic"))
    .cr_error("INVALID_CALIBRATOR", "Unknown calibration type")
  digest <- calibrator$artifact_hash
  calibrator$artifact_hash <- NULL
  if (!identical(digest, .cr_hash(calibrator))) .cr_error("INTEGRITY_ERROR", "Calibration content hash differs")
  calibrator$artifact_hash <- digest
  kind <- calibrator$calibration_type
  if (!identical(calibrator$method_id, paste0("calibration.calibrate_", kind)) ||
      !identical(calibrator$parameter_version, "1.0") ||
      !identical(calibrator$input_kind, if (kind == "isotonic") "score" else "probability") ||
      !is.character(calibrator$calibration_input_fingerprint) ||
      length(calibrator$calibration_input_fingerprint) != 1L ||
      nchar(calibrator$calibration_input_fingerprint) != 64L)
    .cr_error("INVALID_CALIBRATOR", "Invalid method, parameter version, input kind or fingerprint")
  meta <- .cr_cal_metadata(calibrator$metadata, calibrator$calibration_type != "intercept")
  if (!identical(.cr_hash(calibrator$target), .cr_hash(meta$target)) || !.cr_object(calibrator$parameters))
    .cr_error("INVALID_CALIBRATOR", "Inconsistent target or parameters")
  calibrator
}

.cr_cal_array <- function(x) {
  if (is.list(x) && is.null(names(x))) {
    if (!all(vapply(x, function(v) is.numeric(v) && !is.complex(v) && length(v) == 1L, logical(1))))
      .cr_error("INVALID_CALIBRATOR", "Numeric array required")
    return(as.numeric(unlist(x)))
  }
  x
}

apply_calibrator <- function(calibrator, values, target, base_model_version, context = NULL, groups = NULL) {
  raw <- if (is.list(calibrator) && !is.null(calibrator$value)) calibrator$value else calibrator
  if (is.list(raw) && is.character(raw$calibration_type) && length(raw$calibration_type) == 1L && raw$calibration_type %in% c("beta", "hierarchical", "lgd_level"))
    return(.cr_cm_apply(calibrator, values, target, base_model_version, groups, context))
  artifact <- .cr_cal_check(calibrator)
  if (!identical(.cr_hash(.cr_cal_target(target)), .cr_hash(artifact$target)))
    .cr_error("INCOMPATIBLE_TARGET", "Calibration target, horizon, measure or conditioning differs")
  if (!identical(base_model_version, artifact$metadata$base_model_version))
    .cr_error("INCOMPATIBLE_MODEL_VERSION", "Base model version differs")
  params <- artifact$parameters; kind <- artifact$calibration_type
  if (kind == "isotonic") {
    x <- .cr_numeric(values, "values", allow_empty = TRUE, vector_only = TRUE)
    knots <- .cr_numeric(.cr_cal_array(params$knots), "knots", vector_only = TRUE)
    fitted <- .cr_probability(.cr_cal_array(params$values), vector_only = TRUE)
    if (length(knots) != length(fitted) || any(diff(knots) <= 0)) .cr_error("INVALID_CALIBRATOR", "Invalid knots")
    increasing <- params$increasing
    if (!is.logical(increasing) || length(increasing) != 1L || is.na(increasing) ||
        any((if (increasing) 1 else -1)*diff(fitted) < 0))
      .cr_error("INVALID_CALIBRATOR", "Invalid monotonicity")
    if (length(params$out_of_bounds) != 1L || !params$out_of_bounds %in% c("error", "clip") ||
        !identical(params$interpolation, "left_constant"))
      .cr_error("INVALID_CALIBRATOR", "Invalid interpolation or boundary policy")
    outside <- x < knots[1] | x > tail(knots, 1)
    if (any(outside) && params$out_of_bounds == "error")
      .cr_error("OUT_OF_SUPPORT", "Score outside positive-weight calibration support")
    value <- fitted[pmin(length(knots), pmax(1L, findInterval(x, knots)))]
    trace <- list(out_of_support_count = sum(outside), interpolation = "left_constant")
  } else {
    p <- .cr_probability(values, boundary_policy = if (kind == "intercept") "closed" else "open", vector_only = TRUE)
    a <- .cr_numeric(params$intercept, "intercept"); b <- .cr_numeric(params$slope, "slope")
    if (length(a) != 1L || length(b) != 1L || (kind == "intercept" && b != 1))
      .cr_error("INVALID_CALIBRATOR", "Invalid scalar coefficients")
    if (!identical(params$boundary_policy, if (kind == "intercept") "preserve" else "reject"))
      .cr_error("INVALID_CALIBRATOR", "Invalid boundary policy")
    value <- p
    interior <- p > 0 & p < 1
    value[interior] <- stats::plogis(a+b*stats::qlogis(p[interior]))
    trace <- list(intercept = a, slope = b, boundary_policy = params$boundary_policy)
  }
  trace$calibration_hash <- artifact$artifact_hash; trace$calibration_type <- kind
  .cr_result(value, "calibration.apply_calibrator", "probability", .cr_cal_context(context, artifact$target),
      diagnostics = trace, provenance = list(parameter_origin = "ESTIMATED"),
      limitations = "Frozen calibration does not establish performance on new observations.")
}
