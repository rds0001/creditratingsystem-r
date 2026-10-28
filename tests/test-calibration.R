library(creditratingsystem)

# Independent exact binomial/count and hand-PAVA references.
near_cal <- function(actual, expected, tol = 2e-9) {
  stopifnot(length(actual) == length(expected), all(abs(actual-expected) <= tol))
}
error_cal <- function(expr, code) {
  caught <- tryCatch(force(expr), error = identity)
  stopifnot(inherits(caught, "creditratingsystem_error"), identical(caught$code, code))
}
metadata_cal <- list(
  target = list(event = "first_default", horizon = 12, time_unit = "months",
                probability_measure = "PHYSICAL", conditioning = "PIT"),
  calibration = list(start = "2020-01-01", end = "2021-12-31", known_at = "2022-01-01",
                     split_id = "calibration-only", prediction_origin = "HOLDOUT"),
  base_model_version = "synthetic-score-v1", population_definition = "Synthetic examples",
  outcome_policy = "Fully observed first-default outcomes", weight_type = "frequency", data_origin = "SYNTHETIC")
apply_cal <- function(fit, x) apply_calibrator(fit, x, metadata_cal$target, metadata_cal$base_model_version)

# Odds 1/9 doubled -> 2/11; shift=log(2), independent of implementation.
offset_cal <- calibrate_intercept(c(.1, .1), 2/11, metadata = metadata_cal)
near_cal(offset_cal$value$parameters$intercept, log(2), 2e-12)
near_cal(apply_cal(offset_cal, c(.1, .2, .5))$value, c(2/11, 1/3, 2/3), 2e-13)
stopifnot(abs(offset_cal$diagnostics$residual) <= 1e-12)
boundary_cal <- calibrate_intercept(c(0, .2, 1), .5, c(1, 2, 1), metadata = metadata_cal)
near_cal(boundary_cal$value$parameters$intercept, log(4), 2e-12)
near_cal(apply_cal(boundary_cal, c(1, .2, 0))$value, c(1, .5, 0), 2e-13)
for (target_rate in c(0, .25, .75, 1))
  error_cal(calibrate_intercept(c(0, .2, 1), target_rate, c(1, 2, 1), metadata = metadata_cal), "INFEASIBLE_TARGET")
error_cal(calibrate_intercept(c(0, 1), .5, metadata = metadata_cal), "NOT_IDENTIFIABLE")
identity_cal <- calibrate_intercept(c(.25, .75), .5, metadata = metadata_cal)
stopifnot(identity_cal$value$parameters$intercept == 0)

# Group MLE rates 1/4,3/4 at logits +/-log(4); information diag(1.5,1.5*log(4)^2).
logistic_cal <- calibrate_logistic(c(.2, .2, .8, .8), c(0, 1, 0, 1), c(3, 1, 1, 3), metadata = metadata_cal)
near_cal(unlist(logistic_cal$value$parameters[c("intercept", "slope")]), c(0, log(3)/log(4)))
near_cal(apply_cal(logistic_cal, c(.8, .2, .5))$value, c(.75, .25, .5))
near_cal(unlist(logistic_cal$diagnostics$covariance), c(2/3, 0, 0, 2/(3*log(4)^2)))
near_cal(logistic_cal$diagnostics$negative_log_likelihood, -2*(3*log(.75)+log(.25)))
stopifnot(!logistic_cal$diagnostics$slope_nonpositive)
inverse_cal <- calibrate_logistic(c(.2, .2, .8, .8), c(0, 1, 0, 1), c(1, 3, 3, 1), metadata = metadata_cal)
near_cal(inverse_cal$value$parameters$slope, -log(3)/log(4))
stopifnot(inverse_cal$diagnostics$slope_nonpositive)
flat_cal <- calibrate_logistic(c(.2, .2, .8, .8), c(0, 1, 0, 1), metadata = metadata_cal)
near_cal(flat_cal$value$parameters$slope, 0)
error_cal(calibrate_logistic(c(.1, .2, .8, .9), c(0, 0, 1, 1), metadata = metadata_cal), "SEPARATION")
error_cal(calibrate_logistic(c(.1, .5, .5, .9), c(0, 0, 1, 1), metadata = metadata_cal), "SEPARATION")
error_cal(calibrate_logistic(c(.1, .2), c(1, 1), metadata = metadata_cal), "SEPARATION")
error_cal(calibrate_logistic(c(.5, .5), c(0, 1), metadata = metadata_cal), "NOT_IDENTIFIABLE")
error_cal(calibrate_logistic(c(0, .2, .8, 1), c(0, 1, 0, 1), metadata = metadata_cal), "INVALID_PROBABILITY")
error_cal(calibrate_logistic(c(.2, .2, .8, .8), c(0, 1, 0, 1), c(3, 1, 1, 3), metadata = metadata_cal, max_iterations = 1), "NON_CONVERGENCE")

# Pool scores 2,3: event total=2, frequency=3; left-constant application.
isotonic_cal <- calibrate_isotonic(c(1, 2, 3, 4), c(0, 1, 0, 1), c(1, 2, 1, 1), metadata = metadata_cal)
near_cal(unlist(isotonic_cal$value$parameters$values), c(0, 2/3, 2/3, 1))
near_cal(apply_cal(isotonic_cal, c(4, 2.5, 1, 3))$value, c(1, 2/3, 0, 2/3))
near_cal(isotonic_cal$diagnostics$weighted_fitted_mean, 3/5)
tied_cal <- calibrate_isotonic(c(3, 1, 2, 1), c(1, 0, 0, 1), c(1, 1, 1, 3), metadata = metadata_cal)
near_cal(unlist(tied_cal$value$parameters$values), c(.6, .6, 1))
near_cal(apply_cal(tied_cal, c(1, 3, 1, 2))$value, c(.6, 1, .6, .6))
decreasing_cal <- calibrate_isotonic(1:4, c(1, 0, 1, 0), metadata = metadata_cal, increasing = FALSE, out_of_bounds = "clip")
near_cal(apply_cal(decreasing_cal, c(0, 2, 3, 5))$value, c(1, .5, .5, 0))
stopifnot(apply_cal(decreasing_cal, c(0, 5))$diagnostics$out_of_support_count == 2)
error_cal(apply_cal(isotonic_cal, c(5)), "OUT_OF_SUPPORT")
zero_weight_cal <- calibrate_isotonic(c(-100, 1, 2, 100), c(1, 0, 1, 0), c(0, 1, 1, 0), metadata = metadata_cal)
near_cal(unlist(zero_weight_cal$value$parameters$knots), c(1, 2))
stopifnot(zero_weight_cal$diagnostics$zero_weight_count == 2)
error_cal(apply_cal(zero_weight_cal, 100), "OUT_OF_SUPPORT")
for (weights_bad in list(c(0, 0), c(-1, 2), c(.5, 1.5)))
  error_cal(calibrate_isotonic(c(0, 1), c(0, 1), weights_bad, metadata = metadata_cal), "INVALID_WEIGHTS")
error_cal(calibrate_isotonic(c(0, 1), c(0, 1), 1, metadata = metadata_cal), "INCOMPATIBLE_SHAPE")
error_cal(calibrate_isotonic(numeric(), numeric(), metadata = metadata_cal), "EMPTY_INPUT")
error_cal(calibrate_isotonic(c(0, 1), c(0, .5), metadata = metadata_cal), "INVALID_OUTCOME")
error_cal(calibrate_isotonic(c(0, 1), NULL, metadata = metadata_cal), "MISSING_INPUT")
error_cal(calibrate_isotonic(c(0, NA_real_), c(0, 1), metadata = metadata_cal), "MISSING_INPUT")
error_cal(calibrate_isotonic(c(0, NaN), c(0, 1), metadata = metadata_cal), "NON_FINITE_INPUT")
for (field in c("target", "calibration", "base_model_version", "population_definition", "outcome_policy", "data_origin")) {
  missing_cal <- metadata_cal; missing_cal[[field]] <- NULL
  error_cal(calibrate_intercept(.2, .3, metadata = missing_cal), "INVALID_CALIBRATION_METADATA")
}
invalid_cal <- metadata_cal; invalid_cal$calibration$prediction_origin <- "TRAINING"
error_cal(calibrate_isotonic(c(0, 1), c(0, 1), metadata = invalid_cal), "INVALID_CALIBRATION_METADATA")
different_target_cal <- metadata_cal$target; different_target_cal$horizon <- 24
error_cal(apply_calibrator(offset_cal, .2, different_target_cal, metadata_cal$base_model_version), "INCOMPATIBLE_TARGET")
error_cal(apply_calibrator(offset_cal, .2, metadata_cal$target, "new-version"), "INCOMPATIBLE_MODEL_VERSION")
for (fit_cal in list(offset_cal, logistic_cal, isotonic_cal)) {
  # Public model bundle serialization retains full binary64 precision in JSON.
  empty_cal <- structure(list(), names = character())
  bundle_cal <- list(schema_version = "1.0", model_id = "synthetic-model", model_version = "1",
       method_id = "calibration_bundle_example", feature_order = list("base_pd"),
       parameters = list(identity = TRUE), preprocessing = empty_cal, target = metadata_cal$target,
       training = list(data_fingerprint = "synthetic-training", split_id = "train", known_at = "2020-01-01"),
       diagnostics = empty_cal, provenance = list(data_origin = "SYNTHETIC", parameter_origin = "USER_SUPPLIED"),
       calibration_artifact = fit_cal$value)
  bundle_path_cal <- tempfile("calibration-bundle-", tmpdir = tempdir(), fileext = ".json")
  serialize_fitted_model(bundle_cal, bundle_path_cal)
  replay_cal <- load_fitted_model(bundle_path_cal)$value$calibration_artifact
  unlink(bundle_path_cal)
  input_cal <- if (fit_cal$value$calibration_type == "isotonic") c(1, 2) else c(.2, .8)
  near_cal(apply_cal(fit_cal, input_cal)$value, apply_cal(replay_cal, input_cal)$value, 0)
  stopifnot(length(apply_cal(fit_cal, numeric())$value) == 0,
            fit_cal$provenance$data_origin == "SYNTHETIC",
            fit_cal$provenance$validation_status == "NOT_VALIDATED",
            nchar(fit_cal$value$calibration_input_fingerprint) == 64)
  replay_cal$parameter_version <- "tampered"
  error_cal(apply_cal(replay_cal, input_cal), "INTEGRITY_ERROR")
}
bad_context_cal <- build_calculation_context(horizon = 24, time_unit = "months")
error_cal(apply_calibrator(offset_cal, .2, metadata_cal$target, metadata_cal$base_model_version,
                           context = bad_context_cal), "INCOMPATIBLE_TARGET")
cat("Native calibration analytical, boundary, provenance and replay checks passed\n")

# A correct content hash cannot make an invalid monotone artifact admissible.
invalid_artifact_cal <- isotonic_cal$value
invalid_artifact_cal$parameters$values <- list(1, 0, 0, 1)
invalid_artifact_cal$artifact_hash <- NULL
hash_cal <- getFromNamespace(".cr_hash", "creditratingsystem")
invalid_artifact_cal$artifact_hash <- hash_cal(invalid_artifact_cal)
error_cal(apply_cal(invalid_artifact_cal, 2), "INVALID_CALIBRATOR")
