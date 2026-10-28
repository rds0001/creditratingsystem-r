library(creditratingsystem)

equal <- function(actual, expected, tolerance = 1e-12) {
  stopifnot(isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE)))
}
error_code <- function(expr, code) {
  err <- tryCatch(force(expr), creditratingsystem_error = identity)
  stopifnot(inherits(err, "creditratingsystem_error"), identical(err$code, code))
}

# Independent direct arithmetic, including uncapped economic recovery cases.
equal(expected_loss(0.02, 0.4, 100000)$value, 800)
equal(expected_loss(c(0, 0.1, 1), 0.5, 200)$value, c(0, 10, 100))
equal(expected_loss(1, c(-0.2, 1.5), 100)$value, c(-20, 150))
error_code(expected_loss(0.2, 0.4, -1), "INVALID_EAD")
error_code(expected_loss(c(0.1, 0.2), c(1, 1, 1), 100), "INCOMPATIBLE_SHAPE")
error_code(expected_loss(numeric(), 0.4, 1), "EMPTY_INPUT")
error_code(expected_loss(matrix(0.2), 0.4, 1), "INVALID_INPUT")
error_code(expected_loss(1, 1e308, 1e308), "NUMERICAL_FAILURE")

# Survive 90% of period one; 20% of survivors default in period two.
equal(cumulative_pd_from_conditional(c(0.1, 0.2))$value, c(0.1, 0.28))
equal(marginal_pd_from_conditional(c(0.1, 0.2))$value, c(0.1, 0.18))
equal(cumulative_pd_from_conditional(c(0, 1, 0.3))$value, c(0, 1, 1))
equal(marginal_pd_from_conditional(c(0, 1, 0.3))$value, c(0, 1, 0))
equal(cumulative_pd_from_conditional(numeric())$value, numeric())
equal(marginal_pd_from_conditional(numeric())$value, numeric())
# Exact polynomial q + q - q*q; relative error is asserted separately.
tiny <- 1e-20
stopifnot(abs(cumulative_pd_from_conditional(c(tiny, tiny))$value[2] /
              (2 * tiny - tiny * tiny) - 1) < 1e-14)
equal(validate_probability(matrix(c(0, 1), nrow = 1))$value,
      matrix(c(0, 1), nrow = 1))
for (bad in list(TRUE, "0.2", 0.2 + 1i)) error_code(validate_probability(bad), "INVALID_INPUT")
error_code(validate_probability(NA_real_), "MISSING_INPUT")
error_code(validate_probability(NaN), "NON_FINITE_INPUT")
error_code(validate_probability(Inf), "NON_FINITE_INPUT")
error_code(validate_probability(2), "INVALID_PROBABILITY")
error_code(validate_probability(0, "open"), "INVALID_PROBABILITY")
stopifnot(inherits(expected_loss(0.2, 0.3, 10), "CalcResult"))

scale <- list(schema_version = "1.0", scale_id = "test", version = "1",
  boundary_convention = "left_closed", grades = list(
    list(grade = "A", lower = 0, upper = 0.1, representative_pd = 0.02, is_default = FALSE),
    list(grade = "B", lower = 0.1, upper = 1, representative_pd = 0.5, is_default = FALSE),
    list(grade = "D", lower = 1, upper = 1, representative_pd = 1, is_default = TRUE)))
stopifnot(identical(map_pd_to_grade(c(0, 0.1, 0.99, 1), scale)$value, c("A", "B", "B", "D")))
right <- scale; right$boundary_convention <- "right_closed"
stopifnot(identical(map_pd_to_grade(c(0, 0.1, 1), validate_rating_scale(right))$value, c("A", "A", "D")))
error_code(validate_rating_scale(right, "left_closed"), "INVALID_RATING_SCALE")
bad <- scale; bad$grades[[2]]$lower <- 0.11
error_code(validate_rating_scale(bad), "INVALID_RATING_SCALE")
bad <- scale; bad$grades[[2]]$grade <- "A"
error_code(validate_rating_scale(bad), "INVALID_RATING_SCALE")
bad <- scale; bad$grades[[1]]$representative_pd <- 0.1
error_code(validate_rating_scale(bad), "INVALID_RATING_SCALE")

ctx <- build_calculation_context("2025-12-31", "2026-01-15", currency = "EUR",
  data_manifest = list(data_origin = "SYNTHETIC", generator = "analytic"))
stopifnot(expected_loss(0.2, 0.3, 100, ctx)$unit == "EUR",
          expected_loss(0.2, 0.3, 100, ctx)$provenance$data_origin == "SYNTHETIC")
a <- build_calculation_context(data_manifest = list(a = 1, b = list(TRUE, NULL)))$value$context_hash
b <- build_calculation_context(data_manifest = list(b = list(TRUE, NULL), a = 1))$value$context_hash
stopifnot(identical(a, b), !identical(a, build_calculation_context(data_manifest = list(a = 2, b = list(TRUE, NULL)))$value$context_hash))
error_code(build_calculation_context("2025-02-29", "2025-03-01"), "INVALID_DATE")
error_code(build_calculation_context("2024-02-29"), "MISSING_INPUT")
error_code(build_calculation_context(horizon = 1), "MISSING_INPUT")

empty_object <- structure(list(), names = character())
model <- list(schema_version = "1.0", model_id = "example", model_version = "1",
  method_id = "classification.logistic", feature_order = c("x"),
  parameters = list(intercept = -1, coefficients = list(0.5)), preprocessing = empty_object,
  target = list(event = "default", horizon = 1, time_unit = "years", probability_measure = "PHYSICAL", conditioning = "unconditional"),
  training = list(data_fingerprint = "analytic", split_id = "train", known_at = "2025-12-31"),
  diagnostics = empty_object, provenance = list(data_origin = "SYNTHETIC", parameter_origin = "SYNTHETIC_REFERENCE"))
f <- tempfile("model-", fileext = ".json")
serialize_fitted_model(model, f)
loaded <- load_fitted_model(f)$value
stopifnot(identical(loaded$feature_order, list("x")), loaded$parameters$intercept == -1)
error_code(serialize_fitted_model(model, f), "FILE_EXISTS")
error_code(load_fitted_model("https://example.invalid/model"), "INVALID_PATH")
text <- readLines(f)
writeLines(sub('"intercept":-1.0', '"intercept":-2.0', text, fixed = TRUE), f)
error_code(load_fitted_model(f), "INTEGRITY_ERROR")
writeLines('{"format":"creditratingsystem-model","format":"duplicate"}', f)
error_code(load_fitted_model(f), "INVALID_JSON")
unlink(f)

# Binary scaling must retain representable products despite tiny intermediates.
stopifnot(abs(expected_loss(1e-308, 1e-308, 1e308)$value / 1e-308 - 1) < 2e-14)
stopifnot(all(abs(expected_loss(1e-308, c(-1e-308, -1e308),
  c(1e308, 1e-308))$value / -1e-308 - 1) < 2e-14))
stopifnot(abs(expected_loss(1e-308, 1e308, 1e308)$value / 1e308 - 1) < 2e-14)
smallest <- .Machine$double.xmin * .Machine$double.eps
stopifnot(expected_loss(1, 1, smallest)$value == smallest,
          expected_loss(0.5, 1, smallest)$value == 0,
          expected_loss(0.75, 1, smallest)$value == smallest)

# Known adjacent double and scale extremes must survive the JSON wire exactly.
model$parameters$coefficients <- as.list(c(1 + .Machine$double.eps, 1e100, 1e-200, smallest))
f <- tempfile("precision-model-", fileext = ".json")
serialize_fitted_model(model, f)
stopifnot(identical(load_fitted_model(f)$value$parameters$coefficients,
                    model$parameters$coefficients))
writeLines('{"integer":9007199254740992}', f)
error_code(load_fitted_model(f), "INVALID_JSON")
unlink(f)
