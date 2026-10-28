.cr_physical_horizon <- function(horizon, time_unit, probability_measure, context) {
  .cr_numeric(horizon, "horizon", vector_only = TRUE)
  if (length(horizon) != 1L || horizon <= 0 || length(time_unit) != 1L || is.na(time_unit) || !time_unit %in% c("days", "months", "years"))
    .cr_error("INCOMPATIBLE_HORIZON", "Positive common horizon and explicit time unit required")
  if (!identical(probability_measure, "PHYSICAL")) .cr_error("INCOMPATIBLE_MEASURE", "PHYSICAL probability measure required")
  ctx <- .cr_result(NULL, "context", context = context)$context
  metadata <- list(horizon = horizon, time_unit = time_unit, probability_measure = "PHYSICAL")
  for (key in names(metadata)) if (!is.null(ctx[[key]]) && !identical(.cr_hash(ctx[[key]]), .cr_hash(metadata[[key]])))
    .cr_error("TARGET_MISMATCH", paste("Context", key, "conflicts with method contract"))
  list(context = ctx, metadata = metadata)
}

pit_ttc_transform <- function(pd, systematic_factor, asset_correlation, direction,
    factor_distribution, horizon, time_unit, probability_measure, context = NULL) {
  if (!is.character(direction) || length(direction) != 1L || is.na(direction) || !direction %in% c("ttc_to_pit", "pit_to_ttc"))
    .cr_error("INVALID_INPUT", "direction must be ttc_to_pit or pit_to_ttc")
  if (!.cr_object(factor_distribution) || !identical(factor_distribution$family, "standard_normal"))
    .cr_error("UNSUPPORTED_FACTOR_DISTRIBUTION", "Explicit standard_normal factor distribution required")
  for (key in c("factor_id", "reference_period", "model_version")) .cr_string(factor_distribution[[key]], key, "INVALID_FACTOR_SPEC")
  info <- .cr_physical_horizon(horizon, time_unit, probability_measure, context)
  p <- .cr_probability(pd, "pd", allow_empty = FALSE, vector_only = TRUE)
  z <- .cr_numeric(systematic_factor, "systematic_factor", vector_only = TRUE)
  rho <- .cr_numeric(asset_correlation, "asset_correlation", vector_only = TRUE)
  sizes <- c(length(p), length(z), length(rho)); n <- max(sizes)
  if (any(!sizes %in% c(1L, n))) .cr_error("INCOMPATIBLE_SHAPE", "Only scalar expansion or equal lengths supported")
  p <- rep_len(p, n); z <- rep_len(z, n); rho <- rep_len(rho, n)
  if (any(rho < 0 | rho >= 1)) .cr_error("INVALID_CORRELATION", "Asset correlation must lie in [0,1)")
  output <- p
  active <- rho > 0 & p > 0 & p < 1
  q <- stats::qnorm(p[active])
  latent <- if (direction == "ttc_to_pit") (q - sqrt(rho[active])*z[active])/sqrt(1-rho[active]) else
    sqrt(1-rho[active])*q + sqrt(rho[active])*z[active]
  output[active] <- stats::pnorm(latent)
  boundary <- active & (output == 0 | output == 1)
  source <- if (direction == "ttc_to_pit") "TTC" else "PIT"
  destination <- if (direction == "ttc_to_pit") "PIT" else "TTC"
  diagnostics <- c(info$metadata, list(direction = direction, source_philosophy = source,
    destination_philosophy = destination, factor_distribution = factor_distribution,
    systematic_factor = as.list(z), asset_correlation = as.list(rho), factor_sign = "POSITIVE_IS_GOOD",
    input_factor_conditioning = if (source == "TTC") "integrated_standard_normal" else "given_factor",
    output_factor_conditioning = if (destination == "TTC") "integrated_standard_normal" else "given_factor",
    numerical_boundary_count = sum(boundary), inverse_information_loss = any(boundary), zero_factor_is_ttc = FALSE))
  .cr_result(output, "pd_term_structure.pit_ttc_transform", "probability", info$context, diagnostics,
    limitations = c("Conversion depends on the declared Gaussian latent-factor model and shared horizon; factor and correlation suitability are not estimated here.",
                    "Finite-precision output at zero or one cannot recover an interior input through the inverse mapping."))
}
