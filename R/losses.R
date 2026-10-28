#' Expected default loss with explicit scalar expansion.
expected_loss <- function(pd, lgd, ead, context = NULL) {
  pd <- .cr_probability(pd, "pd", allow_empty = FALSE, vector_only = TRUE)
  lgd <- .cr_numeric(lgd, "lgd", vector_only = TRUE)
  ead <- .cr_numeric(ead, "ead", vector_only = TRUE)
  if (any(ead < 0)) .cr_error("INVALID_EAD", "ead must be nonnegative")
  sizes <- c(length(pd), length(lgd), length(ead))
  n <- max(sizes)
  if (any(sizes != 1L & sizes != n))
    .cr_error("INCOMPATIBLE_SHAPE", "Inputs must have equal length or length one")
  factors <- cbind(rep_len(pd, n), rep_len(lgd, n), rep_len(ead, n))
  value <- numeric(n)
  active <- rowSums(factors == 0) == 0
  if (any(active)) {
    a <- abs(factors[active, , drop = FALSE])
    exponent <- pmin(floor(log2(a)), 1023)
    mantissa <- a / 2^exponent
    m <- apply(mantissa, 1L, prod)
    e <- rowSums(exponent)
    adjustment <- floor(log2(m))
    m <- m / 2^adjustment
    e <- e + adjustment
    answer <- numeric(length(m))
    normal <- e >= -1022
    answer[normal] <- m[normal] * 2^e[normal]
    answer[!normal] <- (m[!normal] * 2^-1022) * 2^(e[!normal] + 1022)
    value[active] <- answer * sign(factors[active, 2L])
  }
  if (any(!is.finite(value))) .cr_error("NUMERICAL_FAILURE", "Expected loss overflowed")
  ctx <- if (inherits(context, "CalcResult")) context$value else context
  unit <- if (is.list(ctx) && !is.null(ctx$currency)) ctx$currency else "ead_unit"
  .cr_result(value, "losses.expected_loss", unit, context,
             limitations = "Product requires deterministic or jointly specified conditional-on-default LGD and EAD; separate marginal means may not suffice.")
}
