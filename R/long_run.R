# Aggregate observed annual rates under explicit weighting and uncertainty assumptions.
estimate_long_run_pd <- function(cohorts, selected_cohort_ids, knowledge_date, weighting,
    uncertainty, dependence_assumption, representativeness, target, confidence_level = .95, context = NULL) {
  if (!.cr_object(target)) .cr_error("INVALID_TARGET", "Explicit target required")
  for (key in c("event", "default_definition", "conditioning")) .cr_string(target[[key]], key, "INVALID_TARGET")
  info <- .cr_physical_horizon(target$horizon, target$time_unit, target$probability_measure, context)
  if (target$horizon != 1 || target$time_unit != "years") .cr_error("INCOMPATIBLE_HORIZON", "One-year cohorts required")
  cutoff <- .cr_date(knowledge_date, "knowledge_date")
  if (!is.character(weighting) || length(weighting) != 1L || is.na(weighting) || !weighting %in% c("annual_mean", "pooled_obligor") ||
      !is.character(uncertainty) || length(uncertainty) != 1L || is.na(uncertainty) || !uncertainty %in% c("none", "year_t", "clopper_pearson")) .cr_error("INVALID_INPUT", "Unsupported weighting or uncertainty")
  if (!is.character(dependence_assumption) || length(dependence_assumption) != 1L || is.na(dependence_assumption) || !dependence_assumption %in% c("unspecified", "independent_years", "independent_homogeneous_obligors")) .cr_error("INVALID_INPUT", "Supported dependence assumption required")
  .cr_numeric(confidence_level, "confidence_level", vector_only = TRUE)
  if (length(confidence_level) != 1L || confidence_level <= 0 || confidence_level >= 1) .cr_error("INVALID_INPUT", "confidence_level must lie in (0,1)")
  if (!.cr_object(representativeness)) .cr_error("INVALID_REPRESENTATIVENESS", "Explicit assessment required")
  assessment <- representativeness$assessment
  if (!is.character(assessment) || length(assessment) != 1L || is.na(assessment) || !assessment %in% c("representative", "not_representative", "unknown")) .cr_error("INVALID_REPRESENTATIVENESS", "Explicit assessment required")
  for (key in c("rationale", "reference_period")) .cr_string(representativeness[[key]], key, "INVALID_REPRESENTATIVENESS")
  if (is.data.frame(cohorts)) cohorts <- lapply(seq_len(nrow(cohorts)), function(i) as.list(cohorts[i, , drop = FALSE]))
  if (!is.list(cohorts) || !length(cohorts)) .cr_error("EMPTY_INPUT", "Nonempty cohort records required")
  if (is.list(selected_cohort_ids)) selected_cohort_ids <- unlist(selected_cohort_ids)
  ids <- selected_cohort_ids
  if (!is.character(ids) || !length(ids) || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) .cr_error("INVALID_COHORT_SELECTION", "Select unique cohort identifiers")
  registry <- character()
  for (row in cohorts) {
    if (!.cr_object(row)) .cr_error("INVALID_COHORT_SELECTION", "Cohort object required")
    .cr_string(row$cohort_id, "cohort_id", "INVALID_COHORT_SELECTION")
    registry <- c(registry, row$cohort_id)
  }
  if (anyDuplicated(registry) || any(!ids %in% registry)) .cr_error("INVALID_COHORT_SELECTION", "Duplicate or unknown cohort")
  rows <- cohorts[match(ids, registry)]
  starts <- ends <- character(); counts <- totals <- numeric()
  for (row in rows) {
    start <- .cr_date(row$period_start, "period_start"); end <- .cr_date(row$period_end, "period_end"); known <- .cr_date(row$known_at, "known_at")
    year <- as.integer(substr(start, 1, 4)) + 1L
    if (year > 9999) .cr_error("INVALID_DATE", "Anniversary outside supported calendar")
    anniversary <- sprintf("%04d%s", year, substr(start, 5, 10))
    if (substr(start, 6, 10) == "02-29") anniversary <- sprintf("%04d-02-28", year)
    if (end != anniversary) .cr_error("INCOMPATIBLE_HORIZON", "Each cohort must cover one calendar year")
    if (!identical(row$mature, TRUE) || end > cutoff || known > cutoff || known < end) .cr_error("IMMATURE_COHORT", "Selected cohorts must be mature and available")
    if (!identical(row$default_definition, target$default_definition)) .cr_error("TARGET_MISMATCH", "Cohort definition differs from target")
    d <- .cr_numeric(row$defaults, "defaults", vector_only = TRUE); n <- .cr_numeric(row$denominator, "denominator", vector_only = TRUE)
    if (length(d) != 1L || length(n) != 1L || d < 0 || n <= 0 || d > n || d != floor(d) || n != floor(n) || n > 2^53-1) .cr_error("INVALID_COUNTS", "Invalid integer counts")
    starts <- c(starts, start); ends <- c(ends, end); counts <- c(counts, d); totals <- c(totals, n)
  }
  rates <- counts/totals; pooled_n <- sum(totals); pooled_d <- sum(counts)
  if (pooled_n > 2^53-1) .cr_error("INVALID_COUNTS", "Total denominator exceeds exact integer range")
  estimate <- if (weighting == "annual_mean") mean(rates) else pooled_d/pooled_n
  ord <- order(starts, ends); overlap <- if (length(ord) < 2L) FALSE else any(starts[ord][-1L] < ends[ord][-length(ord)])
  lower <- upper <- NULL
  if (uncertainty == "year_t") {
    if (weighting != "annual_mean" || dependence_assumption != "independent_years" || overlap || length(rates) < 2L) .cr_error("INCOMPATIBLE_UNCERTAINTY", "Year-t needs annual mean and independent nonoverlapping years")
    delta <- stats::qt((1+confidence_level)/2, length(rates)-1)*stats::sd(rates)/sqrt(length(rates))
    lower <- max(0, estimate-delta); upper <- min(1, estimate+delta)
  }
  if (uncertainty == "clopper_pearson") {
    if (weighting != "pooled_obligor" || dependence_assumption != "independent_homogeneous_obligors" || overlap) .cr_error("INCOMPATIBLE_UNCERTAINTY", "Exact interval needs pooled independent homogeneous obligors")
    alpha <- (1-confidence_level)/2
    lower <- if (pooled_d == 0) 0 else stats::qbeta(alpha, pooled_d, pooled_n-pooled_d+1)
    upper <- if (pooled_d == pooled_n) 1 else stats::qbeta(1-alpha, pooled_d+1, pooled_n-pooled_d)
  }
  .cr_result(list(pd = estimate, lower = lower, upper = upper), "calibration.estimate_long_run_pd", "probability", info$context,
    diagnostics = list(selected_cohort_ids = as.list(ids), cohort_rates = as.list(rates), defaults = pooled_d, denominator = pooled_n,
      weighting = weighting, uncertainty = uncertainty, confidence_level = confidence_level, dependence_assumption = dependence_assumption,
      overlapping_periods = overlap, knowledge_date = knowledge_date, target = target, representativeness = representativeness,
      representativeness_verified = FALSE, automatically_ttc = FALSE),
    limitations = c("Representativeness and independence are user assertions, not verified from aggregate counts.",
      "Year-t intervals are approximate and intersected with [0,1]; pooled binomial intervals require homogeneous independent observations.",
      "Observed long-run rates are not automatically TTC probabilities; selection, missing defaults, repeated obligors and censoring require upstream assessment."))
}
