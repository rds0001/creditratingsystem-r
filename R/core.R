.cr_error <- function(code, message, details = list()) {
  stop(structure(list(message = message, call = NULL, code = code,
                      details = details),
                 class = c(paste0("creditratingsystem_", code),
                           "creditratingsystem_error", "error", "condition")))
}

.cr_fit_result <- function(model, method_id, diagnostics = list(), context = NULL) {
  result <- .cr_result(list(model = model), method_id, "fitted_model", context,
                       diagnostics = diagnostics, provenance = model$provenance)
  result$model <- result$value$model
  class(result) <- c("FitResult", "CalcResult")
  result
}

.cr_numeric <- function(x, name, allow_empty = FALSE, vector_only = FALSE) {
  if (is.null(x)) .cr_error("MISSING_INPUT", paste(name, "is required"))
  if (!(is.integer(x) || is.double(x)) || is.object(x))
    .cr_error("INVALID_INPUT", paste(name, "must contain real numeric values"))
  if (vector_only && !is.null(dim(x)))
    .cr_error("INVALID_INPUT", paste(name, "must be one-dimensional"))
  if (!allow_empty && length(x) == 0L)
    .cr_error("EMPTY_INPUT", paste(name, "must not be empty"))
  if (any(is.na(x) & !is.nan(x)))
    .cr_error("MISSING_INPUT", paste(name, "contains missing values"))
  if (any(!is.finite(x)))
    .cr_error("NON_FINITE_INPUT", paste(name, "must contain only finite values"))
  x
}

.cr_probability <- function(x, name = "value", boundary_policy = "closed",
                            allow_empty = TRUE, vector_only = FALSE) {
  if (length(boundary_policy) != 1L || is.na(boundary_policy) ||
      !boundary_policy %in% c("closed", "open"))
    .cr_error("INVALID_INPUT", "boundary_policy must be 'closed' or 'open'")
  x <- .cr_numeric(x, name, allow_empty, vector_only)
  bad <- if (boundary_policy == "closed") x < 0 | x > 1 else x <= 0 | x >= 1
  if (any(bad)) .cr_error("INVALID_PROBABILITY", paste(name, "is outside the probability domain"),
                          list(indices = which(bad)))
  x
}

.cr_result <- function(value, method_id, unit = "dimensionless", context = NULL,
                       diagnostics = list(), limitations = character(), provenance = list()) {
  if (inherits(context, "CalcResult")) {
    if (!identical(context$method_id, "core.build_calculation_context"))
      .cr_error("INVALID_CONTEXT", "Expected a context result")
    context <- context$value
  }
  if (is.null(context)) context <- list(mode = "TIME_INDEPENDENT")
  if (!is.list(context) || is.null(names(context)))
    .cr_error("INVALID_CONTEXT", "context must be a named list or context result")
  origin <- function(key) {
    if (is.list(context$data_manifest) && !is.null(context$data_manifest[[key]]))
      context$data_manifest[[key]] else "USER_SUPPLIED"
  }
  provenance <- utils::modifyList(list(data_origin = origin("data_origin"),
       parameter_origin = origin("parameter_origin"), validation_status = "NOT_VALIDATED", method_version = "1.0"), provenance)
  structure(list(value = value, unit = unit, method_id = method_id,
                 context = context, status = "OK", provenance = provenance,
                 diagnostics = diagnostics, limitations = limitations), class = "CalcResult")
}

#' Validate finite probabilities without clipping.
validate_probability <- function(value, boundary_policy = "closed", context = NULL) {
  value <- .cr_probability(value, boundary_policy = boundary_policy)
  .cr_result(value, "core.validate_probability", "probability", context,
             diagnostics = list(boundary_policy = boundary_policy, violations = list()))
}

print.CalcResult <- function(x, ...) {
  cat("CalcResult:", x$method_id, "[", x$unit, "]", x$status, "\n")
  print(x$value, ...)
  invisible(x)
}

.cr_object <- function(x) is.list(x) && !is.null(names(x))
.cr_empty_object <- function() structure(list(), names = character())
.cr_string <- function(x, name, code = "INVALID_INPUT") {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
    .cr_error(code, paste(name, "must be a nonempty string"))
  x
}
.cr_date <- function(x, name, code = "INVALID_DATE") {
  .cr_string(x, name, code)
  d <- tryCatch(as.Date(x, format = "%Y-%m-%d"), error = function(e) NA)
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x) || is.na(d) || format(d, "%Y-%m-%d") != x)
    .cr_error(code, paste(name, "must be a valid ISO calendar date"))
  x
}

# Canonical encoding is independent of JSON whitespace and decimal formatting.
.cr_canonical <- function(x) {
  enc <- function(s) charToRaw(enc2utf8(s))
  if (is.null(x)) return(enc("n"))
  if (is.object(x)) .cr_error("INVALID_JSON", "Canonical values must be plain JSON types")
  if (is.list(x)) {
    if (.cr_object(x)) {
      keys <- names(x)
      if (anyNA(keys) || anyDuplicated(keys) || any(!grepl("^[ -~]+$", keys)))
        .cr_error("INVALID_JSON", "Object keys must be unique printable ASCII strings")
      ord <- order(keys, method = "radix")
      parts <- lapply(ord, function(i) c(.cr_canonical(keys[i]), .cr_canonical(x[[i]])))
      return(c(enc(paste0("o", length(x), ":")), do.call(c, parts)))
    }
    return(c(enc(paste0("a", length(x), ":")), do.call(c, lapply(x, .cr_canonical))))
  }
  if (!is.null(dim(x))) .cr_error("INVALID_JSON", "JSON arrays must be explicit lists")
  if (length(x) != 1L) return(.cr_canonical(as.list(x)))
  if (is.logical(x) && !is.na(x)) return(enc(if (x) "t" else "f"))
  if (is.character(x) && !is.na(x)) {
    s <- enc(x)
    return(c(enc(paste0("s", length(s), ":")), s))
  }
  if (is.numeric(x) && !is.complex(x) && is.finite(x)) {
    if (is.integer(x) && abs(x) > 2^53 - 1)
      .cr_error("INVALID_JSON", "Integer-valued JSON numbers exceed exact binary64 range")
    if (x == 0) x <- 0
    return(enc(paste0("d", paste(sprintf("%02x", as.integer(writeBin(as.double(x), raw(), size = 8L, endian = "big"))), collapse = ""))))
  }
  .cr_error("INVALID_JSON", "Canonical values must be finite JSON values")
}
.cr_hash <- function(x) digest::digest(.cr_canonical(x), algo = "sha256", serialize = FALSE)

# Seventeen significant decimal digits preserve every binary64 value on reading.
# String escaping is delegated to the established JSON implementation.
.cr_json_encode <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) {
    values <- vapply(x, .cr_json_encode, character(1))
    if (.cr_object(x)) {
      if (!length(x)) return("{}")
      keys <- vapply(names(x), function(k) as.character(jsonlite::toJSON(k, auto_unbox = TRUE)), character(1))
      return(paste0("{", paste0(keys, ":", values, collapse = ","), "}"))
    }
    return(paste0("[", paste(values, collapse = ","), "]"))
  }
  if (length(x) != 1L) return(.cr_json_encode(as.list(x)))
  if (is.character(x)) return(as.character(jsonlite::toJSON(x, auto_unbox = TRUE)))
  if (is.logical(x)) return(if (x) "true" else "false")
  number <- trimws(formatC(x, digits = 17, format = "g", decimal.mark = "."))
  if (!grepl("[.eE]", number)) number <- paste0(number, ".0")
  number
}

.cr_check_integer_tokens <- function(text) {
  # Consume quoted strings as complete tokens so digits inside text are ignored.
  pattern <- '"([^"\\\\]|\\\\.)*"|-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?'
  tokens <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]
  integers <- tokens[!grepl('^"|[.eE]', tokens)]
  if (any(abs(as.double(integers)) > 2^53 - 1))
    .cr_error("INVALID_JSON", "Integer literals exceed the exact binary64 range")
  invisible(NULL)
}

build_calculation_context <- function(as_of_date = NULL, knowledge_date = NULL,
    horizon = NULL, time_unit = NULL, currency = NULL, probability_measure = NULL,
    conditioning = NULL, model_version = NULL, data_manifest = NULL,
    scenario_id = NULL, policy_id = NULL) {
  if (xor(is.null(as_of_date), is.null(knowledge_date)))
    .cr_error("MISSING_INPUT", "Both as_of_date and knowledge_date are required together")
  if (!is.null(as_of_date)) {
    .cr_date(as_of_date, "as_of_date")
    .cr_date(knowledge_date, "knowledge_date")
  }
  if (xor(is.null(horizon), is.null(time_unit)))
    .cr_error("MISSING_INPUT", "horizon and time_unit are required together")
  if (!is.null(horizon)) {
    .cr_numeric(horizon, "horizon", vector_only = TRUE)
    if (length(horizon) != 1L || horizon <= 0) .cr_error("INCOMPATIBLE_HORIZON", "horizon must be positive scalar")
    if (length(time_unit) != 1L || is.na(time_unit) || !time_unit %in% c("days", "months", "years"))
      .cr_error("INCOMPATIBLE_HORIZON", "Invalid time_unit")
  }
  ids <- list(currency = currency, conditioning = conditioning, model_version = model_version,
              scenario_id = scenario_id, policy_id = policy_id)
  for (nm in names(ids)) if (!is.null(ids[[nm]])) .cr_string(ids[[nm]], nm, "INVALID_CONTEXT")
  if (!is.null(probability_measure) && (length(probability_measure) != 1L ||
      is.na(probability_measure) || !probability_measure %in% c("PHYSICAL", "RISK_NEUTRAL")))
    .cr_error("INVALID_CONTEXT", "Invalid probability_measure")
  if (!is.null(data_manifest) && !.cr_object(data_manifest))
    .cr_error("INVALID_CONTEXT", "data_manifest must be a named list")
  out <- list(schema_version = "1.0", mode = if (is.null(as_of_date)) "TIME_INDEPENDENT" else "DATED",
      as_of = as_of_date, known_at = knowledge_date, horizon = horizon, time_unit = time_unit,
      currency = currency, probability_measure = probability_measure, conditioning = conditioning,
      model_version = model_version, data_manifest = data_manifest, scenario_id = scenario_id, policy_id = policy_id)
  out$context_hash <- .cr_hash(out)
  .cr_result(out, "core.build_calculation_context", "context")
}

.cr_scale <- function(rating_scale, boundary_convention = NULL) {
  if (inherits(rating_scale, "CalcResult")) {
    if (!identical(rating_scale$method_id, "core.validate_rating_scale"))
      .cr_error("INVALID_RATING_SCALE", "Expected a scale result")
    rating_scale <- rating_scale$value
  }
  fail <- function(msg) .cr_error("INVALID_RATING_SCALE", msg)
  if (!.cr_object(rating_scale)) fail("rating_scale must be a named list")
  s <- rating_scale
  if (!identical(s$schema_version, "1.0")) fail("Unsupported scale schema_version")
  .cr_string(s$scale_id, "scale_id", "INVALID_RATING_SCALE")
  .cr_string(s$version, "version", "INVALID_RATING_SCALE")
  bc <- s$boundary_convention
  if (!is.character(bc) || length(bc) != 1L || is.na(bc) || !bc %in% c("left_closed", "right_closed")) fail("Invalid boundary_convention")
  if (!is.null(boundary_convention) && !identical(bc, boundary_convention)) fail("Conflicting boundary conventions")
  g <- s$grades
  if (!is.list(g) || !length(g) || !is.null(names(g))) fail("grades must be a nonempty unnamed list")
  n <- length(g)
  grade <- character(n); lo <- hi <- rp <- numeric(n); defaults <- logical(n)
  for (i in seq_len(n)) {
    z <- g[[i]]
    if (!.cr_object(z)) fail("Each grade must be a named list")
    grade[i] <- .cr_string(z$grade, "grade", "INVALID_RATING_SCALE")
    for (nm in c("lower", "upper", "representative_pd")) {
      v <- z[[nm]]
      if (!(is.integer(v) || is.double(v)) || is.object(v) || length(v) != 1L || !is.finite(v) || v < 0 || v > 1) fail(paste("Invalid", nm))
    }
    if (!is.logical(z$is_default) || length(z$is_default) != 1L || is.na(z$is_default)) fail("is_default must be logical")
    lo[i] <- z$lower; hi[i] <- z$upper; rp[i] <- z$representative_pd; defaults[i] <- z$is_default
  }
  if (anyDuplicated(grade) || lo[1] != 0 || hi[n] != 1 || any(lo > hi) ||
      (n > 1 && any(lo[-1] != hi[-n])) || any(diff(rp) <= 0) ||
      sum(defaults) > 1 || any(which(defaults) != n)) fail("Scale ordering, coverage or default status is invalid")
  singleton <- lo == hi
  if (any(singleton & !(seq_len(n) == n & lo == 1 & defaults))) fail("Only a terminal default singleton is permitted")
  idx <- .cr_scale_indices(rp, lo, hi, bc)
  if (any(idx != seq_len(n))) fail("Representative PD must belong to its own interval")
  s
}
.cr_scale_indices <- function(pd, lo, hi, convention) {
  n <- length(lo)
  vapply(pd, function(p) {
    if (lo[n] == 1 && p == 1) return(n)
    inside <- if (convention == "left_closed") p >= lo & (p < hi | (seq_len(n) == n & p == 1)) else
      (p > lo | (seq_len(n) == 1L & p == 0)) & p <= hi
    found <- which(inside)
    if (length(found) != 1L) .cr_error("INVALID_RATING_SCALE", "Probability has no unique binding interval")
    found
  }, integer(1L))
}
validate_rating_scale <- function(rating_scale, boundary_convention = NULL) {
  .cr_result(.cr_scale(rating_scale, boundary_convention), "core.validate_rating_scale", "rating_scale",
             diagnostics = list(overlaps = list(), gaps = list(), direction = "increasing_risk"))
}
map_pd_to_grade <- function(pd, rating_scale, context = NULL) {
  pd <- .cr_probability(pd, "pd", vector_only = TRUE)
  s <- .cr_scale(rating_scale)
  lo <- vapply(s$grades, `[[`, numeric(1), "lower")
  hi <- vapply(s$grades, `[[`, numeric(1), "upper")
  ix <- .cr_scale_indices(pd, lo, hi, s$boundary_convention)
  value <- vapply(s$grades[ix], `[[`, character(1), "grade")
  .cr_result(value, "core.map_pd_to_grade", "grade", context,
             diagnostics = list(scale_id = s$scale_id, scale_version = s$version,
               binding_intervals = s$grades[ix]),
             limitations = "A mapped default grade is not an observed default event.")
}

.cr_model <- function(model) {
  fail <- function(msg) .cr_error("INVALID_MODEL", msg)
  if (inherits(model, "FitResult")) model <- model$model else
    if (inherits(model, "CalcResult")) model <- model$value
  if (!.cr_object(model)) fail("Model must be a named list")
  required <- c("schema_version", "model_id", "model_version", "method_id", "feature_order",
                "parameters", "preprocessing", "target", "training", "diagnostics", "provenance")
  if (!all(required %in% names(model))) fail("Model is missing required fields")
  if (!identical(model$schema_version, "1.0")) .cr_error("UNSUPPORTED_SCHEMA_VERSION", "Unsupported model schema")
  for (nm in c("model_id", "model_version", "method_id")) .cr_string(model[[nm]], nm, "INVALID_MODEL")
  features <- model$feature_order
  if (is.character(features)) features <- as.list(features)
  if (!is.list(features) || !is.null(names(features))) fail("feature_order must be an array")
  for (f in features) .cr_string(f, "feature", "INVALID_MODEL")
  if (anyDuplicated(unlist(features))) fail("feature_order must be distinct")
  model$feature_order <- features
  for (nm in c("parameters", "preprocessing", "target", "training", "diagnostics", "provenance"))
    if (!.cr_object(model[[nm]])) fail(paste(nm, "must be an object"))
  t <- model$target
  if (!all(c("event", "horizon", "time_unit", "probability_measure", "conditioning") %in% names(t))) fail("Incomplete target")
  .cr_string(t$event, "target event", "INVALID_MODEL")
  .cr_string(t$conditioning, "target conditioning", "INVALID_MODEL")
  if (!(is.integer(t$horizon) || is.double(t$horizon)) || is.object(t$horizon) || length(t$horizon) != 1L || !is.finite(t$horizon) || t$horizon <= 0) fail("Invalid target horizon")
  if (!is.character(t$time_unit) || length(t$time_unit) != 1L || is.na(t$time_unit) || !t$time_unit %in% c("days", "months", "years")) fail("Invalid target time unit")
  if (!is.character(t$probability_measure) || length(t$probability_measure) != 1L || is.na(t$probability_measure) || !t$probability_measure %in% c("PHYSICAL", "RISK_NEUTRAL")) fail("Invalid target probability measure")
  for (nm in c("data_fingerprint", "split_id")) .cr_string(model$training[[nm]], nm, "INVALID_MODEL")
  .cr_date(model$training$known_at, "training known_at")
  for (nm in c("data_origin", "parameter_origin")) .cr_string(model$provenance[[nm]], nm, "INVALID_MODEL")
  if (!model$provenance$data_origin %in% c("SYNTHETIC", "USER_SUPPLIED", "REFERENCE")) fail("Invalid data_origin")
  if (!model$provenance$parameter_origin %in% c("ESTIMATED", "METHOD_CONSTANT", "USER_SUPPLIED", "SYNTHETIC_REFERENCE")) fail("Invalid parameter_origin")
  .cr_canonical(model)
  model
}
.cr_path <- function(path) {
  .cr_string(path, "path", "INVALID_PATH")
  if (grepl("^[A-Za-z][A-Za-z0-9+.-]*://", path) || grepl("^//|^\\\\", path))
    .cr_error("INVALID_PATH", "Only explicit local paths are supported")
  path
}
serialize_fitted_model <- function(fitted_model, destination, schema_version = "1.0") {
  if (!identical(schema_version, "1.0")) .cr_error("UNSUPPORTED_SCHEMA_VERSION", "Unsupported envelope schema")
  model <- .cr_model(fitted_model)
  destination <- .cr_path(destination)
  if (file.exists(destination)) .cr_error("FILE_EXISTS", "Destination already exists")
  hash <- .cr_hash(model)
  envelope <- list(format = "creditratingsystem-model", schema_version = "1.0",
                   hash_format = "cr-canonical-v1", model = model, sha256 = hash)
  # Exclusive mode prevents replacing a model created since the existence check.
  con <- tryCatch(file(destination, open = "wx", encoding = "UTF-8"), error = function(e) .cr_error("IO_ERROR", conditionMessage(e)))
  on.exit(close(con), add = TRUE)
  tryCatch(writeLines(.cr_json_encode(envelope), con, useBytes = TRUE),
           error = function(e) .cr_error("IO_ERROR", conditionMessage(e)))
  .cr_result(list(path = destination, sha256 = hash, schema_version = "1.0"), "core.serialize_fitted_model", "artifact_manifest")
}
load_fitted_model <- function(manifest_path, allowed_schema_versions = "1.0") {
  manifest_path <- .cr_path(manifest_path)
  if (!is.character(allowed_schema_versions) || anyNA(allowed_schema_versions))
    .cr_error("INVALID_INPUT", "allowed_schema_versions must be a character vector")
  txt <- tryCatch(paste(readLines(manifest_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                  error = function(e) .cr_error("IO_ERROR", conditionMessage(e)))
  .cr_check_integer_tokens(txt)
  envelope <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                       error = function(e) .cr_error("INVALID_JSON", conditionMessage(e)))
  # Includes duplicate keys and non-finite values before any field is selected.
  tryCatch(.cr_canonical(envelope), creditratingsystem_error = function(e) .cr_error("INVALID_JSON", e$message))
  if (!.cr_object(envelope) || !identical(envelope$format, "creditratingsystem-model") ||
      !identical(envelope$hash_format, "cr-canonical-v1")) .cr_error("INVALID_MODEL", "Invalid model envelope")
  if (!identical(envelope$schema_version, "1.0") || !envelope$schema_version %in% allowed_schema_versions)
    .cr_error("UNSUPPORTED_SCHEMA_VERSION", "Unsupported envelope schema")
  model <- .cr_model(envelope$model)
  if (!is.character(envelope$sha256) || length(envelope$sha256) != 1L ||
      !identical(.cr_hash(model), envelope$sha256)) .cr_error("INTEGRITY_ERROR", "Model hash does not match")
  .cr_result(model, "core.load_fitted_model", "model_artifact",
             diagnostics = list(sha256 = envelope$sha256, integrity = "MATCH"), provenance = model$provenance,
             limitations = "A matching content hash is not authenticated authorship or model approval.")
}

# Public canonical binding for caller-supplied data-only simulation recipes.
content_fingerprint <- function(value, context=NULL) {
 .cr_result(.cr_hash(value),'core.content_fingerprint','sha256',context,
  diagnostics=list(encoding='creditratingsystem-canonical-v1'),
  limitations='Identifies canonical content only; it does not authenticate its author or validate financial contracts. Remove a digest field before hashing the payload it is intended to bind.')
}
