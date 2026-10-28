.cr_glm_design <- function(X, feature_order, fit_intercept) {
  x <- .cr_numeric(X, "X", allow_empty = TRUE)
  if (length(dim(x)) != 2L) .cr_error("INVALID_INPUT", "X must be a numeric matrix")
  if (is.list(feature_order) && !length(feature_order)) feature_order <- character()
  if (!is.character(feature_order) || anyNA(feature_order) || any(!nzchar(trimws(feature_order))) ||
      length(feature_order) != ncol(x) || anyDuplicated(feature_order))
    .cr_error("FEATURE_MISMATCH", "Distinct feature names must match matrix columns")
  if (!is.logical(fit_intercept) || length(fit_intercept) != 1L || is.na(fit_intercept))
    .cr_error("INVALID_INPUT", "fit_intercept must be explicitly logical")
  d <- if (fit_intercept) cbind(1, x) else x
  if (!ncol(d)) .cr_error("INVALID_INPUT", "At least one estimable parameter is required")
  list(x = x, d = d, feature_order = feature_order)
}
.cr_glm_vector <- function(x, n, name, default) {
  if (is.null(x)) return(rep(default, n))
  x <- .cr_numeric(x, name, allow_empty = TRUE, vector_only = TRUE)
  if (length(x) != n) .cr_error("INCOMPATIBLE_SHAPE", paste(name, "must have one value per row"))
  x
}
.cr_glm_inverse <- function(eta, link) {
  switch(link, logit = stats::plogis(eta), probit = stats::pnorm(eta),
         cloglog = -expm1(-exp(eta)))
}
.cr_glm_derivative <- function(eta, p, link) {
  switch(link, logit = p * (1 - p), probit = stats::dnorm(eta), cloglog = exp(eta - exp(eta)))
}
.cr_glm_separation <- function(d, y) {
  signed <- sweep(d, 2, apply(abs(d), 2L, max), "/") * (2 * y - 1)
  k <- ncol(d)
  a <- cbind(signed, -signed)
  fit <- lpSolve::lp("max", colMeans(a), rbind(a, cbind(diag(k), diag(k))),
    c(rep(">=", nrow(d)), rep("<=", k)), c(rep(0, nrow(d)), rep(1, k)))
  if (fit$status != 0) .cr_error("NUMERICAL_FAILURE", "Separation linear program failed", list(solver_status = fit$status))
  margins <- as.vector(a %*% fit$solution)
  if (min(margins) < -1e-7) .cr_error("NUMERICAL_FAILURE", "Separation certificate violates feasibility tolerance")
  objective <- mean(margins)
  if (objective > 1e-8) .cr_error("SEPARATION", "Complete or quasi separation: finite unpenalized MLE is unavailable",
                               list(mean_margin = objective, separation_tolerance = 1e-8))
  objective
}

fit_binary_glm <- function(X_train, y_train, link, feature_order, fit_intercept,
    target, training, model_id, model_version, weights = NULL,
    weight_type = "frequency", offset = NULL, max_iter = 100L,
    tolerance = 1e-10, context = NULL) {
  if (!is.character(link) || length(link) != 1L || is.na(link) || !link %in% c("logit", "probit", "cloglog"))
    .cr_error("INVALID_INPUT", "link must be logit, probit or cloglog")
  design <- .cr_glm_design(X_train, feature_order, fit_intercept)
  d <- design$d; n <- nrow(d)
  if (is.null(y_train)) .cr_error("MISSING_INPUT", "Observed y_train is required")
  y <- .cr_glm_vector(y_train, n, "y_train", NA_real_)
  if (!n) .cr_error("EMPTY_INPUT", "Training requires observations")
  if (anyNA(y) || any(!y %in% c(0, 1))) .cr_error("INVALID_OUTCOME", "y_train must contain observed numeric zero/one outcomes")
  if (!identical(weight_type, "frequency")) .cr_error("INVALID_WEIGHT_TYPE", "Only integer frequency weights are supported")
  w <- .cr_glm_vector(weights, n, "weights", 1)
  if (any(w < 0) || any(w != floor(w)) || !any(w > 0) || sum(w) > 2^53 - 1)
    .cr_error("INVALID_WEIGHTS", "Frequency weights must be nonnegative exact integers with positive finite total")
  o <- .cr_glm_vector(offset, n, "offset", 0)
  .cr_numeric(max_iter, "max_iter", vector_only = TRUE)
  if (length(max_iter) != 1L || max_iter < 1 || max_iter != floor(max_iter)) .cr_error("INVALID_INPUT", "max_iter must be a positive integer")
  .cr_numeric(tolerance, "tolerance", vector_only = TRUE)
  if (length(tolerance) != 1L || tolerance <= 0 || tolerance >= 1) .cr_error("INVALID_INPUT", "tolerance must be in (0,1)")
  ctx <- .cr_result(NULL, "classification.fit_binary_glm", context = context)$context
  origin <- if (is.list(training) && !is.null(training$data_origin)) training$data_origin else
    if (is.list(ctx$data_manifest) && !is.null(ctx$data_manifest$data_origin)) ctx$data_manifest$data_origin else "USER_SUPPLIED"
  model <- .cr_model(list(schema_version = "1.0", model_id = model_id, model_version = model_version,
    method_id = "classification.fit_binary_glm", feature_order = as.list(design$feature_order),
    parameters = .cr_empty_object(), preprocessing = list(transformation = "identity"), target = target,
    training = training, diagnostics = .cr_empty_object(),
    provenance = list(data_origin = origin, parameter_origin = "ESTIMATED")))
  active <- w > 0
  d <- d[active, , drop = FALSE]; y <- y[active]; w <- w[active]; o <- o[active]
  if (length(unique(y)) != 2L) .cr_error("INSUFFICIENT_CLASS_SUPPORT", "Both classes need positive frequency weight")
  singular <- svd(d, nu = 0, nv = 0)$d
  rank <- sum(singular > singular[1] * 1e-10)
  if (rank < ncol(d)) .cr_error("RANK_DEFICIENT", "Design is not identifiable at relative rank tolerance 1e-10")
  separation <- .cr_glm_separation(d, y)
  warnings <- character()
  fitted <- tryCatch(withCallingHandlers(
    stats::glm.fit(d, y, weights = w, offset = o, family = stats::binomial(link),
      intercept = fit_intercept, singular.ok = FALSE,
      control = stats::glm.control(epsilon = tolerance, maxit = max_iter)),
    warning = function(warning) { warnings <<- c(warnings, conditionMessage(warning)); invokeRestart("muffleWarning") }),
    error = function(e) .cr_error("NUMERICAL_FAILURE", "GLM estimation failed", list(reason = conditionMessage(e))))
  coefficients <- unname(fitted$coefficients)
  if (!isTRUE(fitted$converged) || any(!is.finite(coefficients))) .cr_error("NON_CONVERGENCE", "GLM did not converge to finite coefficients")
  eta <- as.vector(d %*% coefficients) + o
  p <- .cr_glm_inverse(eta, link)
  derivative <- .cr_glm_derivative(eta, p, link)
  if (any(p <= 0 | p >= 1) || any(derivative <= 0)) .cr_error("NUMERICAL_FAILURE", "Fitted probabilities or derivatives reached a numerical boundary")
  information_weights <- w * derivative^2 / (p * (1 - p))
  information <- crossprod(d, d * information_weights)
  gradient <- as.vector(crossprod(d, w * (y - p) * derivative / (p * (1 - p))))
  scaled_gradient <- max(abs(gradient) / sqrt(diag(information)))
  if (!is.finite(scaled_gradient) || scaled_gradient > max(sqrt(tolerance), 1e-6))
    .cr_error("NON_CONVERGENCE", "Likelihood score exceeds tolerance", list(scaled_gradient = scaled_gradient))
  covariance <- tryCatch(chol2inv(chol(information)), error = function(e) .cr_error("NON_IDENTIFIABLE", "Expected Fisher information is singular"))
  if (any(!is.finite(covariance)) || any(diag(covariance) <= 0)) .cr_error("NON_IDENTIFIABLE", "Coefficient covariance is not finite positive definite")
  ll <- sum(w * (y * log(p) + (1 - y) * log1p(-p)))
  diagnostics <- list(converged = TRUE, iterations = fitted$iter, log_likelihood = ll,
    deviance = -2 * ll, aic = -2 * ll + 2 * rank, bic = -2 * ll + log(sum(w)) * rank,
    n_rows = n, n_active_rows = length(y), frequency_total = sum(w), rank = rank,
    rank_tolerance = 1e-10, condition_number = singular[1] / tail(singular, 1L),
    scaled_gradient = scaled_gradient, gradient_tolerance = max(sqrt(tolerance), 1e-6), score = as.list(gradient),
    separation_mean_margin = separation, separation_tolerance = 1e-8,
    covariance_type = "EXPECTED_FISHER_IID", weight_type = weight_type,
    solver = "IRLS", max_iter = max_iter, tolerance = tolerance, warnings = as.list(warnings))
  model$parameters <- list(link = link, fit_intercept = fit_intercept,
    coefficients = as.list(coefficients), covariance = lapply(seq_len(nrow(covariance)), function(i) as.list(covariance[i, ])),
    covariance_type = "EXPECTED_FISHER_IID", offset_required = !is.null(offset))
  model$diagnostics <- diagnostics
  model <- .cr_model(model)
  .cr_fit_result(model, "classification.fit_binary_glm", diagnostics = diagnostics, context = context)
}

.cr_pd_method_kinds <- c(
 "classification.fit_discriminant_pd"="discriminant", "classification.fit_naive_bayes_pd"="naive_bayes",
 "classification.fit_svm_score"="svm", "classification.fit_firth_logit"="firth_logit",
 "classification.fit_penalized_glm"="penalized_logit", "classification.fit_ordinal_model"="ordinal",
 "classification.fit_gam_pd"="gam", "classification.fit_random_forest_pd"="random_forest",
 "classification.fit_gradient_boosting_pd"="gradient_boosting", "classification.fit_bayesian_pd"="bayesian",
 "classification.fit_neural_pd"="neural", "classification.fit_sequence_pd"="sequence")
.cr_pd_other_methods <- c("classification.fit_binary_glm","hybrid.fit_stacked_pd",
 "selection_bias.fit_joint_selection_model","market_implied.fit_market_to_physical_pd_bridge",
 "calibration.fit_model_selection_protocol","scorecard.fit_scorecard")
.cr_prediction_data <- function(value) {
 if(is.null(value)) return(NULL)
 if(is.factor(value)) value<-as.character(value)
 if(is.data.frame(value)) return(lapply(seq_len(nrow(value)),function(i) unname(lapply(value,function(column) .cr_prediction_data(column[i])))))
 if(is.matrix(value)) return(lapply(seq_len(nrow(value)),function(i) lapply(as.list(unname(value[i,])),.cr_prediction_data)))
 if(is.list(value)) return(lapply(value,.cr_prediction_data))
 if(length(value)==1L) { if(is.na(value)) return(NULL); return(unname(value)) }
 lapply(as.list(value),.cr_prediction_data)
}
# Preserve categorical tables and sequence records; adapt only numeric design inputs.
.cr_pd_numeric_input <- function(X, model) {
 method<-model$method_id
 if(method %in% c("classification.fit_naive_bayes_pd","classification.fit_sequence_pd")) return(X)
 if(is.list(X) && !is.null(names(X)) && "X" %in% names(X)) { X$X<-.cr_pd_numeric_input(X$X,list(method_id="classification.fit_binary_glm"));return(X) }
 if(is.data.frame(X)) return(as.matrix(X))
 if(is.list(X) && is.null(names(X))) {
  widths<-vapply(X,length,integer(1))
  if(length(unique(widths))>1L) .cr_error("INCOMPATIBLE_SHAPE","Numeric design must be rectangular")
  if(!length(X)) return(matrix(numeric(),nrow=0L,ncol=length(model$feature_order)))
  flat<-unlist(X,use.names=FALSE)
  if(!is.numeric(flat) && length(flat)) .cr_error("INVALID_INPUT","Numeric model requires numeric design")
  return(matrix(flat,nrow=length(X),byrow=TRUE))
 }
 X
}
predict_pd <- function(fitted_model,X,feature_order,expected_target,offset=NULL,context=NULL) {
 model<-.cr_model(fitted_model); method<-model$method_id
 if(!method %in% c(names(.cr_pd_method_kinds),.cr_pd_other_methods)) .cr_error("UNSUPPORTED_MODEL","Unsupported frozen classification model")
 if(method %in% names(.cr_pd_method_kinds) && !identical(model$parameters$kind,unname(.cr_pd_method_kinds[[method]]))) .cr_error("INVALID_MODEL","Method identifier and parameter kind disagree")
 if(!identical(as.list(feature_order),model$feature_order)) .cr_error("FEATURE_MISMATCH","Feature order must exactly match fitted model")
 if(!identical(.cr_hash(expected_target),.cr_hash(model$target))) .cr_error("TARGET_MISMATCH","Expected target must exactly match model")
 ctx<-.cr_result(NULL,"classification.predict_pd",context=context)$context
 for(key in c("event","horizon","time_unit","probability_measure","conditioning")) if(!is.null(ctx[[key]]) && !identical(.cr_hash(ctx[[key]]),.cr_hash(model$target[[key]]))) .cr_error("TARGET_MISMATCH",paste("Context",key,"conflicts with frozen target"))
 if(!is.null(ctx$known_at) && .cr_date(model$training$known_at,"model.known_at")>.cr_date(ctx$known_at,"context.known_at")) .cr_error("LOOKAHEAD_BIAS","Frozen model unavailable at prediction knowledge cutoff")
 if(method=="classification.fit_naive_bayes_pd")for(feature in model$parameters$features)for(key in c("missing_index","unknown_index")){index<-feature[[key]];if(!is.null(index)&&(!is.numeric(index)||length(index)!=1L||!is.finite(index)||index<0||index!=floor(index))) .cr_error("INVALID_MODEL","Categorical index must be nonnegative integer")}
 result<-.cr_predict_pd_dispatch(model,.cr_pd_numeric_input(X,model),as.character(unlist(feature_order)),expected_target,if(is.list(offset)) unlist(offset) else offset,context)
 p<-.cr_numeric(unlist(result$value),"predicted PD",allow_empty=TRUE,vector_only=TRUE)
 if(any(p<0 | p>1)) .cr_error("INVALID_PROBABILITY","Frozen inference outside probability range")
 table<-if(is.list(X) && !is.null(names(X)) && "X" %in% names(X)) X$X else X
 count<-if(is.matrix(table) || is.data.frame(table)) nrow(table) else length(table)
 if(length(p)!=count) .cr_error("INCOMPATIBLE_SHAPE","Prediction must preserve one PD per row")
 diagnostics<-result$diagnostics
 additions<-list(model_id=model$model_id,model_version=model$model_version,model_method_id=method,
  model_hash=.cr_hash(model),prediction_input_hash=.cr_hash(list(X=.cr_prediction_data(X),feature_order=as.list(feature_order),offset=if(is.null(offset)) NULL else .cr_prediction_data(as.list(offset)))),
  target=model$target,prediction_count=length(p),training_data_fingerprint=model$training$data_fingerprint,
  training_known_at=model$training$known_at,calibration_applied=if(method=="calibration.fit_model_selection_protocol") "FROZEN_PROTOCOL" else "NONE_IMPLICIT")
 for(key in names(additions)) diagnostics[key]<-additions[key]
 .cr_result(result$value,"classification.predict_pd","probability",context,diagnostics=diagnostics,provenance=model$provenance,limitations=result$limitations)
}

.cr_predict_pd_dispatch <- function(fitted_model, X, feature_order, expected_target, offset = NULL, context = NULL) {
  if (inherits(fitted_model, "FitResult")) fitted_model <- fitted_model$model
  model <- .cr_model(fitted_model)
  ctx <- .cr_result(NULL,"classification.predict_pd",context=context)$context
  for(key in c("horizon","time_unit","probability_measure","conditioning"))
    if(!is.null(ctx[[key]])&&!identical(.cr_hash(ctx[[key]]),.cr_hash(model$target[[key]]))) .cr_error("TARGET_MISMATCH",paste("Context",key,"conflicts with fitted target"))
  if(model$method_id %in% c("classification.fit_discriminant_pd","classification.fit_naive_bayes_pd","classification.fit_svm_score","classification.fit_firth_logit","classification.fit_penalized_glm","classification.fit_ordinal_model","classification.fit_gam_pd","classification.fit_random_forest_pd","classification.fit_gradient_boosting_pd","classification.fit_bayesian_pd","classification.fit_neural_pd","classification.fit_sequence_pd")) {
    if(!is.null(offset)) .cr_error("OFFSET_MISMATCH","This fitted model does not accept prediction offsets")
    return(.cm_predict_pd(model,X,feature_order,expected_target,context))
  }
  if (identical(model$method_id,"hybrid.fit_stacked_pd")) {
    if(!is.null(offset)) .cr_error("OFFSET_MISMATCH","Stacked PD models do not accept prediction offsets")
    return(.hy_predict_stacked_pd(model,X,feature_order,expected_target,context))
  }
  if (identical(model$method_id,"selection_bias.fit_joint_selection_model")) {
    if(!is.null(offset)) .cr_error("OFFSET_MISMATCH","Joint selection models do not accept prediction offsets")
    return(.cr_sb_predict_joint(model,X,feature_order,expected_target,context))
  }
  if (identical(model$method_id,"market_implied.fit_market_to_physical_pd_bridge")) {
    if(!is.null(offset)) .cr_error("OFFSET_MISMATCH","Market bridge models do not accept prediction offsets")
    return(.cr_mkt_predict_bridge(model,X,feature_order,expected_target,context))
  }
  if (identical(model$method_id,"calibration.fit_model_selection_protocol")) {
    if(!is.null(offset)) .cr_error("OFFSET_MISMATCH","Selected frozen models do not accept prediction offsets")
    return(.cr_cm_predict_pd(model,X,feature_order,expected_target,context))
  }
  if (identical(model$method_id,"scorecard.fit_scorecard")) {
    if(!is.null(offset)) .cr_error("OFFSET_MISMATCH","Frozen scorecards do not accept prediction offsets")
    scored<-score_scorecard(model,X,feature_order=feature_order,expected_target=expected_target,context=context)
    return(.cr_result(scored$value$pd,"classification.predict_pd","probability",context,
      diagnostics=list(model_id=model$model_id,model_version=model$model_version,prediction_family="scorecard",target=model$target),
      provenance=model$provenance,limitations="Frozen scorecard inference requires its trained feature transformation and matching target."))
  }
  if (!identical(model$method_id, "classification.fit_binary_glm")) .cr_error("UNSUPPORTED_MODEL", "Unsupported frozen classification model")
  if (!identical(as.list(feature_order), model$feature_order)) .cr_error("FEATURE_MISMATCH", "Feature order must match fitted model")
  if (!identical(.cr_hash(expected_target), .cr_hash(model$target))) .cr_error("TARGET_MISMATCH", "Expected target must match fitted model")
  ctx <- .cr_result(NULL, "classification.predict_pd", context = context)$context
  for (key in c("horizon", "time_unit", "probability_measure", "conditioning"))
    if (!is.null(ctx[[key]]) && !identical(.cr_hash(ctx[[key]]), .cr_hash(model$target[[key]]))) .cr_error("TARGET_MISMATCH", paste("Context", key, "conflicts with fitted target"))
  if (!identical(model$preprocessing, list(transformation = "identity")))
    .cr_error("UNSUPPORTED_MODEL", "This GLM expects already transformed features with identity preprocessing")
  params <- model$parameters
  if (!is.character(params$link) || length(params$link) != 1L || !params$link %in% c("logit", "probit", "cloglog") ||
      !is.logical(params$offset_required) || length(params$offset_required) != 1L || is.na(params$offset_required))
    .cr_error("INVALID_MODEL", "Incomplete or unsupported GLM parameter schema")
  design <- .cr_glm_design(X, feature_order, params$fit_intercept)
  d <- design$d
  if (!is.list(params$coefficients) || !is.null(names(params$coefficients)))
    .cr_error("INVALID_MODEL", "Coefficients must be an array")
  for (coefficient in params$coefficients) {
    .cr_numeric(coefficient, "coefficient", vector_only = TRUE)
    if (length(coefficient) != 1L) .cr_error("INVALID_MODEL", "Coefficients must be scalar numbers")
  }
  coefficients <- unlist(params$coefficients, use.names = FALSE)
  coefficients <- .cr_numeric(coefficients, "coefficients", vector_only = TRUE)
  if (length(coefficients) != ncol(d)) .cr_error("INVALID_MODEL", "Coefficient count does not match design")
  if (!identical(params$offset_required, !is.null(offset))) .cr_error("OFFSET_MISMATCH", "Prediction offset presence must match fitted contract")
  o <- .cr_glm_vector(offset, nrow(d), "offset", 0)
  eta <- as.vector(d %*% coefficients) + o
  if (any(!is.finite(eta))) .cr_error("NUMERICAL_FAILURE", "Prediction linear predictor overflowed")
  p <- .cr_glm_inverse(eta, params$link)
  .cr_result(p, "classification.predict_pd", "probability", context,
    diagnostics = list(model_id = model$model_id, model_version = model$model_version,
      link = params$link, numerical_boundary_count = sum(p == 0 | p == 1)), provenance = model$provenance,
    limitations = "IID likelihood estimation does not establish calibration on future or dependent observations.")
}
