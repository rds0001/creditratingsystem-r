\name{classification_models}
\alias{fit_discriminant_pd}
\alias{fit_naive_bayes_pd}
\alias{fit_svm_score}
\alias{predict_model_score}
\alias{fit_firth_logit}
\alias{fit_penalized_glm}
\alias{fit_ordinal_model}
\title{Portable probabilistic classification models and explicit raw scores}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Native training-only estimation with frozen data-only prediction artifacts, explicit target and feature ordering, and model-specific diagnostics.}
\usage{
fit_discriminant_pd(X_train, y_train, feature_order, variant,
  covariance_estimator = "mle", covariance_ridge = 0, priors = NULL,
  target, training, model_id, model_version, context = NULL)
fit_naive_bayes_pd(X_train, y_train, feature_order, feature_types,
    variant,
  alpha = 1, variance_floor = 0, missing_policy = "error",
  unknown_policy = "error", priors = NULL, target, training,
  model_id, model_version, context = NULL)
fit_svm_score(X_train, y_train, feature_order, kernel, C,
    gamma = NULL,
  scale = TRUE, class_weights = NULL, tolerance = 1e-7,
    max_iter = NULL,
  target, training, model_id, model_version, context = NULL)
predict_model_score(fitted_model, X, feature_order, expected_target,
  context = NULL)
fit_firth_logit(X_train, y_train, feature_order,
    fit_intercept = TRUE,
  max_iter = 1000L, tolerance = 1e-9, target, training, model_id,
  model_version, context = NULL)
fit_penalized_glm(X_train, y_train, feature_order, penalty,
    lambda_path,
  selected_lambda, l1_ratio = NULL, fit_intercept = TRUE,
    scale = TRUE,
  weights = NULL, max_iter = 10000L, tolerance = 1e-9, target,
    training,
  model_id, model_version, context = NULL)
fit_ordinal_model(X_train, y_train, feature_order,
    ordered_categories,
  link, default_categories = NULL, max_iter = 1000L,
    tolerance = 1e-9,
  target, training, model_id, model_version, context = NULL)
}
\arguments{
\item{X_train,X}{Numeric matrix; naive Bayes also accepts a data frame or list of rows preserving mixed numeric/string types.}
\item{y_train}{Observed numeric zero/one outcomes, or category strings for ordinal fitting. No missing outcomes are treated as nondefaults.}
\item{feature_order}{Distinct ordered feature names, exactly matching the frozen model at prediction.}
\item{variant}{lda/qda for discriminants; gaussian/categorical/mixed for naive Bayes.}
\item{covariance_estimator}{mle or unbiased. LDA pooled scatter divides by n or n-2; QDA class scatter by n_k or n_k-1.}
\item{covariance_ridge}{Nonnegative variance added to each diagonal on the supplied feature scale. Singular covariance without sufficient ridge is rejected.}
\item{priors}{Optional positive class probabilities in nondefault/default order, summing to one; otherwise observed proportions.}
\item{target,expected_target}{Full event, horizon, units, probability measure and conditioning contract; expected_target must match exactly.}
\item{training}{Training data fingerprint, split_id, ISO known_at cutoff and optional data_origin. Caller supplies outcomes available by this cutoff.}
\item{model_id,model_version}{Immutable model identifiers.}
\item{context}{Optional calculation context.}
\item{feature_types}{gaussian/categorical per feature. Mixed requires both types.}
\item{alpha}{Strictly positive additive categorical count smoothing.}
\item{variance_floor}{Nonnegative addition to Gaussian maximum-likelihood variance.}
\item{missing_policy}{error, marginalize or category. Marginalize omits the feature likelihood factor; category reserves a categorical missing bin and rejects missing Gaussian values.}
\item{unknown_policy}{error or reserved. Reserved adds a distinct unknown-category bin to smoothing denominators.}
\item{kernel}{linear or rbf.}
\item{C,gamma}{Positive SVM cost and explicit positive RBF gamma. Linear kernel requires NULL gamma.}
\item{scale}{Fit weighted population standard deviation and center only on training rows; freeze them for prediction. Constant scales become one.}
\item{class_weights}{Optional positive SVM loss weights for nondefault/default classes.}
\item{tolerance,max_iter}{Convergence controls. Native e1071 exposes no iteration cap: SVM requires max_iter=NULL and verifies dual KKT residual; non-NULL caps are rejected. Other models require positive integer iteration bounds.}
\item{fitted_model}{Frozen ModelArtifact or FitResult.}
\item{fit_intercept}{Include an intercept, unpenalized for elastic-net models.}
\item{penalty}{ridge, lasso or elasticnet for Bernoulli logistic loss.}
\item{lambda_path,selected_lambda}{Unique decreasing nonnegative penalties and exact caller-selected member. No validation selection is performed implicitly.}
\item{l1_ratio}{Interior elastic-net mixing fraction; NULL for ridge/lasso endpoints.}
\item{weights}{Nonnegative case weights, positive total in both classes. Likelihood is normalized by their sum.}
\item{ordered_categories}{Distinct outcome categories ordered from lower to higher credit risk, all represented in training.}
\item{link}{Ordinal logit or probit.}
\item{default_categories}{Optional ordered terminal tail of categories defining default at target horizon. Required before ordinal PD prediction.}
}
\details{
Gaussian discriminants apply Bayes' theorem to fitted class means/covariances. Naive Bayes additionally assumes conditional independence, and its Gaussian variances use maximum-likelihood denominators. These posteriors do not establish empirical calibration.

Elastic-net minimizes weighted mean negative logistic likelihood plus lambda times ((1-alpha)/2 times squared slope norm plus alpha times absolute slope norm). Each path point records objective and KKT residual. Penalties operate on fitted standardized slopes when scale is TRUE. Zero penalty checks rank and separation and reproduces ordinary logistic likelihood. No post-selection uncertainty is supplied.

Firth maximizes Bernoulli log likelihood plus one half log determinant of expected Fisher information. Native brglm2 MPL_Jeffreys with a=1/2 is checked against the exact adjusted score. Full rank is required; single-class samples and separation are permitted. Stored inverse expected information is a local diagnostic, not a penalized profile confidence interval.

Ordinal models obey P(Y<=j|x)=F(threshold_j-x beta). Prediction scores expose all category probabilities in diagnostics. Default categories must be an ordered terminal tail. The parallel-slope diagnostic jointly stacks threshold-wise binary scores and retains cross-threshold covariance in an IID sandwich Wald test. Singular or separated diagnostic fits yield NOT_ESTIMABLE; the diagnostic does not prove proportionality. Covariance parameters are slopes, first threshold and log subsequent threshold spacings.

SVM outputs are signed margins, never PDs without independent calibration. Frozen support vectors/dual coefficients/intercept are portable; higher margins mean default. Preprocessing never uses prediction rows. Model-based likelihood uncertainty assumes independent observations; temporal/group holdout selection and mature labels remain explicit caller responsibilities.
}
\value{Fits return FitResult with a portable ModelArtifact and diagnostics. predict_model_score returns CalcResult containing a numeric score array, explicit scale, orientation and model lineage; ordinal diagnostics include category probabilities.}
\references{Firth (1993), Bias reduction of maximum likelihood estimates. Kosmidis and Firth (2021), Jeffreys-prior penalty, finiteness and shrinkage in binomial-response generalized linear models. Tay, Narasimhan and Hastie (2023), Elastic Net Regularization Paths for All Generalized Linear Models. Chang and Lin (2011), LIBSVM: A library for support vector machines. MASS package documentation for lda, qda and polr.}
\seealso{\code{predict_pd}, \code{serialize_fitted_model}, \code{load_fitted_model}}
