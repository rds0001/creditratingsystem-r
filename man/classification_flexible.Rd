\name{classification_flexible}
\alias{fit_gam_pd}
\alias{fit_random_forest_pd}
\alias{fit_gradient_boosting_pd}
\title{Constrained logistic spline models and portable tree ensembles}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
Training-only smooth and tree classifiers with explicit constraints, sampling, inner validation and data-only prediction artifacts.}
\usage{
fit_gam_pd(X_train, y_train, feature_order, linear_features,
    smooth_specs,
  variant, max_iter = 2000L, tolerance = 1e-9, target, training,
  model_id, model_version, context = NULL)
fit_random_forest_pd(X_train, y_train, feature_order, sampling_plan,
  max_depth, min_samples_leaf = 1L, min_weight_leaf = 0,
  criterion = "gini", weights = NULL, target, training, model_id,
  model_version, context = NULL)
fit_gradient_boosting_pd(X_train, y_train, feature_order,
    train_row_ids,
  iterations, max_depth, learning_rate,
    monotonic_constraints = NULL,
  l2_regularization = 1, min_child_weight = 1, min_split_loss = 0,
  max_bin = 256L, inner_validation = NULL,
    early_stopping_rounds = NULL,
  target, training, model_id, model_version, context = NULL)
}
\arguments{
\item{X_train,y_train}{Finite numeric training matrix and observed numeric zero/one labels. Both training classes required.}
\item{feature_order}{Distinct ordered feature names, frozen for prediction.}
\item{linear_features}{Names represented linearly in the additive logistic predictor.}
\item{smooth_specs}{List of terms: feature, two boundary_knots, increasing interior_knots, degree 1--3, nonnegative smoothing, monotonicity none/increasing/decreasing and extrapolation error/constant/linear. Each feature occurs exactly once as linear or smooth.}
\item{variant}{unconstrained or monotone. Monotone requires at least one truly constrained smooth.}
\item{max_iter,tolerance}{Positive iteration limit and convergence tolerance below one.}
\item{target,training}{Event/horizon/units/measure/conditioning target and training fingerprint, split_id, known_at information cutoff. Caller supplies mature labels and temporal/group split design.}
\item{model_id,model_version,context}{Immutable model identifiers and optional calculation context.}
\item{sampling_plan}{List of trees with sample_indices (zero-based training row draws, repeats count) and node_features: named list mapping root/L/R/LL/etc. to ordered distinct zero-based candidate feature indices. Every eligible visited node must have a draw.}
\item{max_depth}{Maximum tree depth; forest permits integers 1--20.}
\item{min_samples_leaf,min_weight_leaf}{Minimum sampled row count and total positive case weight per forest leaf.}
\item{criterion}{gini or log_loss for weighted CART impurity.}
\item{weights}{Nonnegative forest case weights with positive support in both training classes.}
\item{train_row_ids}{Distinct nonempty training identifiers, disjoint from supplied inner-validation rows.}
\item{iterations,learning_rate}{Positive boosting round count and shrinkage in (0,1].}
\item{monotonic_constraints}{Optional vector of -1/0/1, one constraint per input feature.}
\item{l2_regularization,min_child_weight,min_split_loss}{Nonnegative XGBoost leaf penalty, minimum child Hessian weight and minimum split gain.}
\item{max_bin}{Histogram bin count, integer at least two.}
\item{inner_validation}{Optional list X/y/row_ids/role/split_id/known_at. Role must be inner_validation, split distinct from training, IDs disjoint, cutoff no later than model training cutoff. One-class validation allowed.}
\item{early_stopping_rounds}{Optional positive patience, requiring inner_validation. Only the best evaluated tree prefix is retained.}
}
\details{
GAM minimizes mean Bernoulli negative log likelihood plus half the smoothing-weighted squared second differences of full spline coefficients. The first coefficient of each smooth is zero to identify its intercept. Monotone spline coefficients are cumulative nonnegative increments, with reversed sign for decreasing terms; constraints operate during L-BFGS-B fitting. Frozen tangent/constant/error boundary behavior preserves the declared policy. A native LP detects feasible separating recession directions in the penalty null space at relative eigenvalue tolerance 1e-10, respecting monotone bounds; smoothing alone does not cure affine separation. Such fits are rejected. Projected KKT residual and EDF on free coefficients are reported. No smoothing-selection or active-constraint uncertainty interval is inferred. No smooths reduces to ordinary logistic MLE.

Forests evaluate all valid weighted CART midpoint splits using the caller's bootstrap/subsample and feature draws. Gain ties use candidate order then increasing threshold at tolerance 1e-14. Weighted leaf proportions are averaged without clipping. OOB predictions exclude all trees containing that original row and report coverage, counts and Brier; OOB is not a temporal or obligor-group holdout. Randomness is explicit in the supplied design rather than hidden in package-global state.

Boosting uses native XGBoost CPU hist Bernoulli loss without stochastic subsampling. Initial margin is the logit of training default fraction. Frozen leaf values already include shrinkage; input casting to float32 and strict-less-than routing match backend prediction. Nonrepresentable values are rejected. All evaluated loss rounds and the selected early-stop prefix are recorded. Feature/label availability and grouping independence remain caller responsibilities; disjoint record IDs alone cannot prove temporal integrity. No empirical calibration or uncertainty interval is claimed by these fits.
}
\value{FitResult with a frozen data-only ModelArtifact and model-specific numerical, design and validation diagnostics. Use predict_pd or predict_model_score with matching target and features.}
\references{Eilers and Marx (1996), Flexible smoothing with B-splines and penalties. Breiman (2001), Random Forests. Chen and Guestrin (2016), XGBoost: A Scalable Tree Boosting System. R splines::splineDesign documentation and XGBoost model/monotonic-constraint documentation.}
\seealso{\code{predict_pd}, \code{predict_model_score}, \code{serialize_fitted_model}}
