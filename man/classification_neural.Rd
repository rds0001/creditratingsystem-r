\name{classification_neural}
\alias{fit_neural_pd}
\alias{fit_sequence_pd}
\title{Native portable MLP, GRU and LSTM credit likelihood models}
\description{\if{latex}{\out{
\global\tolerance=9999
\global\emergencystretch=3em
\gdef\_{\textunderscore\allowbreak}
\gdef\crmanualitem[#1]{\par\addvspace{0.55\baselineskip}\noindent#1\par\nopagebreak\noindent\ignorespaces}
\gdef\ldescription{\par\begingroup\let\item\crmanualitem}
\gdef\endldescription{\par\endgroup}
}}
CPU dense and recurrent neural estimation with actual analytic gradients, explicit Adam orders, frozen weights and causal sequence availability contracts.}
\usage{
fit_neural_pd(X_train, y_train, feature_order, architecture,
  initial_weights, optimizer_spec, train_row_ids, scale = TRUE,
  inner_validation = NULL, target, training, model_id,
    model_version,
  context = NULL)
fit_sequence_pd(sequences, y_train, feature_order, variant,
    hidden_size,
  initial_weights, optimizer_spec, train_row_ids, label_known_times,
  training_as_of_time, include_time_gaps = TRUE, time_gap_scale = 1,
  scale = TRUE, inner_validation = NULL, target, training, model_id,
  model_version, context = NULL)
}
\arguments{
\item{X_train,y_train}{Finite numeric matrix and observed numeric zero/one labels. Both training classes required.}
\item{feature_order}{Distinct ordered raw feature names, matching the frozen prediction contract.}
\item{architecture}{MLP hidden_units and matching activations tanh/relu/sigmoid; empty hidden layers gives a logistic output directly.}
\item{initial_weights}{Explicit finite numeric parameters. MLP keys layer0_weight/layer0_bias etc., matrices input-width by output-width and one-dimensional biases. Recurrent keys input_weight, hidden_weight, input_bias, hidden_bias, output_weight, output_bias; see Details.}
\item{optimizer_spec}{algorithm=adam, positive learning_rate and batch_size, explicit epoch_orders (complete zero-based training permutations). Optional beta1=.9, beta2=.999, epsilon=1e-8, l2=0, positive gradient_clip_norm and early_stopping_patience. Patience requires inner_validation.}
\item{train_row_ids}{Distinct nonempty record identifiers disjoint from inner validation. Caller additionally controls borrower grouping.}
\item{scale}{Train-only population mean/SD scaling. Sequence scaling pools only retained available training observations. Constant SD becomes one.}
\item{inner_validation}{Explicit role=inner_validation, distinct split_id, disjoint row_ids, ISO known_at no later than training cutoff; X/y for MLP or sequences/y/label_known_times for sequences. Does not silently join training.}
\item{target}{Event, positive horizon, units, probability measure and conditioning. Sequence horizon uses the same declared numeric clock unit.}
\item{training}{Fingerprint, split_id and ISO known_at information cutoff.}
\item{model_id,model_version,context}{Immutable model identifiers and optional calculation context.}
\item{sequences}{List of values matrices, observation_times, known_times, numeric zero/one mask and forecast_time. At least one retained available observation per sequence, strictly increasing in time. All numeric padding must be finite.}
\item{variant,hidden_size}{gru or lstm, and arbitrary positive hidden width for one unidirectional recurrent layer.}
\item{label_known_times,training_as_of_time}{Numeric label knowledge times and model data cutoff on the sequence clock. Labels cannot predate forecasts or exceed cutoff; nondefaults require full-horizon maturity.}
\item{include_time_gaps,time_gap_scale}{Append first gap zero and subsequent retained observation gaps divided by a positive fixed scale.}
}
\details{
Forward propagation and full backpropagation are native, with no Python bridge or runtime neural backend. Loss is mean Bernoulli negative log likelihood. Adam bias-corrected moments use a coupled L2 weight-gradient penalty, not AdamW; biases are unpenalized. Optional global gradient clipping occurs after L2. Initialization fingerprints, epoch orders, update counts and full loss paths are recorded. Completion of an epoch budget is not called global optimization convergence.

For input width p and hidden width h, GRU input/hidden weights have shapes (p,3h)/(h,3h), LSTM (p,4h)/(h,4h). Gate-width input and hidden biases are separate. Output weight is (h,1), output bias length one. GRU gate order r,z,n uses reset after the hidden affine: r=sigmoid(input_r+hidden_r), z=sigmoid(input_z+hidden_z), n=tanh(input_n+r*hidden_n), h=(1-z)*n+z*previous_h. LSTM i,f,g,o uses c=f*previous_c+i*g, h=o*tanh(c), sigmoid i/f/o and tanh g. These matrices are transposes of the corresponding PyTorch stored gate matrices. Recurrent and cell states reset to zero for every record and prediction call. Genuine gated BPTT updates all recurrent parameters.

Only mask-one observations whose observation and knowledge times do not exceed forecast time enter sequence state or scaling. Known time cannot precede observation. Padding/future/late-known observations are excluded before processing. At least one observation must remain; retained times are strictly ordered. Time-gap augmentation increases input width by one. Pooling weights each retained sequence observation equally, so longer sequences contribute more to scaling. Label-known times prevent immature negatives, while observed early positive defaults may enter earlier. Correct event evidence and calendar/unit interpretation remain caller responsibilities.

With inner validation, the frozen artifact retains the best validation epoch, including epoch zero when all updates harm validation. Without validation it retains the final epoch. Early stopping never reads an undeclared final test set. Disjoint row IDs do not prove independence of repeated borrower records. No posterior uncertainty or empirical probability calibration is inferred from neural training loss.
}
\value{FitResult containing only portable architecture, weights, preprocessing, target and training/optimization diagnostics. Use predict_pd or predict_model_score with matching features and target; sequence prediction receives the same sequence-record structure.}
\references{Kingma and Ba (2015), Adam: A Method for Stochastic Optimization. PyTorch GRU and LSTM documentation for gate equations and parameter conventions.}
\seealso{\code{predict_pd},\code{predict_model_score},\code{serialize_fitted_model}}
