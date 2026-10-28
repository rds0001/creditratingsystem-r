.cn_activation <- function(z,name)switch(name,tanh=tanh(z),relu=pmax(z,0),sigmoid=stats::plogis(z),identity=z)
.cn_mlp <- function(weights,architecture,x,y=NULL) {
 activations<-c(unlist(architecture$activations),'identity');cache<-list();a<-x
 for(k in seq_along(activations)) {
  z<-sweep(a%*%weights[[paste0('layer',k-1,'_weight')]],2,weights[[paste0('layer',k-1,'_bias')]],'+');out<-.cn_activation(z,activations[k]);cache[[k]]<-list(a=a,z=z,out=out,activation=activations[k]);a<-out
 }
 eta<-a[,1];if(is.null(y))return(eta)
 delta<-matrix((plogis(eta)-y)/length(y),ncol=1);grad<-list()
 for(k in rev(seq_along(cache))) {
  c<-cache[[k]]
  if(c$activation=='tanh')delta<-delta*(1-c$out*c$out) else if(c$activation=='sigmoid')delta<-delta*c$out*(1-c$out) else if(c$activation=='relu')delta<-delta*(c$z>0)
  grad[[paste0('layer',k-1,'_weight')]]<-crossprod(c$a,delta);grad[[paste0('layer',k-1,'_bias')]]<-colSums(delta);delta<-delta%*%t(weights[[paste0('layer',k-1,'_weight')]])
 }
 list(eta=eta,gradient=grad)
}
.cn_recurrent <- function(weights,architecture,sequences,y=NULL) {
 variant<-architecture$variant;hidden<-architecture$hidden_size;etas<-numeric(length(sequences));grad<-lapply(weights,function(v)v*0)
 i1<-seq_len(hidden);i2<-hidden+i1;i3<-2*hidden+i1;i4<-3*hidden+i1
 for(index in seq_along(sequences)) {
  sequence<-sequences[[index]];h<-cstate<-numeric(hidden);cache<-list()
  for(step in seq_len(nrow(sequence))) {
   x<-sequence[step,];hp<-h;cp<-cstate;ai<-as.vector(x%*%weights$input_weight)+weights$input_bias;ah<-as.vector(h%*%weights$hidden_weight)+weights$hidden_bias
   if(variant=='gru') {r<-plogis(ai[i1]+ah[i1]);z<-plogis(ai[i2]+ah[i2]);n<-tanh(ai[i3]+r*ah[i3]);h<-(1-z)*n+z*hp;cache[[step]]<-list(x=x,hp=hp,r=r,z=z,n=n,ah_n=ah[i3])} else {
    pre<-ai+ah;i<-plogis(pre[i1]);f<-plogis(pre[i2]);g<-tanh(pre[i3]);o<-plogis(pre[i4]);cstate<-f*cp+i*g;h<-o*tanh(cstate);cache[[step]]<-list(x=x,hp=hp,cp=cp,i=i,f=f,g=g,o=o,c=cstate)
   }
  }
  eta<-as.numeric(h%*%weights$output_weight+weights$output_bias);etas[index]<-eta;if(is.null(y))next
  delta<-(plogis(eta)-y[index])/length(y);grad$output_weight[,1]<-grad$output_weight[,1]+h*delta;grad$output_bias[1]<-grad$output_bias[1]+delta;dh<-weights$output_weight[,1]*delta;dc<-numeric(hidden)
  for(step in rev(seq_along(cache))) {
   v<-cache[[step]]
   if(variant=='gru') {
    dn<-dh*(1-v$z);dz<-dh*(v$hp-v$n);dan<-dn*(1-v$n*v$n);dar<-dan*v$ah_n*v$r*(1-v$r);daz<-dz*v$z*(1-v$z);dai<-c(dar,daz,dan);dah<-c(dar,daz,dan*v$r);dh<-dh*v$z+as.vector(weights$hidden_weight%*%dah)
   } else {
    tc<-tanh(v$c);dc<-dc+dh*v$o*(1-tc*tc);dai<-c(dc*v$g*v$i*(1-v$i),dc*v$cp*v$f*(1-v$f),dc*v$i*(1-v$g*v$g),dh*tc*v$o*(1-v$o));dah<-dai;dh<-as.vector(weights$hidden_weight%*%dah);dc<-dc*v$f
   }
   grad$input_weight<-grad$input_weight+outer(v$x,dai);grad$hidden_weight<-grad$hidden_weight+outer(v$hp,dah);grad$input_bias<-grad$input_bias+dai;grad$hidden_bias<-grad$hidden_bias+dah
  }
 }
 if(is.null(y))etas else list(eta=etas,gradient=grad)
}
.cn_loss <- function(eta,y)mean(pmax(eta,0)+log1p(exp(-abs(eta)))-y*eta)
.cn_optimize <- function(weights,architecture,inputs,y,optimizer_spec,validation=NULL) {
 initial_hash<-.cr_hash(.cn_frozen_weights(weights))
 spec<-optimizer_spec
 if(!is.list(spec) || !identical(spec$algorithm,'adam')).cr_error('INVALID_OPTIMIZER','Explicit Adam optimizer required')
 val<-function(key,default)if(is.null(spec[[key]]))default else spec[[key]]
 rate<-.cm_number(spec$learning_rate,'learning_rate',strict=TRUE);b1<-.cm_number(val('beta1',.9),'beta1');b2<-.cm_number(val('beta2',.999),'beta2');eps<-.cm_number(val('epsilon',1e-8),'epsilon',strict=TRUE);l2<-.cm_number(val('l2',0),'l2')
 if(b1>=1 || b2>=1).cr_error('INVALID_OPTIMIZER','Adam betas must be below one')
 batch<-spec$batch_size;orders<-spec$epoch_orders;patience<-spec$early_stopping_patience;clip<-spec$gradient_clip_norm
 integer_positive<-function(v)length(v)==1 && is.numeric(v) && is.finite(v) && v==floor(v) && v>=1
 if(!integer_positive(batch) || !is.list(orders) || !length(orders)).cr_error('INVALID_OPTIMIZER','Positive batch size and nonempty explicit epoch orders required')
 if(!is.null(patience) && (!integer_positive(patience) || is.null(validation))).cr_error('INVALID_SPLIT','Early stopping requires explicit inner validation')
 if(!is.null(clip))clip<-.cm_number(clip,'gradient_clip_norm',strict=TRUE)
 checked<-lapply(orders,function(order) {.cr_numeric(unlist(order),'epoch_order',vector_only=TRUE)->a;if(length(a)!=length(y)||!all(sort(a)==seq_along(y)-1L)).cr_error('INVALID_OPTIMIZER','Every epoch needs zero-based permutation of training rows');as.integer(a)})
 forward<-if(architecture$kind=='mlp').cn_mlp else .cn_recurrent
 subset<-function(values,index)if(is.matrix(values))values[index,,drop=FALSE] else values[index]
 loss<-function(values,labels,w).cn_loss(forward(w,architecture,values),labels)
 initial_loss<-loss(inputs,y,weights);first<-lapply(weights,function(v)v*0);second<-first;best<-weights;best_epoch<-0L;best_val<-if(!is.null(validation))loss(validation$inputs,validation$y,weights) else NULL;stale<-updates<-clipped<-0L;history<-list()
 for(epoch in seq_along(checked)) {
  order<-checked[[epoch]]+1L
  for(start in seq.int(1,length(y),by=batch)) {
   index<-order[seq.int(start,min(length(y),start+batch-1))];result<-forward(weights,architecture,subset(inputs,index),y[index]);gradient<-result$gradient
   for(key in names(gradient))if(endsWith(key,'_weight'))gradient[[key]]<-gradient[[key]]+l2*weights[[key]]
   norm<-sqrt(sum(vapply(gradient,function(g)sum(g*g),numeric(1))))
   if(!is.finite(norm)).cr_error('NUMERICAL_FAILURE','Neural gradient nonfinite')
   if(!is.null(clip) && norm>clip) {gradient<-lapply(gradient,function(g)g*clip/norm);clipped<-clipped+1L}
   updates<-updates+1L
   for(key in names(weights)) {g<-gradient[[key]];first[[key]]<-b1*first[[key]]+(1-b1)*g;second[[key]]<-b2*second[[key]]+(1-b2)*g*g;weights[[key]]<-weights[[key]]-rate*(first[[key]]/(1-b1^updates))/(sqrt(second[[key]]/(1-b2^updates))+eps);if(any(!is.finite(weights[[key]]))).cr_error('NUMERICAL_FAILURE','Neural parameter update nonfinite')}
  }
  trainloss<-loss(inputs,y,weights);valloss<-if(!is.null(validation))loss(validation$inputs,validation$y,weights) else NULL;penalty<-.5*l2*sum(vapply(names(weights),function(k)if(endsWith(k,'_weight'))sum(weights[[k]]^2) else 0,numeric(1)));history[[epoch]]<-list(epoch=epoch,train_log_loss=trainloss,penalized_train_objective=trainloss+penalty,inner_validation_log_loss=valloss)
  if(!is.null(validation)) {
   if(valloss<best_val-1e-12) {best_val<-valloss;best<-weights;best_epoch<-epoch;stale<-0L} else stale<-stale+1L
   if(!is.null(patience) && stale>=patience)break
  } else {best<-weights;best_epoch<-epoch}
 }
 gradient<-forward(best,architecture,inputs,y)$gradient;for(k in names(gradient))if(endsWith(k,'_weight'))gradient[[k]]<-gradient[[k]]+l2*best[[k]]
 diagnostics<-list(initial_weights_fingerprint=initial_hash,optimization_status=if(length(history)<length(checked))'EARLY_STOPPED' else 'EPOCH_BUDGET_COMPLETED',initial_train_log_loss=initial_loss,loss_path=history,selected_epoch=best_epoch,executed_epochs=length(history),requested_epochs=length(checked),adam_updates=updates,clipped_updates=clipped,selected_gradient_norm=sqrt(sum(vapply(gradient,function(g)sum(g*g),numeric(1)))),optimizer='ADAM_COUPLED_L2_WEIGHT_PENALTY',optimizer_parameters=list(learning_rate=rate,beta1=b1,beta2=b2,epsilon=eps,l2=l2,batch_size=batch,gradient_clip_norm=clip),epoch_orders=lapply(checked[seq_along(history)],as.list),early_stopping_patience=patience,global_optimum_claimed=FALSE,uncertainty='NOT_ESTIMATED',calibration='BERNOULLI_MODEL_NOT_HOLDOUT_CALIBRATED',training_only=TRUE)
 list(weights=best,diagnostics=diagnostics)
}
.cn_ids <- function(ids,n) {
 ids<-as.character(unlist(ids));if(length(ids)!=n || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)).cr_error('INVALID_SPLIT','Unique nonempty row identifiers required');ids
}
.cn_validation <- function(validation,ids,training) {
 if(is.null(validation))return(NULL)
 vi<-.cn_ids(validation$row_ids,length(validation$y))
 if(any(vi %in% ids) || !identical(validation$role,'inner_validation') || is.null(validation$split_id) || !nzchar(validation$split_id) || identical(validation$split_id,training$split_id)).cr_error('INVALID_SPLIT','Distinct explicit inner validation role, split and IDs required')
 cutoff<-tryCatch(as.POSIXct(validation$known_at,tz='UTC'),error=function(e)NA);traincut<-tryCatch(as.POSIXct(training$known_at,tz='UTC'),error=function(e)NA)
 if(length(cutoff)!=1 || length(traincut)!=1 || is.na(cutoff) || is.na(traincut)).cr_error('INVALID_SPLIT','Comparable ISO cutoffs required')
 if(cutoff>traincut).cr_error('LOOKAHEAD','Inner validation exceeds training information cutoff')
 result<-validation[c('row_ids','role','split_id','known_at')];result$row_ids<-as.list(vi);result
}
.cn_weights <- function(initial,shapes) {
 if(!is.list(initial) || !setequal(names(initial),names(shapes))).cr_error('INVALID_ARCHITECTURE','Initial parameter keys must match architecture')
 weights<-list()
 for(key in names(shapes)) {
  shape<-shapes[[key]];v<-initial[[key]]
  if(length(shape)==2 && !is.matrix(v))v<-.cm_matrix(v) else if(length(shape)==1)v<-unlist(v)
  v<-.cr_numeric(v,key);actual<-if(is.null(dim(v)))length(v) else dim(v)
  if(length(actual)!=length(shape) || !all(actual==shape)).cr_error('INVALID_ARCHITECTURE',paste('Initial shape mismatch for',key))
  weights[[key]]<-v
 }
 weights
}
.cn_frozen_weights <- function(weights)lapply(weights,function(v)if(is.matrix(v)).cm_rows(v) else as.list(unname(v)))
.cn_thaw_weights <- function(weights) {for(key in names(weights))weights[[key]]<-if(endsWith(key,'_weight')).cm_matrix(weights[[key]]) else as.numeric(unlist(weights[[key]]));weights}
fit_neural_pd <- function(X_train,y_train,feature_order,architecture,initial_weights,optimizer_spec,train_row_ids,scale=TRUE,inner_validation=NULL,target,training,model_id,model_version,context=NULL) {
 x<-.cr_glm_design(X_train,feature_order,FALSE)$x;y<-.cm_y(y_train,nrow(x));ids<-.cn_ids(train_row_ids,nrow(x));contract<-.cn_validation(inner_validation,ids,training)
 if(!is.list(architecture)).cr_error('INVALID_ARCHITECTURE','Architecture list required')
 hidden<-if(!length(architecture$hidden_units))numeric() else .cr_numeric(unlist(architecture$hidden_units),"hidden_units",vector_only=TRUE);activations<-as.character(unlist(architecture$activations))
 if(length(hidden)!=length(activations) || any(hidden<1) || any(hidden!=floor(hidden)) || is.logical(hidden) || any(!activations %in% c('tanh','relu','sigmoid'))).cr_error('INVALID_ARCHITECTURE','Positive hidden units and matching tanh/relu/sigmoid activations required')
 arch<-list(kind='mlp',hidden_units=as.list(as.integer(hidden)),activations=as.list(activations));sizes<-c(ncol(x),hidden,1);shapes<-list()
 for(k in seq_len(length(sizes)-1L)) {shapes[[paste0('layer',k-1,'_weight')]]<-sizes[c(k,k+1)];shapes[[paste0('layer',k-1,'_bias')]]<-sizes[k+1]}
 weights<-.cn_weights(initial_weights,shapes)
 if(!is.logical(scale)||length(scale)!=1||is.na(scale)).cr_error('INVALID_INPUT','scale must be boolean')
 center<-if(scale)colMeans(x) else rep(0,ncol(x));scaling<-if(scale)sqrt(colMeans(sweep(x,2,center,'-')^2)) else rep(1,ncol(x));scaling[scaling==0]<-1;z<-sweep(sweep(x,2,center,'-'),2,scaling,'/');validation<-NULL
 if(!is.null(inner_validation)) {vx<-.cr_glm_design(inner_validation$X,feature_order,FALSE)$x;vy<-.cm_y(inner_validation$y,nrow(vx),require_both=FALSE);validation<-list(inputs=sweep(sweep(vx,2,center,'-'),2,scaling,'/'),y=vy)}
 fitted<-.cn_optimize(weights,arch,z,y,optimizer_spec,validation);diagnostics<-c(fitted$diagnostics,list(inner_validation=contract,train_row_ids=as.list(ids)))
 .cm_model('fit_neural_pd',feature_order,list(kind='neural',architecture=arch,weights=.cn_frozen_weights(fitted$weights)),list(transformation='standardize',center=as.list(center),scale=as.list(scaling)),diagnostics,target,training,model_id,model_version,context)
}
.cn_sequences <- function(records,features,include_time_gaps,time_gap_scale) {
 if(!is.list(records) || !length(records)).cr_error('INVALID_SEQUENCE','Nonempty sequence record list required')
 output<-metadata<-list()
 for(record in records) {
  if(!is.list(record)).cr_error('INVALID_SEQUENCE','Every sequence must be a record')
  raw<-record$values;if(!is.matrix(raw))raw<-.cm_matrix(raw);values<-.cr_glm_design(raw,features,FALSE)$x;times<-.cr_numeric(unlist(record$observation_times),'observation_times',vector_only=TRUE);known<-.cr_numeric(unlist(record$known_times),'known_times',vector_only=TRUE);mask<-.cr_numeric(unlist(record$mask),'mask',vector_only=TRUE);forecast<-.cm_number(record$forecast_time,'forecast_time',lower=-Inf)
  if(any(c(length(times),length(known),length(mask))!=nrow(values)) || any(!mask %in% c(0,1)) || any(known<times)).cr_error('INVALID_SEQUENCE','Aligned times/known_times/mask required; known time cannot precede observation')
  valid<-mask==1 & times<=forecast & known<=forecast;t<-times[valid];v<-values[valid,,drop=FALSE]
  if(!length(t) || any(diff(t)<=0)).cr_error('INVALID_SEQUENCE','Causally available observations in strictly increasing order required')
  if(include_time_gaps)v<-cbind(v,c(0,diff(t))/time_gap_scale)
  output[[length(output)+1L]]<-v;metadata[[length(metadata)+1L]]<-list(forecast_time=forecast,used_steps=nrow(v),excluded_steps=sum(!valid),last_observation_time=tail(t,1))
 }
 list(inputs=output,metadata=metadata)
}
fit_sequence_pd <- function(sequences,y_train,feature_order,variant,hidden_size,initial_weights,optimizer_spec,train_row_ids,label_known_times,training_as_of_time,include_time_gaps=TRUE,time_gap_scale=1,scale=TRUE,inner_validation=NULL,target,training,model_id,model_version,context=NULL) {
 if(!variant %in% c('gru','lstm') || length(hidden_size)!=1 || is.logical(hidden_size) || hidden_size<1 || hidden_size!=floor(hidden_size)).cr_error('INVALID_ARCHITECTURE','GRU/LSTM and positive integer hidden_size required')
 if(!is.logical(include_time_gaps)||length(include_time_gaps)!=1||is.na(include_time_gaps)||!is.logical(scale)||length(scale)!=1||is.na(scale)).cr_error('INVALID_INPUT','Time-gap and scale flags must be boolean')
 gapscale<-.cm_number(time_gap_scale,'time_gap_scale',strict=TRUE);records<-.cn_sequences(sequences,feature_order,include_time_gaps,gapscale);inputs<-records$inputs;recordmeta<-records$metadata;y<-.cm_y(y_train,length(inputs));ids<-.cn_ids(train_row_ids,length(y));asof<-.cm_number(training_as_of_time,'training_as_of_time',lower=-Inf);known<-.cr_numeric(unlist(label_known_times),'label_known_times',vector_only=TRUE)
 if(length(known)!=length(y)).cr_error('INVALID_SEQUENCE','One label known time per row required')
 forecast<-vapply(recordmeta,`[[`,numeric(1),'forecast_time');horizon<-target$horizon
 if(any(known>asof) || any(known<forecast) || any(y==0 & known<forecast+horizon)).cr_error('LOOKAHEAD','Labels must be observed by cutoff; nondefaults require full horizon maturity')
 arch<-list(kind='recurrent',variant=variant,hidden_size=as.integer(hidden_size),gate_order=as.list(if(variant=='gru')c('r','z','n') else c('i','f','g','o')),gru_reset='AFTER_HIDDEN_AFFINE',initial_state='ZERO_PER_SEQUENCE',include_time_gaps=include_time_gaps,time_gap_scale=gapscale)
 p<-length(feature_order)+as.integer(include_time_gaps);h<-hidden_size;g<-if(variant=='gru')3L else 4L;shapes<-list(input_weight=c(p,g*h),hidden_weight=c(h,g*h),input_bias=g*h,hidden_bias=g*h,output_weight=c(h,1),output_bias=1)
 weights<-.cn_weights(initial_weights,shapes);pooled<-do.call(rbind,inputs);center<-if(scale)colMeans(pooled) else rep(0,p);scaling<-if(scale)sqrt(colMeans(sweep(pooled,2,center,'-')^2)) else rep(1,p);scaling[scaling==0]<-1;transform<-function(v)sweep(sweep(v,2,center,'-'),2,scaling,'/');inputs<-lapply(inputs,transform);validation<-NULL;contract<-.cn_validation(inner_validation,ids,training)
 if(!is.null(inner_validation)) {
  vr<-.cn_sequences(inner_validation$sequences,feature_order,include_time_gaps,gapscale);vy<-.cm_y(inner_validation$y,length(vr$inputs),require_both=FALSE);vk<-.cr_numeric(unlist(inner_validation$label_known_times),'validation label_known_times',vector_only=TRUE);vf<-vapply(vr$metadata,`[[`,numeric(1),'forecast_time')
  if(length(vk)!=length(vy) || any(vk>asof) || any(vk<vf) || any(vy==0 & vk<vf+horizon)).cr_error('LOOKAHEAD','Inner validation sequence labels need cutoff and horizon maturity')
  validation<-list(inputs=lapply(vr$inputs,transform),y=vy)
 }
 fitted<-.cn_optimize(weights,arch,inputs,y,optimizer_spec,validation);diagnostics<-c(fitted$diagnostics,list(inner_validation=contract,train_row_ids=as.list(ids),sequence_information=recordmeta,label_known_times=as.list(known),training_as_of_time=asof,temporal_contract='ONLY_OBSERVED_AND_KNOWN_BY_FORECAST; NEGATIVE_LABEL_HORIZON_MATURE'))
 .cm_model('fit_sequence_pd',feature_order,list(kind='sequence',architecture=arch,weights=.cn_frozen_weights(fitted$weights)),list(transformation='standardize',center=as.list(center),scale=as.list(scaling)),diagnostics,target,training,model_id,model_version,context)
}
.cn_raw <- function(model,X,feature_order) {
 params<-model$parameters;arch<-params$architecture;weights<-.cn_thaw_weights(params$weights);center<-unlist(model$preprocessing$center);scaling<-unlist(model$preprocessing$scale);transform<-function(x)sweep(sweep(x,2,center,'-'),2,scaling,'/')
 if(params$kind=='neural')return(.cn_mlp(weights,arch,transform(.cr_glm_design(X,feature_order,FALSE)$x)))
 sequences<-.cn_sequences(X,feature_order,arch$include_time_gaps,arch$time_gap_scale);.cn_recurrent(weights,arch,lapply(sequences$inputs,transform))
}
