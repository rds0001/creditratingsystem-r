library(creditratingsystem)
for(name in c('.cn_raw','.cn_mlp','.cn_recurrent','.cn_loss'))if(!exists(name))assign(name,getFromNamespace(name,'creditratingsystem'))
public_check<-function(m,x,features,expected) {stopifnot(max(abs(unlist(predict_pd(m,x,feature_order=features,expected_target=target)$value)-expected))<1e-12);for(z in list(list(offset=0),list(context=list(horizon=2)))) {e<-tryCatch(do.call(predict_pd,c(list(m,x,feature_order=features,expected_target=target),z)),error=identity);stopifnot(inherits(e,'error'),e$code%in%c('OFFSET_MISMATCH','TARGET_MISMATCH'))}}
target<-list(event='default',horizon=1,time_unit='years',probability_measure='PHYSICAL',conditioning='performing');meta<-list(target=target,training=list(data_fingerprint='neural-analytic',split_id='train',known_at='2025-01-01',data_origin='SYNTHETIC'),model_id='neural',model_version='1')
fit<-function(fun,...)do.call(fun,c(list(...),meta));close<-function(a,b,tol=1e-9)stopifnot(max(abs(a-b))<tol)
optimizer<-function(n,epochs=30L)list(algorithm='adam',learning_rate=.03,beta1=.9,beta2=.999,epsilon=1e-8,l2=.001,batch_size=n,epoch_orders=rep(list(as.list(0:(n-1))),epochs))
rw<-function(variant,p=1L,h=2L) {g<-if(variant=='gru')3L else 4L;list(input_weight=matrix(seq(-.2,.3,length.out=p*g*h),p,g*h),hidden_weight=matrix(seq(.25,-.15,length.out=h*g*h),h,g*h),input_bias=seq(-.1,.2,length.out=g*h),hidden_bias=seq(.15,-.1,length.out=g*h),output_weight=matrix(seq(.2,-.3,length.out=h),h,1),output_bias=.1)}
for(variant in c('gru','lstm')) {
 w<-rw(variant);arch<-list(kind='recurrent',variant=variant,hidden_size=2L);x<-list(matrix(c(.2,.7),ncol=1),matrix(c(-.3,.5),ncol=1));y<-c(0,1);result<-.cn_recurrent(w,arch,x,y)
 for(key in names(w))for(i in seq_along(w[[key]])) {old<-w[[key]][i];w[[key]][i]<-old+1e-5;up<-.cn_loss(.cn_recurrent(w,arch,x),y);w[[key]][i]<-old-1e-5;down<-.cn_loss(.cn_recurrent(w,arch,x),y);w[[key]][i]<-old;close(result$gradient[[key]][i],(up-down)/2e-5,1e-10)}
}
w<-list(layer0_weight=matrix(c(.2,-.1),1,2),layer0_bias=c(.3,-.2),layer1_weight=matrix(c(.4,-.6),2,1),layer1_bias=.1);arch<-list(kind='mlp',hidden_units=2L,activations='tanh');x<-matrix(c(-1,.3,2),ncol=1);y<-c(0,1,1)
result<-.cn_mlp(w,arch,x,y);expected<-tanh(sweep(x%*%w$layer0_weight,2,w$layer0_bias,'+'))%*%w$layer1_weight+w$layer1_bias;close(result$eta,as.vector(expected))
for(key in names(w))for(i in seq_along(w[[key]])) {old<-w[[key]][i];w[[key]][i]<-old+1e-5;up<-.cn_loss(.cn_mlp(w,arch,x),y);w[[key]][i]<-old-1e-5;down<-.cn_loss(.cn_mlp(w,arch,x),y);w[[key]][i]<-old;close(result$gradient[[key]][i],(up-down)/2e-5,1e-10)}
m<-fit(fit_neural_pd,x,y,feature_order='x',architecture=arch,initial_weights=w,optimizer_spec=optimizer(3L),train_row_ids=c('a','b','c'))
stopifnot(tail(m$diagnostics$loss_path,1)[[1]]$train_log_loss<m$diagnostics$initial_train_log_loss-.15,!m$diagnostics$global_optimum_claimed)
public_check(m,x,'x',plogis(.cn_raw(m$model,x,'x')))
records<-function()lapply(c(-1,-.5,.5,1),function(a)list(values=matrix(c(a,a+.2),ncol=1),observation_times=c(0,1),known_times=c(0,1),mask=c(1,1),forecast_time=1.5))
for(variant in c('gru','lstm')) {
 rows<-records();w<-rw(variant,p=2L)
 m<-fit(fit_sequence_pd,rows,c(0,0,1,1),feature_order='x',variant=variant,hidden_size=2L,initial_weights=w,optimizer_spec=optimizer(4L,50L),train_row_ids=letters[1:4],label_known_times=rep(3,4),training_as_of_time=3)
 stopifnot(tail(m$diagnostics$loss_path,1)[[1]]$train_log_loss<m$diagnostics$initial_train_log_loss-.1)
 initial<-.cn_raw(m$model,rows,'x');public_check(m,rows,'x',plogis(initial));altered<-lapply(rows,function(row) {row$values<-rbind(row$values,999,-999);row$observation_times<-c(row$observation_times,2,3);row$known_times<-c(row$known_times,2,3);row$mask<-c(row$mask,1,0);row})
 close(.cn_raw(m$model,altered,'x'),initial,1e-15);close(.cn_raw(m$model,rows[c(4,1)],'x'),initial[c(4,1)],1e-15)
}
x<-matrix(c(-2,-1,1,2),ncol=1);y<-c(0,0,1,1);w<-list(layer0_weight=matrix(.1,1,1),layer0_bias=0);arch<-list(hidden_units=list(),activations=list());inner<-list(X=x,y=1-y,row_ids=letters[23:26],role='inner_validation',split_id='inner',known_at='2024-12-31');opt<-optimizer(4L,20L);opt$early_stopping_patience<-3L
m<-fit(fit_neural_pd,x,y,feature_order='x',architecture=arch,initial_weights=w,optimizer_spec=opt,train_row_ids=letters[1:4],inner_validation=inner)
stopifnot(m$diagnostics$selected_epoch==0,m$diagnostics$executed_epochs==3,m$model$preprocessing$center[[1]]==0)
for(variant in c('gru','lstm')) {
 gates<-if(variant=='gru')3L else 4L;w<-list(input_weight=matrix(0,1,gates),hidden_weight=matrix(0,1,gates),input_bias=rep(0,gates),hidden_bias=rep(0,gates),output_weight=matrix(1,1,1),output_bias=0);w$input_weight[1,3]<-1
 actual<-.cn_recurrent(w,list(kind='recurrent',variant=variant,hidden_size=1L),list(matrix(c(1,2),ncol=1)));state<-.25*tanh(1)+.5*tanh(2);expected<-if(variant=='gru')state else .5*tanh(state);close(actual,expected,1e-15)
}
