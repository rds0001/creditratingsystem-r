compute_credit_ale<-function(predictor,X,model_contract,selected_features,bins,feature_types,weights=NULL,min_bin_count=1,context=NULL){
 checked<-.cr_ex_contract(model_contract,context);features<-checked$features;ctx<-checked$context;selected<-.cr_pfs_ids(selected_features,'selected_features');x<-.cr_pfs_matrix(X,'X');order<-length(selected)
 if(!order%in%c(1,2)||any(!selected%in%features)||ncol(x)!=length(features)) .cr_error('ALE_SHAPE','One or two selected features and aligned reference matrix required')
 if(!.cr_object(feature_types)||any(vapply(selected,function(f)!identical(feature_types[[f]],'continuous'),logical(1)))) .cr_error('ALE_FEATURE_TYPE','Selected numeric features require explicit continuous type; no categorical interpolation')
 if(!.cr_object(bins)||!setequal(names(bins),selected)||length(min_bin_count)!=1||!is.numeric(min_bin_count)||!is.finite(min_bin_count)||min_bin_count<1||min_bin_count!=floor(min_bin_count)) .cr_error('ALE_BINS','Explicit bins for selected features and positive support count required')
 w<-.cr_pf_weights(if(is.null(weights))rep(1,nrow(x))else weights,nrow(x));edges<-indices<-coordinates<-list();positions<-match(selected,features)
 for(i in seq_along(selected)){j<-positions[i];b<-.cr_numeric(unlist(bins[[selected[i]]]),'bins',vector_only=TRUE)
  if(length(b)<2||any(diff(b)<=0)||any(!is.finite(diff(b)))||any(x[,j]<b[1])||any(x[,j]>tail(b,1))) .cr_error('ALE_BINS','Increasing finite edges must cover all reference values')
  k<-pmin(findInterval(x[,j],b),length(b)-1L);t<-(x[,j]-b[k])/(b[k+1]-b[k]);edges[[i]]<-b;indices[[i]]<-k;coordinates[[i]]<-t
 }
 shape<-vapply(edges,length,integer(1))-1L;dims<-if(order==1)c(shape,1L)else shape;effects<-matrix(NA_real_,dims[1],dims[2]);counts<-matrix(0L,dims[1],dims[2]);masses<-matrix(0.,dims[1],dims[2]);unsupported<-list()
 for(a in seq_len(dims[1]))for(b in seq_len(dims[2])){
  member<-indices[[1]]==a;if(order==2)member<-member&indices[[2]]==b;usable<-member&w>0;counts[a,b]<-sum(usable);masses[a,b]<-.cr_pfs_sum(w[usable])
  if(counts[a,b]<min_bin_count||masses[a,b]<=0){unsupported[[length(unsupported)+1L]]<-if(order==1)list(a-1L)else list(a-1L,b-1L);next}
  z<-x[usable,,drop=FALSE];cw<-w[usable]/masses[a,b]
  if(order==1){low<-high<-z;low[,positions[1]]<-edges[[1]][a];high[,positions[1]]<-edges[[1]][a+1L];delta<-.cr_ex_predict(predictor,high,model_contract)-.cr_ex_predict(predictor,low,model_contract)}else{
   results<-list();for(da in 0:1)for(db in 0:1){corner<-z;corner[,positions[1]]<-edges[[1]][a+da];corner[,positions[2]]<-edges[[2]][b+db];results[[paste0(da,db)]]<-.cr_ex_predict(predictor,corner,model_contract)}
   delta<-results[['11']]-results[['10']]-results[['01']]+results[['00']]
  };effects[a,b]<-.cr_pfs_sum(cw*delta)
 }
 curve<-reference<-center<-purification<-NULL
 if(!length(unsupported)){
  if(order==1){accumulated<-c(0,cumsum(effects[,1]));raw<-accumulated[indices[[1]]]+coordinates[[1]]*effects[indices[[1]],1];center<-.cr_pfs_sum(w*raw);curve<-as.list(accumulated-center);reference<-as.list(raw-center)}else{
   accumulated<-matrix(0,dims[1]+1L,dims[2]+1L);double<-effects;for(j in seq_len(ncol(double)))double[,j]<-cumsum(double[,j]);for(i in seq_len(nrow(double)))double[i,]<-cumsum(double[i,]);accumulated[-1,-1]<-double
   surface<-function(ki,kj,ti,tj)(1-ti)*(1-tj)*accumulated[cbind(ki,kj)]+ti*(1-tj)*accumulated[cbind(ki+1L,kj)]+(1-ti)*tj*accumulated[cbind(ki,kj+1L)]+ti*tj*accumulated[cbind(ki+1L,kj+1L)]
   ki<-indices[[1]];kj<-indices[[2]];ti<-coordinates[[1]];tj<-coordinates[[2]];dx<-numeric(shape[1]);dy<-numeric(shape[2])
   for(k in seq_len(shape[1])){mask<-ki==k&w>0;cw<-w[mask]/.cr_pfs_sum(w[mask]);dx[k]<-.cr_pfs_sum(cw*(surface(ki[mask],kj[mask],rep(1,sum(mask)),tj[mask])-surface(ki[mask],kj[mask],rep(0,sum(mask)),tj[mask])))}
   for(k in seq_len(shape[2])){mask<-kj==k&w>0;cw<-w[mask]/.cr_pfs_sum(w[mask]);dy[k]<-.cr_pfs_sum(cw*(surface(ki[mask],kj[mask],ti[mask],rep(1,sum(mask)))-surface(ki[mask],kj[mask],ti[mask],rep(0,sum(mask)))))}
   gx<-c(0,cumsum(dx));gy<-c(0,cumsum(dy));raw<-surface(ki,kj,ti,tj)-(gx[ki]+ti*dx[ki])-(gy[kj]+tj*dy[kj]);center<-.cr_pfs_sum(w*raw);curve<-.cr_pfs_rows(sweep(sweep(accumulated,1,gx,'-'),2,gy,'-')-center);reference<-as.list(raw-center);purification<-list(first_axis_accumulated=as.list(gx),second_axis_accumulated=as.list(gy))
  }
 }
 local<-lapply(seq_len(dims[1]),function(i)lapply(seq_len(dims[2]),function(j)if(is.finite(effects[i,j]))effects[i,j]else NULL));if(order==1)local<-lapply(seq_len(dims[1]),function(i)local[[i]][[1]])
 selected_values<-x[,positions,drop=FALSE];means<-colSums(selected_values*w);centered<-sweep(selected_values,2,means,'-');cov<-crossprod(centered,centered*w);sd<-sqrt(diag(cov));corr<-lapply(seq_len(order),function(i)lapply(seq_len(order),function(j)if(sd[i]==0||sd[j]==0)NULL else cov[i,j]/(sd[i]*sd[j])))
 .cr_result(list(status=if(length(unsupported))'NON_ESTIMABLE_SUPPORT'else'ESTIMABLE',order=order,selected_features=as.list(selected),bin_edges=stats::setNames(lapply(edges,as.list),selected),positive_weight_counts=if(order==1)as.list(counts[,1])else .cr_pfs_rows(counts),bin_masses=if(order==1)as.list(masses[,1])else .cr_pfs_rows(masses),local_differences=local,unsupported_cells=unsupported,centered_ale_grid=curve,reference_ale=reference,centering_constant=center,purification=purification,weighted_selected_correlation=corr,min_bin_count=min_bin_count,interpolation='WITHIN_BIN_LINEAR_OR_BILINEAR_NO_GAP_FILLING'),'explainability.compute_credit_ale',model_contract$output_scale,ctx,diagnostics=list(model_contract_hash=.cr_hash(model_contract),reference_hash=.cr_hash(list(X=.cr_pfs_rows(x),weights=as.list(w))),bins_hash=.cr_hash(bins)),limitations='Local finite differences use frozen predictions at declared bin boundaries. First-order effects are accumulated with within-bin linear interpolation and weighted empirical centering. Second-order effects use mixed differences, double accumulation and removal of both first-order accumulated components before centering. Empty or insufficient positive-weight cells make the global curve non-estimable; supported local differences remain available. Correlation and counts do not establish off-manifold boundary support or causal effects. Output effects use the declared prediction scale.')
}
