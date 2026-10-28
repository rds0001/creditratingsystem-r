library(creditratingsystem)
T<-list(event='default',horizon=1,time_unit='years',probability_measure='PHYSICAL',conditioning='performing');C<-list(network_id='synthetic-network',version='1',population_id='synthetic',default_node='C',default_state='bad',known_at='2024-01-01',as_of='2024-01-01',target=T)
node<-function(name,parents,cpt,states=c('good','bad'))list(node_id=name,states=as.list(states),parents=as.list(parents),cpt=cpt,parameter_origin='FIXED',reference='analytic oracle',known_at='2024-01-01')
root<-function(name,p,states=c('good','bad'))node(name,character(),list(list(given=list(),probabilities=as.list(p))),states)
close<-function(a,b,tol=1e-10)stopifnot(max(abs(unlist(a)-unlist(b)))<tol)
reject<-function(expr,code){e<-tryCatch(force(expr),error=identity);stopifnot(inherits(e,'error'),grepl(code,e$code,fixed=TRUE))}
a<-root('A',c(.2,.3,.5),c('low','medium','high'));b<-root('B',c(.7,.3));cpt<-list();i<-0L
for(bv in c('good','bad'))for(av in c('low','medium','high')){i<-i+1L;p<-c(.01,.1,.3,.2,.5,.9)[i];cpt[[i]]<-list(given=list(B=bv,A=av),probabilities=as.list(c(1-p,p)))}
child<-node('C',c('B','A'),cpt);nodes<-list(child,b,a);fitted<-compile_discrete_bayesian_network(nodes,C,elimination_order=c('A','B','C'))
result<-infer_discrete_credit_network(fitted,list(B='bad'))$value;close(result$evidence_probability,.3);close(result$default_probability,.2*.2+.3*.5+.5*.9)
observed<-infer_discrete_credit_network(fitted,list(B='bad',C='bad'))$value;close(observed$default_probability,1);close(observed$evidence_probability,.3*(.2*.2+.3*.5+.5*.9))
marginal<-infer_discrete_credit_network(fitted,list())$value;close(marginal$default_probability,.7*(.2*.01+.3*.1+.5*.3)+.3*(.2*.2+.3*.5+.5*.9))
bad<-nodes;bad[[3]]$parents<-list('C');reject(compile_discrete_bayesian_network(bad,C),'CYCLE')
fixed<-compile_discrete_bayesian_network(list(root('C',c(1,0))),C);reject(infer_discrete_credit_network(fixed,list(C='bad')),'IMPOSSIBLE')
nodes<-c(list(root('C',c(.9,.1))),lapply(0:29,function(i)root(paste0('E',i),c(1,1e-30))));fitted<-compile_discrete_bayesian_network(nodes,C,max_factor_cells=2);evidence<-as.list(setNames(rep('bad',30),paste0('E',0:29)));result<-infer_discrete_credit_network(fitted,evidence)$value;stopifnot(result$evidence_probability_underflow);close(result$log_evidence_probability,30*log(1e-30));close(result$default_probability,.1)
cpt<-list();for(av in c('good','bad'))for(bv in c('good','bad'))cpt[[length(cpt)+1L]]<-list(given=list(A=av,B=bv),probabilities=list(.5,.5));nodes<-list(root('A',c(.5,.5)),root('B',c(.5,.5)),node('C',c('A','B'),cpt),node('D',c('A','B'),cpt));contract<-C;contract$default_node<-'A';fitted<-compile_discrete_bayesian_network(nodes,contract,elimination_order=c('B','C','D','A'),max_factor_cells=8);reject(infer_discrete_credit_network(fitted,list()),'FACTOR_LIMIT')
