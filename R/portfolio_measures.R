# Canonical identifier order uses UTF-8 radix collation; numeric loss ordering is unchanged.
.cr_pf_ids<-function(x,name) .cr_loss_ids(x,name)
.cr_pf_weights<-function(weights,n){w<-.cr_numeric(weights,'weights',vector_only=TRUE);if(length(w)!=n||any(w<0)||!any(w>0)) .cr_error('PORTFOLIO_WEIGHTS','Aligned nonnegative weights with positive mass required');original<-w;w<-w/max(w);w<-w/sum(w);if(any(original>0&w==0)) .cr_error('NUMERICAL_FAILURE','Positive scenario weight underflowed');w}
.cr_pf_mean<-function(x,w){scale<-max(abs(x));value<-if(scale==0)0 else scale*sum((x/scale)*w);if(!is.finite(value)) .cr_error('NUMERICAL_FAILURE','Portfolio mean exceeds finite representation');value}
.cr_pf_atoms<-function(loss,w){o<-which(w>0);o<-o[order(loss[o])];x<-loss[o];starts<-c(1L,which(diff(x)!=0)+1L);ends<-c(starts[-1L]-1L,length(x));list(support=x[starts],masses=vapply(seq_along(starts),function(i)sum(w[o[seq.int(starts[i],ends[i])]]),0.))}
.cr_pf_tail<-function(loss,w,level){
 level<-.cr_numeric(level,'level');if(length(level)!=1||level<0||level>1) .cr_error('PORTFOLIO_LEVEL','Tail level must be in [0,1]')
 atoms<-.cr_pf_atoms(loss,w);cdf<-cumsum(atoms$masses);cdf[length(cdf)]<-1;index<-which(cdf>=level)[1];var<-atoms$support[index];tail<-numeric(length(loss));equal<-loss==var
 if(level==1){tail[equal]<-w[equal]/sum(w[equal])}else{above<-loss>var;tail[above]<-w[above];remaining<-max(0,1-level-sum(tail));tail[equal]<-w[equal]*(remaining/sum(w[equal]));tail<-tail/(1-level)}
 list(var=var,es=.cr_pf_mean(loss,tail),weights=tail)
}
credit_loss_distribution<-function(loss_paths,weights,scenario_ids,levels=c(.95,.99),currency,context=NULL){
 ids<-.cr_pf_ids(scenario_ids,'scenario_ids');loss<-.cr_numeric(loss_paths,'loss_paths',vector_only=TRUE)
 if(length(loss)!=length(ids)) .cr_error('PORTFOLIO_SHAPE','One loss per scenario required')
 w<-.cr_pf_weights(weights,length(ids));ctx<-.cr_result(NULL,'portfolio.context',context=context)$context;.cr_string(currency,'currency')
 if(!is.null(ctx$currency)&&ctx$currency!=currency) .cr_error('TARGET_MISMATCH','Explicit matching loss currency required')
 levels<-.cr_numeric(levels,'levels',vector_only=TRUE);if(anyDuplicated(levels)) .cr_error('PORTFOLIO_LEVEL','Distinct levels required')
 tails<-lapply(levels,function(level){t<-.cr_pf_tail(loss,w,level);list(level=level,value_at_risk=t$var,expected_shortfall=t$es,normalized_tail_weights=as.list(t$weights))})
 average<-.cr_pf_mean(loss,w);scale<-max(abs(loss));sd<-if(scale==0)0 else scale*sqrt(sum(w*(loss/scale-average/scale)^2));if(!is.finite(sd)) .cr_error('NUMERICAL_FAILURE','Portfolio standard deviation exceeds finite representation')
 atoms<-.cr_pf_atoms(loss,w);atoms<-lapply(seq_along(atoms$support),function(i)list(loss=atoms$support[i],probability=atoms$masses[i]))
 .cr_result(list(scenario_ids=as.list(ids),loss_paths=as.list(loss),normalized_weights=as.list(w),currency=currency,expected_loss=average,standard_deviation=sd,atoms=atoms,tails=tails,quantile_convention='LEFT_INVERSE_CDF',tail_convention='UPPER_MASS_PROPORTIONAL_BOUNDARY_TIES_LEVEL_ONE_LIMIT'),'portfolio.credit_loss_distribution',currency,ctx,diagnostics=list(paths_hash=.cr_hash(list(scenario_ids=as.list(ids),loss_paths=as.list(loss),weights=as.list(w)))),limitations='Supplied weighted distribution does not prove independent sampling. No confidence interval inferred. Signed losses retained. ES takes exactly upper 1-alpha mass with proportional boundary ties; alpha=1 is maximum-loss limit.')
}
allocate_expected_shortfall<-function(position_loss_paths,weights,scenario_ids,position_ids,level,currency,groups=NULL,context=NULL){
 ids<-.cr_pf_ids(position_ids,'position_ids');scenarios<-.cr_pf_ids(scenario_ids,'scenario_ids');x<-.cr_numeric(position_loss_paths,'position_loss_paths')
 if(!is.matrix(x)||!identical(dim(x),c(length(scenarios),length(ids)))) .cr_error('PORTFOLIO_SHAPE','Scenario-by-position monetary loss matrix required')
 total<-apply(x,1,sum);distribution<-credit_loss_distribution(total,weights,scenarios,levels=level,currency=currency,context=context)$value;tail<-distribution$tails[[1]];tw<-unlist(tail$normalized_tail_weights);contributions<-vapply(seq_along(ids),function(j).cr_pf_mean(x[,j],tw),0.);group_results<-list()
 if(!is.null(groups)){
 if(!.cr_object(groups)||!setequal(names(groups),ids)||any(!vapply(groups,function(v)is.character(v)&&length(v)==1&&!is.na(v)&&nzchar(v),logical(1)))) .cr_error('PORTFOLIO_GROUP','Exactly one explicit group per position required')
 group_results<-lapply(sort(enc2utf8(as.character(unique(unlist(groups)))),method='radix'),function(g)list(group_id=g,contribution=sum(contributions[vapply(ids,function(p)groups[[p]]==g,logical(1))])))
 }
 residual<-sum(contributions)-tail$expected_shortfall;if(abs(residual)>1e-12*max(1,max(abs(contributions)))) .cr_error('NUMERICAL_FAILURE','ES allocation does not reconcile')
 .cr_result(list(level=level,value_at_risk=tail$value_at_risk,expected_shortfall=tail$expected_shortfall,scenario_ids=as.list(scenarios),normalized_tail_weights=tail$normalized_tail_weights,contributions=lapply(seq_along(ids),function(j)list(position_id=ids[j],contribution=contributions[j])),groups=group_results,reconciliation_residual=residual),'portfolio.allocate_expected_shortfall',currency,context,diagnostics=list(position_paths_hash=.cr_hash(list(position_ids=as.list(ids),scenario_ids=as.list(scenarios),values=lapply(seq_len(nrow(x)),function(i)as.list(x[i,]))))),limitations='Conditional loss allocation with proportional boundary ties, not a unique derivative at discrete VaR atoms. Signed hedge contributions retained; all positions share the same portfolio tail event.')
}
incremental_credit_risk<-function(base_loss_paths,change_loss_paths,weights,scenario_ids,levels=c(.95,.99),currency,change_id,context=NULL){
 base<-.cr_numeric(base_loss_paths,'base_loss_paths',vector_only=TRUE);change<-.cr_numeric(change_loss_paths,'change_loss_paths',vector_only=TRUE)
 .cr_string(change_id,'change_id');if(length(base)!=length(change)) .cr_error('PORTFOLIO_CHANGE','Aligned paired change paths required')
 before<-credit_loss_distribution(base,weights,scenario_ids,levels,currency,context)$value;after<-credit_loss_distribution(base+change,weights,scenario_ids,levels,currency,context)$value
 changes<-lapply(seq_along(before$tails),function(i){b<-before$tails[[i]];a<-after$tails[[i]];list(level=b$level,value_at_risk_change=a$value_at_risk-b$value_at_risk,expected_shortfall_change=a$expected_shortfall-b$expected_shortfall)})
 .cr_result(list(change_id=change_id,base=before,changed=after,expected_loss_change=after$expected_loss-before$expected_loss,tail_changes=changes),'portfolio.incremental_credit_risk',currency,context,diagnostics=list(change_hash=.cr_hash(as.list(change))),limitations='Paired finite differences on fixed scenarios, not marginal derivatives or causal effects. Add/remove/scale through explicit signed changes, without new random worlds.')
}
credit_concentration_measures<-function(positions,weight_field,group_fields,weight_unit,context=NULL){
 rows<-.cr_data_rows(positions,'positions');fields<-.cr_pf_ids(group_fields,'group_fields');ids<-character();amounts<-numeric();.cr_string(weight_unit,'weight_unit')
 for(row in rows){.cr_data_required(row,c('position_id',weight_field,fields));if(row$position_id%in%ids) .cr_error('PORTFOLIO_IDS','Duplicate position ID');ids<-c(ids,row$position_id);v<-.cr_numeric(row[[weight_field]],weight_field);if(length(v)!=1||v<0) .cr_error('PORTFOLIO_WEIGHT','Finite nonnegative scalar concentration weight required');for(f in fields).cr_string(row[[f]],f);amounts<-c(amounts,v)}
 total<-sum(amounts);if(!is.finite(total)) .cr_error('NUMERICAL_FAILURE','Portfolio sum exceeds finite representation');reports<-list()
 for(field in c('position_id',fields)){
 labels<-vapply(rows,function(r)r[[field]],'');parts<-split(amounts,labels);names<-sort(enc2utf8(as.character(names(parts))),method='radix');mass<-vapply(parts[names],sum,0.);shares<-if(total>0)mass/total else rep(0,length(mass));hhi<-if(total>0)sum(shares^2)else NULL;entropy<-if(total>0)-sum(shares[shares>0]*log(shares[shares>0]))else NULL
 reports[[length(reports)+1L]]<-list(dimension=field,groups=lapply(seq_along(names),function(i)list(group_id=names[i],weight=mass[i],share=shares[i])),hhi=hhi,effective_number=if(is.null(hhi))NULL else 1/hhi,entropy=entropy,entropy_effective_number=if(is.null(entropy))NULL else exp(entropy),largest_share=if(length(shares))max(shares)else 0)
 }
 .cr_result(list(status=if(total>0)'ESTIMABLE'else'ZERO_WEIGHT_DENOMINATOR',weight_field=weight_field,weight_unit=weight_unit,total_weight=total,dimensions=reports),'portfolio.credit_concentration_measures','concentration',context,diagnostics=list(position_hash=.cr_hash(rows)),limitations='Descriptive concentration on explicit nonnegative denominator. Group definitions caller-supplied, not legal connected-client determination. HHI/effective counts are not tail loss or regulatory large-exposure limits.')
}
asymptotic_factor_loss<-function(pd,lgd,asset_correlation,systematic_factor,context=NULL){
 values<-lapply(list(pd=pd,lgd=lgd,asset_correlation=asset_correlation),function(v).cr_numeric(v,'homogeneous parameter'));if(any(lengths(values)!=1)) .cr_error('PORTFOLIO_ASRF','Homogeneous scalar parameters required')
 p<-values$pd;l<-values$lgd;rho<-values$asset_correlation;y<-.cr_numeric(systematic_factor,'systematic_factor',vector_only=TRUE);ctx<-.cr_result(NULL,'portfolio.context',context=context)$context
 if(!is.null(ctx$probability_measure)&&ctx$probability_measure!='PHYSICAL') .cr_error('TARGET_MISMATCH','ASRF benchmark requires physical probabilities')
 if(p<0||p>1||rho<0||rho>=1) .cr_error('PORTFOLIO_ASRF','PD in [0,1] and asset correlation in [0,1) required')
 conditional<-if(p%in%c(0,1)||rho==0)rep(p,length(y))else stats::pnorm((stats::qnorm(p)-sqrt(rho)*y)/sqrt(1-rho));loss<-conditional*l
 .cr_result(list(pd=p,lgd=l,asset_correlation=rho,systematic_factor=as.list(y),conditional_pd=as.list(conditional),conditional_loss_rate=as.list(loss),unconditional_expected_loss_rate=p*l),'portfolio.asymptotic_factor_loss','loss_rate',context,diagnostics=list(factor_hash=.cr_hash(as.list(y))),limitations='Physical homogeneous Gaussian single-factor infinitesimal-exposure benchmark with fixed LGD; adverse factor negative. Latent asset correlation is not default correlation. Not a finite concentrated/multifactor portfolio or regulatory capital calculation.')
}
