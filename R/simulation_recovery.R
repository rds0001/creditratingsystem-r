simulate_recovery_waterfall<-function(default_events,claims,realization_paths,recovery_policy,context=NULL){
 events<-.cr_data_rows(default_events,'default_events');claims<-.cr_data_rows(claims,'claims');paths<-.cr_data_rows(realization_paths,'realization_paths');policy<-recovery_policy
 if(!.cr_object(policy)) .cr_error('RECOVERY_POLICY','Explicit recovery policy required')
 .cr_data_required(policy,c('policy_id','version','currency','allocation','cost_rule','completion'))
 for(k in c('policy_id','version','currency','allocation','cost_rule','completion')) .cr_sim_text(policy[[k]],k)
 if(!(policy$allocation%in%c('LEGAL_CAP','UNCAPPED_PRO_RATA'))||policy$cost_rule!='PROCEEDS_FIRST_EXPLICIT_DEFICIT_SHARES'||policy$completion!='ULTIMATE_COMPLETE') .cr_error('RECOVERY_POLICY','Supported allocation, explicit cost treatment and complete ultimate realization required')
 ctx<-.cr_result(NULL,'simulation.context',context=context)$context
 if(!is.null(ctx$currency)&&ctx$currency!=policy$currency) .cr_error('TARGET_MISMATCH','Recovery currency conflicts with context')
 keyfn<-function(...) .cr_hash(list(...))
 emap<-groups<-pmap<-list();seen<-realizations<-collateral<-subjects<-character()
 for(e in events){
 .cr_data_required(e,c('scenario_id','default_event_id','subject_id','default_date'))
 for(k in c('scenario_id','default_event_id','subject_id')) .cr_sim_text(e[[k]],k)
 .cr_data_date(e$default_date,'default_date');key<-keyfn(e$scenario_id,e$default_event_id);subject<-keyfn(e$scenario_id,e$subject_id)
 if(!is.null(emap[[key]])||subject%in%subjects) .cr_error('RECOVERY_DUPLICATE','Unique first-default event per subject and scenario required')
 subjects<-c(subjects,subject);emap[[key]]<-e
 }
 for(c in claims){
 .cr_data_required(c,c('scenario_id','default_event_id','subject_id','claim_id','pool_id','currency','ead_at_default','legal_balance','priority','deficit_cost_share'))
 for(k in c('scenario_id','default_event_id','subject_id','claim_id','pool_id','currency')) .cr_sim_text(c[[k]],k)
 key<-keyfn(c$scenario_id,c$default_event_id);ck<-keyfn(c$scenario_id,c$claim_id);event<-emap[[key]]
 if(is.null(event)||c$subject_id!=event$subject_id||c$currency!=policy$currency) .cr_error('RECOVERY_EVENT','Claim must match shared event, subject and currency')
 if(ck%in%seen) .cr_error('RECOVERY_DUPLICATE','Claim may occur in only one collateral pool per scenario')
 seen<-c(seen,ck)
 for(k in c('ead_at_default','legal_balance','deficit_cost_share')) .cr_sim_number(c[[k]],k,0)
 .cr_sim_number(c$priority,'priority');g<-keyfn(c$scenario_id,c$default_event_id,c$pool_id);groups[[g]]<-c(groups[[g]],list(c))
 }
 for(p in paths){
 .cr_data_required(p,c('scenario_id','default_event_id','pool_id','currency','realization_id','collateral_id','date','gross_value','realization_fraction','cost','discount_factor','discount_anchor_date'))
 for(k in c('scenario_id','default_event_id','pool_id','currency','realization_id','collateral_id')) .cr_sim_text(p[[k]],k)
 key<-keyfn(p$scenario_id,p$default_event_id,p$pool_id);event<-emap[[keyfn(p$scenario_id,p$default_event_id)]]
 if(is.null(groups[[key]])||is.null(event)||p$currency!=policy$currency||p$discount_anchor_date!=event$default_date) .cr_error('RECOVERY_EVENT','Realization must match event/pool/currency and default-date discount anchor')
 if(.cr_data_date(p$date,'date')<.cr_data_date(event$default_date,'default_date')) .cr_error('RECOVERY_DATE','Recovery cannot precede shared default')
 rid<-keyfn(p$scenario_id,p$realization_id);cid<-keyfn(p$scenario_id,p$collateral_id)
 if(rid%in%realizations||cid%in%collateral) .cr_error('RECOVERY_DUPLICATE','Each realization and collateral asset may be consumed once per scenario')
 realizations<-c(realizations,rid);collateral<-c(collateral,cid)
 for(k in c('gross_value','realization_fraction','cost','discount_factor')) .cr_sim_number(p[[k]],k,0)
 if(p$realization_fraction>1||p$discount_factor<=0) .cr_error('RECOVERY_VALUE','Realization fraction in [0,1] and positive discount factor required')
 pmap[[key]]<-c(pmap[[key]],list(p))
 }
 if(!setequal(names(groups),names(pmap))) .cr_error('RECOVERY_COMPLETENESS','Every claim pool needs explicit complete realization evidence, including zero recovery')
 covered<-vapply(groups,function(cs)keyfn(cs[[1]]$scenario_id,cs[[1]]$default_event_id),'')
 if(!setequal(covered,names(emap))) .cr_error('RECOVERY_COMPLETENESS','Every supplied default event must have claims and a complete recovery path')
 out<-ledger<-pools<-list()
 orderkeys<-if(length(groups))names(groups)[order(vapply(groups,function(cs)cs[[1]]$scenario_id,''),vapply(groups,function(cs)cs[[1]]$default_event_id,''),vapply(groups,function(cs)cs[[1]]$pool_id,''))]else character()
 for(key in orderkeys){
 cs<-groups[[key]];first<-cs[[1]];shares<-vapply(cs,function(c)c$deficit_cost_share,0.);ranks<-vapply(cs,function(c)c$priority,0.);balances<-vapply(cs,function(c)c$legal_balance,0.)
 if(!is.finite(sum(shares))||abs(sum(shares)-1)>1e-12) .cr_error('RECOVERY_COST','Explicit deficit cost shares must sum to one in each pool')
 shares<-shares/sum(shares)
 if(!is.finite(sum(balances))) .cr_error('NUMERICAL_FAILURE','Recovery totals exceed finite representation')
 if(policy$allocation=='UNCAPPED_PRO_RATA'&&(length(unique(ranks))!=1||sum(balances)<=0)) .cr_error('RECOVERY_POLICY','Uncapped participation requires one rank and positive legal allocation weights')
 pv<-received<-funded<-rep(0,length(cs));surplus<-gross_sum<-cost_sum<-0;ps<-pmap[[key]];ps<-ps[order(vapply(ps,function(p)p$date,''),vapply(ps,function(p)p$realization_id,''))]
 for(p in ps){
 gross<-p$gross_value*p$realization_fraction;cost<-p$cost;available<-max(gross-cost,0);deficit<-max(cost-gross,0);allocated<-rep(0,length(cs))
 if(policy$allocation=='UNCAPPED_PRO_RATA'){allocated<-available*(balances/sum(balances));available<-0}else{
 for(rank in sort(unique(ranks))){idx<-which(ranks==rank);total<-sum(balances[idx]);amount<-min(available,total);if(total>0){allocated[idx]<-amount*(balances[idx]/total);balances[idx]<-pmax(balances[idx]-allocated[idx],0)};available<-max(available-amount,0)}
 }
 for(i in seq_along(cs)){
 c<-cs[[i]];contribution<-deficit*shares[i];net<-allocated[i]-contribution;present<-net*p$discount_factor;received[i]<-received[i]+allocated[i];funded[i]<-funded[i]+contribution;pv[i]<-pv[i]+present
 ledger[[length(ledger)+1L]]<-list(scenario_id=c$scenario_id,default_event_id=c$default_event_id,pool_id=c$pool_id,claim_id=c$claim_id,realization_id=p$realization_id,date=p$date,gross_allocation=allocated[i],deficit_cost_contribution=contribution,net_recovery=net,discount_factor=p$discount_factor,discounted_net_recovery=present)
 }
 surplus<-surplus+available;gross_sum<-gross_sum+gross;cost_sum<-cost_sum+cost
 }
 for(i in seq_along(cs)){
 c<-cs[[i]];ead<-c$ead_at_default;loss<-ead-pv[i]
 if(any(!is.finite(c(pv[i],loss,received[i],funded[i])))) .cr_error('NUMERICAL_FAILURE','Recovery exceeds finite representation')
 lgd<-if(ead==0)NULL else loss/ead
 if(!is.null(lgd)&&!is.finite(lgd)) .cr_error('NUMERICAL_FAILURE','LGD exceeds finite representation')
 out[[length(out)+1L]]<-list(scenario_id=c$scenario_id,default_event_id=c$default_event_id,subject_id=c$subject_id,claim_id=c$claim_id,pool_id=c$pool_id,currency=policy$currency,default_date=emap[[keyfn(c$scenario_id,c$default_event_id)]]$default_date,ead_at_default=ead,gross_recovery=received[i],deficit_cost_contribution=funded[i],discounted_net_recovery=pv[i],economic_loss=loss,lgd=lgd,ultimate_complete=TRUE)
 }
 if(any(!is.finite(c(gross_sum,cost_sum,surplus,sum(received),sum(funded))))) .cr_error('NUMERICAL_FAILURE','Pool totals exceed finite representation')
 pools[[length(pools)+1L]]<-list(scenario_id=first$scenario_id,default_event_id=first$default_event_id,pool_id=first$pool_id,realized_proceeds=gross_sum,costs=cost_sum,creditor_gross_recovery=sum(received),creditor_deficit_funding=sum(funded),unallocated_surplus=surplus)
 }
 .cr_result(list(claim_results=out,allocation_ledger=ledger,pool_results=pools,policy=policy),'simulation.simulate_recovery_waterfall','recovery_waterfall',ctx,diagnostics=list(input_fingerprint=.cr_hash(list(default_events=events,claims=claims,realization_paths=paths))),limitations='Ultimate completeness and legal priority/entitlement are supplied evidence. Each collateral identity is realized once per scenario; split sales require nonoverlapping identities. Legal caps apply to undiscounted claims; EAD is a separate loss denominator. Costs paid once from proceeds, only deficits charged to creditor shares. Raw LGD uncapped and undefined at zero EAD. No independent recovery/default resampling.')
}
