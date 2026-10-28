# Dated drawn exposure, conversion factors and counterparty exposure paths.
exposure_schedule <- function(contract,events,annual_interest_rates,period_end_dates,day_count,default_timing,allow_overlimit,currency,context=NULL) {
 ctx<-.cr_lgd_currency(currency,context);if(!.cr_object(contract)) .cr_error('EXPOSURE_CONTRACT','Explicit facility snapshot required');.cr_data_required(contract,c('facility_id','currency','as_of','drawn_principal','credit_limit','accrued_interest'));if(!identical(contract$currency,currency)) .cr_error('TARGET_MISMATCH','Facility currency differs')
 start<-.cr_data_date(contract$as_of,'as_of');principal<-.cr_lgd_scalar(contract$drawn_principal,'drawn_principal',0);limit<-.cr_lgd_scalar(contract$credit_limit,'credit_limit',0);interest<-.cr_lgd_scalar(contract$accrued_interest,'accrued_interest',0)
 .cr_string(day_count,'day_count','EXPOSURE_CONVENTION');.cr_string(default_timing,'default_timing','EXPOSURE_CONVENTION')
 if(!(day_count %in% c('ACT/365F','ACT/360'))) .cr_error('EXPOSURE_CONVENTION','Choose ACT/365F or ACT/360 for simple annual interest')
 if(!(default_timing %in% c('BEFORE_PERIOD_END_EVENTS','AFTER_PERIOD_END_EVENTS'))||!is.logical(allow_overlimit)||length(allow_overlimit)!=1L||is.na(allow_overlimit)) .cr_error('EXPOSURE_CONVENTION','Declare default timing and Boolean overlimit permission')
 if(is.list(period_end_dates)&&is.null(names(period_end_dates)))period_end_dates<-unlist(period_end_dates,use.names=FALSE);if(!is.character(period_end_dates)||!length(period_end_dates)) .cr_error('EXPOSURE_DATE','Explicit increasing period-end dates required');dates<-lapply(period_end_dates,.cr_data_date,name='period_end_date')
 if(dates[[1]]<=start||any(diff(as.numeric(do.call(c,dates)))<=0)) .cr_error('EXPOSURE_DATE','Period ends must increase after snapshot')
 rates<-.cr_numeric(annual_interest_rates,'annual_interest_rates',vector_only=TRUE);if(length(rates)!=length(dates)||any(rates<0)) .cr_error('EXPOSURE_RATE','One finite nonnegative simple annual rate per period required')
 entries<-.cr_data_rows(events,'events');.cr_data_unique(entries,'event_id','events');seen<-character()
 for(row in entries) {
  .cr_data_required(row,c('event_date','known_at','sequence','kind','amount'));date<-.cr_data_date(row$event_date,'event_date');known<-.cr_data_date(row$known_at,'known_at');sequence<-.cr_data_days(row$sequence,'sequence');.cr_lgd_scalar(row$amount,'event amount',0)
  if(date<=start||date>dates[[length(dates)]]||known>start) .cr_error('EXPOSURE_DATE','Scheduled events must follow snapshot, fit horizon and already be known at snapshot')
  if(!(row$kind %in% c('DRAW','PRINCIPAL_REPAYMENT','INTEREST_PAYMENT','LIMIT_CHANGE'))) .cr_error('EXPOSURE_EVENT','Unknown exposure event kind');key<-paste(row$event_date,sequence,sep=':');if(key %in% seen) .cr_error('EXPOSURE_AMBIGUITY','Resolve same-day event sequence');seen<-c(seen,key)
 }
 if(length(entries))entries<-entries[order(vapply(entries,`[[`,character(1),'event_date'),vapply(entries,function(x)as.numeric(x$sequence),numeric(1)),method='radix')]
 rows<-ledger<-list();clock<-start;pointer<-1L;denominator<-if(day_count=='ACT/365F')365 else 360
 for(j in seq_along(dates)) {
  end<-dates[[j]];opening<-principal;opening_interest<-interest;new_interest<-0;before_end<-NULL
  while(pointer<=length(entries)&&.cr_data_date(entries[[pointer]]$event_date,'event_date')<=end) {
   row<-entries[[pointer]];when<-.cr_data_date(row$event_date,'event_date');accrued<-.cr_loss_multiply(principal,rates[j],as.numeric(when-clock)/denominator);interest<-.cr_loss_sum(c(interest,accrued));new_interest<-.cr_loss_sum(c(new_interest,accrued));clock<-when;if(when==end&&is.null(before_end))before_end<-.cr_loss_sum(c(principal,interest))
   amount<-as.numeric(row$amount);kind<-row$kind;old_principal<-principal;old_interest<-interest;old_limit<-limit
   if(kind=='DRAW'){principal<-.cr_loss_sum(c(principal,amount));if(principal>limit&&!allow_overlimit) .cr_error('EXPOSURE_LIMIT','Draw would exceed declared available commitment')}
   else if(kind=='PRINCIPAL_REPAYMENT'){if(amount>principal) .cr_error('EXPOSURE_PAYMENT','Principal repayment exceeds outstanding; allocate refunds separately');principal<-principal-amount}
   else if(kind=='INTEREST_PAYMENT'){if(amount>interest) .cr_error('EXPOSURE_PAYMENT','Interest payment exceeds accrued interest');interest<-interest-amount}
   else limit<-amount
   ledger[[length(ledger)+1L]]<-list(event_id=row$event_id,event_date=row$event_date,sequence=row$sequence,kind=kind,amount=amount,principal_before=old_principal,principal_after=principal,interest_before=old_interest,interest_after=interest,limit_before=old_limit,limit_after=limit);pointer<-pointer+1L
  }
  accrued<-.cr_loss_multiply(principal,rates[j],as.numeric(end-clock)/denominator);interest<-.cr_loss_sum(c(interest,accrued));new_interest<-.cr_loss_sum(c(new_interest,accrued));clock<-end;after<-.cr_loss_sum(c(principal,interest));if(is.null(before_end))before_end<-after
  rows[[j]]<-list(period_end=period_end_dates[j],opening_principal=opening,opening_accrued_interest=opening_interest,interest_accrued=new_interest,closing_principal=principal,accrued_interest=interest,credit_limit=limit,undrawn_commitment=max(limit-principal,0),overlimit_amount=max(principal-limit,0),exposure_before_period_end_events=before_end,exposure_after_period_end_events=after,conditional_ead=if(default_timing=='BEFORE_PERIOD_END_EVENTS')before_end else after)
 }
 value<-list(facility_id=contract$facility_id,as_of=contract$as_of,currency=currency,day_count=day_count,interest_convention='simple_annual_noncompounding_on_drawn_principal',default_timing=default_timing,allow_overlimit=allow_overlimit,schedule=rows,event_ledger=ledger);value$schedule_hash<-.cr_hash(value)
 .cr_result(value,'ead.exposure_schedule',currency,ctx,diagnostics=list(input_fingerprint=.cr_hash(list(contract=contract,events=entries,annual_interest_rates=as.list(rates),period_end_dates=as.list(period_end_dates)))),limitations='Known scheduled draws and payments are explicit assumptions, not forecasts inferred from future behavior. Interest is not automatically capitalized; fees, FX and collateral require separately declared inputs.')
}
realized_ccf <- function(reference_drawn,reference_limit,ead_at_default,reference_date,default_date,reference_known_at,default_known_at,evaluation_cutoff,currency,minimum_undrawn=0,context=NULL) {
 ctx<-.cr_lgd_currency(currency,context);drawn<-.cr_lgd_scalar(reference_drawn,'reference_drawn',0);limit<-.cr_lgd_scalar(reference_limit,'reference_limit',0);ead<-.cr_lgd_scalar(ead_at_default,'ead_at_default',0);threshold<-.cr_lgd_scalar(minimum_undrawn,'minimum_undrawn',0)
 ref<-.cr_data_date(reference_date,'reference_date');default<-.cr_data_date(default_date,'default_date');rknown<-.cr_data_date(reference_known_at,'reference_known_at');dknown<-.cr_data_date(default_known_at,'default_known_at');cutoff<-.cr_data_date(evaluation_cutoff,'evaluation_cutoff')
 if(!(rknown<=ref&&ref<default&&default<=dknown&&dknown<=cutoff)) .cr_error('EXPOSURE_DATE','Require frozen reference before default, default known by evaluation')
 denominator<-.cr_loss_sum(c(limit,-drawn));increase<-.cr_loss_sum(c(ead,-drawn));ratio<-NULL
 if(denominator<=0)status<-'NO_POSITIVE_UNDRAWN' else if(denominator<=threshold)status<-'BELOW_MINIMUM_UNDRAWN' else {ratio<-increase/denominator;if(!is.finite(ratio)) .cr_error('NUMERICAL_FAILURE','CCF outside finite range');status<-'DEFINED'}
 .cr_result(list(ccf=ratio,denominator_status=status,undrawn_at_reference=denominator,exposure_increase=increase,reference_drawn=drawn,reference_limit=limit,ead_at_default=ead,reconstructed_ead=if(is.null(ratio))NULL else .cr_loss_sum(c(drawn,.cr_loss_multiply(ratio,denominator)))),'ead.realized_ccf','conversion_factor',ctx,diagnostics=list(currency=currency,reference_date=reference_date,default_date=default_date,reference_known_at=reference_known_at,default_known_at=default_known_at,evaluation_cutoff=evaluation_cutoff,minimum_undrawn=threshold),limitations='Reference commitment and exposure must share the same currency and exposure definition. Raw CCF may be negative or exceed one; no floor, ceiling or automatic limit-change adjustment is imposed.')
}
