# Dated card authorization ledger; amounts are reconciled in explicit integer units.
reconcile_card_authorizations <- function(authorizations, events, policy, as_of, known_at, currency, context=NULL) {
 anchor <- .cr_data_date(as_of,'as_of'); cutoff <- .cr_data_date(known_at,'known_at'); ctx <- .cr_result(NULL,'ead.context',context=context)$context
 .cr_string(currency,'currency','AUTHORIZATION_CURRENCY')
 for (key in c('currency','as_of','known_at')) {
  expected <- switch(key,currency=currency,as_of=as_of,known_at=known_at)
  if (!is.null(ctx[[key]]) && !identical(ctx[[key]],expected)) .cr_error('TARGET_MISMATCH',paste('Authorization context differs from',key))
 }
 if (!.cr_object(policy)) .cr_error('AUTHORIZATION_POLICY','Explicit policy required')
 .cr_data_required(policy,c('policy_id','version','known_at','expiry_boundary','late_settlement_allowed','authorization_conversion_factor','minor_unit'))
 for (key in c('policy_id','version')) .cr_string(policy[[key]],key,'AUTHORIZATION_POLICY')
 if (.cr_data_date(policy$known_at,'policy.known_at')>cutoff || !identical(policy$expiry_boundary,'EXCLUSIVE')) .cr_error('AUTHORIZATION_POLICY','Policy must be known; declare exclusive expiry boundary')
 if (!is.logical(policy$late_settlement_allowed)||length(policy$late_settlement_allowed)!=1L||is.na(policy$late_settlement_allowed)) .cr_error('AUTHORIZATION_POLICY','Explicit Boolean late settlement rule required')
 number <- function(value,name) {
  if (!is.numeric(value)||length(value)!=1L||is.na(value)||!is.finite(value)||value<0) .cr_error('AUTHORIZATION_AMOUNT',paste(name,'requires a finite nonnegative number'))
  as.numeric(value)
 }
 minor <- number(policy$minor_unit,'minor_unit'); if(minor<=0) .cr_error('AUTHORIZATION_POLICY','Positive explicit monetary minor unit required')
 maximum_units <- 2^40
 units <- function(value) {
  scaled <- number(value,'amount')/minor
  if(!is.finite(scaled)||scaled>maximum_units) .cr_error('AUTHORIZATION_AMOUNT','Amount exceeds supported minor-unit range')
  rounded <- round(scaled); tolerance <- max(1e-9,min(1e-3,8*2^-52*abs(scaled)))
  if(abs(scaled-rounded)>tolerance) .cr_error('AUTHORIZATION_AMOUNT','Amount is not a whole monetary minor unit')
  rounded
 }
 money <- function(value) {
  result <- value*minor
  if(abs(value)>maximum_units||!is.finite(result)) .cr_error('AUTHORIZATION_AMOUNT','Aggregated amount exceeds supported range')
  result
 }
 factor <- number(policy$authorization_conversion_factor,'authorization_conversion_factor')
 if(factor>1) .cr_error('AUTHORIZATION_POLICY','Authorization conversion factor must lie in [0,1]')
 auths <- .cr_data_rows(authorizations,'authorizations'); entries <- .cr_data_rows(events,'events')
 .cr_data_unique(auths,'authorization_id','authorizations'); .cr_data_unique(entries,'event_id','events')
 keys <- character(); all_auth <- visible <- excluded <- ordered <- list(); accounts <- character()
 base <- function(row,identity,date_key) {
  .cr_data_required(row,c(identity,'account_id',date_key,'known_at','sequence','currency','amount'))
  for(key in c(identity,'account_id')) .cr_string(row[[key]],key,'AUTHORIZATION_ID')
  date <- .cr_data_date(row[[date_key]],date_key); known <- .cr_data_date(row$known_at,'known_at')
  if(known<date||!identical(row$currency,currency)) .cr_error('AUTHORIZATION_CONTRACT','Knowledge/order or currency mismatch')
  seq <- number(row$sequence,'sequence')
  if(seq>2^40||seq!=floor(seq)) .cr_error('AUTHORIZATION_ORDER','Integer event sequence required')
  key <- .cr_hash(list(account=row$account_id,date=row[[date_key]],sequence=seq))
  if(key%in%keys) .cr_error('AUTHORIZATION_ORDER','Resolve same-account same-date sequence')
  keys <<- c(keys,key); units(row$amount)
  if(date>anchor||known>cutoff) {
   excluded[[length(excluded)+1L]] <<- list(record_id=row[[identity]],reason=if(date>anchor)'FUTURE_OCCURRENCE' else 'NOT_YET_KNOWN'); return(FALSE)
  }
  accounts <<- union(accounts,row$account_id); TRUE
 }
 for(row in auths) {
  present <- base(row,'authorization_id','approved_at'); .cr_data_required(row,'expires_at')
  if(.cr_data_date(row$expires_at,'expires_at')<=.cr_data_date(row$approved_at,'approved_at')) .cr_error('AUTHORIZATION_EXPIRY','Expiry must follow authorization')
  all_auth[[row$authorization_id]] <- row
  if(present) {
   visible[[row$authorization_id]] <- list(source=row,captured=0,reversed=0,closed=FALSE,cancellation=FALSE)
   ordered[[length(ordered)+1L]] <- list(date=row$approved_at,sequence=row$sequence,kind='AUTHORIZATION',row=row)
  }
 }
 for(row in entries) {
  present <- base(row,'event_id','occurred_at'); .cr_data_required(row,c('kind','authorization_id'))
  .cr_string(row$kind,'kind','AUTHORIZATION_EVENT');.cr_string(row$authorization_id,'authorization_id','AUTHORIZATION_REFERENCE')
  if(!(row$kind%in%c('SETTLEMENT','REVERSAL','CANCELLATION'))) .cr_error('AUTHORIZATION_EVENT','Unsupported event kind')
  source <- all_auth[[row$authorization_id]]
  if(is.null(source)) .cr_error('AUTHORIZATION_REFERENCE','Every event requires a supplied authorization')
  if(!identical(row$account_id,source$account_id)||row$occurred_at<source$approved_at||(row$occurred_at==source$approved_at&&row$sequence<=source$sequence)) .cr_error('AUTHORIZATION_ORDER','Event must follow its authorization on the same account')
  if(row$kind=='SETTLEMENT'&&(!is.logical(row$authorization_complete)||length(row$authorization_complete)!=1L||is.na(row$authorization_complete))) .cr_error('AUTHORIZATION_EVENT','Settlement requires explicit completion flag')
  if(row$kind=='REVERSAL') {.cr_data_required(row,'reverses_settlement_id'); .cr_string(row$reverses_settlement_id,'reverses_settlement_id','AUTHORIZATION_REFERENCE')}
  if(row$kind=='CANCELLATION'&&row$amount!=0) .cr_error('AUTHORIZATION_EVENT','Cancellation releases reservation, not booked money')
  if(present) {
   if(is.null(visible[[row$authorization_id]])) .cr_error('AUTHORIZATION_REFERENCE','Known event refers to unavailable authorization')
   ordered[[length(ordered)+1L]] <- list(date=row$occurred_at,sequence=row$sequence,kind=row$kind,row=row)
  }
 }
 if(length(ordered)) ordered <- ordered[order(vapply(ordered,`[[`,character(1),'date'),vapply(ordered,function(x)as.numeric(x$sequence),numeric(1)),vapply(ordered,function(x)x$row$account_id,character(1)),method='radix')]
 settlements <- ledger <- list()
 for(entry in ordered) {
  row <- entry$row; kind <- entry$kind; aid <- row$authorization_id; state <- visible[[aid]]; source <- state$source; amount <- units(row$amount)
  if(kind=='SETTLEMENT') {
   if(state$closed) .cr_error('AUTHORIZATION_CLOSED','Cannot settle cancelled or completed authorization')
   if(entry$date>=source$expires_at&&!policy$late_settlement_allowed) .cr_error('AUTHORIZATION_EXPIRED','Late settlement is not allowed by supplied policy')
   if(state$captured+amount>units(source$amount)) .cr_error('AUTHORIZATION_OVERCAPTURE','Capture exceeds immutable authorization amount')
   state$captured <- state$captured+amount; state$closed <- row$authorization_complete
   settlements[[row$event_id]] <- list(authorization_id=aid,remaining=amount)
  } else if(kind=='REVERSAL') {
   original <- settlements[[row$reverses_settlement_id]]
   if(is.null(original)||!identical(original$authorization_id,aid)||amount>original$remaining) .cr_error('AUTHORIZATION_REVERSAL','Reversal requires earlier matching settlement and available booked amount')
   original$remaining <- original$remaining-amount; settlements[[row$reverses_settlement_id]] <- original;state$reversed <- state$reversed+amount
  } else if(kind=='CANCELLATION') {state$closed <- TRUE;state$cancellation <- TRUE}
  visible[[aid]] <- state
  ledger[[length(ledger)+1L]] <- list(record_id=if(kind=='AUTHORIZATION')aid else row$event_id,authorization_id=aid,account_id=source$account_id,date=entry$date,sequence=entry$sequence,kind=kind,cumulative_captured=money(state$captured),cumulative_reversed=money(state$reversed),booked_exposure=money(state$captured-state$reversed),closed=state$closed)
 }
 output <- list()
 for(aid in sort(names(visible),method='radix')) {
  state <- visible[[aid]];source <- state$source;expired <- as_of>=source$expires_at
  pending <- if(expired||state$closed)0 else units(source$amount)-state$captured;booked <- state$captured-state$reversed
  output[[length(output)+1L]] <- list(authorization_id=aid,account_id=source$account_id,authorization_amount=source$amount,captured_amount=money(state$captured),reversed_amount=money(state$reversed),booked_exposure=money(booked),pending_authorization=money(pending),expired=expired,completed_or_cancelled=state$closed,scenario_exposure=money(booked+factor*pending))
 }
 totals <- list()
 for(account in sort(accounts,method='radix')) {
  rows <- Filter(function(x)x$account_id==account,output)
  booked <- money(sum(vapply(rows,function(x)units(x$booked_exposure),numeric(1))))
  pending <- money(sum(vapply(rows,function(x)units(x$pending_authorization),numeric(1))))
  totals[[length(totals)+1L]] <- list(account_id=account,booked_exposure=booked,pending_authorization=pending,scenario_exposure=money(units(booked)+factor*units(pending)))
 }
 if(length(excluded)) excluded <- excluded[order(vapply(excluded,`[[`,character(1),'record_id'),vapply(excluded,`[[`,character(1),'reason'),method='radix')]
 value <- list(as_of=as_of,known_at=known_at,currency=currency,scope='SUPPLIED_AUTHORIZATION_LEDGER',authorization_conversion_factor=factor,authorizations=output,accounts=totals,event_ledger=ledger,excluded=excluded,policy=policy)
 value$artifact_hash <- .cr_hash(value)
 .cr_result(value,'ead.reconcile_card_authorizations',currency,ctx,diagnostics=list(input_hash=.cr_hash(list(authorizations=auths,events=entries,policy=policy))),limitations='The supplied ledger is not total account exposure. Conversion of live reservations is an explicit scenario factor, not an estimated CCF. Reversal does not reopen a reservation. Additional captures require a new explicit authorization; no network or national legal convention is inferred.')
}
