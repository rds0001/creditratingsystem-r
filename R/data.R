# Local typed data and dated credit-event contracts.
.cr_data_rows <- function(x, name) {
  if (is.data.frame(x)) return(lapply(seq_len(nrow(x)), function(i) lapply(x[i,,drop=FALSE], function(v) { if (length(v)==1 && is.na(v)) NULL else if (inherits(v,'Date')) format(v,'%Y-%m-%d') else v[[1]] })))
  if (!is.list(x) || any(!vapply(x, is.list, logical(1)))) .cr_error('DATA_SCHEMA',paste(name,'must be a data.frame or list of named rows'))
  x
}
.cr_data_missing <- function(x) is.null(x) || (length(x)==1L && is.atomic(x) && is.na(x))
.cr_data_required <- function(r, fields) for (f in fields) { if (.cr_data_missing(r[[f]])) .cr_error('DATA_SCHEMA',paste('Missing required field',f)); if(grepl('_id$',f)&&(!is.character(r[[f]])||length(r[[f]])!=1L||!nzchar(r[[f]]))) .cr_error('DATA_SCHEMA',paste(f,'must be a nonempty string')) }
.cr_data_date <- function(x, name) {
  if (!is.character(x) || length(x)!=1L || is.na(x) || !grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$',x)) .cr_error('DATA_DATE',paste(name,'requires ISO YYYY-MM-DD'))
  d <- tryCatch(as.Date(x,format='%Y-%m-%d'),error=function(e) NA)
  if (is.na(d) || format(d,'%Y-%m-%d')!=x) .cr_error('DATA_DATE',paste('Invalid date in',name))
  d
}
.cr_data_days <- function(x,name,minimum=0) { if (!is.numeric(x)||length(x)!=1L||!is.finite(x)||x!=floor(x)||x<minimum) .cr_error('DATA_SCHEMA',paste(name,'must be a nonnegative integer')); x }
.cr_data_key <- function(r, keys) paste(vapply(keys,function(k) { z<-r[[k]]; if (is.null(z)) 'N' else paste0(nchar(as.character(z)),':',z) },character(1)),collapse='|')
.cr_data_unique <- function(rows,keys,name) { if(!is.character(keys)||!length(keys)||any(!nzchar(keys))) .cr_error('DATA_SCHEMA','Keys must be nonempty column names'); seen<-character(); for(r in rows) { .cr_data_required(r,keys); if(any(!vapply(keys,function(k)is.character(r[[k]])&&length(r[[k]])==1L&&!is.na(r[[k]])&&nzchar(r[[k]]),logical(1)))) .cr_error('DATA_SCHEMA','Key fields must be nonempty strings'); k<-.cr_data_key(r,keys); if(k %in% seen) .cr_error('DATA_DUPLICATE',paste('Duplicate key in',name)); seen<-c(seen,k) } }
.cr_data_amount <- function(x) is.numeric(x)&&length(x)==1L&&is.finite(x)&&x>=0
.cr_data_iso <- function(x) format(x,'%Y-%m-%d')

read_local_dataset <- function(path,schema,encoding='utf-8',delimiter=',',decimal_mark='.') {
  if (!is.character(path)||length(path)!=1L) .cr_error('DATA_SCHEMA','path must be a string')
  if (grepl('^[A-Za-z][A-Za-z0-9+.-]*://',path)||startsWith(path,'//')||startsWith(path,'\\\\')) .cr_error('DATA_REMOTE_PATH','Only local filesystem paths are supported')
  if (!is.list(schema)||!identical(schema$version,'1.0')||!is.list(schema$columns)||!length(schema$columns)||is.null(names(schema$columns))) .cr_error('DATA_SCHEMA','schema requires version 1.0 and named columns')
  if (!(decimal_mark %in% c('.',','))||nchar(delimiter)!=1L) .cr_error('DATA_SCHEMA','Invalid decimal mark or delimiter')
  columns<-schema$columns
  for(f in names(columns)) { s<-columns[[f]]; if(!is.list(s)||!(s$type %in% c('string','integer','number','boolean','date'))) .cr_error('DATA_SCHEMA',paste('Invalid column',f)); if(s$type %in% c('integer','number') && !is.character(s$unit)) .cr_error('DATA_SCHEMA',paste('Numeric column needs unit',f)) }
  ext<-tolower(tools::file_ext(path))
  if (!(ext %in% c('csv','json'))) .cr_error('DATA_FORMAT','Supported formats are .csv and .json')
  if (!file.exists(path)||dir.exists(path)) .cr_error('DATA_READ','File does not exist')
  rows<-tryCatch({
    if(ext=='csv') {
      d<-utils::read.table(path,header=TRUE,sep=delimiter,quote='"',comment.char='',colClasses='character',na.strings=NULL,check.names=FALSE,fileEncoding=encoding,fill=FALSE,blank.lines.skip=TRUE)
      if(anyDuplicated(names(d))||!setequal(names(d),names(columns))) .cr_error('DATA_SCHEMA','File columns must equal schema columns')
      .cr_data_rows(d,'dataset')
    } else {
      con<-file(path,open='rt',encoding=encoding); on.exit(close(con),add=TRUE)
      obj<-jsonlite::fromJSON(paste(readLines(con,warn=FALSE),collapse='\n'),simplifyVector=FALSE)
      check_keys<-function(x) { if(is.list(x)) { if(anyDuplicated(names(x))) .cr_error('DATA_DUPLICATE','Duplicate JSON key'); lapply(x,check_keys) }; invisible(NULL) }; check_keys(obj); obj
    }
  },error=function(e) { if(inherits(e,'creditratingsystem_error')) stop(e); .cr_error('DATA_READ',conditionMessage(e)) })
  rows<-.cr_data_rows(rows,'dataset')
  if(!length(rows)&&!isTRUE(schema$allow_empty)) .cr_error('DATA_EMPTY','Empty table requires allow_empty=true')
  output<-list()
  for(i in seq_along(rows)) {
    r<-rows[[i]]; if(!setequal(names(r),names(columns))) .cr_error('DATA_SCHEMA',paste('Wrong columns in row',i))
    out<-list()
    for(f in names(columns)) {
      s<-columns[[f]]; v<-r[[f]]
      if(.cr_data_missing(v)||identical(v,'')) { if(!isTRUE(s$nullable)) .cr_error('DATA_NULL',paste(f,'is not nullable')); out[f]<-list(NULL); next }
      if(s$type=='string') { if(!is.character(v)||length(v)!=1L) .cr_error('DATA_TYPE','Strings must be supplied as strings'); parsed<-v
      } else if(s$type=='date') parsed<-.cr_data_iso(.cr_data_date(v,f))
      else if(s$type=='boolean') {
        if(identical(v,TRUE)||identical(v,'true')) parsed<-TRUE else if(identical(v,FALSE)||identical(v,'false')) parsed<-FALSE else .cr_error('DATA_TYPE','Boolean must be true or false')
      } else {
        if(is.logical(v)||length(v)!=1L) .cr_error('DATA_TYPE','Invalid numeric type')
        txt<-as.character(v)
        if(is.character(v)&&decimal_mark==',') { if(grepl('.',txt,fixed=TRUE)) .cr_error('DATA_TYPE','Unexpected decimal point'); txt<-sub(',','.',txt,fixed=TRUE) }
        if(is.character(v)&&!grepl('^[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$',trimws(txt))) .cr_error('DATA_TYPE','Invalid decimal numeric syntax')
        if(s$type=='integer'&&is.character(v)&&!grepl('^[+-]?[0-9]+$',trimws(txt))) .cr_error('DATA_TYPE','Integer strings must contain signed digits')
        parsed<-suppressWarnings(as.numeric(txt))
        if(!is.finite(parsed)) .cr_error('DATA_TYPE','Nonfinite numeric field')
        if(s$type=='integer'&&(parsed!=floor(parsed)||abs(parsed)>9007199254740991)) .cr_error('DATA_TYPE','Integer outside exact JSON range')
      }
      out[f]<-list(parsed)
    }
    output[[i]]<-out
  }
  if(length(schema$primary_key)) { keys<-unlist(schema$primary_key); if(!is.character(keys)||any(!keys %in% names(columns))) .cr_error('DATA_SCHEMA','Primary key columns must exist'); .cr_data_unique(output,keys,'dataset') }
  .cr_result(output,'data.read_local_dataset',diagnostics=list(row_count=length(output),schema_version='1.0',units=lapply(columns,function(x)x$unit)),provenance=list(source_sha256=digest::digest(file=path,algo='sha256'),data_origin='USER_SUPPLIED'))
}

validate_credit_dataset <- function(tables,contract_version='1.0') {
  defs<-list(obligors='entity_id',observations=c('entity_id','observed_at'),facilities='facility_id',defaults='episode_id',recoveries='cashflow_id')
  req<-list(obligors='entity_id',observations=c('entity_id','observed_at','available_at'),facilities=c('facility_id','entity_id','ead'),defaults=c('episode_id','entity_id','default_at'),recoveries=c('cashflow_id','episode_id','cashflow_at','amount','kind'))
  if(!identical(contract_version,'1.0')||!is.list(tables)||length(setdiff(names(tables),names(defs)))) .cr_error('DATA_SCHEMA','Invalid tables or version')
  data<-lapply(names(tables),function(t).cr_data_rows(tables[[t]],t)); names(data)<-names(tables)
  issues<-list(); issue<-function(t,i,code,field) { issues[[length(issues)+1L]]<<-list(table=t,row=i,code=code,field=field) }
  for(t in names(data)) { seen<-character(); for(i in seq_along(data[[t]])) {
    r<-data[[t]][[i]]
    for(f in req[[t]]) if(.cr_data_missing(r[[f]])) issue(t,i,'MISSING_FIELD',f)
    if(any(vapply(defs[[t]],function(f) !.cr_data_missing(r[[f]])&&(!is.character(r[[f]])||length(r[[f]])!=1L||!nzchar(r[[f]])),logical(1)))) issue(t,i,'INVALID_KEY',paste(defs[[t]],collapse=','))
    if(all(vapply(defs[[t]],function(f) is.character(r[[f]])&&length(r[[f]])==1L&&!is.na(r[[f]])&&nzchar(r[[f]]),logical(1)))) { k<-.cr_data_key(r,defs[[t]]); if(k %in% seen) issue(t,i,'DUPLICATE_KEY',paste(defs[[t]],collapse=',')); seen<-c(seen,k) }
    for(f in c('ead','amount')) if(f %in% names(r)&&!.cr_data_amount(r[[f]])) issue(t,i,'INVALID_AMOUNT',f)
    for(f in c('observed_at','available_at','default_at','cashflow_at','cure_at','closed_at')) if(!.cr_data_missing(r[[f]])&&inherits(try(.cr_data_date(r[[f]],f),silent=TRUE),'try-error')) issue(t,i,'INVALID_DATE',f)
    for(pair in list(c('observed_at','available_at'),c('default_at','cure_at'),c('default_at','closed_at'))) if(!.cr_data_missing(r[[pair[1]]])&&!.cr_data_missing(r[[pair[2]]])&&as.character(r[[pair[1]]])>as.character(r[[pair[2]]])) issue(t,i,'DATE_ORDER',pair[2])
    if(t=='recoveries'&&!(r$kind %in% c('recovery','cost'))) issue(t,i,'INVALID_KIND','kind')
  } }
  for(link in list(c('observations','entity_id','obligors','entity_id'),c('facilities','entity_id','obligors','entity_id'),c('defaults','entity_id','obligors','entity_id'),c('recoveries','episode_id','defaults','episode_id'))) {
    ids<-vapply(data[[link[3]]],function(r) if(!is.character(r[[link[4]]])||length(r[[link[4]]])!=1L) '' else r[[link[4]]],character(1))
    for(i in seq_along(data[[link[1]]])) { id<-data[[link[1]]][[i]][[link[2]]]; if(!is.character(id)||length(id)!=1L||!(id %in% ids)) issue(link[1],i,'FOREIGN_KEY',link[2]) }
  }
  for(i in seq_along(data$recoveries)) { r<-data$recoveries[[i]]; ds<-Filter(function(d) identical(d$episode_id,r$episode_id),data$defaults); if(length(ds)&&!is.null(r$cashflow_at)&&!is.null(ds[[length(ds)]]$default_at)&&r$cashflow_at<ds[[length(ds)]]$default_at) issue('recoveries',i,'BEFORE_DEFAULT','cashflow_at') }
  .cr_result(list(valid=!length(issues),violations=issues,row_counts=lapply(data,length)),'data.validate_credit_dataset')
}

join_as_of <- function(left,right,entity_keys='entity_id',as_of='as_of',known_at='known_at',observed_at='observed_at',available_at='available_at',release_id='release_id',suffix='_right') {
  left<-.cr_data_rows(left,'left'); right<-.cr_data_rows(right,'right')
  if(!length(entity_keys)||!nzchar(suffix)) .cr_error('DATA_SCHEMA','Need keys and suffix')
  .cr_data_unique(right,release_id,'right releases')
  for(r in right) { .cr_data_required(r,c(entity_keys,observed_at,available_at)); .cr_data_date(r[[observed_at]],observed_at); .cr_data_date(r[[available_at]],available_at) }
  cols<-sort(setdiff(unique(unlist(lapply(right,names))),entity_keys)); out<-list()
  for(i in seq_along(left)) {
    r<-left[[i]]; .cr_data_required(r,c(entity_keys,as_of,known_at)); a<-.cr_data_date(r[[as_of]],as_of); k<-.cr_data_date(r[[known_at]],known_at)
    candidates<-Filter(function(z) all(vapply(entity_keys,function(f) identical(z[[f]],r[[f]]),logical(1)))&&.cr_data_date(z[[observed_at]],observed_at)<=a&&.cr_data_date(z[[available_at]],available_at)<=k,right)
    selected<-NULL
    if(length(candidates)) {
      obs<-vapply(candidates,function(z)as.character(z[[observed_at]]),character(1)); candidates<-candidates[obs==max(obs)]
      avail<-vapply(candidates,function(z)as.character(z[[available_at]]),character(1)); candidates<-candidates[avail==max(avail)]
      if(length(candidates)!=1L) .cr_error('DATA_AMBIGUOUS_JOIN','Multiple releases tie on dates')
      selected<-candidates[[1]]
    }
    result<-r
    for(col in cols) { target<-if(col %in% names(r)) paste0(col,suffix) else col; if(target %in% names(result)) .cr_error('DATA_COLUMN_COLLISION',paste('Join collision',target)); result[target]<-list(selected[[col]]) }
    out[[i]]<-result
  }
  .cr_result(out,'data.join_as_of',diagnostics=list(ordering=c(observed_at,available_at),right_rows=length(right)))
}

build_observation_windows <- function(ratings,default_events,exits,horizon_days,evaluation_cutoff,entity_key='entity_id') {
  horizon_days<-.cr_data_days(horizon_days,'horizon_days',1); cutoff<-.cr_data_date(evaluation_cutoff,'evaluation_cutoff')
  ratings<-.cr_data_rows(ratings,'ratings'); events<-.cr_data_rows(default_events,'default_events'); exits<-.cr_data_rows(exits,'exits')
  .cr_data_unique(ratings,'observation_id','ratings'); .cr_data_unique(events,'event_id','default_events')
  for(pair in list(list(events,'default_at'),list(exits,'exit_at'))) for(r in pair[[1]]) { .cr_data_required(r,c(entity_key,pair[[2]],'known_at')); if(.cr_data_date(r$known_at,'known_at')<.cr_data_date(r[[pair[[2]]]],pair[[2]])) .cr_error('DATA_DATE_ORDER','Event known before occurrence') }
  out<-list()
  for(i in seq_along(ratings)) {
    r<-ratings[[i]]; .cr_data_required(r,c(entity_key,'as_of')); start<-.cr_data_date(if(is.null(r$performance_start))r$as_of else r$performance_start,'performance_start')
    if(start<.cr_data_date(r$as_of,'as_of')) .cr_error('DATA_DATE_ORDER','Performance precedes prediction')
    end<-start+horizon_days; followup<-.cr_data_date(if(is.null(r$followup_end))evaluation_cutoff else r$followup_end,'followup_end'); stop_at<-min(end,cutoff,followup)
    known<-Filter(function(e) identical(e[[entity_key]],r[[entity_key]])&&.cr_data_date(e$known_at,'known_at')<=cutoff,events)
    prior<-any(vapply(known,function(e).cr_data_date(e$default_at,'default_at')<=start,logical(1)))
    ex<-Filter(function(e) identical(e[[entity_key]],r[[entity_key]])&&.cr_data_date(e$known_at,'known_at')<=cutoff&&.cr_data_date(e$exit_at,'exit_at')>start,exits)
    exit_date<-if(length(ex)) .cr_data_date(min(vapply(ex,function(e)e$exit_at,character(1))),'exit_at') else NULL
    if(!is.null(exit_date)) stop_at<-min(stop_at,exit_date)
    matches<-Filter(function(e) .cr_data_date(e$default_at,'default_at')>start&&.cr_data_date(e$default_at,'default_at')<=stop_at,known)
    if(length(matches)) matches<-matches[order(vapply(matches,function(e)e$default_at,character(1)),vapply(matches,function(e)e$event_id,character(1)))]
    event<-if(length(matches))matches[[1]] else NULL; funded<-if(is.null(r$funded))TRUE else r$funded
    if(!is.logical(funded)||length(funded)!=1L||is.na(funded)) .cr_error('DATA_SCHEMA','funded must be boolean')
    pending<-!is.null(r$outcome_available_at)&&.cr_data_date(r$outcome_available_at,'outcome_available_at')>cutoff
    reason<-if(!funded)'not_funded' else if(prior)'prior_default' else if(stop_at<start)'not_started' else if(!is.null(event))'default' else if(!is.null(exit_date)&&exit_date<=min(end,cutoff,followup))'exit' else if(pending)'outcome_pending' else if(stop_at==end)'mature' else 'right_censored'
    label<-if(reason=='default')1 else if(reason=='mature')0 else NULL
    r$performance_start<-.cr_data_iso(start); r$outcome_end<-.cr_data_iso(end); r$observed_until<-.cr_data_iso(if(reason=='default').cr_data_date(event$default_at,'default_at') else max(start,stop_at)); r['target']<-list(label); r$censored<-reason %in% c('exit','right_censored','not_started','outcome_pending'); r$eligible<-funded&&!prior&&stop_at>=start; r$label_status<-reason; r['event_id']<-list(if(reason=='default')event$event_id else NULL)
    out[[i]]<-r
  }
  .cr_result(out,'data.build_observation_windows',unit='binary',diagnostics=list(boundary='(start,end]',target='first_default',exit_policy='censor; default wins same-day ties'))
}

build_recovery_episodes <- function(default_events,recovery_cashflows,cures=NULL,reopening_days=90,evaluation_cutoff=NULL,closures=NULL) {
  reopening_days<-.cr_data_days(reopening_days,'reopening_days'); defaults<-.cr_data_rows(default_events,'default_events'); cashflows<-.cr_data_rows(recovery_cashflows,'recovery_cashflows'); cures<-.cr_data_rows(if(is.null(cures))list() else cures,'cures')
  cutoff<-if(is.null(evaluation_cutoff))as.Date('9999-12-31') else .cr_data_date(evaluation_cutoff,'evaluation_cutoff')
  .cr_data_unique(defaults,'episode_id','default_events'); .cr_data_unique(cashflows,'cashflow_id','recovery_cashflows'); .cr_data_unique(cures,'episode_id','cures')
  for(d in defaults) { .cr_data_required(d,c('episode_id','entity_id','default_at','known_at','ead','currency')); if(!.cr_data_amount(d$ead)) .cr_error('DATA_AMOUNT','EAD must be finite nonnegative'); if(.cr_data_date(d$known_at,'known_at')<.cr_data_date(d$default_at,'default_at')) .cr_error('DATA_DATE_ORDER','Default known before occurrence') }
  dmap<-setNames(defaults,vapply(defaults,function(d)d$episode_id,character(1))); cmap<-list()
  for(c in cures) {
    .cr_data_required(c,c('episode_id','cure_at','known_at')); if(!(c$episode_id %in% names(dmap))) .cr_error('DATA_FOREIGN_KEY','Cure references unknown default')
    if(.cr_data_date(c$cure_at,'cure_at')<.cr_data_date(dmap[[c$episode_id]]$default_at,'default_at')||.cr_data_date(c$known_at,'known_at')<.cr_data_date(c$cure_at,'cure_at')) .cr_error('DATA_DATE_ORDER','Invalid cure dates')
    if(.cr_data_date(c$known_at,'known_at')<=cutoff) cmap[[c$episode_id]]<-c
  }
  visible<-Filter(function(d).cr_data_date(d$known_at,'known_at')<=cutoff,defaults)
  if(length(visible)) visible<-visible[order(vapply(visible,function(d)d$entity_id,character(1)),vapply(visible,function(d)d$default_at,character(1)),vapply(visible,function(d)d$episode_id,character(1)))]
  .cr_data_unique(visible,c('entity_id','default_at'),'entity defaults'); episodes<-list(); links<-list(); last<-list()
  for(d in visible) {
    previous<-last[[d$entity_id]]; reopen<-FALSE
    if(!is.null(previous)) { c<-cmap[[previous$episode_id]]; reopen<-is.null(c)||.cr_data_date(d$default_at,'default_at')<=.cr_data_date(c$cure_at,'cure_at')+reopening_days }
    if(reopen) { idx<-links[[previous$episode_id]]; ep<-episodes[[idx]]; if(ep$currency!=d$currency) .cr_error('DATA_CURRENCY','Reopened currency differs'); ep$default_ids<-c(ep$default_ids,list(d$episode_id))
    } else { idx<-length(episodes)+1L; ep<-list(workout_id=d$episode_id,entity_id=d$entity_id,default_at=d$default_at,ead_at_first_default=d$ead,currency=d$currency,default_ids=list(d$episode_id),cashflow_ids=list(),recoveries=0,costs=0,closed_at=NULL,status='open') }
    links[[d$episode_id]]<-idx; last[[d$entity_id]]<-d; ep['closed_at']<-list(NULL); ep$status<-'open'; cure<-cmap[[d$episode_id]]
    if(!is.null(cure$closed_at)) { closed<-.cr_data_date(cure$closed_at,'closed_at'); if(closed<.cr_data_date(cure$cure_at,'cure_at')) .cr_error('DATA_DATE_ORDER','Closure precedes cure'); .cr_data_required(cure,'closed_known_at'); closed_known<-.cr_data_date(cure$closed_known_at,'closed_known_at'); if(closed_known<closed) .cr_error('DATA_DATE_ORDER','Closure known before closure date'); if(closed_known<=cutoff) { ep$closed_at<-.cr_data_iso(closed); ep$status<-'closed' } }
    episodes[[idx]]<-ep
  }
  terminals<-.cr_data_rows(if(is.null(closures))list() else closures,'closures')
  .cr_data_unique(terminals,'closure_id','closures'); .cr_data_unique(terminals,'episode_id','closures'); closed_workouts<-character()
  for(terminal in terminals) {
    .cr_data_required(terminal,c('closure_id','episode_id','closed_at','known_at','closure_reason')); key<-terminal$episode_id
    if(!(key %in% names(dmap))) .cr_error('DATA_FOREIGN_KEY','Closure references unknown default')
    if(!(terminal$closure_reason %in% c('SETTLEMENT','WRITE_OFF','LEGAL_TERMINATION'))) .cr_error('DATA_SCHEMA','Unknown legal closure reason')
    closed<-.cr_data_date(terminal$closed_at,'closed_at'); known<-.cr_data_date(terminal$known_at,'known_at')
    if(closed<.cr_data_date(dmap[[key]]$default_at,'default_at')||known<closed) .cr_error('DATA_DATE_ORDER','Invalid legal closure dates')
    if(known>cutoff) next
    idx<-links[[key]]; if(is.null(idx)) .cr_error('DATA_FOREIGN_KEY','Closure visible before its default'); ep<-episodes[[idx]]
    legacy<-any(vapply(ep$default_ids,function(i) !is.null(cmap[[i]]$closed_at)&&.cr_data_date(cmap[[i]]$closed_known_at,'closed_known_at')<=cutoff,logical(1)))
    if(ep$workout_id %in% closed_workouts||legacy) .cr_error('DATA_TERMINAL_CONFLICT','Multiple terminal records for one workout')
    if(any(vapply(ep$default_ids,function(i).cr_data_date(dmap[[i]]$default_at,'default_at')>closed,logical(1)))) .cr_error('DATA_DATE_ORDER','Legal closure precedes a default in its continuous workout')
    closed_workouts<-c(closed_workouts,ep$workout_id); ep$closed_at<-.cr_data_iso(closed); ep$status<-'closed'; ep$closure_id<-terminal$closure_id; ep$closure_reason<-terminal$closure_reason; episodes[[idx]]<-ep
  }
  allocated<-list()
  for(cf in cashflows) {
    .cr_data_required(cf,c('cashflow_id','episode_id','cashflow_at','known_at','amount','kind','currency'))
    if(!(cf$episode_id %in% names(dmap))) .cr_error('DATA_FOREIGN_KEY','Cashflow references unknown default')
    when<-.cr_data_date(cf$cashflow_at,'cashflow_at'); known<-.cr_data_date(cf$known_at,'known_at')
    if(when<.cr_data_date(dmap[[cf$episode_id]]$default_at,'default_at')||known<when) .cr_error('DATA_DATE_ORDER','Invalid recovery dates')
    if(!(cf$kind %in% c('recovery','cost'))||!.cr_data_amount(cf$amount)) .cr_error('DATA_AMOUNT','Invalid cashflow amount or kind')
    if(known>cutoff) next
    idx<-links[[cf$episode_id]]; if(is.null(idx)) .cr_error('DATA_FOREIGN_KEY','Cashflow visible before default'); ep<-episodes[[idx]]
    if(cf$currency!=ep$currency) .cr_error('DATA_CURRENCY','Cashflow currency differs')
    if(!is.null(ep$closed_at)&&when>.cr_data_date(ep$closed_at,'closed_at')) .cr_error('DATA_CLOSED_EPISODE','Cashflow after closure requires revised closure or reopening')
    ep$cashflow_ids<-c(ep$cashflow_ids,list(cf$cashflow_id)); field<-if(cf$kind=='recovery')'recoveries' else 'costs'; ep[[field]]<-ep[[field]]+cf$amount
    cf$workout_id<-ep$workout_id; cf$days_since_first_default<-as.numeric(when-.cr_data_date(ep$default_at,'default_at')); allocated[[length(allocated)+1L]]<-cf; episodes[[idx]]<-ep
  }
  episodes<-lapply(episodes,function(ep) {ep$net_recovery<-ep$recoveries-ep$costs;ep})
  .cr_result(list(episodes=episodes,cashflows=allocated),'data.build_recovery_episodes',limitations=c('Undiscounted currency amounts; original cashflow dates retained for discounting.','Cure alone does not establish workout closure; only explicit closed_at does.'))
}

make_grouped_temporal_splits <- function(panel,train_end,calibration_end,test_end,embargo_days=0,entity_key='entity_id',group_key='group_id',time_key='observed_at',outcome_end_key='outcome_end') {
  panel<-.cr_data_rows(panel,'panel'); embargo<-.cr_data_days(embargo_days,'embargo_days'); t1<-.cr_data_date(train_end,'train_end'); t2<-.cr_data_date(calibration_end,'calibration_end'); t3<-.cr_data_date(test_end,'test_end')
  if(!(t1<t2&&t2<t3)) .cr_error('DATA_DATE_ORDER','Split boundaries must increase'); .cr_data_unique(panel,'observation_id','panel')
  parent<-new.env(hash=TRUE,parent=emptyenv())
  root<-function(x) { if(!exists(x,envir=parent,inherits=FALSE)) assign(x,x,envir=parent); while(get(x,envir=parent)!=x) x<-get(x,envir=parent); x }
  for(r in panel) {
    .cr_data_required(r,c(entity_key,time_key,outcome_end_key)); if(!is.character(r[[entity_key]])||(!is.null(r[[group_key]])&&!is.character(r[[group_key]]))) .cr_error('DATA_SCHEMA','Entity and group IDs must be strings')
    a<-root(paste0('e:',r[[entity_key]])); if(!is.null(r[[group_key]])) { b<-root(paste0('g:',r[[group_key]])); assign(b,a,envir=parent) }
  }
  candidates<-list(); latest<-list()
  for(i in seq_along(panel)) {
    r<-panel[[i]]; time<-.cr_data_date(r[[time_key]],time_key); end<-.cr_data_date(r[[outcome_end_key]],outcome_end_key)
    if(end<time) .cr_error('DATA_DATE_ORDER','Outcome precedes observation')
    part<-if(time<=t1)1L else if(time<=t2)2L else if(time<=t3)3L else 4L; component<-root(paste0('e:',r[[entity_key]]))
    latest[[component]]<-max(if(is.null(latest[[component]]))0 else latest[[component]],part); candidates[[i]]<-list(r=r,time=time,end=end,part=part,component=component)
  }
  labels<-c('train','calibration','test'); assignments<-list()
  for(i in seq_along(candidates)) {
    c<-candidates[[i]]; reason<-NULL
    if(c$part==4L) reason<-'outside_test_window' else if(c$part!=latest[[c$component]]) reason<-'group_isolation'
    else if((c$part==1L&&c$end+embargo>t1)||(c$part==2L&&c$end+embargo>t2)||(c$part==3L&&c$end>t3)) reason<-'outcome_overlap'
    else if((c$part==2L&&c$time<=t1+embargo)||(c$part==3L&&c$time<=t2+embargo)) reason<-'embargo'
    assignments[[i]]<-list(observation_id=c$r$observation_id,split=if(is.null(reason))labels[c$part] else 'excluded',reason=reason)
  }
  counts<-setNames(lapply(c(labels,'excluded'),function(l)sum(vapply(assignments,function(a)a$split==l,logical(1)))),c(labels,'excluded'))
  .cr_result(list(assignments=assignments,counts=counts,group_overlap=FALSE),'data.make_grouped_temporal_splits',diagnostics=list(group_rule='connected entity/group components reserved to latest partition, including excluded future rows',embargo_days=embargo_days))
}
