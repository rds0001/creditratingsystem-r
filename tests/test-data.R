# Independent hand-derived data boundary cases. Supports R CMD check and source use.
if (!exists('join_as_of')) library(creditratingsystem)
expect_code <- function(code,expr) {
  e<-tryCatch({force(expr);NULL},error=identity)
  stopifnot(inherits(e,'creditratingsystem_error'),identical(e$code,code))
}
left<-list(list(entity_id='A',as_of='2024-02-01',known_at='2024-02-01'),list(entity_id='A',as_of='2024-02-01',known_at='2024-03-01'))
right<-list(list(entity_id='A',observed_at='2024-01-01',available_at='2024-01-05',release_id='old',score=1),list(entity_id='A',observed_at='2024-01-01',available_at='2024-02-15',release_id='revision',score=2),list(entity_id='A',observed_at='2024-03-01',available_at='2024-03-01',release_id='future',score=9))
stopifnot(identical(vapply(join_as_of(left,right)$value,function(r)r$score,numeric(1)),c(1,2)))
tie<-right[[1]]; tie$release_id<-'tie'
expect_code('DATA_AMBIGUOUS_JOIN',join_as_of(left,c(right,list(tie))))
nomatch<-left[[1]]; nomatch$entity_id<-'B'
stopifnot(is.null(join_as_of(list(nomatch),right)$value[[1]]$score))
ratings<-lapply(LETTERS[1:6],function(i)list(observation_id=i,entity_id=i,as_of='2024-01-01'))
ratings[[2]]$outcome_available_at<-'2024-03-01'
ratings[[3]]$followup_end<-'2024-01-15'; ratings[[4]]$funded<-FALSE
events<-list(list(event_id='a',entity_id='A',default_at='2024-01-31',known_at='2024-01-31'),list(event_id='b',entity_id='B',default_at='2024-01-31',known_at='2024-03-01'),list(event_id='f',entity_id='F',default_at='2024-01-01',known_at='2024-01-01'))
exits<-list(list(entity_id='E',exit_at='2024-01-20',known_at='2024-01-20'))
v<-build_observation_windows(ratings,events,exits,30,'2024-02-01')$value
stopifnot(identical(vapply(v,function(r)r$label_status,character(1)),c('default','outcome_pending','right_censored','not_funded','exit','prior_default')),v[[1]]$target==1,is.null(v[[2]]$target),is.null(v[[3]]$target),v[[1]]$outcome_end=='2024-01-31')
stopifnot(build_observation_windows(data.frame(observation_id='L',entity_id='L',as_of='2024-02-28'),list(),list(),1,'2024-02-29')$value[[1]]$outcome_end=='2024-02-29')
defaults<-list(list(episode_id='d1',entity_id='A',default_at='2024-01-01',known_at='2024-01-01',ead=100,currency='EUR'),list(episode_id='d2',entity_id='A',default_at='2024-03-01',known_at='2024-03-01',ead=100,currency='EUR'),list(episode_id='d3',entity_id='A',default_at='2024-12-01',known_at='2024-12-01',ead=100,currency='EUR'))
cures<-list(list(episode_id='d1',cure_at='2024-02-01',known_at='2024-02-01'),list(episode_id='d2',cure_at='2024-04-01',known_at='2024-04-01',closed_at='2024-05-01',closed_known_at='2024-05-01'))
flows<-list(list(cashflow_id='c1',episode_id='d1',cashflow_at='2024-01-11',known_at='2024-01-11',amount=30,kind='recovery',currency='EUR'),list(cashflow_id='c2',episode_id='d2',cashflow_at='2024-03-11',known_at='2024-03-11',amount=5,kind='cost',currency='EUR'))
v<-build_recovery_episodes(defaults,flows,cures,90,'2024-12-31')$value
stopifnot(length(v$episodes)==2,identical(v$episodes[[1]]$default_ids,list('d1','d2')),v$episodes[[1]]$net_recovery==25,v$episodes[[1]]$ead_at_first_default==100,v$episodes[[2]]$status=='open',identical(vapply(v$cashflows,function(r)r$days_since_first_default,numeric(1)),c(10,70)))
expect_code('DATA_DUPLICATE',build_recovery_episodes(defaults,c(flows,flows[1]),cures))
stopifnot(length(build_recovery_episodes(defaults,flows,cures,90,'2024-02-01')$value$episodes)==1)
panel<-data.frame(observation_id=as.character(1:7),entity_id=c('A','B','B','C','D','E','F'),group_id=c('G','G','H','H','Z','Y','X'),observed_at=c('2024-01-01','2024-04-10','2024-07-10','2024-01-10','2024-01-10','2024-03-31','2024-04-02'),outcome_end=c('2024-01-15','2024-04-20','2024-07-20','2024-01-20','2024-02-10','2024-04-05','2024-04-04'))
v<-make_grouped_temporal_splits(panel,'2024-03-31','2024-06-30','2024-09-30',5)$value
stopifnot(identical(vapply(v$assignments,function(r)r$split,character(1)),c('excluded','excluded','test','excluded','train','excluded','excluded')),v$assignments[[6]]$reason=='outcome_overlap',v$assignments[[7]]$reason=='embargo',!v$group_overlap)
tables<-list(obligors=list(list(entity_id='A')),observations=rep(list(list(entity_id='A',observed_at='2024-01-01',available_at='2024-01-02')),2),facilities=list(list(entity_id='A',facility_id='F',ead=-1)),defaults=list(list(entity_id='A',episode_id='D',default_at='2024-03-01')),recoveries=list(list(cashflow_id='C',episode_id='unknown',cashflow_at='2024-03-02',amount=1,kind='recovery')))
v<-validate_credit_dataset(tables)$value
stopifnot(!v$valid,setequal(vapply(v$violations,function(r)r$code,character(1)),c('DUPLICATE_KEY','INVALID_AMOUNT','FOREIGN_KEY')))
# R check's current directory is local to the check workspace; no tempdir used.
path<-'_data_import_fixture.csv'
schema<-list(version='1.0',columns=list(id=list(type='string'),amount=list(type='number',unit='EUR'),active=list(type='boolean')),primary_key='id')
writeLines(c('id;amount;active','001;12,50;true'),path)
v<-read_local_dataset(path,schema,delimiter=';',decimal_mark=',')
stopifnot(v$value[[1]]$id=='001',v$value[[1]]$amount==12.5,isTRUE(v$value[[1]]$active),nchar(v$provenance$source_sha256)==64)
expect_code('DATA_TYPE',read_local_dataset(path,schema,delimiter=';'))
expect_code('DATA_REMOTE_PATH',read_local_dataset('https://example.org/a.csv',schema))
writeLines('id;amount;active',path)
expect_code('DATA_EMPTY',read_local_dataset(path,schema,delimiter=';'))
schema$allow_empty<-TRUE
stopifnot(length(read_local_dataset(path,schema,delimiter=';')$value)==0)
unlink(path)
cat('data contract tests passed\n')

closure_defaults<-list(list(episode_id='D',entity_id='A',default_at='2024-01-01',known_at='2024-01-01',ead=100,currency='EUR'))
closure_cures<-list(list(episode_id='D',cure_at='2024-02-01',known_at='2024-02-01',closed_at='2024-03-01',closed_known_at='2024-04-01'))
stopifnot(build_recovery_episodes(closure_defaults,list(),closure_cures,evaluation_cutoff='2024-03-15')$value$episodes[[1]]$status=='open',build_recovery_episodes(closure_defaults,list(),closure_cures,evaluation_cutoff='2024-04-01')$value$episodes[[1]]$status=='closed')
closure_cures[[1]]$closed_known_at<-NULL
expect_code('DATA_SCHEMA',build_recovery_episodes(closure_defaults,list(),closure_cures))
expect_code('DATA_SCHEMA',build_observation_windows(list(list(observation_id=list(),entity_id='A',as_of='2024-01-01')),list(),list(),30,'2024-02-01'))
stopifnot(validate_credit_dataset(list(obligors=list(list(entity_id=list()))))$value$violations[[1]]$code=='INVALID_KEY')
json_path<-'_data_duplicate_fixture.json'
writeLines('[{"id":"A","id":"B","amount":1,"active":true}]',json_path)
expect_code('DATA_DUPLICATE',read_local_dataset(json_path,schema))
unlink(json_path)
# Legal claim termination without inventing borrower cure.
d<-list(episode_id='D',entity_id='A',default_at='2024-01-01',known_at='2024-01-02',ead=100,currency='EUR')
cf<-list(cashflow_id='R',episode_id='D',cashflow_at='2024-03-01',known_at='2024-03-02',amount=60,kind='recovery',currency='EUR')
tm<-list(closure_id='C',episode_id='D',closed_at='2024-03-01',known_at='2024-03-05',closure_reason='SETTLEMENT')
run_closed<-function(ds=list(d),cs=list(cf),ts=list(tm),...)build_recovery_episodes(ds,cs,closures=ts,...)$value
ep<-run_closed()$episodes[[1]]
stopifnot(ep$status=='closed',ep$net_recovery==60,ep$closure_reason=='SETTLEMENT',ep$closed_at=='2024-03-01',run_closed(evaluation_cutoff='2024-03-04')$episodes[[1]]$status=='open')
expect_code('DATA_CLOSED_EPISODE',run_closed(cs=list(modifyList(cf,list(cashflow_at='2024-03-02')))))
expect_code('DATA_DUPLICATE',run_closed(ts=list(tm,tm)))
expect_code('DATA_FOREIGN_KEY',run_closed(ts=list(modifyList(tm,list(episode_id='missing')))))
expect_code('DATA_DATE_ORDER',run_closed(ts=list(modifyList(tm,list(known_at='2024-02-01')))))
expect_code('DATA_SCHEMA',run_closed(ts=list(modifyList(tm,list(closure_reason='CURE')))))
expect_code('DATA_DATE_ORDER',run_closed(ds=list(d,modifyList(d,list(episode_id='D2',default_at='2024-04-01',known_at='2024-04-02')))))
expect_code('DATA_TERMINAL_CONFLICT',run_closed(ds=list(d,modifyList(d,list(episode_id='D2',default_at='2024-02-01',known_at='2024-02-02'))),ts=list(tm,modifyList(tm,list(episode_id='D2',closure_id='C2')))))
expect_code('DATA_TERMINAL_CONFLICT',run_closed(cures=list(list(episode_id='D',cure_at='2024-02-01',known_at='2024-02-02',closed_at='2024-03-01',closed_known_at='2024-03-05'))))
stopifnot(identical(build_recovery_episodes(list(),list(),reopening_days=90)$value,build_recovery_episodes(list(),list(),reopening_days=90L)$value))
for(bad in list(TRUE,90.5,NaN,Inf,'90',-1)) expect_code('DATA_SCHEMA',build_recovery_episodes(list(),list(),reopening_days=bad))
