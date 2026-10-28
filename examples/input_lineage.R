# Read local bank tables, select a historical release and audit actual join rows.
prepare_input_snapshot <- function(accounts_path,releases_path,config,cutoff) {
 accounts<-read_local_dataset(accounts_path,config$left_schema)
 releases<-read_local_dataset(releases_path,config$right_schema)
 left<-lapply(accounts$value,function(r){r$known_at<-cutoff$known_at;r})
 joined<-join_as_of(left,releases$value)$value
 if(any(vapply(joined,function(r)is.null(r$release_id),logical(1))))stop('Missing historical release: provide coverage or handle referral before scoring')
 ids<-vapply(joined,function(r)r$release_id,character(1));selected<-Filter(function(r)r$release_id%in%ids,releases$value)
 ledger<-function(rows,side)lapply(rows,function(r)list(record_id=if(side=='right')r$release_id else r$record_id,join_key=r$entity_id,weight=1,amounts=list(balance=if(side=='right')0 else r$balance)))
 node<-function(identity,kind,rows,side,observed,available){
  n<-list(node_id=identity,kind=kind,schema_version='1',vintage=if(is.null(config$vintage))'2025-01'else config$vintage,revision=cutoff$revision,transformation_version=if(kind=='SOURCE')'selected-snapshot-v1' else 'join-as-of-v1',content_hash=content_fingerprint(rows)$value,observed_at=observed,available_at=available,records=ledger(rows,side))
  if(kind=='SOURCE'){n$source_id<-identity;n$semantic_role<-'FEATURE'};n
 }
 maximum<-function(rows,key)max(vapply(rows,function(r)r[[key]],character(1)))
 nodes<-list(node('accounts','SOURCE',left,'left',maximum(left,'as_of'),maximum(left,'known_at')),node('releases','SOURCE',selected,'right',maximum(selected,'observed_at'),maximum(selected,'available_at')),node('joined','FEATURE',joined,'left',maximum(left,'as_of'),cutoff$known_at))
 control<-config$controls;right_ids<-lapply(control$product_ids,function(p)paste0(p,'-',cutoff$revision))
 recs<-lapply(nodes,function(n)list(reconciliation_id=paste0('control-',n$node_id),node_id=n$node_id,reference='separately-supplied-account-control-ledger',expected_record_ids=if(n$node_id=='releases')right_ids else control$record_ids,expected_total_weight=control$total_weight,expected_amounts=list(balance=if(n$node_id=='releases')0 else control$total_balance)))
 join<-list(join_id='historical-snapshot-join',left_node_id='accounts',right_node_id='releases',output_node_id='joined',cardinality='one_to_one',join_type='left',weight_contract='LEFT_WEIGHT_ALLOCATION',amount_origin='LEFT',mapping=lapply(joined,function(r)list(output_id=r$record_id,left_id=r$record_id,right_id=r$release_id,allocation_weight=1)),excluded_left_ids=list(),unmatched_right_ids=list())
 contract<-list(contract_id='local-input-control',version='1',reference='independent-balance-and-selected-release-control',amount_fields=list(balance='EUR'),absolute_tolerance=0,relative_tolerance=0,required_node_ids=list('accounts','releases','joined'),required_use_ids=list('assessment-input'),required_join_ids=list('historical-snapshot-join'))
 hashes<-list(accounts=nodes[[1]]$content_hash,releases=nodes[[2]]$content_hash)
 args<-list(nodes=nodes,edges=list(list(parent_id='accounts',child_id='joined'),list(parent_id='releases',child_id='joined')),joins=list(join),reconciliations=recs,uses=list(list(use_id='assessment-input',node_id='joined',purpose='FEATURE',as_of=maximum(left,'as_of'),known_at=cutoff$known_at,source_snapshot_hashes=hashes)),contract=contract)
 audit<-do.call(validate_source_lineage,args)$value
 list(rows=joined,audit=audit,audit_inputs=args,archive_sha256=list(accounts=accounts$provenance$source_sha256,releases=releases$provenance$source_sha256),selected_snapshot_hash=nodes[[2]]$content_hash)
}

run_input_lineage <- function(accounts_path=NULL,releases_path=NULL,config=NULL,verify_reference=TRUE) {
 if(is.null(config))config<-jsonlite::fromJSON(system.file('extdata/input_lineage.json',package='creditratingsystem'),simplifyVector=FALSE)
 if(is.null(accounts_path))accounts_path<-system.file('extdata/lineage_accounts.csv',package='creditratingsystem')
 if(is.null(releases_path))releases_path<-system.file('extdata/lineage_releases.csv',package='creditratingsystem')
 outputs<-list()
 for(cutoff in config$cutoffs){
  result<-prepare_input_snapshot(accounts_path,releases_path,config,cutoff)
  if(verify_reference)stopifnot(result$audit$decision$status=='PASS',identical(vapply(result$rows,function(r)r$income,numeric(1),USE.NAMES=FALSE),as.numeric(unlist(cutoff$expected_income))),result$audit$node_totals$joined$amounts$balance==config$controls$total_balance)
  outputs[[cutoff$name]]<-result
 }
 replay<-prepare_input_snapshot(accounts_path,releases_path,config,config$cutoffs[[1]])
 stopifnot(identical(replay,outputs[[config$cutoffs[[1]]$name]]))
 list(snapshots=outputs,original_replay_exact=TRUE,interpretation='Historical source selection and independently supplied ledger controls; content hashes identify bytes/values and do not authenticate the source author.')
}
