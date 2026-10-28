library(creditratingsystem)
canonical_id_fixture<-function(reverse=FALSE) {
 ids<-c('a','B','A','b','é');entities<-lapply(ids,function(x)list(entity_id=x))
 edge<-function(i,a,b)list(edge_id=i,source_id=a,target_id=b,edge_type='CROSS_DEFAULT',valid_from='2024-01-01',valid_until=NULL,known_at='2024-01-01',evidence_reference='explicit mutual cross-default')
 edges<-list(edge('z','A','a'),edge('Z','a','A'))
 rows<-lapply(ids,function(x)list(position_id=paste0('p',x),claim_id=paste0('c',x),facility_id=paste0('f',x),entity_id=x,currency='EUR',ead=100,facility_ead=100,sector=x,region='r',exposure_basis='GROSS',collateral_treatment='NONE'))
 if(reverse){entities<-rev(entities);edges<-rev(edges);rows<-rev(rows)}
 graph<-build_entity_graph(entities,edges,list(policy_id='graph',version='1',edge_types=list('CROSS_DEFAULT')),as_of='2024-01-01',known_at='2024-01-01')$value
 policy<-list(policy_id='default',version='1',edge_types=list('CROSS_DEFAULT'),direction_policy='mutual_only',transitive=TRUE)
 contract<-list(portfolio_id='mixed-case',version='1',currency='EUR',as_of='2024-01-01',known_at='2024-01-01',horizon=1,time_unit='years',probability_measure='PHYSICAL',conditioning='initially_performing',event='first_default')
 portfolio<-prepare_credit_portfolio(rows,graph,policy,contract)$value
 concentration<-credit_concentration_measures(rows,weight_field='ead',group_fields=list('sector'),weight_unit='EUR')$value
 list(graph=graph,portfolio=portfolio,concentration=concentration)
}
value<-canonical_id_fixture();stopifnot(identical(value,canonical_id_fixture(TRUE)),identical(vapply(value$graph$entities,function(x)x$entity_id,character(1)),c('A','B','a','b','é')),identical(vapply(value$graph$edges,function(x)x$edge_id,character(1)),c('Z','z')),identical(vapply(value$portfolio$positions,function(x)x$position_id,character(1)),c('pA','pB','pa','pb','pé')),identical(vapply(value$concentration$dimensions[[1]]$groups,function(x)x$group_id,character(1)),c('pA','pB','pa','pb','pé')))
for(name in c('graph','portfolio')){field<-paste0(name,'_hash');sealed<-value[[name]];hash<-sealed[[field]];sealed[[field]]<-NULL;stopifnot(content_fingerprint(sealed)$value==hash)}
stopifnot(any(vapply(value$portfolio$default_group_assignment$groups,function(x)identical(unlist(x$members),c('A','a')),logical(1))))
