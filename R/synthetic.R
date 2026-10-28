# Native streamed synthetic worlds; actual shared innovations, no internal RNG.
.cr_syn_tiers <- list(
 tiny=list(entities=64L,years=3L,macro_months=36L,paths=512L,steps=24L,applications=320L,households=160L,persons=240L,application_months=12L,account_months=24L,pools=2L,pool_assets=80L,subjects=500L),
 standard=list(entities=3000L,years=10L,macro_months=240L,paths=16384L,steps=60L,applications=50000L,households=20000L,persons=30000L,application_months=36L,account_months=60L,pools=12L,pool_assets=30000L,subjects=60000L),
 stress=list(entities=20000L,years=15L,macro_months=360L,paths=131072L,steps=120L,applications=500000L,households=200000L,persons=300000L,application_months=84L,account_months=120L,pools=100L,pool_assets=100000L,subjects=600000L))
.cr_syn_layers <- c("observable_inputs","latent_truth","frozen_predictions","decision_history","observed_outcomes")
.cr_syn_factors <- c("activity","employment","rate","property","transport","credit","fx","energy")
.cr_syn_date <- function(x) {
 if(!is.character(x)||length(x)!=1L||is.na(x)||!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",x)) .cr_error("SYNTHETIC_DATE","Require ISO date")
 d<-tryCatch(as.Date(x),error=function(e) NA)
 if(is.na(d)||format(d,"%Y-%m-%d")!=x) .cr_error("SYNTHETIC_DATE","Require ISO date")
 d
}
.cr_syn_month <- function(d,n,end=FALSE) {
 total<-as.integer(format(d,"%Y"))*12+as.integer(format(d,"%m"))-1+n;y<-total%/%12;m<-total%%12+1
 first<-as.Date(sprintf("%04d-%02d-01",y,m));last<-seq(first,by="month",length.out=2L)[2L]-1
 as.Date(sprintf("%04d-%02d-%02d",y,m,if(end) as.integer(format(last,"%d")) else min(as.integer(format(d,"%d")),as.integer(format(last,"%d")))))
}
.cr_syn_spec <- function(s) {
 if(!is.list(s)||is.null(s$tier)||!s$tier %in% names(.cr_syn_tiers)||is.null(s$dgp_family)||!s$dgp_family %in% c("well_specified","misspecified")) .cr_error("SYNTHETIC_SPEC","Declare tier and DGP family")
 if(.cr_syn_date(s$observation_cutoff)<.cr_syn_date(s$start_date)) .cr_error("SYNTHETIC_SPEC","Cutoff precedes start")
 .cr_syn_tiers[[s$tier]]
}
.cr_syn_json <- function(x) as.character(jsonlite::toJSON(x,auto_unbox=TRUE,null="null",na="null",digits=17,force=TRUE))
.cr_syn_sha <- function(path) digest::digest(file=path,algo="sha256",serialize=FALSE)
.cr_syn_draws <- function(directory) {
 manifest<-tryCatch(jsonlite::fromJSON(.cr_syn_resource(directory,"manifest.json"),simplifyVector=FALSE),error=function(e) .cr_error("SYNTHETIC_INNOVATIONS","Readable innovation manifest required"))
 if(!identical(manifest$format,"creditratingsystem-u32le-v1")) .cr_error("SYNTHETIC_INNOVATIONS","Unsupported innovation format")
 cache<-new.env(parent=emptyenv()); row<-function(stream,index,minimum) {
  if(!exists(stream,cache,inherits=FALSE)) {
   d<-manifest$streams[[stream]];shape<-unlist(d$shape)
   if(is.null(d)||!is.character(d$file)||length(d$file)!=1L||!is.character(d$sha256)||length(d$sha256)!=1L||!is.numeric(shape)||length(shape)!=2L||any(!is.finite(shape))||any(shape<=0)||any(shape!=floor(shape))||basename(d$file)!=d$file) .cr_error("SYNTHETIC_INNOVATIONS","Missing/invalid stream")
   path<-.cr_syn_resource(directory,d$file)
   if(!file.exists(path)||file.info(path)$size!=prod(shape)*4||.cr_syn_sha(path)!=d$sha256) .cr_error("SYNTHETIC_INNOVATIONS","Stream size/hash mismatch")
   # Bounded block cache, independent of requested row order.
   assign(stream,list(path=path,shape=shape,block=-1L,values=NULL),cache)
  }
  d<-get(stream,cache,inherits=FALSE)
  if(index<0||index>=d$shape[1L]||d$shape[2L]<minimum) .cr_error("SYNTHETIC_INNOVATIONS","Stream capacity too small")
  block<-floor(index/1024)
  if(block!=d$block) {
   con<-file(d$path,"rb");seek(con,where=block*1024*d$shape[2L]*4,origin="start")
   raw<-readBin(con,"raw",n=min(1024,d$shape[1L]-block*1024)*d$shape[2L]*4);close(con)
   bytes<-matrix(as.numeric(raw),nrow=4L)
   values<-colSums(bytes*c(1,256,65536,16777216))
   d$values<-matrix((values+.5)/4294967296,ncol=d$shape[2L],byrow=TRUE);d$block<-block;assign(stream,d,cache)
  }
  d$values[index-block*1024+1L,]
 }
 normals<-function(stream,index,count) {
  u<-row(stream,index,2*ceiling(count/2));z<-numeric()
  for(j in seq.int(1L,2*ceiling(count/2),2L)) {radius<-sqrt(-2*log(u[j]));angle<-2*pi*u[j+1L];z<-c(z,radius*cos(angle),radius*sin(angle))}
  z[seq_len(count)]
 }
 list(manifest=manifest,row=row,normals=normals)
}
.cr_syn_writer <- function(output_directory,world_type,specification,innovations,chunk_rows,memory_budget_mb) {
 if(length(chunk_rows)!=1L||!is.numeric(chunk_rows)||!is.finite(chunk_rows)||chunk_rows!=floor(chunk_rows)||chunk_rows<1||chunk_rows>50000||!is.numeric(memory_budget_mb)||length(memory_budget_mb)!=1L||!is.finite(memory_budget_mb)||memory_budget_mb<32||memory_budget_mb!=floor(memory_budget_mb)) .cr_error("SYNTHETIC_BUDGET","Invalid chunk or memory budget")
 estimated_mb<-32+chunk_rows*.004+.cr_syn_tiers[[specification$tier]]$entities*.002
 if(memory_budget_mb<estimated_mb) .cr_error("SYNTHETIC_BUDGET","Declared budget below conservative stream/index estimate",list(estimated_working_mb=estimated_mb))
 if(dir.exists(output_directory)&&length(list.files(output_directory,all.files=TRUE,no..=TRUE))) .cr_error("SYNTHETIC_OUTPUT","Output must be absent or empty")
 dir.create(output_directory,recursive=TRUE,showWarnings=FALSE)
 for(layer in .cr_syn_layers) dir.create(file.path(output_directory,layer),showWarnings=FALSE)
 e<-new.env(parent=emptyenv());e$tables<-.cr_empty_object();e$files<-list();e$binary<-.cr_empty_object();e$buffers<-list();e$buffered<-0L;started<-proc.time()[[3L]]
 flush_rows<-function() {
  for(key in names(e$buffers)) {
   batch<-e$buffers[[key]];if(!length(batch)) next
   columns<-lapply(names(batch[[1L]]),function(name) {
    values<-lapply(batch,function(r) r[[name]]);present<-Filter(Negate(is.null),values)
    prototype<-if(length(present)) present[[1L]] else NA
    if(is.character(prototype)) vapply(values,function(x) if(is.null(x)) NA_character_ else x,character(1))
    else if(is.logical(prototype)) vapply(values,function(x) if(is.null(x)) NA else x,logical(1))
    else vapply(values,function(x) if(is.null(x)) NA_real_ else as.numeric(x),numeric(1))
   })
   names(columns)<-names(batch[[1L]]);frame<-as.data.frame(columns,stringsAsFactors=FALSE,check.names=FALSE)
   jsonlite::stream_out(frame,e$files[[key]],pagesize=chunk_rows,verbose=FALSE,digits=17,na="null")
  }
  e$buffers<-list();e$buffered<-0L
 }
 add<-function(layer,table,row) {
  key<-paste(layer,table,sep="/")
  if(is.null(e$files[[key]])) {
   relative<-paste0(key,".jsonl.gz");e$files[[key]]<-gzfile(file.path(output_directory,relative),"wb")
   e$tables[[key]]<-list(file=relative,rows=0L,columns=as.list(names(row)))
  }
  if(!identical(names(row),unlist(e$tables[[key]]$columns))) .cr_error("SYNTHETIC_SCHEMA","Columns changed")
  batch<-e$buffers[[key]];batch[[length(batch)+1L]]<-row;e$buffers[[key]]<-batch;e$buffered<-e$buffered+1L
  e$tables[[key]]$rows<-e$tables[[key]]$rows+1L
  if(e$buffered>=chunk_rows) flush_rows()
 }
 finish<-function(additional=list()) {
  flush_rows()
  for(con in e$files) close(con)
  for(key in names(e$tables)) e$tables[[key]]$sha256<-.cr_syn_sha(file.path(output_directory,e$tables[[key]]$file))
  manifest<-c(list(schema_version="1.0",world_type=world_type,generator_version="1.0",data_origin="SYNTHETIC",specification=specification,innovation_manifest_hash=if(is.null(innovations)) NULL else .cr_hash(innovations$manifest),chunk_rows=chunk_rows,memory_budget_mb=memory_budget_mb,estimated_working_mb=estimated_mb,layers=as.list(.cr_syn_layers),tables=e$tables,binary_resources=e$binary,limitations=list("Synthetic engineering evidence does not establish real portfolio performance."),actual_rows=sum(vapply(e$tables,function(x) x$rows,numeric(1))),runtime_seconds=proc.time()[[3L]]-started),additional)
  writeLines(.cr_syn_json(manifest),file.path(output_directory,"manifest.json"),useBytes=TRUE)
  .cr_result(list(directory=output_directory,manifest=manifest),paste0("synthetic.generate_",world_type),"synthetic_world",provenance=list(data_origin="SYNTHETIC",parameter_origin="SYNTHETIC_REFERENCE"))
 }
 list(add=add,finish=finish,env=e,root=output_directory)
}
.cr_syn_world <- function(world) {
 if(inherits(world,"CalcResult")) world<-world$value
 root<-if(is.list(world)) world$directory else world
 path<-.cr_syn_resource(root,"manifest.json")
 manifest<-tryCatch(jsonlite::fromJSON(path,simplifyVector=FALSE),error=function(e) .cr_error("SYNTHETIC_WORLD","Readable generated world required"))
 if(!identical(manifest$schema_version,"1.0")||!identical(manifest$data_origin,"SYNTHETIC")) .cr_error("SYNTHETIC_WORLD","Invalid world")
 list(root=root,manifest=manifest)
}
.cr_syn_resource <- function(root,relative) {
 if(!is.character(relative)||length(relative)!=1L||is.na(relative)||!nzchar(relative)||grepl("^/|^[A-Za-z]:|\\\\",relative)||".." %in% strsplit(relative,"/",fixed=TRUE)[[1L]]) .cr_error("SYNTHETIC_WORLD","Resource path must be relative and contained")
 path<-file.path(root,relative)
 if(!file.exists(path)||dir.exists(path)) .cr_error("SYNTHETIC_WORLD","Resource is missing")
 # Use one separator on all platforms before checking the directory boundary.
 canonical<-normalizePath(path,winslash="/",mustWork=TRUE)
 base<-normalizePath(root,winslash="/",mustWork=TRUE)
 if(!startsWith(canonical,paste0(sub("/+$","",base),"/"))) .cr_error("SYNTHETIC_WORLD","Resource escapes its world directory")
 path
}
.cr_syn_linked <- function(world,s,world_type=NULL) {
 x<-.cr_syn_world(world)
 if(!is.null(world_type)&&x$manifest$world_type!=world_type) .cr_error("SYNTHETIC_LINK","Unexpected source world type")
 for(k in c("tier","start_date","observation_cutoff")) if(!identical(x$manifest$specification[[k]],s[[k]])) .cr_error("SYNTHETIC_LINK","Source tier/start/cutoff must agree")
 invisible(x)
}
.cr_syn_require <- function(draws,requirements) {
 for(r in requirements) draws$row(r[[1L]],r[[2L]]-1L,r[[3L]])
 invisible(NULL)
}
.cr_syn_each <- function(world,layer,table,fun,check_hashes=TRUE) {
 x<-.cr_syn_world(world);d<-x$manifest$tables[[paste(layer,table,sep="/")]]
 if(is.null(d)) return(invisible(NULL))
 if(!identical(d$file,paste0(layer,"/",table,".jsonl.gz"))) .cr_error("SYNTHETIC_WORLD","Unsafe table path")
 path<-.cr_syn_resource(x$root,d$file)
 if(check_hashes&&!identical(.cr_syn_sha(path),d$sha256)) .cr_error("SYNTHETIC_WORLD","Source table checksum mismatch")
 con<-gzfile(path,"rt");on.exit(close(con))
 repeat {lines<-readLines(con,n=1000L,warn=FALSE);if(!length(lines)) break;for(line in lines) fun(jsonlite::fromJSON(line,simplifyVector=FALSE))}
 invisible(NULL)
}
.cr_syn_table <- function(world,layer,table) {
 rows<-list();.cr_syn_each(world,layer,table,function(r) rows[[length(rows)+1L]]<<-r);rows
}
.cr_syn_splits <- function(group,period,periods) {
 entity<-if(group%%10<6) "train" else if(group%%10<8) "calibration" else "test"
 temporal<-if(period<.6*periods) "train" else if(period<.8*periods) "calibration" else "test"
 list(entity=entity,temporal=temporal,split=if(entity==temporal) entity else "excluded")
}

generate_macro_reference_world <- function(specification,innovations,output_directory,chunk_rows=1000L,memory_budget_mb=256L) {
 s<-specification;t<-.cr_syn_spec(s);draws<-.cr_syn_draws(innovations);.cr_syn_require(draws,list(list("macro_history",t$macro_months,10L),list("macro_revision",t$macro_months,8L),list("macro_paths",t$paths*t$steps,10L)));w<-.cr_syn_writer(output_directory,"macro_reference_world",s,draws,chunk_rows,memory_budget_mb)
 start<-.cr_syn_date(s$start_date);cutoff<-.cr_syn_date(s$observation_cutoff);rho<-c(.85,.8,.75,.7,.9,.65,.6,.5);state<-rep(0,8);indices<-rep(100,8)
 for(m in seq_len(t$macro_months)-1L) {
  z<-draws$normals("macro_history",m,9L);u<-draws$row("macro_revision",m,8L);regime<-if(s$dgp_family=="misspecified"&&m>=floor(.7*t$macro_months)) "stress" else "baseline"
  for(j in seq_len(8L)) {shock<-.25*z[1L]+sqrt(.9375)*z[j+1L];state[j]<-rho[j]*state[j]+sqrt(1-rho[j]^2)*shock+if(regime=="stress"&&j %in% c(1L,4L,5L)) -.3 else 0;indices[j]<-indices[j]*exp(.0015+.002*state[j])}
  observed<-.cr_syn_month(start,m,TRUE)
  values<-c(indices[1],min(.3,max(.005,.06-.008*state[2])),min(.3,max(-.01,.03+.004*state[3])),indices[4],indices[5],min(.4,max(.001,.02-.003*state[6])),exp(.08*state[7]),indices[8])
  w$add("latent_truth","macro_factors",c(list(period=m,date=as.character(observed),regime=regime),as.list(stats::setNames(values,.cr_syn_factors))))
  for(j in seq_len(8L)) for(revision in 0:1) {
   known<-observed+c(30,90)[revision+1L]
   if(known<=cutoff) w$add("observable_inputs","macro_releases",list(release_id=sprintf("macro-%04d-%d-%s",m,j-1L,c("initial","revised")[revision+1L]),series=.cr_syn_factors[j],observed_at=as.character(observed),known_at=as.character(known),revision=c("initial","revised")[revision+1L],value=if(revision==0L) values[j]*(1+(u[j]-.5)*.01) else values[j],unit=if(j %in% c(2,3,6)) "decimal_rate" else if(j==7L) "fx_ratio" else "index"))
  }
 }
 path<-file.path(output_directory,"latent_truth/scenario_cube.f64le");con<-file(path,"wb")
 for(p in seq_len(t$paths)-1L) {
  x<-state;block<-numeric(t$steps*8L)
  for(step in seq_len(t$steps)-1L) {z<-draws$normals("macro_paths",p*t$steps+step,9L);x<-rho*x+sqrt(1-rho^2)*(.25*z[1L]+sqrt(.9375)*z[2:9]);block[(step*8L+1L):(step*8L+8L)]<-x}
  writeBin(block,con,size=8L,endian="little")
 };close(con)
 w$env$binary$scenario_cube<-list(file="latent_truth/scenario_cube.f64le",dtype="float64-little-endian",shape=as.list(c(t$paths,t$steps,8L)),order="path,month,factor",factors=as.list(.cr_syn_factors),origin=as.character(.cr_syn_month(start,t$macro_months-1L,TRUE)),sha256=.cr_syn_sha(path))
 w$finish(list(dgp=list(rho=as.list(rho),common_innovation_loading=.25,factor_stationary_variance_baseline=1,release_lags_days=list(30,90),forecast_history_separation="scenario dates strictly after final macro history date")))
}

generate_obligor_panel <- function(macro_world,specification,innovations,output_directory,chunk_rows=1000L,memory_budget_mb=256L) {
 s<-specification;t<-.cr_syn_spec(s);draws<-.cr_syn_draws(innovations);.cr_syn_require(draws,list(list("obligor_static",t$entities,16L),list("obligor_year",t$entities*t$years,16L),list("obligor_groups",ceiling(t$entities/4)*t$years,2L),list("country_transfer",6*t$years,2L)));.cr_syn_linked(macro_world,s,"macro_reference_world");macro<-.cr_syn_table(macro_world,"latent_truth","macro_factors")
 if(length(macro)<12*t$years) .cr_error("SYNTHETIC_LINK","Macro history must cover financial years")
 w<-.cr_syn_writer(output_directory,"obligor_panel",s,draws,chunk_rows,memory_budget_mb);start<-.cr_syn_date(s$start_date);cutoff<-.cr_syn_date(s$observation_cutoff)
 transfer<-list()
 for(c in 0:5) for(y in seq_len(t$years)-1L) {
  tu<-draws$row("country_transfer",c*t$years+y,2L);probability<-.005+.04*max(0,macro[[y*12+12]]$employment-.04);transfer[[paste(c,y)]]<-list(pd=probability,event=tu[2]<probability)
  w$add("latent_truth","country_transfer",list(country_id=paste0("country-",c),period=y,transfer_pd=probability,transfer_event=tu[2]<probability,currency_scope="foreign_currency"))
 }
 for(i in seq_len(t$entities)-1L) {
  u<-draws$row("obligor_static",i,16L);oid<-sprintf("obligor-%06d",i);group<-i%/%4;country<-paste0("country-",i%%6)
  sector<-c("industry","trade","property","transport","energy","services")[i%%6+1L];assets0<-exp(13+u[1]*7);leverage<-.15+.65*u[2];sigma<-.12+.3*u[3]
  w$add("observable_inputs","obligors",list(obligor_id=oid,group_id=sprintf("group-%06d",group),country_id=country,sector=sector,currency="EUR",established_at=as.character(start),known_at=as.character(start)))
  if(i%%4!=0) w$add("observable_inputs","support_edges",list(edge_id=paste0("support-",oid),supporter_id=sprintf("obligor-%06d",4*group),beneficiary_id=oid,scope="senior_unsecured",capacity=assets0*.1,effective_from=as.character(start),effective_to=as.character(.cr_syn_month(start,t$years*12)),known_at=as.character(start)))
  for(y in seq_len(t$years)-1L) {
   observed<-.cr_syn_month(start,y*12+11,TRUE);known<-observed+90;v<-draws$row("obligor_year",i*t$years+y,16L);g<-draws$normals("obligor_groups",group*t$years+y,1L)[1]
   m<-macro[[min(y*12+12,length(macro))]];cycle<-(m$activity-100)/20;assets<-assets0*exp(.02*y+.07*(v[1]-.5));debt<-assets*leverage;equity<-assets-debt;revenue<-assets*(.4+u[4])
   margin<-.02+.18*u[5]+.015*cycle;ebitda<-revenue*margin;depreciation<-assets*.025;ebit<-ebitda-depreciation;interest<-debt*(.025+m$rate);tax<-max(0,ebit-interest)*.25;capex<-assets*.03;cfads<-ebitda-tax-capex;debt_service<-interest+debt*.1
   features_eta<--4.2+3*(leverage-.4)-2*margin-.35*cycle;true_eta<-if(s$dgp_family=="well_specified") features_eta else features_eta+.6*g+3*(leverage-.5)^2+.7*(margin<.05)
   hazard<-if(i%%32==0) 0 else if(i%%32==1) .0001 else stats::plogis(true_eta)
   d1<-(log(assets/debt)+(m$rate+.5*sigma*sigma))/sigma;d2<-d1-sigma;market_equity<-assets*stats::pnorm(d1)-debt*exp(-m$rate)*stats::pnorm(d2);equity_vol<-sigma*assets*stats::pnorm(d1)/market_equity
   splits<-.cr_syn_splits(group,y,t$years);snapshot_id<-sprintf("financial-%06d-%02d",i,y)
   if(known<=cutoff) {
    w$add("observable_inputs","financial_statements",list(snapshot_id=snapshot_id,obligor_id=oid,observed_at=as.character(observed),known_at=as.character(known),assets=assets,debt=debt,equity=equity,revenue=revenue,ebitda=ebitda,depreciation=depreciation,ebit=ebit,interest=interest,tax=tax,capex=capex,cfads=cfads,debt_service=debt_service,market_equity=market_equity,equity_volatility=equity_vol,risk_free_rate=m$rate,entity_split=splits$entity,time_split=splits$temporal,split=splits$split))
    opinion<-min(7,max(1,round(2+4*leverage-8*margin+(v[3]-.5)*2)))
    w$add("observable_inputs","external_opinions",list(opinion_id=paste0("opinion-",snapshot_id),obligor_id=oid,observed_at=as.character(observed),known_at=as.character(known),grade=opinion,provider="synthetic_independent_opinion",valid_until=as.character(known+365)))
    w$add("frozen_predictions","obligor_predictions",list(snapshot_id=snapshot_id,obligor_id=oid,as_of=as.character(known),model_version="fixed-corporate-logit-v1",pd=stats::plogis(features_eta),horizon_months=12))
   }
   defaulted<-v[4]<hazard;event<-if(defaulted) known+1+floor(v[5]*364) else NULL
   w$add("latent_truth","obligor_period",list(snapshot_id=snapshot_id,obligor_id=oid,period=y,as_of=as.character(known),true_pd=hazard,asset_value=assets,asset_volatility=sigma,origination_leverage=leverage,group_factor=g,dgp_stratum=if(i%%32==0) "zero_default" else if(i%%32==1) "low_default" else "ordinary",potential_default_at=if(is.null(event)) NULL else as.character(event)))
   tr<-transfer[[paste(i%%6,y)]]
   w$add("latent_truth","foreign_currency_risk",list(snapshot_id=snapshot_id,obligor_id=oid,country_id=country,base_pd=hazard,transfer_pd=tr$pd,independent_union_pd=1-(1-hazard)*(1-tr$pd),base_event=defaulted,transfer_event=tr$event,joint_default_event=defaulted||tr$event,scope="foreign_currency_sensitivity_separate_from_base_labels"))
   if(known<=cutoff) {matured<-known+365<=cutoff;event_known<-if(!is.null(event)) event+14 else NULL;seen<-!is.null(event_known)&&event_known<=cutoff
    w$add("observed_outcomes","obligor_labels",list(snapshot_id=snapshot_id,obligor_id=oid,window_start=as.character(known),window_end=as.character(known+365),evaluation_cutoff=as.character(cutoff),default_indicator=if(seen) 1 else if(matured) 0 else NULL,censored=!matured&&!seen))}
   if(!is.null(event)&&event+14<=cutoff) w$add("observed_outcomes","default_events",list(event_id=paste0("default-",oid),obligor_id=oid,event_at=as.character(event),known_at=as.character(event+14),event_type="DEFAULT"))
   kind<-c("commercial_property","aircraft","ship","construction_project","leasing")[i%%5+1L];units<-10+floor(u[6]*90);occupied<-floor(units*(.65+.3*v[6]))
   if(known<=cutoff) w$add("observable_inputs","operating_assets",list(asset_id=paste0("asset-",oid),obligor_id=oid,asset_type=kind,observed_at=as.character(observed),known_at=as.character(known),market_value=assets*.7,capacity=units,occupied_units=occupied,utilization=occupied/units,contract_end=as.character(.cr_syn_month(known,24+floor(u[7]*120))),gross_rent_or_revenue=revenue,operating_cost=revenue-ebitda,maintenance_reserve=assets*.01,cfads=cfads,debt_service=debt_service,completion_fraction=min(1,(y+1)/3),lessee_id=paste0("lessee-",oid),lessor_id=paste0("lessor-",oid),spc_id=paste0("spc-",oid)))
   if(known<=cutoff) {
    tenants<-if(kind=="commercial_property") 3L else 1L
    for(tenant in seq_len(tenants)-1L) w$add("observable_inputs","operating_contracts",list(contract_id=paste0("operating-contract-",oid,"-",y,"-",tenant),asset_id=paste0("asset-",oid),obligor_id=oid,counterparty_id=if(kind=="commercial_property") paste0("tenant-",oid,"-",tenant) else paste0("lessee-",oid),contract_type=if(kind %in% c("commercial_property","leasing")) "lease" else if(kind %in% c("aircraft","ship")) "charter" else "offtake",known_at=as.character(known),effective_from=as.character(known),effective_to=as.character(.cr_syn_month(known,24+tenant*12+floor(u[7]*60))),annual_contract_revenue=revenue/tenants,occupied_capacity=occupied/tenants,currency="EUR"))
    if(i%%11==0) w$add("observable_inputs","rating_overrides",list(override_id=paste0("override-",snapshot_id),obligor_id=oid,known_at=as.character(known),effective_from=as.character(known),effective_to=as.character(.cr_syn_month(known,6)),grade_before=opinion,grade_after=min(7,opinion+1),reason_code="documented_liquidity_concern",approved_by="synthetic_second_line"))
   }
   if(defaulted) break
  }
 }
 w$finish(list(dependencies=list(macro_manifest_hash=.cr_hash(.cr_syn_world(macro_world)$manifest)),dgp=list(first_default_absorbing=TRUE,financial_reporting_lag_days=90,outcome_reporting_lag_days=14,split_unit="economic_group",split_rule="group modulo10 and forward time60/20/20; cross-cells excluded",structural_equity="European call on asset value, horizon1year")))
}

generate_facility_workouts <- function(obligor_world,specification,innovations,output_directory,chunk_rows=1000L,memory_budget_mb=256L) {
 s<-specification;t<-.cr_syn_spec(s);draws<-.cr_syn_draws(innovations);draws$row("facilities",t$entities-1L,16L);.cr_syn_linked(obligor_world,s,"obligor_panel");w<-.cr_syn_writer(output_directory,"facility_workouts",s,draws,chunk_rows,memory_budget_mb);start<-.cr_syn_date(s$start_date);cutoff<-.cr_syn_date(s$observation_cutoff)
 defaults<-list();.cr_syn_each(obligor_world,"observed_outcomes","default_events",function(r) defaults[[r$obligor_id]]<<-r);i<--1L
 origin_leverage<-list();.cr_syn_each(obligor_world,"latent_truth","obligor_period",function(r) origin_leverage[[r$obligor_id]]<<-r$origination_leverage);tenor_months<-(t$years+2)*12;exposure_dates<-seq(as.Date(format(start,"%Y-%m-01")),by="month",length.out=t$years*12+1L)[-1L]-1;exposure_strings<-as.character(exposure_dates)
 .cr_syn_each(obligor_world,"observable_inputs","obligors",function(o) {
  i<<-i+1L;u<-draws$row("facilities",i,16L);limit0<-exp(12+6*u[1]);badness<-.5*u[2]+.5*((origin_leverage[[o$obligor_id]]-.15)/.65)
  for(k in 0:1) {
   fid<-sprintf("facility-%06d-%d",i,k);limit<-limit0*if(k==0) 1 else .4;revolving<-k==1;balance<-if(revolving) limit*(.25+.5*u[3]) else limit
   w$add("observable_inputs","facilities",list(facility_id=fid,obligor_id=o$obligor_id,product=if(revolving) "revolving" else "term_loan",currency="EUR",origination_at=as.character(start),known_at=as.character(start),maturity_at=as.character(.cr_syn_month(start,tenor_months)),original_limit=limit,original_balance=balance,interest_rate=.035+.025*badness,seniority=if(k==0) "senior_secured" else "senior_unsecured",collateral_id=if(k==0) paste0("collateral-",fid) else NULL))
   w$add("observable_inputs","collateral",list(collateral_id=paste0("collateral-",fid),facility_id=fid,valuation_at=as.character(start),known_at=as.character(start),value=if(k==0) limit*(1.1+.5*u[4]) else 0,haircut=.15+.15*badness,lien_rank=1,currency="EUR"))
   event<-defaults[[o$obligor_id]]
   for(month in seq_len(t$years*12)-1L) {
    d<-exposure_dates[month+1L];if(d>cutoff||(!is.null(event)&&d>=.cr_syn_date(event$event_at))) break
    amount<-if(revolving) limit*(.25+.5*u[3]+.1*sin(month/6)) else balance*max(0,1-month/tenor_months)
    w$add("observable_inputs","facility_exposures",list(facility_id=fid,observed_at=exposure_strings[month+1L],known_at=exposure_strings[month+1L],drawn_balance=amount,credit_limit=limit,undrawn=limit-amount))
   }
   if(!is.null(event)) {
    d<-.cr_syn_date(event$event_at);elapsed<-(as.integer(format(d,"%Y"))-as.integer(format(start,"%Y")))*12+as.integer(format(d,"%m"))-as.integer(format(start,"%m"))
    prior<-if(revolving) limit*(.25+.5*u[3]+.1*sin(elapsed/6)) else balance*max(0,1-elapsed/tenor_months)
    ead<-if(revolving) prior+max(0,limit-prior)*(.3+.6*badness) else prior;recovery_fraction<-if(k==0) .65+.45*u[5]-.15*badness else .05+.6*u[5]-.05*badness;cost_fraction<-.03+.15*badness;rate<-.04;total_pv<-0
    for(n in 0:2) {
     delay<-c(90,360,900)[n+1L];rd<-d+delay;last<-rd;recovery<-ead*recovery_fraction*c(.2,.3,.5)[n+1L];cost<-ead*cost_fraction/3;total_pv<-total_pv+(recovery-cost)/(1+rate)^(delay/365)
     row<-list(cashflow_id=paste0("recovery-",fid,"-",n),facility_id=fid,default_event_id=event$event_id,event_at=as.character(rd),known_at=as.character(rd+7),recovery_amount=recovery,cost_amount=cost,currency="EUR")
     w$add("latent_truth","future_recoveries",row);if(rd+7<=cutoff) w$add("observed_outcomes","recovery_cashflows",row)
    }
    w$add("observed_outcomes","workouts",list(workout_id=paste0("workout-",fid),facility_id=fid,default_event_id=event$event_id,default_at=as.character(d),known_at=event$known_at,ead=ead,discount_rate=rate,status=if(last+7<=cutoff) "closed" else "open",closed_at=if(last+7<=cutoff) as.character(last) else NULL))
    w$add("latent_truth","workout_parameters",list(facility_id=fid,ead=ead,recovery_fraction=recovery_fraction,cost_fraction=cost_fraction,ultimate_discounted_lgd=1-total_pv/ead,driver=badness))
   }
  }
 })
 names<-c("negative_lgd","lgd_above_one","total_loss","no_loss");recoveries<-c(120,0,0,100);costs<-c(0,20,0,0)
 for(i in seq_len(4L)) w$add("latent_truth","analytical_workout_fixtures",list(case_id=names[i],ead=100,recovery_at_default=recoveries[i],cost_at_default=costs[i],expected_raw_lgd=1-(recoveries[i]-costs[i])/100,constructed=TRUE))
 w$add("latent_truth","constructed_open_workouts",list(case_id="open_workout",default_at=as.character(max(start,cutoff-30)),evaluation_cutoff=as.character(cutoff),ead=100,observed_recovery=20,observed_cost=5,observed_loss_fraction=.85,ultimate_lgd=NULL,status="open",constructed=TRUE,excluded_from_stochastic_counts=TRUE))
 w$finish(list(dependencies=list(obligor_manifest_hash=.cr_hash(.cr_syn_world(obligor_world)$manifest)),dgp=list(workout_lags_days=list(90,360,900),discount_day_basis=365,raw_lgd_is_not_clipped=TRUE,observed_recoveries_respect_cutoff=TRUE)))
}

generate_retail_reference_world <- function(macro_world,specification,innovations,output_directory,chunk_rows=1000L,memory_budget_mb=256L) {
 s<-specification;t<-.cr_syn_spec(s)
 if(is.null(s$selection_mechanism)||!s$selection_mechanism %in% c("MAR","MNAR","missing_overlap")) .cr_error("SYNTHETIC_SPEC","Declare selection mechanism")
 draws<-.cr_syn_draws(innovations);.cr_syn_require(draws,list(list("retail_households",t$households,16L),list("retail_apps",t$applications,24L),list("retail_account",t$applications*t$account_months,8L)));.cr_syn_linked(macro_world,s,"macro_reference_world");w<-.cr_syn_writer(output_directory,"retail_reference_world",s,draws,chunk_rows,memory_budget_mb);start<-.cr_syn_date(s$start_date);cutoff<-.cr_syn_date(s$observation_cutoff);macro<-.cr_syn_table(macro_world,"latent_truth","macro_factors")
 if(!length(macro)) .cr_error("SYNTHETIC_LINK","Macro factors required")
 calendar_dates<-seq(as.Date(format(start,"%Y-%m-01")),by="month",length.out=t$application_months+t$account_months+3L)[-1L]-1;calendar_strings<-as.character(calendar_dates)
 products<-c("mortgage","consumer_installment","auto_loan","credit_card","overdraft","bnpl","small_business","retail_leasing")
 for(h in seq_len(t$households)-1L) {u<-draws$row("retail_households",h,16L);w$add("observable_inputs","households",list(household_id=sprintf("household-%06d",h),created_at=as.character(start),known_at=as.character(start),dependants=floor(u[1]*4),currency="EUR"))}
 for(p in seq_len(t$persons)-1L) {
  h<-p%%t$households;u<-draws$row("retail_households",h,16L)
  w$add("observable_inputs","persons",list(person_id=sprintf("person-%06d",p),household_id=sprintf("household-%06d",h),birth_year=as.integer(format(start,"%Y"))-21-floor(u[2]*45),employment=if(p%%5==0) "self_employed" else "salaried",known_at=as.character(start)))
  w$add("observable_inputs","household_memberships",list(membership_id=sprintf("membership-%06d",p),person_id=sprintf("person-%06d",p),household_id=sprintf("household-%06d",h),valid_from=as.character(start),valid_to=NULL,known_at=as.character(start)))
 }
 funded_count<-0L;capacity<-c(tiny=160L,standard=24000L,stress=240000L)[[s$tier]]
 for(i in seq_len(t$applications)-1L) {
  person<-i%%t$persons;h<-person%%t$households;hu<-draws$row("retail_households",h,16L);u<-draws$row("retail_apps",i,24L);month<-i%%t$application_months
  d<-.cr_syn_month(start,month)+floor(u[1]*27);app<-sprintf("application-%07d",i);if(d>cutoff) next
  income<-exp(7.4+1.2*hu[3]);expenses<-income*(.3+.25*hu[4]);debt<-income*(.05+.25*hu[5]);latent<-2*(hu[6]-.5);m<-macro[[min(month+1L,length(macro))]]
  product<-products[i%%length(products)+1L];term<-if(product=="mortgage") 240 else if(product %in% c("bnpl","overdraft","credit_card")) 12 else 60
  principal<-income*if(product=="mortgage") 45 else if(product=="bnpl") 2 else if(product %in% c("overdraft","credit_card")) 8 else 15
  rate<-if(product=="mortgage") .045 else .11;periodic<-rate/12;balloon<-if(product=="auto_loan") principal*.15 else if(product=="retail_leasing") principal*.3 else 0;installment<-(principal-balloon/(1+periodic)^term)*periodic/(-expm1(-term*log1p(periodic)))
  dti<-(installment+debt)/income;bureau_risk<-min(1,max(0,.25+.25*latent+.2*(u[2]-.5)));thin<-i%%13==0;nohit<-i%%29==0;stale<-i%%17==0
  missing_mechanism<-c("MCAR","MAR","MNAR")[i%%3+1L];missing_p<-if(missing_mechanism=="MCAR") .06 else if(missing_mechanism=="MAR") .03+.15*(income<2500) else stats::plogis(-3+latent)
  missing_income<-u[3]<missing_p;scored_dti<-(installment+debt)/(if(missing_income) 2500 else income);score_eta<--4+2.5*scored_dti+2*(if(nohit) .5 else bureau_risk)+.4*missing_income;prediction<-stats::plogis(score_eta)
  true_eta<-if(s$dgp_family=="well_specified") score_eta else score_eta+.7*latent+2*(dti-.4)^2+3*(m$employment-.06);true_pd<-stats::plogis(true_eta);potential_default<-u[4]<true_pd;splits<-.cr_syn_splits(h,month,t$application_months)
  w$add("observable_inputs","applications",list(application_id=app,person_id=sprintf("person-%06d",person),household_id=sprintf("household-%06d",h),product=product,application_at=as.character(d),known_at=as.character(d),requested_amount=principal,term_months=term,currency="EUR",net_monthly_income=if(missing_income) NULL else income,monthly_expenses=expenses,existing_debt_service=debt,proposed_installment=installment,income_basis="net",income_missing=missing_income,entity_split=splits$entity,time_split=splits$temporal,split=splits$split))
  w$add("observable_inputs","application_parties",list(application_id=app,person_id=sprintf("person-%06d",person),role="primary_borrower",household_id=sprintf("household-%06d",h),known_at=as.character(d),joint_and_several=TRUE))
  partner<-if(person<t$households) h+t$households else h
  if(i%%2==0&&partner<t$persons&&partner!=person) w$add("observable_inputs","application_parties",list(application_id=app,person_id=sprintf("person-%06d",partner),role="co_borrower",household_id=sprintf("household-%06d",h),known_at=as.character(d),joint_and_several=TRUE))
  pull<-paste0("bureau-",app);observed<-d-if(stale) 400 else 7
  w$add("observable_inputs","bureau_pulls",list(pull_id=pull,application_id=app,observed_at=as.character(observed),known_at=as.character(d),no_hit=nohit,thin_file=thin,stale=stale,risk_score=if(nohit) NULL else bureau_risk))
  if(i%%19==0&&d+45<=cutoff) w$add("observable_inputs","bureau_revisions",list(revision_id=paste0("revision-",pull),pull_id=pull,observed_at=as.character(observed),known_at=as.character(d+45),revised_risk_score=min(1,bureau_risk+.1),supersedes="initial",eligible_at_application=FALSE))
  if(!nohit) for(b in seq_len(if(thin) 1L else 3L)-1L) w$add("observable_inputs","bureau_tradelines",list(tradeline_id=paste0(pull,"-",b),pull_id=pull,observed_at=as.character(observed),known_at=as.character(d),balance=debt*12/(if(thin) 1 else 3),monthly_payment=debt/(if(thin) 1 else 3),status=if(bureau_risk<.5) "current" else "delinquent"))
  for(j in 0:2) {
   txd<-d-30*(j+1);prefix<-paste0("transaction-",app,"-",j);kinds<-c("salary","living_expense","internal_transfer","internal_transfer","card_charge","reversal");amounts<-c(income,-expenses,-100,100,-25,25);links<-list(NULL,NULL,paste0(prefix,"-3"),paste0(prefix,"-2"),paste0(prefix,"-5"),paste0(prefix,"-4"))
   for(k in 0:5) w$add("observable_inputs","transactions",list(transaction_id=paste0(prefix,"-",k),application_id=app,household_id=sprintf("household-%06d",h),account_id=sprintf("external-account-%06d",h),event_at=as.character(txd),known_at=as.character(d),category=kinds[k+1L],amount=amounts[k+1L],currency="EUR",linked_transaction_id=links[[k+1L]]))
  }
  policy_version<-if(month<t$application_months%/%2) "retail-policy-v1" else "retail-policy-v2"
  acceptance<-stats::plogis(2.2-5*prediction-.5*scored_dti-(if(s$selection_mechanism=="MNAR") .7*latent else 0))
  if(s$selection_mechanism=="missing_overlap"&&scored_dti>.55) acceptance<-0
  if(endsWith(policy_version,"v2")) acceptance<-acceptance*.9
  accepted<-u[5]<acceptance;taken<-accepted&&u[6]<.65;funded<-taken&&funded_count<capacity;account<-if(funded) paste0("account-",app) else NULL
  w$add("frozen_predictions","application_predictions",list(application_id=app,as_of=as.character(d),model_version="fixed-retail-logit-v1",pd=prediction,horizon_months=12,feature_source="observable application and bureau only"))
  for(stage in c("acceptance","take_up","funding")) w$add("decision_history","decisions",list(decision_id=paste0(app,"-",stage),application_id=app,stage=stage,decided_at=as.character(d),policy_version=policy_version,result=switch(stage,acceptance=accepted,take_up=taken,funding=funded),account_id=account,capacity_exhausted=taken&&!funded))
  default_mob<-if(potential_default) 1+floor(u[7]*11) else NULL
  w$add("latent_truth","application_parameters",list(application_id=app,true_pd=true_pd,latent_frailty=latent,acceptance_probability=acceptance,potential_default_mob=default_mob,potential_loan_outcome=if(potential_default) 1 else 0,observed_income_missing=missing_income,missing_mechanism=missing_mechanism))
  matured<-.cr_syn_month(d,12)<=cutoff
  if(!funded) {
   w$add("observed_outcomes","application_labels",list(application_id=app,account_id=NULL,funded=FALSE,window_end=as.character(.cr_syn_month(d,12)),evaluation_cutoff=as.character(cutoff),own_loan_default=NULL,censored=FALSE));next
  }
  funded_count<-funded_count+1L;secured<-product %in% c("mortgage","auto_loan","retail_leasing");revolving<-product %in% c("credit_card","overdraft")
  w$add("observable_inputs","accounts",list(account_id=account,application_id=app,customer_id=sprintf("person-%06d",person),household_id=sprintf("household-%06d",h),product_id=product,opened_at=as.character(d),known_at=as.character(d),currency="EUR",original_amount=principal,credit_limit=if(revolving) principal else 0,contract_months=term,annual_rate=rate,rate_type=if(product=="mortgage") "floating" else "fixed",reset_month=if(product=="mortgage") 12 else NULL,balloon_amount=balloon,collateral_value=if(secured) principal/.75 else NULL,first_lien_amount=if(secured) principal else NULL))
  origin_month<-(as.integer(format(d,"%Y"))-as.integer(format(start,"%Y")))*12+as.integer(format(d,"%m"))-as.integer(format(start,"%m"));balance<-principal*if(revolving) .4 else 1;cumulative_loss<-0;default_at<-NULL;close_at<-NULL;first_default_at<-NULL;cured<-FALSE;past_due<-0;first_unpaid_at<-NULL;episode_number<-0L;episode_default_mob<-default_mob
  for(mob in seq_len(t$account_months)-1L) {
   date_m<-calendar_dates[origin_month+mob+1L];date_string<-calendar_strings[origin_month+mob+1L];if(date_m>cutoff) break
   av<-draws$row("retail_account",i*t$account_months+mob,8L);if(!is.null(close_at)) break;event_type<-NULL
   if(!is.null(default_mob)&&default_mob==mob) {default_at<-date_m;first_default_at<-date_m;event_type<-"DEFAULT";cumulative_loss<-balance;episode_number<-1L;episode_default_mob<-mob}
   else if(cured&&mob==default_mob+10&&u[9]<.5) {default_at<-date_m;event_type<-"DEFAULT";cumulative_loss<-balance;episode_number<-episode_number+1L;episode_default_mob<-mob;cured<-FALSE}
   else if(!is.null(default_at)&&mob==episode_default_mob+3) event_type<-"CHARGEOFF"
   else if(!is.null(default_at)&&mob==episode_default_mob+6&&u[8]<.2) {event_type<-"CURED";default_at<-NULL;cured<-TRUE}
   else if(is.null(default_at)&&mob>1&&av[1]<.008) {event_type<-"PREPAID";close_at<-date_m}
   else if(mob==term&&is.null(default_at)) {event_type<-"CLOSED";close_at<-date_m}
   prior<-balance;draw<-0;payment<-0;recovery<-0
   if(product=="mortgage"&&mob==12) {
    rate<-.02+macro[[min(month+mob+1L,length(macro))]]$rate;periodic<-rate/12;remaining<-max(1,term-mob)
    installment<-(prior-balloon/(1+periodic)^remaining)*periodic/(-expm1(-remaining*log1p(periodic)))
   }
   interest<-if(is.null(default_at)) prior*periodic else 0
   if(is.null(default_at)) {
    scheduled<-if(!revolving) min(prior+interest,installment+past_due) else if(mob%%6==0) prior+interest else prior*.03+interest+past_due
    scheduled<-min(prior+interest,scheduled);missed<-av[3]<.04&&is.null(close_at);payment<-if(missed) 0 else scheduled
    if(missed) {past_due<-scheduled;if(is.null(first_unpaid_at)) first_unpaid_at<-as.Date(format(date_m,"%Y-%m-01"))} else {past_due<-0;first_unpaid_at<-NULL}
    balance<-max(0,prior+interest-payment)
    if(revolving&&mob%%6==1&&is.null(close_at)) {draw<-max(0,principal*(.2+.6*av[2])-balance);balance<-balance+draw}
   } else {
    scheduled<-installment;elapsed<-mob-episode_default_mob;recovery<-if(elapsed>0) min(balance,principal*.01*(1-hu[6])) else 0;balance<-max(0,balance-recovery)
   }
   if(!is.null(close_at)) {payment<-prior+interest;balance<-0;past_due<-0;first_unpaid_at<-NULL}
   if(mob==0) {balance<-prior;interest<-0;payment<-0;draw<-0;recovery<-0;past_due<-0;first_unpaid_at<-NULL;scheduled<-installment}
   dpd<-if(!is.null(default_at)) 90+30*(mob-episode_default_mob) else if(!is.null(first_unpaid_at)) as.numeric(date_m-first_unpaid_at) else 0;state<-if(!is.null(close_at)) "CLOSED" else if(!is.null(default_at)) "DEFAULT" else if(cured) "CURED" else "ACTIVE"
   w$add("observable_inputs","account_months",list(account_id=account,observed_at=date_string,known_at=date_string,months_on_book=mob,opening_balance=prior,balance=balance,credit_limit=if(revolving) principal else 0,principal_draw=draw,payment=payment,interest=interest,recovery_amount=recovery,annual_rate=rate,scheduled_payment=scheduled,days_past_due=dpd,past_due_amount=if(!is.null(default_at)) min(balance,installment*(dpd%/%30)) else past_due,state=state,active_zero_balance=state %in% c("ACTIVE","CURED")&&balance==0,currency="EUR"))
   if(!is.null(event_type)&&date_m+14<=cutoff) w$add("observed_outcomes","account_events",list(event_id=paste0("event-",account,"-",mob),account_id=account,event_at=as.character(date_m),known_at=as.character(date_m+14),event_type=event_type,amount=if(event_type=="CHARGEOFF") cumulative_loss else 0))
   if(!is.null(default_at)&&mob>=default_mob) {
    elapsed<-mob-episode_default_mob;action<-if(elapsed<3) "contact" else if(elapsed<6) "restructure" else "legal_recovery";cost<-if(action=="contact") 5 else if(action=="restructure") 20 else 40
    w$add("observed_outcomes","collections",list(episode_id=paste0("collection-",account,"-",episode_number),account_id=account,event_at=date_string,known_at=date_string,months_since_default=elapsed,action=action,recovery_amount=recovery,cost_amount=cost,currency="EUR",open=balance>0))
   }
  }
  observed_default<-!is.null(first_default_at)&&first_default_at+14<=cutoff;complete<-matured||!is.null(close_at)
  w$add("observed_outcomes","application_labels",list(application_id=app,account_id=account,funded=TRUE,window_end=as.character(.cr_syn_month(d,12)),evaluation_cutoff=as.character(cutoff),own_loan_default=if(observed_default) 1 else if(complete) 0 else NULL,censored=!complete&&!observed_default))

 }
 w$finish(list(dependencies=list(macro_manifest_hash=.cr_hash(.cr_syn_world(macro_world)$manifest)),actual_funded_accounts=funded_count,dgp=list(selection_mechanism=s$selection_mechanism,missingness=list("MCAR","MAR","MNAR"),funded_account_capacity=capacity,capacity_rule="first funded candidates in application-ID order",potential_outcomes_are_latent=TRUE,split_unit="household",no_implicit_fitting=TRUE)))
}

generate_asset_pool_reference_world <- function(worlds,specification,innovations,output_directory,chunk_rows=1000L,memory_budget_mb=256L) {
 s<-specification;t<-.cr_syn_spec(s);draws<-.cr_syn_draws(innovations)
 if(!is.list(worlds)||!length(worlds)) .cr_error("SYNTHETIC_LINK","Supply source worlds")
 for(source in worlds) .cr_syn_linked(source,s)
 w<-.cr_syn_writer(output_directory,"asset_pool_reference_world",s,draws,chunk_rows,memory_budget_mb);start<-.cr_syn_date(s$start_date);cutoff<-.cr_syn_date(s$observation_cutoff);totals<-rep(0,t$pools);pool_known<-rep(start,t$pools);count<-0L;seen<-new.env(parent=emptyenv());per_source<-max(1,t$pool_assets%/%length(worlds))
 for(source in worlds) {
  manifest<-.cr_syn_world(source)$manifest;selected<-new.env(parent=emptyenv());nsource<-0L
  for(table in c("accounts","facilities")) {
   idcol<-if(table=="accounts") "account_id" else "facility_id";amountcol<-if(table=="accounts") "original_amount" else "original_balance";datecol<-if(table=="accounts") "opened_at" else "origination_at"
   .cr_syn_each(source,"observable_inputs",table,function(asset) {
    if(nsource>=per_source||count>=t$pool_assets) return(NULL)
    aid<-asset[[idcol]];if(exists(aid,seen,inherits=FALSE)) .cr_error("SYNTHETIC_LINK","Duplicate source asset")
    assign(aid,TRUE,seen);p<-count%%t$pools;pid<-sprintf("pool-%04d",p);amount<-asset[[amountcol]];valid<-max(start,.cr_syn_date(asset[[datecol]]));expiry<-.cr_syn_month(valid,t$account_months)
    assign(aid,list(pid=pid,valid=valid,expiry=expiry),selected);totals[p+1L]<<-totals[p+1L]+amount;pool_known[p+1L]<<-max(pool_known[p+1L],valid)
    w$add("observable_inputs","pool_memberships",list(membership_id=sprintf("pool-member-%07d",count),pool_id=pid,source_world_type=manifest$world_type,source_asset_id=aid,asset_type=table,partition_share=1,original_principal=amount,valid_from=as.character(valid),valid_to=as.character(expiry),known_at=as.character(valid)))
    term<-asset$contract_months;if(is.null(term)) term<-(t$years+2)*12;rate<-asset$annual_rate;if(is.null(rate)) rate<-asset$interest_rate;if(is.null(rate)) rate<-.05;rate<-rate/12
    balloon<-asset$balloon_amount;if(is.null(balloon)) balloon<-0
    scheduled<-if(rate!=0) (amount-balloon/(1+rate)^term)*rate/(-expm1(-term*log1p(rate))) else (amount-balloon)/term;opening<-amount
    for(mob in seq_len(t$account_months)-1L) {
     d<-.cr_syn_month(valid,mob,TRUE);interest<-if(mob>0) opening*rate else 0;principal<-if(mob>0) min(opening,max(0,scheduled-interest)) else 0;if(mob==term) principal<-opening;closing<-opening-principal
     w$add("latent_truth","contractual_asset_cashflows",list(cashflow_id=sprintf("pool-cash-%07d-%03d",count,mob),pool_id=pid,source_asset_id=aid,event_at=as.character(d),opening_principal=opening,scheduled_principal=principal,interest=interest,closing_principal=closing,assumption="no_default_no_prepayment_projection"));opening<-closing
    };count<<-count+1L;nsource<<-nsource+1L
   })
  }
  configs<-list(list(layer="observable_inputs",table="account_months",id="account_id",date="observed_at",components=list(payment="payment")),list(layer="observed_outcomes",table="account_events",id="account_id",date="event_at",components=list(event="amount")),list(layer="observed_outcomes",table="collections",id="account_id",date="event_at",components=list(recovery="recovery_amount",cost="cost_amount")),list(layer="observed_outcomes",table="recovery_cashflows",id="facility_id",date="event_at",components=list(recovery="recovery_amount",cost="cost_amount")),list(layer="observed_outcomes",table="workouts",id="facility_id",date="default_at",components=list(default_exposure="ead")))
  for(config in configs) {
   index<--1L
   .cr_syn_each(source,config$layer,config$table,function(row) {
    index<<-index+1L;aid<-row[[config$id]];if(!exists(aid,selected,inherits=FALSE)) return(NULL)
    sel<-get(aid,selected,inherits=FALSE);d<-.cr_syn_date(row[[config$date]]);known<-.cr_syn_date(row$known_at)
    if(d<sel$valid||d>=sel$expiry||known>cutoff) return(NULL)
    for(kind in names(config$components)) w$add("observed_outcomes","servicing_cashflows",list(cashflow_id=paste0("source-",aid,"-",config$table,"-",index,"-",kind),pool_id=sel$pid,source_asset_id=aid,source_table=config$table,event_at=as.character(d),known_at=as.character(known),component=if(kind=="event") paste0("event_",row$event_type) else kind,amount=row[[config$components[[kind]]]],currency="EUR"))
   })
  }
 }
 if(count==0) .cr_error("SYNTHETIC_LINK","No generated assets available")
 for(p in seq_len(t$pools)-1L) {
  pid<-sprintf("pool-%04d",p);total<-totals[p+1L]
  w$add("observable_inputs","pools",list(pool_id=pid,currency="EUR",created_at=as.character(start),known_at=as.character(pool_known[p+1L]),initial_principal=total,notional_basis="sum_original_assigned_principal",revolving=TRUE,clean_up_call_fraction=.1))
  for(rank in seq_len(4L)) w$add("observable_inputs","tranches",list(tranche_id=paste0(pid,"-",c("senior","mezzanine","junior","equity")[rank]),pool_id=pid,seniority_rank=rank,original_principal=total*c(.7,.15,.1,.05)[rank],coupon_rate=.03+.015*(rank-1),known_at=as.character(pool_known[p+1L])))
 }
 w$finish(list(actual_source_assets=count,asset_capacity=t$pool_assets,dependencies=list(source_manifest_hashes=lapply(worlds,function(x) .cr_hash(.cr_syn_world(x)$manifest))),dgp=list(membership_partition_share=1,source_reuse=FALSE,tranches_per_pool=4,servicing_origin="actual source ledgers",contractual_projection="no default or prepayment; latent only")))
}


generate_system_event_reference_world <- function(worlds,specification,innovations,output_directory,chunk_rows=1000L,memory_budget_mb=256L) {
 s<-specification;t<-.cr_syn_spec(s);draws<-.cr_syn_draws(innovations)
 if(!is.list(worlds)||!length(worlds)) .cr_error("SYNTHETIC_LINK","Supply subject worlds")
 for(source in worlds) .cr_syn_linked(source,s)
 w<-.cr_syn_writer(output_directory,"system_event_reference_world",s,draws,chunk_rows,memory_budget_mb);start<-.cr_syn_date(s$start_date);cutoff<-.cr_syn_date(s$observation_cutoff);count<-0L
 for(version in c("v1","v2")) {
  offset<-if(version=="v1") 0 else 120
  w$add("observable_inputs","model_versions",list(model_version=version,model_role="constructed_score_demonstration",algorithm="fixed_score_schedule",parameter_origin="SYNTHETIC_REFERENCE",intercept=if(version=="v1") 100 else 105,known_at=as.character(start),effective_from=as.character(start+offset)))
  w$add("observable_inputs","policy_versions",list(policy_version=paste0("policy-",version),known_at=as.character(start),effective_from=as.character(start+offset),minimum_score=if(version=="v1") 80 else 90,policy_scope="constructed_checkpoint_fixture"))
 }
 w$add("observable_inputs","calibration_versions",list(calibration_version="calibration-v1",known_at=as.character(start),effective_from=as.character(start),transform="identity_score",intercept=0,slope=1,estimated=FALSE));source_models<-character();seen_subjects<-new.env(parent=emptyenv())
 for(source in worlds) {
  manifest<-.cr_syn_world(source)$manifest
  source_model<-switch(manifest$world_type,obligor_panel="fixed-corporate-logit-v1",retail_reference_world="fixed-retail-logit-v1",NULL)
  if(!is.null(source_model)&&!source_model %in% source_models) {w$add("observable_inputs","source_model_versions",list(model_version=source_model,source_world_type=manifest$world_type,model_role="frozen_pd_demonstration",known_at=as.character(start),estimated=FALSE));source_models<-c(source_models,source_model)}
  for(table in c("obligors","accounts","facilities","pools")) {
   idcol<-switch(table,obligors="obligor_id",accounts="account_id",facilities="facility_id",pools="pool_id")
   .cr_syn_each(source,"observable_inputs",table,function(subject) {
    if(count>=t$subjects) return(NULL)
    sid<-subject[[idcol]];source_known<-if(is.null(subject$known_at)) start else .cr_syn_date(subject$known_at);origin<-max(start,source_known);hash<-.cr_hash(subject)
    if(exists(sid,seen_subjects,inherits=FALSE)) .cr_error("SYNTHETIC_LINK","Duplicate source subject")
    assign(sid,TRUE,seen_subjects)
    w$add("observable_inputs","subjects",list(subject_id=sid,subject_type=table,source_world_type=manifest$world_type,source_snapshot_hash=hash,known_at=as.character(origin)))
    w$add("observable_inputs",paste0("source_",table),c(subject,list(snapshot_hash=hash)))
    effective<-c(0,30,30,45,90,120);delay<-c(0,30,60,45,90,120);kind<-c("SUBJECT_CREATED","SCORE_UPDATED","LATE_CORRECTION","FAILED_BATCH","GUARANTEE_EXPIRED","MODEL_SWITCH");versions<-c("v1","v1","v1","v1","v1","v2");values<-c(100,110,105,999,0,105);valids<-c(TRUE,TRUE,TRUE,FALSE,TRUE,TRUE)
    for(j in 0:5) {
     occurred<-origin+effective[j+1L];known<-origin+delay[j+1L];if(known>cutoff) next;eid<-sprintf("system-%06d-%d",count,j)
     row<-list(delivery_id=paste0(eid,"-delivery-1"),event_id=eid,subject_id=sid,event_type=kind[j+1L],effective_at=as.character(occurred),known_at=as.character(known),model_version=versions[j+1L],policy_version=paste0("policy-",versions[j+1L]),calibration_version="calibration-v1",value=values[j+1L],valid=valids[j+1L],source_snapshot_hash=hash,supersedes_event_id=if(kind[j+1L]=="LATE_CORRECTION") sprintf("system-%06d-1",count) else NULL)
     w$add("observed_outcomes","system_events",row);if(j==1) {row$delivery_id<-paste0(eid,"-delivery-2");w$add("observed_outcomes","system_events",row)}
    }
    for(j in seq_len(4L)) {
     asof<-c(35,35,95,125)[j];knowledge<-c(35,65,95,125)[j];if(origin+knowledge>cutoff) next
     version<-c("v1","v1","v1","v2")[j]
     w$add("latent_truth","independent_checkpoints",list(checkpoint_id=sprintf("checkpoint-%06d-%d-%d",count,asof,knowledge),subject_id=sid,as_of=as.character(origin+asof),known_at=as.character(origin+knowledge),expected_score=c(110,105,105,105)[j],expected_support_capacity=c(100,100,0,0)[j],expected_model_version=version,expected_policy_version=paste0("policy-",version),expected_calibration_version="calibration-v1",expected_duplicate_effect=0,expected_failed_batch_effect=0,oracle="closed-form authored state schedule; no replay call"))
    };count<<-count+1L
   })
  }
 }
 if(count==0) .cr_error("SYNTHETIC_LINK","No existing subjects")
 w$finish(list(actual_subjects=count,dependencies=list(source_manifest_hashes=lapply(worlds,function(x) .cr_hash(.cr_syn_world(x)$manifest))),dgp=list(duplicate_semantics="event_id idempotent; delivery_id distinct",failure_semantics="invalid batch has no state effect",checkpoint_oracle="authored independently of event replay")))
}

generate_challenge_cases <- function(case_ids=NULL,context=NULL) {
 cases<-list(
 list(case_id="complete_separation",inputs=list(x=list(-2,-1,1,2),y=list(0,0,1,1)),expected=list(unpenalized_finite_mle=FALSE)),
 list(case_id="zero_default",inputs=list(defaults=0,observations=100,alpha=.05),expected=list(exact_one_sided_upper_pd=1-.05^.01)),
 list(case_id="spiegelhalter_half",inputs=list(pd=list(.5,.5,.5,.5),outcome=list(0,1,0,1)),expected=list(variance=0,normal_z_defined=FALSE)),
 list(case_id="singular_covariance",inputs=list(covariance=list(list(1,1),list(1,1))),expected=list(rank=1,determinant=0)),
 list(case_id="cyclic_support",inputs=list(edges=list(list("a","b"),list("b","a"))),expected=list(acyclic=FALSE)),
 list(case_id="transfer_extremes",inputs=list(base_pd=.1,transfer_pd=list(0,1)),expected=list(independent_union_pd=list(.1,1))),
 list(case_id="censored_target",inputs=list(origin="2020-01-01",horizon_months=12,cutoff="2020-06-30",events=list()),expected=list(binary_target=NULL,censored=TRUE)),
 list(case_id="raw_lgd_bounds",inputs=list(ead=list(100,100),recovery=list(120,0),cost=list(0,20)),expected=list(raw_lgd=list(-.2,1.2))),
 list(case_id="cohort_100",inputs=list(initial_active=100,defaulted=10,closed=20,active_zero_balance=1),expected=list(surviving_active=70,cumulative_pd=.1,denominator=100)),
 list(case_id="future_feature",inputs=list(as_of="2020-01-31",feature_known_at="2020-02-01"),expected=list(feature_eligible=FALSE)))
 if(!is.null(case_ids)) {
  ids<-if(!length(case_ids)) character() else unlist(case_ids);if(!is.character(ids)||anyDuplicated(ids)||any(!ids %in% vapply(cases,function(x) x$case_id,character(1)))) .cr_error("SYNTHETIC_CASE","Unknown/duplicate case")
  cases<-Filter(function(x) x$case_id %in% ids,cases)
 }
 .cr_result(list(cases=cases,constructed=TRUE,oracle_origin="independent analytical identities",data_origin="SYNTHETIC"),"synthetic.generate_challenge_cases","synthetic_challenges",context,provenance=list(data_origin="SYNTHETIC",parameter_origin="SYNTHETIC_REFERENCE"))
}

validate_synthetic_world <- function(world,check_hashes=TRUE,context=NULL) {
 if(!is.logical(check_hashes)||length(check_hashes)!=1L||is.na(check_hashes)) .cr_error("SYNTHETIC_INPUT","check_hashes must be boolean")
 x<-.cr_syn_world(world);root<-x$root;manifest<-x$manifest;issues<-list();rows<-0;cutoff<-manifest$specification$observation_cutoff;counts<-list();actual_defaults<-0;funded<-character();account_ids<-character();pool_sums<-list();tranche_sums<-list();sources<-character()
 design_pd_sum<-0;latent_default_count<-0;risk_observations<-0;actual_retail_defaults<-new.env(parent=emptyenv())
 issue<-function(code,table,index) {if(length(issues)<100L) issues[[length(issues)+1L]]<<-list(code=code,table=table,row=index)}
 forbidden<-c("true_pd","latent_frailty","potential_default_mob","potential_loan_outcome","ultimate_discounted_lgd","potential_default_at","asset_volatility","origination_leverage","missing_mechanism")
 registries<-list();definitions<-list(obligors="obligor_id",households="household_id",persons="person_id",applications="application_id",bureau_pulls="pull_id",accounts="account_id",facilities="facility_id",pools="pool_id",subjects="subject_id",model_versions="model_version",policy_versions="policy_version",calibration_versions="calibration_version")
 for(table in names(definitions)) if(!is.null(manifest$tables[[paste0("observable_inputs/",table)]])) {
  column<-definitions[[table]];ids<-new.env(parent=emptyenv());index<-0L
  .cr_syn_each(world,"observable_inputs",table,function(row) {index<<-index+1L;if(exists(row[[column]],ids,inherits=FALSE)) issue("DUPLICATE_PRIMARY_KEY",table,index);assign(row[[column]],TRUE,ids)},check_hashes=FALSE)
  registries[[column]]<-ids
 }
 source_hashes<-new.env(parent=emptyenv())
 for(table in c("source_obligors","source_accounts","source_facilities","source_pools")) {
  column<-switch(table,source_obligors="obligor_id",source_accounts="account_id",source_facilities="facility_id",source_pools="pool_id")
  .cr_syn_each(world,"observable_inputs",table,function(row) assign(row[[column]],.cr_hash(row[names(row)!="snapshot_hash"]),source_hashes),check_hashes=FALSE)
 }
 for(key in names(manifest$tables)) {
  meta<-manifest$tables[[key]];parts<-strsplit(key,"/",fixed=TRUE)[[1]];layer<-parts[1];table<-parts[2];path<-.cr_syn_resource(root,meta$file)
  if(check_hashes&&.cr_syn_sha(path)!=meta$sha256) issue("HASH_MISMATCH",key,NULL)
  n<-0L
  .cr_syn_each(world,layer,table,function(r) {
   n<<-n+1L;if(!identical(names(r),unlist(meta$columns))) issue("COLUMN_MISMATCH",key,n)
   if(layer=="observable_inputs"&&length(intersect(forbidden,names(r)))) issue("LATENT_LEAKAGE",key,n)
   known<-r$known_at;if(is.null(known)) known<-r$decided_at;if(is.null(known)) known<-r$as_of;if(is.null(known)) known<-cutoff
   if(layer!="latent_truth"&&known>cutoff) issue("FUTURE_KNOWLEDGE",key,n)
   if(!is.null(r$observed_at)&&!is.null(r$known_at)&&r$observed_at>r$known_at) issue("KNOWLEDGE_BEFORE_OBSERVATION",key,n)
   for(column in names(registries)) if(!is.null(r[[column]])&&!exists(r[[column]],registries[[column]],inherits=FALSE)&&!(table=="transactions"&&column=="account_id")&&!(table=="source_model_versions"&&column=="model_version")) issue("FOREIGN_KEY",key,n)
   if(!is.null(r$source_snapshot_hash)&&(!exists(r$subject_id,source_hashes,inherits=FALSE)||r$source_snapshot_hash!=get(r$subject_id,source_hashes,inherits=FALSE))) issue("SOURCE_SNAPSHOT_HASH",key,n)
   if(!is.null(r$snapshot_hash)&&r$snapshot_hash!=.cr_hash(r[names(r)!="snapshot_hash"])) issue("SOURCE_SNAPSHOT_HASH",key,n)
   if(table=="account_months"&&r$balance<0) issue("NEGATIVE_ACCOUNT_BALANCE",key,n)
   if(table=="account_months"&&abs(r$opening_balance+r$principal_draw+r$interest-r$payment-r$recovery_amount-r$balance)>max(1e-8,1e-12*abs(r$balance))) issue("ACCOUNT_CASHFLOW_RECONCILIATION",key,n)
   if(table=="facility_exposures"&&abs(r$drawn_balance+r$undrawn-r$credit_limit)>1e-12*abs(r$credit_limit)) issue("LIMIT_RECONCILIATION",key,n)
   if(table=="contractual_asset_cashflows"&&abs(r$opening_principal-r$scheduled_principal-r$closing_principal)>max(1e-8,1e-12*abs(r$closing_principal))) issue("CASHFLOW_RECONCILIATION",key,n)
   if(table=="financial_statements") {
    if(abs(r$assets-r$debt-r$equity)>1e-12*abs(r$assets)) issue("BALANCE_SHEET",key,n)
    if(abs(r$ebitda-r$depreciation-r$ebit)>1e-12*max(abs(r$ebit),1e-30)) issue("EARNINGS",key,n)
   }
   if(table=="accounts") account_ids<<-c(account_ids,r$account_id)
   if(table=="application_labels") {
    if(!r$funded&&(!is.null(r$own_loan_default)||!is.null(r$account_id))) issue("REJECT_LABEL_LEAKAGE",key,n)
    if(r$funded) funded<<-c(funded,r$account_id)
   }
   if(table=="default_events") actual_defaults<<-actual_defaults+1L
   if(table %in% c("obligor_period","application_parameters")) {design_pd_sum<<-design_pd_sum+r$true_pd;risk_observations<<-risk_observations+1L;latent_default_count<<-latent_default_count+if(table=="obligor_period") as.numeric(!is.null(r$potential_default_at)) else r$potential_loan_outcome}
   if(table=="account_events"&&r$event_type=="DEFAULT") assign(r$account_id,TRUE,actual_retail_defaults)
   if(table=="pool_memberships") {
    source<-paste(r$source_world_type,r$source_asset_id,sep="/");if(source %in% sources) issue("DUPLICATE_POOL_ASSET",key,n);sources<<-c(sources,source)
    old<-pool_sums[[r$pool_id]];if(is.null(old)) old<-0;pool_sums[[r$pool_id]]<<-old+r$original_principal*r$partition_share
   }
   if(table=="tranches") {old<-tranche_sums[[r$pool_id]];if(is.null(old)) old<-0;tranche_sums[[r$pool_id]]<<-old+r$original_principal}
   if(!is.null(r$split)&&!is.null(r$entity_split)) {expected<-if(r$entity_split==r$time_split) r$entity_split else "excluded";if(r$split!=expected) issue("SPLIT_LEAKAGE",key,n)}
  },check_hashes=FALSE)
  if(n!=meta$rows) issue("ROW_COUNT",key,NULL);counts[[key]]<-n;rows<-rows+n
 }
 if(manifest$world_type=="retail_reference_world"&&!setequal(account_ids,funded)) issue("FUNDED_ACCOUNT_LINK","accounts",NULL)
 for(p in names(pool_sums)) {total<-pool_sums[[p]];other<-tranche_sums[[p]];if(is.null(other)) other<-0;if(abs(total-other)>max(1e-8,1e-12*abs(total))) issue("TRANCHE_NOTIONAL","tranches",NULL)}
 for(key in names(manifest$binary_resources)) {meta<-manifest$binary_resources[[key]];path<-.cr_syn_resource(root,meta$file);if(file.info(path)$size!=prod(unlist(meta$shape))*8) issue("BINARY_SHAPE",key,NULL);if(check_hashes&&.cr_syn_sha(path)!=meta$sha256) issue("HASH_MISMATCH",key,NULL)}
 .cr_result(list(passed=!length(issues),issues=issues,rows_checked=rows,table_counts=counts,realized_defaults=actual_defaults,actual_funded_account_defaults=length(ls(actual_retail_defaults)),design_vs_realized=list(risk_observations=risk_observations,sum_conditional_pd=design_pd_sum,latent_default_count=latent_default_count,interpretation="Descriptive conditional hazard sum; dependent observations do not imply a binomial test"),diagnostic_policy="Report realized outcomes without reseeding or target-rate adjustment",data_origin="SYNTHETIC"),"synthetic.validate_synthetic_world","synthetic_validation",context,provenance=list(data_origin="SYNTHETIC",parameter_origin="SYNTHETIC_REFERENCE"))
}
