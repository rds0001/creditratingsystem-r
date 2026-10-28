# Offline authoring: Rscript synthetic_innovations.R DIR TIER [--retail] [EXPERIMENT_ID]
# SHA256 ASCII counter bytes match the bundled Python authoring script exactly.
write_synthetic_innovations <- function(directory,tier,retail=FALSE,experiment_id="reference-v1") {
 if(length(experiment_id)!=1L||!grepl("^[A-Za-z0-9_-]+$",experiment_id)) stop("Invalid experiment ID")
 profile<-switch(tier,tiny=c(64,3,36,512,24,320,160,24),standard=c(3000,10,240,16384,60,50000,20000,60),stress=c(20000,15,360,131072,120,500000,200000,120),stop("Unknown tier"))
 e<-profile[1];y<-profile[2];m<-profile[3];p<-profile[4];s<-profile[5];a<-profile[6];h<-profile[7];am<-profile[8]
 shapes<-list(macro_history=c(m,10),macro_revision=c(m,8),macro_paths=c(p*s,10),obligor_static=c(e,16),obligor_year=c(e*y,16),obligor_groups=c(ceiling(e/4)*y,2),facilities=c(e,16),country_transfer=c(6*y,2))
 if(retail) shapes<-c(shapes,list(retail_households=c(h,16),retail_apps=c(a,24),retail_account=c(a*am,8)))
 if(dir.exists(directory)&&length(list.files(directory,all.files=TRUE,no..=TRUE))) stop("Output must be absent or empty")
 dir.create(directory,recursive=TRUE,showWarnings=FALSE)
 manifest<-list(format="creditratingsystem-u32le-v1",authoring_method="SHA256 counter bytes v1; no outcome reselection",experiment_id=experiment_id,streams=list())
 for(stream in names(shapes)) {
  shape<-shapes[[stream]];path<-file.path(directory,paste0(stream,".u32le"));con<-file(path,"wb")
  for(row in seq_len(shape[1])-1L) {
   payload<-raw()
   for(block in seq_len(ceiling(shape[2]*4/32))-1L) payload<-c(payload,digest::digest(charToRaw(paste("creditratingsystem",experiment_id,stream,row,block,sep="/")),algo="sha256",serialize=FALSE,raw=TRUE))
   writeBin(payload[seq_len(shape[2]*4)],con)
  };close(con)
  manifest$streams[[stream]]<-list(file=basename(path),shape=as.list(shape),sha256=digest::digest(file=path,algo="sha256",serialize=FALSE))
 }
 writeLines(jsonlite::toJSON(manifest,auto_unbox=TRUE,digits=17,pretty=TRUE),file.path(directory,"manifest.json"))
 invisible(manifest)
}
if(sys.nframe()==0L) {
 args<-commandArgs(trailingOnly=TRUE);if(length(args)<2L) stop("Supply DIR TIER [--retail] [EXPERIMENT_ID]")
 extras<-args[-c(1L,2L)];retail<-"--retail" %in% extras;id<-setdiff(extras,"--retail");if(!length(id)) id<-"reference-v1"
 manifest<-write_synthetic_innovations(args[1L],args[2L],retail,id[1L]);cat("Streams:",length(manifest$streams),"\n")
}
