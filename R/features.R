.cr_feature_missing <- function(x) is.null(x)||(length(x)==1L&&is.atomic(x)&&is.na(x))
.cr_feature_records <- function(x) {
 if(is.data.frame(x))x<-lapply(seq_len(nrow(x)),function(i)as.list(x[i,,drop=FALSE]))
 if(!is.list(x)||!length(x)||any(!vapply(x,.cr_object,logical(1)))).cr_error("INVALID_TABLE","Nonempty list of row objects required")
 x
}
.cr_feature_number <- function(x,name) {
 x<-.cr_numeric(x,name,vector_only=TRUE)
 if(length(x)!=1L).cr_error("INVALID_INPUT",paste(name,"must be scalar"))
 as.numeric(x)
}
.cr_feature_choice <- function(x,choices)is.character(x)&&length(x)==1L&&!is.na(x)&&x %in% choices
.cr_feature_artifact <- function(method,features,parameters,target,training,model_id,model_version) {
 if(!.cr_object(training)).cr_error("INVALID_MODEL","Explicit training metadata required")
 origin<-if(is.null(training$data_origin))"USER_SUPPLIED" else training$data_origin
 .cr_model(list(schema_version="1.0",model_id=model_id,model_version=model_version,method_id=method,feature_order=as.list(features),parameters=parameters,preprocessing=list(transformation="frozen"),target=target,training=training,diagnostics=.cr_empty_object(),provenance=list(data_origin=origin,parameter_origin="ESTIMATED")))
}
fit_preprocessor <- function(train_features,feature_schema,target,training,model_id,model_version,context=NULL) {
 rows<-.cr_feature_records(train_features)
 if(!is.list(feature_schema)||!length(feature_schema)).cr_error("INVALID_SCHEMA","Ordered feature specifications required")
 names<-character();specs<-list();output<-character();lineage<-list()
 for(spec in feature_schema) {
  if(!.cr_object(spec)).cr_error("INVALID_SCHEMA","Feature specification required")
  name<-.cr_string(spec$name,"name","INVALID_SCHEMA")
  if(name %in% names).cr_error("INVALID_SCHEMA","Unique feature names required")
  names<-c(names,name)
  if(any(!vapply(rows,function(row)name %in% names(row),logical(1)))).cr_error("MISSING_FEATURE","Feature absent from training schema")
  values<-lapply(rows,function(row)row[[name]]);missing<-vapply(values,.cr_feature_missing,logical(1));observed<-values[!missing]
  fitted<-list(name=name,type=spec$type,missing_count=sum(missing),n_training=length(rows))
  if(identical(spec$type,"numeric")) {
   policy<-spec$imputation;scale<-spec$scaling
   if(!.cr_feature_choice(policy,c("error","mean","median","constant"))||!.cr_feature_choice(scale,c("none","standard","robust"))).cr_error("INVALID_SCHEMA","Declare numeric imputation and scaling")
   numeric<-vapply(observed,.cr_feature_number,numeric(1),name=name)
   if(policy=="error"&&any(missing)).cr_error("MISSING_INPUT","Missing training values")
   if(policy %in% c("mean","median")&&!length(numeric)).cr_error("NO_OBSERVED_VALUES","Cannot estimate imputer")
   fill<-if(policy=="constant").cr_feature_number(spec$fill_value,"fill_value") else if(policy=="mean")mean(numeric) else if(policy=="median")stats::median(numeric) else NULL
   complete<-vapply(seq_along(values),function(i)if(missing[i])fill else .cr_feature_number(values[[i]],name),numeric(1))
   center<-if(scale=="standard")mean(complete) else if(scale=="robust")stats::median(complete) else 0
   divisor<-if(scale=="standard")sqrt(mean((complete-center)^2)) else if(scale=="robust")diff(stats::quantile(complete,c(.25,.75),names=FALSE,type=7)) else 1
   constant<-divisor==0;if(constant)divisor<-1
   indicator<-if(is.null(spec$missing_indicator))FALSE else spec$missing_indicator
   if(!is.logical(indicator)||length(indicator)!=1L||is.na(indicator)).cr_error("INVALID_SCHEMA","missing_indicator must be boolean")
   fitted<-c(fitted,list(imputation=policy,fill_value=fill,scaling=scale,center=center,divisor=divisor,constant_training_scale=constant,missing_indicator=indicator))
   columns<-c(name,if(indicator)paste0(name,"::missing") else character())
  } else if(identical(spec$type,"categorical")) {
   if(any(!vapply(observed,function(v)is.character(v)&&length(v)==1L,logical(1)))).cr_error("INVALID_FEATURE_TYPE","Categorical values must be strings")
   missing_policy<-spec$missing_policy;unknown<-spec$unknown_policy
   if(!.cr_feature_choice(missing_policy,c("error","separate","mode"))||!.cr_feature_choice(unknown,c("error","separate"))).cr_error("INVALID_SCHEMA","Declare categorical missing/unknown policies")
   observed<-unlist(observed,use.names=FALSE);levels<-if(length(observed))sort(unique(observed),method="radix") else character();counts<-vapply(levels,function(level)sum(observed==level),numeric(1))
   if(missing_policy=="error"&&any(missing)).cr_error("MISSING_INPUT","Missing category")
   if(missing_policy=="mode"&&!length(levels)).cr_error("NO_OBSERVED_VALUES","Cannot estimate categorical mode")
   mode<-if(length(levels))levels[which.max(counts)] else NULL;reference<-spec$drop_reference
   if(!is.null(reference)&&!reference %in% levels).cr_error("INVALID_SCHEMA","Reference must be training category")
   fitted<-c(fitted,list(levels=as.list(levels),counts=as.list(unname(counts)),missing_policy=missing_policy,unknown_policy=unknown,mode=mode,drop_reference=reference))
   columns<-if(length(levels))paste0(name,"::level:",seq_along(levels)-1L) else character()
   if(!is.null(reference))columns<-columns[levels!=reference]
   if(missing_policy=="separate")columns<-c(columns,paste0(name,"::missing"))
   if(unknown=="separate")columns<-c(columns,paste0(name,"::unknown"))
  } else .cr_error("INVALID_SCHEMA","Feature type must be numeric or categorical")
  fitted$output_columns<-as.list(columns);specs[[length(specs)+1L]]<-fitted;output<-c(output,columns)
  for(col in columns)lineage[[length(lineage)+1L]]<-list(output=col,input=name)
 }
 if(anyDuplicated(output)||!length(output)).cr_error("INVALID_SCHEMA","Empty or colliding output names")
 model<-.cr_feature_artifact("features.fit_preprocessor",names,list(feature_specs=specs,output_order=as.list(output),lineage=lineage),target,training,model_id,model_version)
 input<-lapply(rows,function(row){out<-row[names];for(name in names)if(.cr_feature_missing(out[[name]]))out[name]<-list(NULL);out})
 diagnostics<-list(n_training=length(rows),training_input_fingerprint=.cr_hash(input),fit_scope="TRAINING_ONLY",output_order=as.list(output));model$diagnostics<-diagnostics
 .cr_fit_result(model,"features.fit_preprocessor",diagnostics,context)
}
transform_features <- function(features,fitted_preprocessor,context=NULL) {
 rows<-.cr_feature_records(features);model<-.cr_model(fitted_preprocessor);params<-model$parameters;names<-unlist(model$feature_order)
 if(any(vapply(rows,function(row)any(!names %in% names(row)),logical(1)))).cr_error("MISSING_FEATURE","Required input column absent")
 missing_counts<-as.list(stats::setNames(rep(0,length(names)),names));unknown_counts<-missing_counts;design<-list()
 if(identical(model$method_id,"features.fit_monotone_bins")) {
  name<-names[1];boundaries<-params$upper_boundaries;missing_index<-length(boundaries);finite<-unlist(head(boundaries,-1L))
  for(row in rows) {
   value<-row[[name]]
   if(.cr_feature_missing(value)){index<-missing_index;missing_counts[[name]]<-missing_counts[[name]]+1} else index<-sum(.cr_feature_number(value,name)>finite)
   design[[length(design)+1L]]<-as.numeric(index)
  }
  output<-paste0(name,"::bin");lineage<-list(list(output=output,input=name))
 } else if(identical(model$method_id,"features.fit_preprocessor")) {
  output<-unlist(params$output_order);lineage<-params$lineage
  for(row in rows) {
   result<-numeric()
   for(spec in params$feature_specs) {
    name<-spec$name;value<-row[[name]];missing<-.cr_feature_missing(value)
    if(missing)missing_counts[[name]]<-missing_counts[[name]]+1
    if(spec$type=="numeric") {
     if(missing&&spec$imputation=="error").cr_error("MISSING_INPUT","Required numeric value missing")
     v<-if(missing)spec$fill_value else .cr_feature_number(value,name)
     result<-c(result,(v-spec$center)/spec$divisor)
     if(spec$missing_indicator)result<-c(result,as.numeric(missing))
    } else {
     if(missing&&spec$missing_policy=="error").cr_error("MISSING_INPUT","Required category missing")
     if(!missing&&(!is.character(value)||length(value)!=1L)).cr_error("INVALID_FEATURE_TYPE","Categorical value must be string")
     if(missing&&spec$missing_policy=="mode"){value<-spec$mode;missing<-FALSE}
     levels<-unlist(spec$levels);unknown<-!missing&&!value %in% levels
     if(unknown){unknown_counts[[name]]<-unknown_counts[[name]]+1;if(spec$unknown_policy=="error").cr_error("UNKNOWN_CATEGORY","Category absent from training")}
     for(level in levels)if(is.null(spec$drop_reference)||level!=spec$drop_reference)result<-c(result,as.numeric(!missing&&identical(value,level)))
     if(spec$missing_policy=="separate")result<-c(result,as.numeric(missing))
     if(spec$unknown_policy=="separate")result<-c(result,as.numeric(unknown))
    }
   }
   design[[length(design)+1L]]<-result
  }
 } else .cr_error("UNSUPPORTED_MODEL","Expected preprocessor or binning artifact")
 design<-do.call(rbind,design)
 if(any(!is.finite(design))).cr_error("NUMERICAL_RANGE","Nonfinite transformed value")
 .cr_result(list(design_matrix=design,feature_order=as.list(output),lineage=lineage),"features.transform_features","design_matrix",context,
  diagnostics=list(model_id=model$model_id,model_version=model$model_version,missing_counts=missing_counts,unknown_counts=unknown_counts,refitted=FALSE))
}
fit_monotone_bins <- function(x_train,y_train,feature_name,direction,min_bin_count,max_prebins,target,training,model_id,model_version,context=NULL) {
 .cr_string(feature_name,"feature_name","INVALID_SCHEMA")
 if(!.cr_feature_choice(direction,c("increasing","decreasing"))).cr_error("INVALID_INPUT","Explicit direction required")
 minimum<-.cr_feature_number(min_bin_count,"min_bin_count");maximum<-.cr_feature_number(max_prebins,"max_prebins")
 if(minimum<1||maximum<1||minimum!=floor(minimum)||maximum!=floor(maximum)).cr_error("INVALID_INPUT","Positive integer bin controls required")
 if(!is.list(x_train)&&!is.atomic(x_train)).cr_error("INVALID_INPUT","Feature vector required")
 y<-.cr_numeric(y_train,"y_train",vector_only=TRUE)
 if(length(x_train)!=length(y)||any(!y %in% c(0,1))).cr_error("INVALID_OUTCOME","Equal-length binary outcomes required")
 missing<-vapply(as.list(x_train),.cr_feature_missing,logical(1));missing_n<-sum(missing);missing_d<-sum(y[missing])
 x<-vapply(as.list(x_train)[!missing],.cr_feature_number,numeric(1),name="x_train");events<-y[!missing]
 if(length(x)<minimum).cr_error("INSUFFICIENT_SUPPORT","Too few nonmissing observations")
 unique<-sort(unique(x));groups<-lapply(unique,function(v)c(v,sum(x==v),sum(events[x==v])))
 bins<-list();current<-NULL;target_count<-ceiling(length(x)/maximum)
 for(group in groups) {
  if(is.null(current))current<-c(group[1],group[1],group[2],group[3]) else {current[2]<-group[1];current[3:4]<-current[3:4]+group[2:3]}
  if(current[3]>=target_count){bins[[length(bins)+1L]]<-current;current<-NULL}
 }
 if(!is.null(current))bins[[length(bins)+1L]]<-current
 merge<-function(i){left<-bins[[i]];right<-bins[[i+1L]];replacement<-c(left[1],right[2],left[3]+right[3],left[4]+right[4]);bins[[i]]<<-replacement;bins[[i+1L]]<<-NULL}
 while(length(bins)>1&&min(vapply(bins,function(b)b[3],numeric(1)))<minimum) {
  i<-which.min(vapply(bins,function(b)b[3],numeric(1)))
  if(i==1)merge(1) else if(i==length(bins))merge(i-1L) else {
   rate<-bins[[i]][4]/bins[[i]][3];left<-abs(rate-bins[[i-1L]][4]/bins[[i-1L]][3]);right<-abs(rate-bins[[i+1L]][4]/bins[[i+1L]][3]);merge(if(left<=right)i-1L else i)
  }
 }
 sign<-if(direction=="increasing")1 else -1;i<-1L
 while(i<length(bins)) {
  if(sign*bins[[i]][4]/bins[[i]][3]>sign*bins[[i+1L]][4]/bins[[i+1L]][3]){merge(i);i<-max(1L,i-1L)} else i<-i+1L
 }
 table<-lapply(seq_along(bins),function(i){b<-bins[[i]];list(bin_id=i-1L,count=b[3],defaults=b[4],nondefaults=b[3]-b[4],default_rate=b[4]/b[3],observed_min=b[1],observed_max=b[2],missing=FALSE)})
 table[[length(table)+1L]]<-list(bin_id=length(bins),count=missing_n,defaults=missing_d,nondefaults=missing_n-missing_d,default_rate=if(missing_n)missing_d/missing_n else NULL,observed_min=NULL,observed_max=NULL,missing=TRUE)
 upper<-lapply(head(bins,-1L),function(b)b[2]);upper[length(upper)+1L]<-list(NULL)
 params<-list(upper_boundaries=upper,bins=table,direction=direction,interval_convention="right_closed",missing_bin_id=length(bins),min_bin_count=minimum,max_prebins=maximum)
 model<-.cr_feature_artifact("features.fit_monotone_bins",feature_name,params,target,training,model_id,model_version)
 model$diagnostics<-list(fit_scope="TRAINING_ONLY",missing_bin_exempt_from_monotonicity_and_support=TRUE,algorithm="tied_value_prebin_support_merge_pava",optimal_partition_claim=FALSE)
 .cr_fit_result(model,"features.fit_monotone_bins",model$diagnostics,context)
}
compute_woe_iv <- function(bin_counts,smoothing,woe_sign,context=NULL) {
 rows<-.cr_feature_records(bin_counts);alpha<-.cr_feature_number(smoothing,"smoothing")
 if(alpha<0||!.cr_feature_choice(woe_sign,c("nondefault_over_default","default_over_nondefault"))).cr_error("INVALID_INPUT","Nonnegative smoothing and explicit sign required")
 ids<-list();counts<-matrix(0,length(rows),2)
 for(i in seq_along(rows)) {
  identifier<-rows[[i]]$bin_id
  if(length(identifier)!=1L||is.na(identifier)||(!is.character(identifier)&&(!is.numeric(identifier)||identifier!=floor(identifier)))||any(vapply(ids,function(x)identical(.cr_hash(x),.cr_hash(identifier)),logical(1)))).cr_error("INVALID_BIN","Unique string/integer bin ID required")
  ids[[i]]<-identifier;d<-.cr_feature_number(rows[[i]]$defaults,"defaults");g<-.cr_feature_number(rows[[i]]$nondefaults,"nondefaults")
  if(min(d,g)<0||d!=floor(d)||g!=floor(g)||max(d,g)>2^53-1).cr_error("INVALID_COUNTS","Integer class counts required")
  counts[i,]<-c(d,g)
 }
 totals<-colSums(counts)
 if(any(totals>2^53-1)||any(!is.finite(totals+alpha*length(rows)))).cr_error("INVALID_COUNTS","Count or smoothing total exceeds supported range")
 if(any(totals<=0)).cr_error("INSUFFICIENT_CLASS_SUPPORT","Both classes must be observed before smoothing")
 if(alpha==0&&any(counts==0)).cr_error("ZERO_CLASS_CELL","Positive smoothing required for zero class cells")
 shares<-sweep(counts+alpha,2,totals+alpha*length(rows),"/");table<-list();total<-0
 for(i in seq_len(nrow(counts))) {
  d<-shares[i,1];g<-shares[i,2];canonical<-log(g)-log(d);iv<-(g-d)*canonical;total<-total+iv
  table[[i]]<-list(bin_id=ids[[i]],default_share=d,nondefault_share=g,woe=if(woe_sign=="nondefault_over_default")canonical else -canonical,iv_contribution=iv,zero_default_count=counts[i,1]==0,zero_nondefault_count=counts[i,2]==0)
 }
 .cr_result(list(bins=table,iv=total),"features.compute_woe_iv","information_value",context,diagnostics=list(smoothing=alpha,woe_sign=woe_sign,smoothing_scope="this_bin_partition",iv_sign_invariant=TRUE))
}
build_financial_ratios <- function(financial_statements,ratio_spec,context=NULL) {
 statements<-.cr_feature_records(financial_statements);specs<-.cr_feature_records(ratio_spec);output<-list();names<-character()
 for(spec in specs) {
  name<-.cr_string(spec$name,"name","INVALID_SCHEMA")
  if(name %in% names).cr_error("INVALID_SCHEMA","Unique ratio name required")
  names<-c(names,name)
  if(!.cr_feature_choice(spec$denominator_policy,c("positive","nonzero"))||!is.character(spec$output_unit)||length(spec$output_unit)!=1L).cr_error("INVALID_SCHEMA","Explicit denominator policy and unit required")
  for(side in c("numerator","denominator"))if(!.cr_object(spec[[side]])||!length(spec[[side]])).cr_error("INVALID_SCHEMA","Named field coefficients required")
 }
 for(row in statements) {
  for(key in c("entity_id","currency","as_of","period_start","period_end")) .cr_string(row[[key]],key,"INVALID_STATEMENT")
  dates<-vapply(c("as_of","period_start","period_end"),function(k).cr_date(row[[k]],k),character(1))
  if(dates[2]>dates[3]||dates[3]!=dates[1]).cr_error("INVALID_STATEMENT","Require period_start <= period_end == as_of")
  if(!.cr_object(row$values)||!.cr_object(row$units)).cr_error("INVALID_STATEMENT","Values and field units required")
  result<-list(entity_id=row$entity_id,as_of=row$as_of,currency=row$currency,ratios=list())
  for(spec in specs) {
   parts<-numeric();units<-character();missing<-FALSE
   for(side in c("numerator","denominator")) {
    amount<-0;unit<-NULL
    for(field in names(spec[[side]])) {
     if(!field %in% names(row$values)||!field %in% names(row$units)).cr_error("MISSING_FEATURE","Statement field absent")
     field_unit<-.cr_string(row$units[[field]],"field unit","INVALID_UNIT")
     if(!field_unit %in% c("dimensionless",row$currency)).cr_error("INCOMPATIBLE_UNIT","Field currency conflicts with statement currency")
     if(!is.null(unit)&&field_unit!=unit).cr_error("INCOMPATIBLE_UNIT","Cannot sum different units")
     unit<-field_unit;weight<-.cr_feature_number(spec[[side]][[field]],"coefficient");value<-row$values[[field]]
     if(.cr_feature_missing(value))missing<-TRUE else amount<-amount+weight*.cr_feature_number(value,field)
    }
    parts<-c(parts,amount);units<-c(units,unit)
   }
   if(units[1]!=units[2]||spec$output_unit!="dimensionless").cr_error("INCOMPATIBLE_UNIT","Matching input units and dimensionless output required")
   numerator<-parts[1];denominator<-parts[2]
   status<-if(missing)"MISSING_INPUT" else if(denominator==0)"ZERO_DENOMINATOR" else if(denominator<0&&spec$denominator_policy=="positive")"NONPOSITIVE_DENOMINATOR" else "OK"
   value<-if(status=="OK")numerator/denominator else NULL
   if(!is.null(value)&&!is.finite(value)).cr_error("NUMERICAL_RANGE","Ratio outside finite range")
   result$ratios[[spec$name]]<-list(value=value,status=status,unit="dimensionless",numerator=if(missing)NULL else numerator,denominator=if(missing)NULL else denominator,formula=spec)
  }
  output[[length(output)+1L]]<-result
 }
 .cr_result(output,"features.build_financial_ratios","ratio_table",context,limitations="All fields in a statement row must share the declared currency, reporting date and period. No automatic currency or accounting-definition conversion.")
}
build_distributed_lags <- function(series,observations,lag_spec,frequency,gap_policy="null",context=NULL) {
 records<-.cr_feature_records(series);queries<-.cr_feature_records(observations);specs<-.cr_feature_records(lag_spec)
 if(!.cr_feature_choice(frequency,c("days","months","years"))||!.cr_feature_choice(gap_policy,c("null","error"))).cr_error("INVALID_INPUT","Declare frequency and gap policy")
 names<-character()
 for(spec in specs) {
  name<-.cr_string(spec$name,"name","INVALID_SCHEMA");.cr_string(spec$feature,"feature","INVALID_SCHEMA")
  if(name %in% names).cr_error("INVALID_SCHEMA","Unique output names required")
  names<-c(names,name);lag<-.cr_feature_number(spec$lag,"lag")
  if(lag<0||lag!=floor(lag)||!.cr_feature_choice(spec$operation,c("level","difference","relative_change"))).cr_error("INVALID_SCHEMA","Integer lag and supported operation required")
 }
 date<-function(v)as.Date(.cr_date(v,"date"));facts<-list()
 for(row in records) {
  for(key in c("entity_id","feature")) .cr_string(row[[key]],key,"INVALID_SERIES")
  observed<-date(row$observed_at);available<-date(row$available_at);known<-date(row$known_at)
  if(available<observed||known<available).cr_error("INVALID_CHRONOLOGY","Require observed <= available <= known")
  value<-if(.cr_feature_missing(row$value))NULL else .cr_feature_number(row$value,"value")
  facts[[length(facts)+1L]]<-list(entity=row$entity_id,feature=row$feature,observed=observed,available=available,known=known,value=value)
 }
 shift<-function(d,lag) {
  if(frequency=="days")return(d-lag)
  months<-lag*if(frequency=="years")12 else 1;parts<-as.POSIXlt(d);index<-(parts$year+1900)*12+parts$mon-months;year<-floor(index/12);month<-index%%12+1
  if(year<1).cr_error("INVALID_DATE","Lag precedes supported calendar")
  first<-as.Date(sprintf("%04d-%02d-01",year,month));next_first<-if(month==12)as.Date(sprintf("%04d-01-01",year+1)) else as.Date(sprintf("%04d-%02d-01",year,month+1));day<-min(parts$mday,as.numeric(next_first-first))
  first+day-1
 }
 output<-list()
 for(query in queries) {
  eid<-.cr_string(query$entity_id,"entity_id","INVALID_ENTITY");asof<-date(query$as_of);cutoff<-date(query$known_at)
  value<-function(feature,at) {
   selected<-Filter(function(f)f$entity==eid&&f$feature==feature&&f$observed==at&&f$available<=cutoff&&f$known<=cutoff,facts)
   if(!length(selected))return(NULL)
   ord<-order(vapply(selected,function(f)as.numeric(f$known),numeric(1)),vapply(selected,function(f)as.numeric(f$available),numeric(1)));selected<-selected[ord]
   if(length(selected)>1){last<-selected[[length(selected)]];prior<-selected[[length(selected)-1L]];if(last$known==prior$known&&last$available==prior$available).cr_error("AMBIGUOUS_VINTAGE","Equally available duplicate facts")}
   selected[[length(selected)]]$value
  }
  values<-list();statuses<-list()
  for(spec in specs) {
   lagged<-value(spec$feature,shift(asof,spec$lag));current<-if(spec$operation!="level")value(spec$feature,asof) else NULL
   status<-if(is.null(lagged))"MISSING_LAG" else if(is.null(current)&&spec$operation!="level")"MISSING_CURRENT" else if(lagged==0&&spec$operation=="relative_change")"ZERO_BASE" else "OK"
   if(status!="OK"&&gap_policy=="error").cr_error("LAG_UNAVAILABLE",paste(spec$name,status))
   result<-if(status!="OK")NULL else if(spec$operation=="level")lagged else if(spec$operation=="difference")current-lagged else current/lagged-1
   if(!is.null(result)&&!is.finite(result)).cr_error("NUMERICAL_RANGE","Lag transformation overflow")
   values[spec$name]<-list(result);statuses[[spec$name]]<-status
  }
  output[[length(output)+1L]]<-list(entity_id=eid,as_of=query$as_of,known_at=query$known_at,features=values,statuses=statuses,eligible=all(unlist(statuses)=="OK"))
 }
 .cr_result(output,"features.build_distributed_lags","feature_table",context,diagnostics=list(frequency=frequency,gap_policy=gap_policy,calendar_rule="exact_date_clamped_month_end",no_row_shift=TRUE))
}
