# Business/household cash conservation and explicitly documented legal units.
if(!exists('build_retail_sme_features'))library(creditratingsystem)
run_small_business_variants <- function(data=NULL,verify_reference=TRUE) {
 if(is.null(data))data<-jsonlite::fromJSON(system.file('extdata/small_business_variants.json',package='creditratingsystem'),simplifyVector=FALSE)
 a<-data$household_obligations;a$household_graph<-do.call(build_customer_household_graph,data$household_graph)
 household<-do.call(aggregate_household_obligations,a)
 cases<-lapply(data$cases,function(args) {
  a<-args$features;a$household_obligations<-household;features<-do.call(build_retail_sme_features,a)$value
  groups<-assign_default_groups(do.call(build_entity_graph,args$legal_graph),data$default_policy,data$facility_mappings)$value
  list(features=features,default_groups=groups)
 })
 a<-data$working_capital;a$annual_interest_rates<-unlist(a$annual_interest_rates);line<-do.call(exposure_schedule,a)$value
 guarantee<-do.call(allocate_guarantee_coverage,data$guarantee)$value
 if(verify_reference) {
  stopifnot(length(cases$sole_trader$default_groups$groups)==1,length(cases$micro_company$default_groups$groups)==2)
  for(result in cases) {
   f<-result$features
   stopifnot(f$known_operating_cash_before_capex_and_debt==130,f$known_cash_before_business_debt==110,f$cash_after_business_debt==80,f$cash_after_business_and_household_debt==70,f$owner_draw_business_outflows==40,f$matched_owner_draw_household_inflows==40)
  }
  stopifnot(line$schedule[[1]]$conditional_ead==40,line$schedule[[1]]$credit_limit==120,line$schedule[[1]]$undrawn_commitment==80)
  coverage<-vapply(guarantee$allocations,function(x)x$covered_amount,numeric(1));stopifnot(all(coverage==c(100,50)),sum(coverage)==150)
 }
 list(data_origin=data$data_origin,product=data$product,household=household$value,cases=cases,subsequent_working_capital_scenario=line,shared_owner_guarantee=guarantee)
}
