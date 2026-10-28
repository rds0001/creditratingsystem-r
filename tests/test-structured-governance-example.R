if(!exists('compile_credit_decision_policy'))library(creditratingsystem)
governance_example_file<-system.file('examples/structured_governance.R',package='creditratingsystem')
governance_data_file<-system.file('extdata/structured_governance.json',package='creditratingsystem')
if(!nzchar(governance_example_file))governance_example_file<-'creditratingsystem-r/examples/structured_governance.R'
if(!nzchar(governance_data_file))governance_data_file<-'creditratingsystem-r/inst/extdata/structured_governance.json'
source(governance_example_file)
governance_result<-run_structured_governance(jsonlite::fromJSON(governance_data_file,simplifyVector=FALSE))
stopifnot(setequal(names(governance_result$variants),c('SECURITIZATION_TRANCHE','COVERED_BOND','TRADE_RECEIVABLES','COUNTERPARTY_EXPOSURE')))
for(case in governance_result$variants)stopifnot(case$performing_assessment$as_of=='2024-01-15',case$retired$history[[1]]$previous_assessment_hash==case$assessment$assessment_hash,case$assessment$state=='DRAFT',is.null(tail(case$exit_cohort$rows,1)[[1]]$target))
