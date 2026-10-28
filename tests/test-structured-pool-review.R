library(creditratingsystem)
source(system.file('examples/structured_pool_review.R',package='creditratingsystem'))
report<-run_structured_pool_review()$products
a<-report$SECURITIZATION_TRANCHE;m<-a$asset_pool_validation$metrics;p<-a$retail_pool_validation$metrics$pools[[1]]
stopifnot(m$pool_cash_errors$recovery_gross$mean_prediction==8,m$observed_owned_flows$recovery_gross==30,m$pool_cash_errors$recovery_gross$bias== -22,abs(p$lgd_error$mean_outcome-.75)<1e-12,abs(p$lgd_error$mean_prediction-.65)<1e-12,report$COVERED_BOND$asset_pool_validation$metrics$observed_owned_flows$prepayment_principal==100,report$TRADE_RECEIVABLES$retail_pool_validation$applicability=='NOT_APPLICABLE')
data<-jsonlite::fromJSON(system.file('extdata/structured_pool_review.json',package='creditratingsystem'),simplifyVector=FALSE)
data$cases$SECURITIZATION_TRANCHE$pool_accounts[[2]]$closing_cash<-26
result<-run_structured_pool_review(data,verify_reference=FALSE)$products$SECURITIZATION_TRANCHE
stopifnot(abs(result$asset_pool_validation$cash_residuals[[2]]$residual)==1,length(result$asset_pool_validation$violations)>0,is.null(result$incomplete_coverage_validation$metrics$default_probability$brier),result$duplicate_allocation_validation$violation_counts$CASH_OR_LOSS_DOUBLE_ALLOCATION>=1)
