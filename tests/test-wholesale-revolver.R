library(creditratingsystem)
source(system.file("examples/wholesale_revolver.R",package="creditratingsystem"))
result<-run_wholesale_revolver();v<-result$variants
stopifnot(all(vapply(v,function(x)x$reference_drawn==0&&x$subject_count==1,logical(1))))
stopifnot(max(abs(vapply(v[c("working_capital_line","contingent_commitment","cancellable","uncancellable")],function(x)x$conditional_ead,numeric(1))-c(50,100,50,50)))<1e-10)
stopifnot(is.null(v$uncancellable$contractual_cancellation),result$missing_evidence_status=='REFER')
