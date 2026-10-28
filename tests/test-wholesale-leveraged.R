library(creditratingsystem)
source(system.file("examples/wholesale_leveraged.R",package="creditratingsystem"))
v<-run_wholesale_leveraged()$variants
stopifnot(max(abs(vapply(v[c("sponsor_backed","high_leverage","covenant_lite")],function(x)x$refinancing_shortfall,numeric(1))-c(114,156,114)))<1e-10)
stopifnot(v$sponsor_backed$supported_pd<v$sponsor_backed$standalone_pd,is.null(v$covenant_lite$supported_pd))
