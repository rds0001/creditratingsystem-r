if(!exists('content_fingerprint'))library(creditratingsystem)
f<-content_fingerprint
reference<-'e2581cdf698074aae7c4881573e93dfc654f1fd8fb256e83a9f2b7966b345018'
stopifnot(f(list(a=1,b=list(2)))$value==reference,f(list(b=list(2),a=1))$value==reference,f(list(1,2))$value!=f(list(2,1))$value,f(TRUE)$value!=f(1)$value,f(structure(list(),names=character()))$value!=f(list())$value,f(-0)$value==f(0)$value)
for(value in list(NaN,Inf,setNames(list(1),''),setNames(list(1),'non-ascii-\u00e4')))stopifnot(tryCatch({f(value);FALSE},creditratingsystem_error=function(e)TRUE))
