library(creditratingsystem)
factor <- list(family="standard_normal",factor_id="cycle",reference_period="synthetic",model_version="1")
transform <- function(p,z,r,direction="ttc_to_pit",...) pit_ttc_transform(p,z,r,direction,factor,1,"years","PHYSICAL",...)
for (parameters in list(c(.01,.2),c(.2,.7),c(.9,.01))) {
  integral <- integrate(function(z) transform(parameters[1],z,parameters[2])$value*dnorm(z),-10,10,abs.tol=1e-12)$value
  stopifnot(abs(integral-parameters[1]) < 1e-10)
}
p <- c(.001,.02,.5,.9); z <- c(-1,0,1,2)
stopifnot(max(abs(transform(transform(p,z,.2)$value,z,.2,"pit_to_ttc")$value-p)) < 1e-12,
  transform(.02,-1,.2)$value > transform(.02,1,.2)$value,
  identical(transform(c(0,1),1,.2)$value,c(0,1)),identical(transform(p,1,0)$value,p),
  transform(.1,100,.99)$diagnostics$inverse_information_loss)
expect_code <- function(expr,code) {
  actual <- tryCatch({force(expr); "NO_ERROR"},creditratingsystem_error=function(e)e$code)
  stopifnot(identical(actual,code))
}
expect_code(transform(.1,0,1),"INVALID_CORRELATION")
expect_code(transform(c(.1,.2),c(0,1,2),.2),"INCOMPATIBLE_SHAPE")
expect_code(transform(.1,0,.2,context=list(horizon=2)),"TARGET_MISMATCH")
