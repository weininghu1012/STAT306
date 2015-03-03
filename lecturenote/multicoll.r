# check on XtXinv for near collinearity


coll=function(n,delta)
{ x1=1:n
  y=1+2*x1+rnorm(n,0,0.3)
  x2=3*x1+runif(n,-delta,delta)
  cat("delta=",delta,"\n")
  fit=lm(y~x1+x2)
  summ=summary(fit)
  print(summ)
  print(summ$cov.unscaled)
  cat("\n============================================================\n")
  invisible(0)
}

coll(20,.1)
coll(20,.05)
coll(20,.01)
coll(20,0)
