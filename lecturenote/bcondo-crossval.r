# leave-one-out cross-validation for Burnaby condominium data
# ffarea, sqfl, age
# ffarea, sqfl, age, beds
# all: ffarea   beds   baths   view     age   mfee    sqfl

b=read.table("/Users/apple/Desktop/STAT306/lecturenote/burnabycondo.txt",header=T,skip=2)
b=b[,2:9]
b$askprice=b$askprice/10000
b$ffarea=b$ffarea/100
b$mfee=b$mfee/10
b$sqfl=sqrt(b$floor)
attach(b)

fit3=lm(askprice~ ffarea+sqfl+age)
summ3=summary(fit3)
print(summ3)
fit4=lm(askprice~ ffarea+sqfl+age+beds)
summ4=summary(fit4)
print(summ4)
Xmat3=model.matrix(fit3); Xmat3=Xmat3[,-1] # omit column of 1s; input to lsfit
Xmat4=model.matrix(fit4); Xmat4=Xmat4[,-1]

# loop deleting one observation at a time 
cat("\nLeave-one-out Cross-validation\n")
cvss3=0; cvss4=0
y=askprice; n=nrow(b); cvres4=rep(0,n)
for(i in 1:n)
{ cvfit3=lsfit(Xmat3[-i,],y[-i])  # delete ith observation 
  cvfit4=lsfit(Xmat4[-i,],y[-i])  # delete ith observation 
  bvec3=cvfit3$coef; bvec4=cvfit4$coef
  ypred3=sum(bvec3*c(1,Xmat3[i,]))  # prediction of omitted y based on its x
  ypred4=sum(bvec4*c(1,Xmat4[i,]))
  cvres4[i]=y[i]-ypred4
  cat("i=",i, " y= ", y[i], "  ypred3=", ypred3, "  ypred4=", ypred4,
     "cvres4 ", cvres4[i],"\n")
  cvss3=cvss3+(y[i]-ypred3)^2; cvss4=cvss4+(cvres4[i])^2
}

cat("\nCross-validated SS for models with 3 and 4 explanatory variables\n")
print(c(cvss3,cvss4))                 #  1900.983 1794.842
print(sqrt(c(cvss3,cvss4)/n))         #  5.248855 5.100215
# compare with residual SDs, cross-validared rmse usually a little larger
cat("residualSDs\n")
print(c(summ3$sigma,summ4$sigma))     #  5.040209 4.848409

# Compute cross-validated root mean squared error of prediction.
# ls.out is a fitted regression model from lsreg or lm.
ls.cvrmse <- function(ls.out)
{ res.cv <- ls.out$residuals / (1.0 - ls.diag(ls.out)$hat)
  # above formula to be explained in class
  is.na.res <- is.na(res.cv)  # Identify NA's and remove them.
  res.cv <- res.cv[!is.na.res]
  cvrmse <- sqrt(sum(res.cv^2) / length(res.cv))
  return(cvrmse)
}


# compare with function ls.cvrmse(); not necessary to do so many regressions
cat("\nCross-validation root mean square error using ls.cvrmse function\n")
cvrmse3=ls.cvrmse(fit3); cvrmse4=ls.cvrmse(fit4)
print(c(cvrmse3,cvrmse4)) #  5.248855 5.100215
# Model with 4 explanatory variables is also better based on leave-one-out
#   cross-validation
# Some of the outputs of ls.diag() will be explained later
fit3.diag=ls.diag(fit3); names(fit3.diag)
# [1] "std.dev"      "hat"          "std.res"      "stud.res"     "cooks"       
# [6] "dfits"        "correlation"  "std.err"      "cov.scaled"   "cov.unscaled"

# Another way to get the cross-validated residuals
XtXinv4=summ4$cov.unscaled; bhat4=fit4$coef; cvres4b=rep(0,n)
Xmat4=model.matrix(fit4); # get back column of 1s
for(i in 1:n)
{ pred4=sum(bhat4*Xmat4[i,]); res4=y[i]-pred4;
  Pii=t(Xmat4[i,])%*%XtXinv4%*%Xmat4[i,]
  cvres4b[i]=res4/(1-Pii)
}

cat("\nCompare two ways of computing cross-validated residuals\n")
cat("Second approach is based on the t(X)%*X matrix\n")
tem=cbind(cvres4,cvres4b)
print(tem[1:10,])
#          cvres4    cvres4b
# [1,] -4.1759498 -4.1759498
# [2,] -5.8206755 -5.8206755
# [3,]  0.4435884  0.4435884
# [4,] -2.3461729 -2.3461729
# [5,] 11.4194207 11.4194207
# [6,] -2.0343146 -2.0343146
# [7,] -7.5799585 -7.5799585
# [8,] 10.9803576 10.9803576
# [9,] -0.3331639 -0.3331639
#[10,]  3.3052023  3.3052023
detach(b)

#============================================================
# training set/ holdout set
set.seed(123) # set seed so that random subsets will be same on re-running code
n=nrow(b)
iperm=sample(n,n)  # random permutation of 1:n
train=b[iperm[1:50],]  # random subset of 50 for training set
holdout=b[iperm[51:n],]  # remainder is holdout set
fit3train=lm(askprice~ ffarea+sqfl+age, data=train)
fit4train=lm(askprice~ ffarea+sqfl+age+beds, data=train)
pred3hold=predict(fit3train,new=holdout)
pred4hold=predict(fit4train,new=holdout)

rmse3hold=sqrt(mean((holdout$askprice-pred3hold)^2))
rmse4hold=sqrt(mean((holdout$askprice-pred4hold)^2))
cat("\nSize of holdout set is ", nrow(holdout),"\n") # 19
cat("Cross-validation root mean square error with holdout set\n")
print(c(rmse3hold,rmse4hold))
#  5.556060 5.131601
