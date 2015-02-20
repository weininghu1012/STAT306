# Multiple regression and SEs for betas and predictions: Burnaby condo data

b=read.table("burnabycondo.txt",header=T,skip=2)
b=b[,2:9] # exclude non-quantitative variables
b$askprice=b$askprice/10000   # scale some variables
b$ffarea=b$ffarea/100
b$mfee=b$mfee/10
b$sqfl=sqrt(b$floor)
attach(b) 
bur4=lm(askprice~ ffarea+sqfl+age+beds)
summ4=summary(bur4)
print(summ4)
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  8.70444    2.96562   2.935  0.00463 ** 
#ffarea       2.52992    0.59183   4.275 6.50e-05 ***
#sqfl         2.55472    0.36846   6.933 2.43e-09 ***
#age         -0.53558    0.05569  -9.617 4.81e-14 ***
#beds         4.75871    1.90433   2.499  0.01504 *  
#Residual standard error: 4.848 on 64 degrees of freedom
#Multiple R-squared:  0.8159,    Adjusted R-squared:  0.8043 

print(names(summ4))
# [1] "call"          "terms"         "residuals"     "coefficients" 
# [5] "aliased"       "sigma"         "df"            "r.squared"    
# [9] "adj.r.squared" "fstatistic"    "cov.unscaled" 

mm4=model.matrix(bur4)  # This is the 69x5 X matrix with a first column of 1s
k=ncol(mm4); n=nrow(b)  # k=5
residSE=sqrt(sum(bur4$resid^2)/(n-k))   # 4.848409
print(summ4$sigma)   # 4.848409
A=t(mm4)%*%mm4 # X^T*X
V=solve(A)    #  (X^T*X)^{-1}
print(A)
#            (Intercept)   ffarea      sqfl       age      beds
#(Intercept)      69.000  579.880  207.2790   870.000  114.0000
#ffarea          579.880 5097.698 1763.3591  7731.450 1010.8800
#sqfl            207.279 1763.359  803.0000  2448.771  347.9459
#age             870.000 7731.450 2448.7710 20428.000 1457.0000
#beds            114.000 1010.880  347.9459  1457.000  208.0000
options(digits=5)
print(V)
#            (Intercept)      ffarea        sqfl         age       beds
#(Intercept)  0.37413828 -0.04962507 -0.01185257  0.00055574  0.0520556
#ffarea      -0.04962507  0.01490007 -0.00112394 -0.00059996 -0.0391333
#sqfl        -0.01185257 -0.00112394  0.00577552  0.00014788  0.0012612
#age          0.00055574 -0.00059996  0.00014788  0.00013195  0.0014395
#beds         0.05205564 -0.03913327  0.00126120  0.00143951  0.1542716

print(summ4$cov.unscaled) # same as above

print(sqrt(diag(V)))
#(Intercept)      ffarea        sqfl         age        beds 
#   0.611668    0.122066    0.075997    0.011487    0.392774 

sebetas=residSE*sqrt(diag(V))  # from diagonal entries of V
print(sebetas)  # this should match regression table
#(Intercept)      ffarea        sqfl         age        beds 
#   2.965619    0.591825    0.368464    0.055694    1.904330 

# SE for a prediction
xnew=b[1,] # new data frame with one tow
xnew$ffarea=16; xnew$age=8; xnew$sqfl=sqrt(6); xnew$beds=3
print(xnew)
#  askprice ffarea beds baths floor view age mfee   sqfl
#1     32.8     16    3   1.5    16    1   8 18.5 2.4495

pred=predict(bur4,new=xnew,interval="prediction", se.fit=T)
print(names(pred))
# [1] "fit"            "se.fit"         "df"             "residual.scale"
print(pred)
#$fit
#     fit    lwr    upr
#1 65.432 54.012 76.853
#$se.fit
#[1] 3.0286
#$df
#[1] 64
#$residual.scale
#[1] 4.8484

# check with equations in coursepack
betahat=bur4$coef
xfut=c(1,xnew$ffarea,xnew$sqfl,xnew$age,xnew$beds)
ypred=sum(betahat*xfut)  # 65.432
sepred=residSE*sqrt(1+xfut%*%V%*%xfut) # 5.7166
sefit=residSE*sqrt(xfut%*%V%*%xfut)  # 3.0286 (for subpopulation mean)
tstar=qt(0.975,n-k)  #  1.9977
moe=tstar*sepred  # 11.2
print(c(ypred-moe,ypred+moe)) #  54.012 76.853
