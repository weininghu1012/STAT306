# Logistic regression model comparisons for 4 explanatory variables;
# the data set is simulated.
 
load("simdat.RData") # load an R  workspace image file (previously save())
rmat=cor(simdat)
print(rmat)
#          x1        x2        x3        x4         y
#x1 1.0000000 0.6805134 0.5234297 0.3836794 0.7150929
#x2 0.6805134 1.0000000 0.7575462 0.6322680 0.7222479
#x3 0.5234297 0.7575462 1.0000000 0.7567970 0.6337037
#x4 0.3836794 0.6322680 0.7567970 1.0000000 0.5849442
#y  0.7150929 0.7222479 0.6337037 0.5849442 1.0000000

fit12=glm(y~x1+x2,family="binomial",data=simdat); summ12=summary(fit12)
fit13=glm(y~x1+x3,family="binomial",data=simdat); summ13=summary(fit13)
fit14=glm(y~x1+x4,family="binomial",data=simdat); summ14=summary(fit14)
fit23=glm(y~x2+x3,family="binomial",data=simdat); summ23=summary(fit23)
fit24=glm(y~x2+x4,family="binomial",data=simdat); summ24=summary(fit24)
fit34=glm(y~x3+x4,family="binomial",data=simdat); summ34=summary(fit34)
fit123=glm(y~x1+x2+x3,family="binomial",data=simdat); summ123=summary(fit123)
fit124=glm(y~x1+x2+x4,family="binomial",data=simdat); summ124=summary(fit124)
fit134=glm(y~x1+x3+x4,family="binomial",data=simdat); summ134=summary(fit134)
fit234=glm(y~x2+x3+x4,family="binomial",data=simdat); summ234=summary(fit234)
fit1234=glm(y~x1+x2+x3+x4,family="binomial",data=simdat); summ1234=summary(fit1234)

residdevvec=c(summ12$deviance,summ13$deviance,summ14$deviance,summ23$deviance,
  summ24$deviance,summ34$deviance,summ123$deviance,summ124$deviance,
  summ134$deviance,summ234$deviance,summ1234$deviance)
aicvec=c(summ12$aic,summ13$aic,summ14$aic,summ23$aic,
  summ24$aic,summ34$aic,summ123$aic,summ124$aic,
  summ134$aic,summ234$aic,summ1234$aic)
subsetvec=c(12,13,14,23,24,34,123,124,134,234,1234)
options(digits=4)
print(cbind(subsetvec,residdevvec,aicvec))
#      subsetvec residdevvec aicvec
# [1,]        12   3.415e+01  40.15
# [2,]        13   2.890e+01  34.90 best 2-variable model
# [3,]        14   2.976e+01  35.76
# [4,]        23   4.805e+01  54.05
# [5,]        24   4.316e+01  49.16
# [6,]        34   7.055e+01  76.55
# [7,]       123   2.112e+01  29.12
# [8,]       124   6.184e+00  14.18 best 3-variable model
# [9,]       134   1.784e+01  25.84
#[10,]       234   4.278e+01  50.78
#[11,]      1234   7.993e-08  10.00 WHY IS RESIDUAL DEVIANCE APPROX 0?
# Best model based on smallest AIC is the 4-variable model.
# No equivalent to leaps/regsubsets??

print(summ13)
#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   -1.802      0.792   -2.28  0.02290 *  
#x1             4.445      1.213    3.67  0.00025 ***
#x3             4.885      1.682    2.90  0.00368 ** 
#    Null deviance: 133.750  on 99  degrees of freedom
#Residual deviance:  28.902  on 97  degrees of freedom
#AIC: 34.9

print(summ124)
#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)
#(Intercept)    -5.42       3.39   -1.60     0.11
#x1             18.83      15.15    1.24     0.21
#x2             15.62      10.97    1.42     0.15
#x4             13.80       9.52    1.45     0.15
#    Null deviance: 133.7496  on 99  degrees of freedom
#Residual deviance:   6.1845  on 96  degrees of freedom
#AIC: 14.18

print(summ1234)
#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)
#(Intercept)    -73.5    14872.3    0.00        1
#x1             192.6    32704.0    0.01        1
#x2             115.6    22790.5    0.01        1
#x3             143.4    30313.2    0.00        1
#x4              89.7    16656.0    0.01        1
#    Null deviance: 1.3375e+02  on 99  degrees of freedom
#Residual deviance: 7.9934e-08  on 95  degrees of freedom
#AIC: 10

# Look at some plots
attach(simdat)
plot(x1,x3,type="n")
text(x1,x3,label=y)
detach(simdat)

# Bonus Question: how can the residual deviance be 0??
