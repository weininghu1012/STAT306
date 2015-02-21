# partial correlations for Burnaby condominium data set

# Assume s is a sample covariance or sample correlation matrix
# with (possible) row and column names, this function outputs
# partial correlation of first two variables given the rest.
pcor=function(s)
{ i=1; j=2
  i1=c(i, j)
  i2=1:nrow(s); i2=i2[c(-i, -j)]
  s11=s[i1,i1]; s12=s[i1,i2]; s21=s[i2,i1]; s22=s[i2,i2];
  condcov = s11 - s12 %*% solve(s22) %*% s21
  condcov[1,2]/sqrt(condcov[1,1] * condcov[2,2])
}

b=read.table("/Users/apple/Desktop/STAT306/lecturenote/burnabycondo.txt",header=T,skip=2)
b=b[,2:9]
b$askprice=b$askprice/10000
b$ffarea=b$ffarea/100
b$mfee=b$mfee/10
b$sqfl=sqrt(b$floor)
attach(b)
options(digits=3)
rmat=cor(b); print(rmat)  # sample correlation matrix
#         askprice ffarea   beds   baths   floor    view     age   mfee    sqfl
#askprice    1.000  0.479 0.5756  0.6234  0.5101  0.3830 -0.4927 0.3774  0.5183
#ffarea      0.479  1.000 0.7954  0.7068  0.0330  0.1329  0.2883 0.8390  0.1063
#beds        0.576  0.795 1.0000  0.8308  0.0405  0.0786  0.0455 0.6916  0.0921
#baths       0.623  0.707 0.8308  1.0000  0.0730  0.0869 -0.1546 0.7186  0.1195
#floor       0.510  0.033 0.0405  0.0730  1.0000  0.5475 -0.1744 0.0272  0.9641
#view        0.383  0.133 0.0786  0.0869  0.5475  1.0000 -0.1854 0.0638  0.5994
#age        -0.493  0.288 0.0455 -0.1546 -0.1744 -0.1854  1.0000 0.2426 -0.1261
#mfee        0.377  0.839 0.6916  0.7186  0.0272  0.0638  0.2426 1.0000  0.0650
#sqfl        0.518  0.106 0.0921  0.1195  0.9641  0.5994 -0.1261 0.0650  1.0000

attach(b)  
bur7=lm(askprice~ ffarea+beds+baths+sqfl+view+age+mfee)
print(summary(bur7))
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   8.2064     3.3535    2.45  0.01730 *  
#ffarea        2.9516     0.7802    3.78  0.00036 ***
#beds          4.4306     2.3992    1.85  0.06964 .  
#baths         0.5278     2.6764    0.20  0.84433    
#sqfl          2.7095     0.4606    5.88  1.8e-07 ***
#view         -0.9676     1.5504   -0.62  0.53489    
#age          -0.5361     0.0665   -8.06  3.4e-11 ***
#mfee         -0.1391     0.1830   -0.76  0.45013    

options(digits=7)
# Part 1: partial correlations : equivalent definitions
str1.order=c('askprice','mfee','ffarea')
r.ymfee.ffarea=pcor(rmat[str1.order,str1.order])
print(r.ymfee.ffarea)  # -0.05153492 partial corr y and mfee given ffarea

str2.order=c('askprice','mfee','ffarea','sqfl')
r.ymfee.ffareasqfl=pcor(rmat[str2.order,str2.order])
print(r.ymfee.ffareasqfl)  # -0.03275904 partial corr y, mfee given ffarea,sqfl

str3.order=c('askprice','mfee','ffarea','sqfl','age')
r.ymfee.ffareasqflage=pcor(rmat[str3.order,str3.order])
print(r.ymfee.ffareasqflage)  # 0.06201165

# some regressions
fit.y.ffarea=lm(askprice~ffarea)   
fit.mfee.ffarea=lm(mfee~ffarea)   
fit.sqfl.ffarea=lm(sqfl~ffarea)   
fit.age.ffarea=lm(age~ffarea)   
fit.beds.ffarea=lm(beds~ffarea)   

cor.res=cor(fit.y.ffarea$residuals,fit.mfee.ffarea$residuals)
print(cor.res)  # -0.05153492, same as r.ymfee.ffarea

# exercise: get r.ymfee.ffareasqfl in a similar way.

par(mfrow=c(2,2))
plot(fit.y.ffarea$residuals,fit.mfee.ffarea$residuals)  # little assoc 
plot(fit.y.ffarea$residuals,fit.sqfl.ffarea$residuals)  # positive assoc
plot(fit.y.ffarea$residuals,fit.age.ffarea$residuals)  # negative assoc
plot(fit.y.ffarea$residuals,fit.beds.ffarea$residuals)  # weak positive assoc

# Part 2: R2 : equivalent identities
# formula for R2 from 2 explanatory
bur2=lm(askprice~ ffarea+sqfl)
print(summary(bur2))
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   7.4413     4.9246   1.511    0.136    
#ffarea        2.5880     0.5538   4.674 1.51e-05 ***
#sqfl          3.1819     0.6177   5.151 2.53e-06 ***
#Residual standard error: 8.248 on 66 degrees of freedom
#Multiple R-squared:  0.4505,    Adjusted R-squared:  0.4338 

str4.order=c('askprice','sqfl','ffarea')
r.ysqfl.ffarea=pcor(rmat[str4.order,str4.order])
print(r.ysqfl.ffarea)  # 0.535509 partial corr y,sqfl given ffarea
print(cor(fit.y.ffarea$residuals,fit.sqfl.ffarea$residuals)) # 0.535509
tem=(1-rmat['askprice','ffarea']^2)*(1-r.ysqfl.ffarea^2)
cat(tem,1-tem,"\n")
# 0.5495139 0.4504861    so R^2 = 1- (1-r_{yx_1}^2)(1-r_{yx_2;x_1}^2)

# Part 3: sign of betahat and matching partial correlation
# sign(betahat)=sign(partialcorrelation)
bur2m=lm(askprice~ ffarea+mfee)
print(summary(bur2m))
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  14.4973     5.5977   2.590  0.01180 * 
#ffarea        3.3121     1.1966   2.768  0.00732 **
#mfee         -0.1375     0.3279  -0.419  0.67641   
#Residual standard error: 9.753 on 66 degrees of freedom
#Multiple R-squared:  0.2316,    Adjusted R-squared:  0.2083 

print(r.ymfee.ffarea)  # -0.05153492 partial corr y and mfee given ffarea
# the sign of beta2hat matches that of r_{yx_2;x_1} and not r_{yx_2}
# here sign of betahat(mfee) matches that of r_{y,mfee;ffarea}, not r_{y,mfee}
# see lecture slides for the identity in the case of p=2 explanatory
