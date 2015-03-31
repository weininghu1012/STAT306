
# blood alcohol content, with #beers drank and gender as explanatory variables
alc<-read.table("/Users/apple/Desktop/STAT306/lecturenote/bloodalc.txt",header=T)
alc$beers.centered=alc$Beers-mean(alc$Beers)  # centered version of Beers
# mean(alc$Beers) = 4.8125
attach(alc)
# postscript(file="bacplots.ps",horiz=F,width=6,height=8)  # save plots in ps file
# pdf(file="bacplots.pdf",width=6,height=8)

par(mfrow=c(2,1))  # 2x1 array of subplots
plot(Beers,BAC,ylab="blood alcohol content",type="n",ylim=c(0,0.2))
text(Beers,BAC,labels=GenderOSU)  # plotting symbol is Gender
# GenderOSU is coded as F or M
legend(x="topleft",legend=c("female","male"), pch=c("F","M"))
title("Plot of BAC vs # beers consumed")

fit1<-lm(BAC~Beers+GenderOSU)
# GenderOSU is a dummy binary variable (0 for F, 1 for M)
print(summary(fit1))
betavec=fit1$coeff
abline(betavec[1],betavec[2])
abline(betavec[1]+betavec[3],betavec[2],lty=2)

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.003476   0.012004  -0.290   0.7767    
#Beers        0.018100   0.002135   8.478 1.18e-06 ***
#GenderOSUM  -0.019763   0.009086  -2.175   0.0487 *  
#Residual standard error: 0.01816 on 13 degs of freedom
#Multiple R-Squared: 0.8532, Adjusted R-squared: 0.8307 

plot(Beers,BAC,ylab="blood alcohol content",type="n",ylim=c(0,0.2))
text(Beers,BAC,labels=GenderOSU)
legend(x="topleft",legend=c("female","male"), pch=c("F","M"))
title("Plot of BAC vs # beers consumed")
fit2<-lm(BAC~Beers+GenderOSU+Beers:GenderOSU)
# Beers:GenderOSU is an example of interaction that allows a different
#  slope for F and M 
print(summary(fit2))
betavec2=fit2$coeff
abline(betavec2[1],betavec2[2])
abline(betavec2[1]+betavec2[3],betavec2[2]+betavec2[4],lty=2)

#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      -0.015667   0.015122  -1.036    0.321    
#Beers             0.020667   0.002897   7.134 1.19e-05 ***
#GenderOSUM        0.005882   0.021965   0.268    0.793    
#Beers:GenderOSUM -0.005326   0.004173  -1.276    0.226    
#Residual standard error: 0.01774 on 12 degrees of freedom
#Multiple R-squared: 0.8708,     Adjusted R-squared: 0.8385 

fit3<-lm(BAC~beers.centered+GenderOSU+beers.centered:GenderOSU)
print(summary(fit3))

#Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                0.083792   0.006274  13.355 1.46e-08 ***
#beers.centered             0.020667   0.002897   7.134 1.19e-05 ***
#GenderOSUM                -0.019750   0.008873  -2.226    0.046 *  
#beers.centered:GenderOSUM -0.005326   0.004173  -1.276    0.226    
#Residual standard error: 0.01774 on 12 degrees of freedom
#Multiple R-Squared: 0.8708,     Adjusted R-squared: 0.8385

# residSE and adjR2 same as fit2, but coefficient of GenderOSUM 
# (for Male) is now interpreted as the difference in mean(BAC)
# in males vs females at the mean beer level of 4.8125

fit4<-lm(BAC~Beers)
print(summary(fit4))

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.012701   0.012638  -1.005    0.332    
#Beers        0.017964   0.002402   7.480 2.97e-06 ***
#Residual standard error: 0.02044 on 14 degrees of freedom
#Multiple R-Squared: 0.7998,     Adjusted R-squared: 0.7855 

# How to interpret and compare the regression models?
