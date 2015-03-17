# logistic discrimination for gauss2.train.txt (simulated data with 2 classes)
gauss=read.table("gauss2.train.txt",header=T)
n=nrow(gauss); sym=rep("o",n); sym[gauss$y==1]<-"+"
options(digits=4)
print(cor(gauss[,2:4]))
#       x1     x2      y
#x1 1.0000 0.6416 0.8316
#x2 0.6416 1.0000 0.5522
#y  0.8316 0.5522 1.0000

attach(gauss)
plot(x1,x2,type="n",xlim=c(1,8),ylim=c(1,15)); text(x1,x2,label=sym)

# Table 7.2, p 7-7, with added 5th row
holdout=matrix(c(3.68,5.65,3.28,5.20,3.67,8.82,4.64,7.98,6.4,6.4),5,2,byrow=T)
holdout=data.frame(holdout); names(holdout)=c("x1","x2")

fit12=glm(y~x1+x2,family="binomial"); print(summary(fit12))
#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)  
#(Intercept) -28.9411    19.1188  -1.514   0.1301  
#x1            4.6253     2.7501   1.682   0.0926 .
#x2            0.7272     0.8204   0.886   0.3754 
#(Dispersion parameter for binomial family taken to be 1)
#    Null deviance: 38.1909  on 29  degrees of freedom
#Residual deviance:  5.9237  on 27  degrees of freedom
#AIC: 11.92
#Number of Fisher Scoring iterations: 9
# sample size is n=22, which may explain why P-values are not small.

fit1=glm(y~x1,family="binomial"); print(summary(fit1))
#            Estimate Std. Error z value Pr(>|z|)  
#(Intercept)   -20.63      11.01   -1.87    0.061 .
#x1              4.26       2.23    1.91    0.057 .
#    Null deviance: 38.1909  on 29  degrees of freedom
#Residual deviance:  7.1173  on 28  degrees of freedom [larger than above]
#AIC: 11.12   [Note that this is smaller than above]
#Number of Fisher Scoring iterations: 8
detach(gauss)

holdpr=predict(fit12,newdata=holdout,type="response")
print(rbind(holdout$x1,holdout$x2,holdpr))
#               1         2        3      4      5
#       3.6800000 3.280e+00 3.670000 4.6400 6.4000
#       5.6500000 5.200e+00 8.820000 7.9800 6.4000
#holdpr 0.0004051 4.593e-05 0.003865 0.1576 0.9951

# add holdout points to plot
par(new=T)
plot(holdout$x1,holdout$x2,type="n",xlim=c(1,8),ylim=c(1,15),xlab="",ylab="")
text(holdout$x1,holdout$x2,label="?")

# plot boundary where 1/(1+exp(-b[1]-b[2]*x1-b[3]*x2)=0.5
# or -b[1]-b[2]*x1-b[3]*x2=0
# or -b[1]= b[2]*x1+b[3]*x2  or x2=(-b[1]-b[2]*x1)/b[3]
b=fit12$coef
abline(-b[1]/b[3],-b[2]/b[3]) # boundary for regression with x1,x2
# plot boundary where 1/(1+exp(-bb[1]-bb[2]*x1)=0.5
# or -bb[1]-bb[2]*x1=0 or x1=-bb[1]/bb[2]
bb=fit1$coef; abline(v=-bb[1]/bb[2],lty=2) # boundary for regression with x1
# exercise: add the boundary from regression with x2 alone
title("gauss data: section 7.5.1") # see Figure 7.3, p 7-6
