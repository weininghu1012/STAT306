# example of principal components in 2 dimensions
set.seed(543); n=50; rho=0.7
x1=rnorm(n);  x2=rho*x1+sqrt(1-rho^2)*rnorm(n)
x1=x1+5; x2=x2+4; xdat=cbind(x1,x2)
pcout=prcomp(xdat,scores=T)
names(pcout)
#[1] "sdev"     "rotation" "center"   "scale"    "x" 
print(summary(pcout))
#Importance of components:
#                          PC1    PC2
#Standard deviation     1.1404 0.5147
#Proportion of Variance 0.8308 0.1692
#Cumulative Proportion  0.8308 1.0000
print(pcout$rotation)
#          PC1        PC2
#x1 -0.6681117 -0.7440610
#x2 -0.7440610  0.6681117

head(pcout$x)
#            PC1         PC2
#[1,] -2.0193411 -0.07302154
#[2,]  0.1127290 -0.42047886
#[3,] -0.4075932 -0.28394924
#[4,]  0.2326817 -0.02273571
#[5,]  0.9363531  0.39498862
#[6,] -0.1518157 -0.96592101
head(xdat)
#           x1       x2
#[1,] 6.351411 5.453673
#[2,] 5.185479 3.635143
#[3,] 5.431527 4.113511
#[4,] 4.809393 3.811627
#[5,] 4.028449 3.567140
#[6,] 5.768067 3.467564

xbar1=mean(x1); xbar2=mean(x2)
load1=pcout$rotation[,1]; load2=pcout$rotation[,2]
pc1=(x1-xbar1)*load1[1]+(x2-xbar2)*load1[2]
pc2=(x1-xbar1)*load2[1]+(x2-xbar2)*load2[2]
print(pc1[1:6])
#[1]  -2.0193411  0.1127290 -0.4075932  0.2326817  0.9363531 -0.1518157
print(pc2[1:6])
#[1] -0.07302154 -0.42047886 -0.28394924 -0.02273571  0.39498862 -0.96592101

# now change sign for first PC (to interpret as weighted sum)
load1=-pcout$rotation[,1]
pc1=(x1-xbar1)*load1[1]+(x2-xbar2)*load1[2]
plot(x1,x2,xlim=c(1.9,7),ylim=c(1.9,7))
# 0 = (x1-xbar1)*load2[1]+(x2-xbar2)*load2[2] is new x-axis (newy=0)
# x2 = (xbar2*load2[2]+xbar1*load2[1])/load2[2] - load2[1]*x1/load2[2]
# 0 = (x1-xbar1)*load1[1]+(x2-xbar2)*load1[2] is new y-axis (newx=0)
# x2 = (xbar2*load1[2]+xbar1*load1[1])/load1[2] - load1[1]*x1/load1[2]
abline((xbar2*load2[2]+xbar1*load2[1])/load2[2], -load2[1]/load2[2])
abline((xbar2*load1[2]+xbar1*load1[1])/load1[2], -load1[1]/load1[2])
# 1 = (x1-xbar1)*load2[1]+(x2-xbar2)*load2[2]  (newy=1 is new space)
# x2 = (1+xbar2*load2[2]+xbar1*load2[1])/load2[2] - load2[1]*x1/load2[2]
# 1 = (x1-xbar1)*load1[1]+(x2-xbar2)*load1[2]  (newx=1 is new space)
# x2 = (1+xbar2*load1[2]+xbar1*load1[1])/load1[2] - load1[1]*x1/load1[2]
abline((1+xbar2*load2[2]+xbar1*load2[1])/load2[2], -load2[1]/load2[2],lty=2)
abline((1+xbar2*load1[2]+xbar1*load1[1])/load1[2], -load1[1]/load1[2],lty=2)
text(x1[1],x2[1],"1") # add text for point 1
title(paste("point 1 is (",round(x1[1],2), round(x2[1],2), ") or (",
   round(pc1[1],2),round(pc2[1],2), ")" ))
