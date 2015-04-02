# bankruptcy data, training + holdout
br.tr=read.table("bankruptcy-train.txt",head=T,skip=5)
br.ho=read.table("bankruptcy-holdout.txt",head=T,skip=5)

attach(br.tr)

#1(a)   cacl has largest absolute correlation with ibr
print(cor(br.tr))
#            cftd       nita       cacl        cans         ibr
#cftd  1.00000000  0.8702781  0.5052091  0.06085016 -0.55864023
#nita  0.87027807  1.0000000  0.4955879  0.13733578 -0.53221770
#cacl  0.50520912  0.4955879  1.0000000  0.36419711 -0.60628353
#cans  0.06085016  0.1373358  0.3641971  1.00000000 -0.04595699
#ibr  -0.55864023 -0.5322177 -0.6062835 -0.04595699  1.00000000

#1(b)
par(mfrow=c(2,2))
# first 3 seem to have discrimination power
plot(factor(ibr),cftd,ylab="cftd")
plot(factor(ibr),nita,ylab="nita")
plot(factor(ibr),cacl,ylab="cacl")  
plot(factor(ibr),cans,ylab="cans") # not good

#1(c)
par(mfrow=c(3,2))
plot(cacl,nita,type="n")
text(cacl,nita,label=ibr)
plot(cacl,cftd,type="n")
text(cacl,cftd,label=ibr)  # best ??
plot(cacl,cans,type="n")
text(cacl,cans,label=ibr)
plot(nita,cftd,type="n")
text(nita,cftd,label=ibr)  
plot(cftd,cans,type="n")
text(cftd,cans,label=ibr)
plot(nita,cans,type="n")
text(nita,cans,label=ibr)  # not good

# Best 2-variables are cacl,cftd  or nita,cftd
# Worst 2-variables are cans with nita ?

#2(a)
fit4=glm(ibr~cftd+nita+cacl+cans,family="binomial")
print(summary(fit4))
#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)   
#(Intercept)    4.851      2.375   2.042  0.04112 * 
#cftd          -6.875      5.706  -1.205  0.22829   
#nita           4.951     12.743   0.389  0.69764   
#cacl          -3.179      1.217  -2.613  0.00898 **
#cans           3.029      3.160   0.959  0.33774   
#    Null deviance: 53.834  on 38  degrees of freedom
#Residual deviance: 25.902  on 34  degrees of freedom
#AIC: 35.902

#2(b)
fit2=glm(ibr~cacl+cftd,family="binomial",data=br.tr)
print(summary(fit2))
#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)   
#(Intercept)   5.3494     1.9810   2.700  0.00693 **
#cacl         -2.7042     0.9987  -2.708  0.00677 **
#cftd         -5.7877     2.8155  -2.056  0.03981 * 
#    Null deviance: 53.834  on 38  degrees of freedom
#Residual deviance: 27.127  on 36  degrees of freedom
#AIC: 33.127 [smaller than 4-variable model]


#2(c)
fit2w=glm(ibr~cans+nita,family="binomial",data=br.tr)
print(summary(fit2w))
#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)   
#(Intercept)   0.06449    1.10164   0.059  0.95332   
#cans         -0.01770    2.13873  -0.008  0.99340   
#nita        -17.17570    6.57781  -2.611  0.00902 **
#    Null deviance: 53.834  on 38  degrees of freedom
#Residual deviance: 38.903  on 36  degrees of freedom
#AIC: 44.903


#2(d)
pred4=predict(fit4,type="response")
print(table(ibr,pred4>0.5))   # this is best for smallest misclassification
#ibr FALSE TRUE
#  0    20    1
#  1     2   16

pred2=predict(fit2,type="response")
print(table(ibr,pred2>0.5))
#ibr FALSE TRUE
#  0    20    1
#  1     3   15

pred2w=predict(fit2w,type="response")
print(table(ibr,pred2w>0.5))
#ibr FALSE TRUE
#  0    18    3
#  1     6   12

# 2(e)
options(digits=4)
pred.ho4=predict(fit4,newdata=br.ho,type="response")
print(pred.ho4)
#        1         2         3         4         5         6         7 
#9.232e-01 7.401e-01 9.786e-01 2.123e-01 1.714e-01 2.931e-02 4.301e-07 

# holdout points #2, #4 have probability closer to 0.5, others near 0 or 1 
print(table(br.ho$ibr,pred.ho4>0.5))
#    FALSE TRUE
#  0     4    0
#  1     0    3
pred.ho2=predict(fit2,newdata=br.ho,type="response")
print(pred.ho2)
#        1         2         3         4         5         6         7 
#9.048e-01 8.622e-01 9.767e-01 1.745e-01 2.286e-01 9.582e-02 8.345e-06 
print(table(br.ho$ibr,pred.ho2>0.5)) # same as above

# 3(a)
pca=princomp(br.tr[,1:4],cor=T,scores=T)
print(summary(pca))
#                       Comp.1 Comp.2 Comp.3 Comp.4
#Standard deviation      1.528  1.031  0.690 0.3534
#Proportion of Variance  0.584  0.266  0.119 0.0312
#Cumulative Proportion   0.584  0.850  0.969 1.0000

# 3(b)
par(mfrow=c(2,3))
plot(pca$scores[,1],pca$scores[,2],col=(br.tr$ibr+1),xlim=c(-3,4),ylim=c(-3,3))
# some overlap

#3(c)
pred.pca=predict(pca,newdata=br.ho)
plot(pred.pca[,1],pred.pca[,2],col=(br.ho$ibr+1),xlim=c(-3,4),ylim=c(-3,3))

scmat=rbind(pca$scores,pred.pca)
y=c(br.tr$ibr,br.ho$ibr)
#pchar=c(rep(111,nrow(br.tr)),rep(43,nrow(br.ho)))
pchar=c(rep("o",nrow(br.tr)),rep("+",nrow(br.ho)))
plot(scmat[,1],scmat[,2],col=(y+1),xlim=c(-3,4),ylim=c(-3,3),pch=pchar)
# Most of the 7 are easy to classify, two of the non-bankrupt cases
#  are near the boundaries of the two groups.

# Exercise: write code to do the classification from pred.pca.
# What does the principal component analysis suggest that is not
#  apparent from the logistic regression analysis?

# 4: The two methods do about the same,
#   the two groups are not completely separated in the 4-dimensional
#   x-space.

