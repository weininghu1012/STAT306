# logistic regression and prediction/classification for mutagenicity data
muta=read.table("mutagenicity.txt",header=T)
muta=muta[,-1]  # 1859 x 46
options(digits=3)
cormat=cor(muta[,2:46])
tem=c(cormat); tem=tem[tem<1]
hist(tem,main="histogram of correlations of mutagenicity vars",xlab="corr")
par(mfrow=c(2,1))
fit1=glm(y~.,family="binomial",data=muta)
print(summary(fit1))
# six NAs for betas, multicollinearity problem as in Chapter 5
# ...
#Ms           -0.75257    0.33770   -2.23   0.0258 *  
#nBT           0.93998    0.39512    2.38   0.0174 *  
#nBO                NA         NA      NA       NA    
#nDB           0.25528    0.06352    4.02  5.8e-05 ***
#nCO          -0.34042    0.13693   -2.49   0.0129 *  
#nNO           2.51455    0.33526    7.50  6.4e-14 ***
#nNO2          0.87967    0.29925    2.94   0.0033 ** 
#nSO2         -2.90750    0.71102   -4.09  4.3e-05 ***
#...
#nCIR          1.42356    0.16289    8.74  < 2e-16 ***
#nR03          0.20885    0.48582    0.43   0.6673    
#nR04         -0.87769    0.98315   -0.89   0.3720 
#nR05         -2.57931    0.44034   -5.86  4.7e-09 ***
#nR06         -2.92322    0.44200   -6.61  3.7e-11 ***
#nR07         -3.46215    0.58380   -5.93  3.0e-09 ***
#nR09         -0.95181    0.22824   -4.17  3.0e-05 ***
#    Null deviance: 2574.6  on 1858  degrees of freedom
#Residual deviance: 1836.1  on 1819  degrees of freedom
#AIC: 1916

fit2=glm(y~Ms+nBT+nDB+nCO+nNO+nNO2+nSO2+nCIR+nR05+nR06+nR07+nR09,
  family="binomial",data=muta)
print(summary(fit2))
#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   0.2997     0.4569    0.66  0.51188    
#Ms           -0.2528     0.1255   -2.01  0.04399 *  
#nBT          -0.0321     0.0123   -2.62  0.00885 ** 
#nDB           0.2402     0.0430    5.58  2.4e-08 ***
#nCO          -0.1988     0.1091   -1.82  0.06832 .  
#nNO           3.0137     0.3130    9.63  < 2e-16 ***
#nNO2          1.6130     0.2508    6.43  1.3e-10 ***
#nSO2         -2.1183     0.6146   -3.45  0.00057 ***
#nCIR          1.3874     0.1228   11.30  < 2e-16 ***
#nR05         -1.4595     0.2210   -6.60  4.0e-11 ***
#nR06         -1.9437     0.2307   -8.42  < 2e-16 ***
#nR07         -3.1850     0.5951   -5.35  8.7e-08 ***
#nR09         -1.0715     0.2023   -5.30  1.2e-07 ***
#    Null deviance: 2574.6  on 1858  degrees of freedom
#Residual deviance: 1999.6  on 1846  degrees of freedom
#AIC: 2026

pred1=predict(fit1,type="response")
pred2=predict(fit2,type="response")
plot(factor(muta$y),pred1,xlab="y (not/is mutagenic)",
  ylab="estimated probability of mutagenicity")  # looks like Fig 7.7 on p 7-21
title("All 45 explanatory variables")
plot(factor(muta$y),pred2,xlab="y (not/is mutagenic)",
  ylab="model 2: probability of mutagenicity")  
title("12 'best' explanatory variables")

# predict a compound to be mutagenic if prediction probability >=0.5 (yhat=1)
tab1=table(muta$y, pred1>=0.5) # in-sample classification, see Table 7.3, p 7-14
print(tab1); print(tab1/1859)
#   FALSE TRUE       FALSE   TRUE    
#  0   824  140   0 0.4432 0.0753   # .0753/.5186=.145 is error rate for non-m
#  1   293  602   1 0.1576 0.3238   # .1576/.4814=.327 is error rate for mut.
tab2=table(muta$y, pred2>=0.5)
print(tab2); print(tab2/1859) 
#    FALSE TRUE      FALSE   TRUE
#  0   851  113   0 0.4578 0.0608   # .0608/.5186=.117 is error rate for non-m
#  1   372  523   1 0.2001 0.2813   # .2001/.4814=.415 is error rate for mut.
# tradeoff: which type of classification is more important,
#  get mutagenic correct, or get non-mutagenic correct
tab3=table(muta$y, pred2>=0.38)   
print(tab3); print(tab3/1859) 
# lower boundary to predict more yhat=1 for compounds that are mutagenic
#    FALSE TRUE          FALSE   TRUE
#  0   740  224       0 0.3981 0.1205  # error rate .1205/.5186=.232
#  1   295  600       1 0.1587 0.3228  # error rate .1587/.4814=.330

# Hosmer-Lemeshow calibration checks
hist(pred1); hist(pred2)
pr1cat=cut(pred1,breaks=seq(0,1,.1))
HLsumm1=tapply(muta$y,pr1cat,mean);  print(HLsumm1)
# (0,0.1] (0.1,0.2] (0.2,0.3] (0.3,0.4] (0.4,0.5] (0.5,0.6] (0.6,0.7] (0.7,0.8] 
#   0.115     0.116     0.218     0.380     0.474     0.539     0.653     0.795 
#(0.8,0.9]   (0.9,1] 
#    0.905     0.926 
pr2cat=cut(pred2,breaks=seq(0,1,.1))
HLsumm2=tapply(muta$y,pr2cat,mean);  print(HLsumm2)
# (0,0.1] (0.1,0.2] (0.2,0.3] (0.3,0.4] (0.4,0.5] (0.5,0.6] (0.6,0.7] (0.7,0.8] 
#   0.139     0.219     0.235     0.350     0.403     0.405     0.692     0.780 
#(0.8,0.9]   (0.9,1] 
#    0.912     0.931 
# full model is better calibrated

# out-of-sample misclassification (with holdout set) is more meaningful than
# in-sample misclassification
# compare with random split into training and holdout
set.seed(321); iperm=sample(1859,1859)
train=muta[iperm[1:1000],]; holdout=muta[iperm[1001:1859],]
fit1.train=glm(y~.,family="binomial",data=train)
pred1.hold=predict(fit1.train,newdata=holdout,type="response")
fit2.train=glm(y~Ms+nBT+nDB+nCO+nNO+nNO2+nSO2+nCIR+nR05+nR06+nR07+nR09,
  family="binomial",data=train)
pred2.hold=predict(fit2.train,newdata=holdout,type="response")
tab1h=table(holdout$y, pred1.hold>=0.5)
tab2h=table(holdout$y, pred2.hold>=0.5)
print(tab1h); print(tab1h/859) 
# more explanatory variables help for predicting y=1 when in fact mutagenic
#    FALSE TRUE          FALSE  TRUE   
#  0   364   93        0 0.424 0.108     # error rate .108/.532=.203
#  1   123  279        1 0.143 0.325     # error rate .143/.468=.306
print(tab2h); print(tab2h/859)
#    FALSE TRUE           FALSE   TRUE  
#  0   397   60        0 0.4622 0.0698   # error rate .0698/.532=.131
#  1   166  236        1 0.1932 0.2747   # error rate .1932/.468=.413
