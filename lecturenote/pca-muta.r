# principal components for mutagenicity data
# PCs of some subsets (variables that are related)

muta=read.table("mutagenicity.txt",header=T)
muta=muta[,-1]
# grp 1a,1b: AMW Mv Me Mp Ms;  MW Sv Se Sp Ss 
# grp 2: (nBT) nBO nBM nDB nTB nAB : cols 14-19
# grp 3: nAT (nSK) nC nN nO nP nS nF nCL nBR nX : cols 12-13,20-28
# grp 4: nCO nNO nNO2 nSO2 nSO3 nPO : cols 29-34
# grp 5: nCIC nCIR nR03 nR04 nR05 nR06 nR07 nR08 nR09 nR10 : cols 37-46
# others: cols 35-36
# response : col 1

# nAT (#atoms), nSK (#non-H atoms) differ in 3 cases; 
# same for nBT (#bonds), nBO (#non-H-bonds) (rows 674  787 1359)
options(digits=3)
grp1a=c(2,8:11); grp1b=3:7
grp2=15:19; grp3=c(12,20:28); grp4=29:34; grp5=37:46; grpr=35:36
# MW dominates if princomp with sample covariance matrix
pca1a=princomp(muta[,grp1a],cor=T,scores=T) # correlation
pca1b=princomp(muta[,grp1b],cor=T,scores=T) # correlation
pca2=princomp(muta[,grp2],cor=F,scores=T)   # covariance because same unit
pca3=princomp(muta[,grp3],cor=F,scores=T)   # covariance
pca4=princomp(muta[,grp4],cor=F,scores=T)   # covariance
pca5=princomp(muta[,grp5],cor=F,scores=T)   # covariance
cat("\nsdev: groups 1a, 1b, 2 to 5\n")
print(pca1a$sdev)  # take first 2
print(pca1b$sdev)  # take first 3 
print(pca2$sdev)  # take first 3
print(pca3$sdev)  # take first 6 or 5? or 3?
print(pca4$sdev)  # keep as original
print(pca5$sdev)  # take first 6, 7?

cat("\ncovariance/correlation matrices for group 4: nCO nNO nNO2 nSO2 nSO3 nPO\n")
print(cov(muta[,grp4]))
print(cor(muta[,grp4]))

cat("\ncovariance/correlation matrices for group 2: nBO nBM nDB nTB nAB\n")
print(cov(muta[,grp2]))
print(cor(muta[,grp2]))

cat("\nloadings and summaries: groups 1a, 1b, 2 to 5\n")
print(summary(muta[,grp1a])); print(pca1a$loadings[,c(1:2)])
print(summary(muta[,grp1b])); print(pca1b$loadings[,1:2])
print(summary(muta[,grp2]));  print(pca2$loadings[,1:3])
print(summary(muta[,grp3]));  print(pca3$loadings[,1:3])
print(summary(muta[,grp4]));  print(pca4$loadings[,1:6])
print(summary(muta[,grp5]));  print(pca5$loadings[,1:4])

# new data frame
pca1adf=data.frame(pca1a$scores[,1:2]) 
names(pca1adf)=paste("grp1a",1:2,sep="")
pca1bdf=data.frame(pca1b$scores[,1:3]) 
names(pca1bdf)=paste("grp1b",1:3,sep="")
pca2df=data.frame(pca2$scores[,1:3])
names(pca2df)=paste("grp2",1:3,sep="")
pca3df=data.frame(pca3$scores[,1:5]) # or 6? 5? 3?
names(pca3df)=paste("grp3",1:5,sep="")
pca5df=data.frame(pca5$scores[,1:7]) # 7 is better than 6 or 8
names(pca5df)=paste("grp5",1:7,sep="")
newmut=cbind(muta[,c(1,grpr,grp4)],pca1adf,pca1bdf,pca2df,pca3df,pca5df) 
cat("number of explanatory:", ncol(newmut)-1,"\n") # 28

tem=cor(newmut); print(max(tem[tem<1]))  # .95 = tem['grp21','grp31']
#hist(tem[tem<1])  # very few large correlations

fitpca=glm(y~.,data=newmut,family="binomial")
print(summary(fitpca))
#    Null deviance: 2574.6  on 1858  degrees of freedom
#Residual deviance: 1864.0  on 1830  degrees of freedom
#AIC: 1922
# full model AIC 1916

predpca=predict(fitpca,type="response")
plot(factor(newmut$y),predpca,xlab="y (not/is mutagenic)",
  ylab="estimated probability of mutagenicity") 
tabpca=table(newmut$y,(predpca>=0.5))
print(tabpca); print(tabpca/1859)
#    FALSE TRUE         FALSE   TRUE
#  0   825  139      0 0.4438 0.0748
#  1   299  596      1 0.1608 0.3206

# Hosmer-Lemeshow calibration checks
hist(predpca); 
prcat=cut(predpca,breaks=seq(0,1,.1))
HLsumm=tapply(newmut$y,prcat,mean);  print(HLsumm)
# (0,0.1] (0.1,0.2] (0.2,0.3] (0.3,0.4] (0.4,0.5] (0.5,0.6] (0.6,0.7] (0.7,0.8] 
#  0.1299    0.0964    0.2315    0.3777    0.4444    0.5400    0.6842    0.7981 
#(0.8,0.9]   (0.9,1] 
#   0.8896    0.9291 

# out-of-sample
set.seed(321); iperm=sample(1859,1859)
train=newmut[iperm[1:1000],]; holdout=newmut[iperm[1001:1859],]
fit.train=glm(y~.,family="binomial",data=train)
predpca.hold=predict(fit.train,newdata=holdout,type="response")
tabpcah=table(holdout$y, predpca.hold>=0.5)
print(tabpcah); print(tabpcah/859)
#    FALSE TRUE        FALSE  TRUE
#  0   370   87      0 0.431 0.101
#  1   132  270      1 0.154 0.314
