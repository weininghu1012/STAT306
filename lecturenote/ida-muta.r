
# descriptive statistics for mutagenicity data (Sections 7.4.1, 7.9)
muta=read.table("mutagenicity.txt",header=T)
names(muta)
#Case y   AMW   MW     Sv    Se    Sp    Ss     Mv   Me   Mp   Ms   nAT  nSK 
# nBT nBO nBM nDB nTB nAB nC  nN  nO  nP  nS  nF  nCL nBR nX  nCO  nNO nNO2 
# nSO2 nSO3 nPO nCS nHA nCIC nCIR nR03 nR04 nR05 nR06 nR07  nR08 nR09 nR10
options(digits=3)
print(cor(muta$y,muta[,3:47]))
#        AMW     MW    Sv    Se     Sp     Ss      Mv     Me      Mp     Ms
#[1,] 0.0056 0.0755 0.101 0.115 0.0918 0.0715 -0.0681 0.0348 -0.0724 -0.138
#       nAT   nSK   nBT   nBO   nBM   nDB     nTB   nAB     nC    nN     nO
#[1,] 0.114 0.113 0.143 0.143 0.221 0.212 -0.0633 0.176 0.0902 0.192 0.0748
#          nP      nS      nF     nCL     nBR      nX     nCO   nNO  nNO2
#[1,] -0.0459 -0.0188 -0.0471 -0.0893 -0.0153 -0.0946 -0.0705 0.241 0.199
#        nSO2   nSO3     nPO     nCS   nHA  nCIC  nCIR  nR03   nR04   nR05  nR06
#[1,] -0.0918 0.0358 -0.0388 -0.0429 0.147 0.255 0.263 0.159 0.0111 0.0799 0.215
#       nR07   nR08   nR09  nR10
#[1,] 0.0113 0.0330 0.0829 0.273
# largest correlations with muta$y are the following.
# nBM 0.221
# nDB 0.212
# nNO 0.241
# nCIC 0.255
# nCIR 0.263
# nR10 0.273
par(mfrow=c(3,3))
attach(muta)
plot(factor(y),nR10,ylab="nR10") # lots of 0s when y=0
title("y=1 mutagenic")
plot(factor(y),nCIR,ylab="nCIR") 
plot(factor(y),nCIC,ylab="nCIC")
plot(factor(y),nDB,ylab="nDB")
plot(factor(y),nBM,ylab="nBM")
nn=length(y)
ysym=rep("o",nn)
ysym[y==1]<-"+"
set.seed(123)
jCIR=nCIR+runif(nn,0,.5)
jCIC=nCIC+runif(nn,0,.5)
ii=sample(nn,100)
plot(jCIR[ii],jCIC[ii],type="n")
text(jCIR[ii],jCIC[ii],label=ysym[ii])
title("random 100 records, jittered")
jDB=nDB+runif(nn,0,.5)
jBM=nBM+runif(nn,0,.5)
plot(jDB[ii],jBM[ii],type="n")
text(jDB[ii],jBM[ii],label=ysym[ii])
title("o for y=0, + for y=1")
plot(jDB[ii],jCIR[ii],type="n")
text(jDB[ii],jCIR[ii],label=ysym[ii])
title("random 100 records")
plot(jDB[ii],jCIC[ii],type="n")
text(jDB[ii],jCIC[ii],label=ysym[ii])
title("jittered")
# cannot discriminate on low dimensions
