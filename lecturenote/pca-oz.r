# principal component analysis of word frequency data for Oz books

baum=matrix(scan("baum.dat.txt"),ncol=50)
thom=matrix(scan("thom.dat.txt"),ncol=50)
rb=matrix(scan("rb.dat.txt"),ncol=50)
baum=data.frame(baum); thom=data.frame(thom); rb=data.frame(rb)
fnwords=c("the","and","to","a/an","of","in","that/ose","it","not","as",
"with","but","for","at","this/ese","so","all","on","from","one/s",
"up","no","out","what","then","if","there","by","who","when",
"into","now","down","over","back","or","well","which","how","here",
"just","very","where","before","upon","about","after","more","why","some")
names(baum)=fnwords; names(thom)=fnwords; names(rb)=fnwords
n1=nrow(baum); n2=nrow(thom)
dat=rbind(baum,thom)
grp=c(rep(1,n1),rep(2,n2))
grp3=c(grp,rep(3,nrow(rb)))
sym=c(rep("o",n1),rep("+",n2))
sym3=c(sym,rep("#",nrow(rb)))

# use sample covariance and not correlation matrix because all variables
#   have the same unit and the magnitude of the frequencies does matter
pca=princomp(dat,cor=FALSE,scores=T)    # not cor=T, 
par(mfrow=c(3,2))
plot(pca$scores[,1:2],type="n",ylim=c(-80,80))
text(pca$scores[,1:2],label=sym)
# print(pca$scores[1:10,1:2])
pred.rb=predict(pca,newdata=rb)
tem.rb=rbind(pca$scores,pred.rb)
plot(tem.rb,type="n",xlab="PC1",ylab="PC2",ylim=c(-80,80))
text(tem.rb,label=sym3)
legend("topright",legend=c("o:baum","+:thompson","#:royal"))

options(digits=3)
cat("The std deviations of the principal components\n")
print(summary(pca))  
# the standard deviations of the principal components (i.e.,
# the square roots of the eigenvalues of the
# covariance/correlation matrix
# [1] 45.813 27.252 23.333 17.454 16.021 12.750  9.978  9.206  8.706  8.577
#[11]  8.150  7.608  7.330  7.002  6.853  5.921  5.796  5.436  5.271  5.053
#[21]  4.826  4.714  4.473  4.315  4.248  4.061  3.986  3.822  3.611  3.509
#[31]  3.260  3.213  3.125  2.943  2.849  2.828  2.531  2.395  2.331  2.258
#[41]  2.135  1.936  1.919  1.767  1.702  1.652  1.442  1.269  1.044  0.922

# print the loadings/eigenvectors
load2=pca$loadings[,1:2]  # first two loadings
cat("\nThe loadings for the first two principal components\n")
print(load2)
#            Comp.1   Comp.2
#the       0.926429 -0.32331
#and      -0.018541 -0.04996
#to        0.166984  0.48439
#a/an     -0.215826 -0.39216
# ...

plot(load2,type="n",xlab="loading1",ylab="loading2")
text(load2,label=fnwords,cex=0.75)
# this is different from Binongo's article
plot(factor(sym),dat[,1],ylab="frequency of the")
plot(factor(sym),dat[,3],ylab="frequency of to")
plot(dat[,c(1,3)],type="n",xlab="frequency of the",ylab="frequency of to")
text(dat[,c(1,3)],label=sym)
