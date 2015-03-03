# burnaby condo: find best subsets (size 1,2,...) of explanatory variables 
# ffarea beds baths sqfl view age mfee 
options(digits=3)
b=read.table("burnabycondo.txt",header=T,skip=2)
b=b[,2:9]
b$askprice=b$askprice/10000
b$ffarea=b$ffarea/100
b$mfee=b$mfee/10
b$sqfl=sqrt(b$floor)
cat("Sample correlation matrix\n")
print(cor(b))
options(digits=4)

bur0=lm(askprice~ ffarea+beds+baths+sqfl+view+age+mfee,data=b)
cat("\nregression with all explanatory variables\n")
print(summary(bur0))

library(leaps)
#  leaps() performs an exhaustive search for the best subsets of the
#     variables in x for predicting y in linear regression, using an
#     efficient branch-and-bound algorithm. 
# Original source: Furnival and Wilson (1974), Regression by
#     leaps and bounds, Technometrics, v 16, pp 499-511.
# regsubsets               package:leaps      
#    Model selection by exhaustive search, forward or backward
#    stepwise, or sequential replacement

cat("\nexhaustive\n")
out.exh=regsubsets(askprice~ffarea+beds+baths+sqfl+view+age+mfee,data=b,nbest=1)
summ.exh=summary(out.exh)
#names(summ.exh)
#[1] "which"  "rsq"    "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"
print(summ.exh$outmat)
cat("Cp = SS(Res)/MS(Res:full) + 2*ncol(Xmat) - n: smaller is better\n")
print(summ.exh$cp)
cat("adjr: larger is better\n")
print(summ.exh$adjr)

cat("\nbackward\n")
out.back=regsubsets(askprice~ffarea+beds+baths+sqfl+view+age+mfee,data=b,method="backward")
summ.back=summary(out.back)
print(summ.back$outmat)
cat("Cp and adjr\n")
print(summ.back$cp)
print(summ.back$adjr)
# can be compared with function ls.back.elim() in coursepack

cat("\nforward\n")
out.forw=regsubsets(askprice~ffarea+beds+baths+sqfl+view+age+mfee,data=b,method="forward")
summ.forw=summary(out.forw)
print(summ.forw$outmat)
cat("Cp and adjr\n")
print(summ.forw$cp)
print(summ.forw$adjr)

cat("\nseqrep\n")
out.sequ=regsubsets(askprice~ffarea+beds+baths+sqfl+view+age+mfee,data=b,method="seqrep")
summ.sequ=summary(out.sequ)
print(summ.sequ$outmat)
cat("Cp and adjr\n")
print(summ.sequ$cp)
print(summ.sequ$adjr)
