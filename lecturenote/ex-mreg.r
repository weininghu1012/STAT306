# Multiple regression equations and diagnostics: Burnaby condo data
# Initial plots, regression with different subsets, residual plots

b=read.table("burnabycondo.txt",header=T,skip=2) # data frame
print(names(b))
# [1] "MLS"      "askprice" "ffarea"   "beds"     "baths"    "floor"   
# [7] "view"     "age"      "mfee"     "region"  
b=b[,2:9] # exclude non-quantitative variables
b$askprice=b$askprice/10000   # scale some variables
b$ffarea=b$ffarea/100
b$mfee=b$mfee/10
b$sqfl=sqrt(b$floor)
attach(b)   # attach data frame so that variables can be access without $
options(digits=3)
print(cor(b)) # correlation matrix of quantitative variables
par(mfrow=c(3,3))  # 3x3 grid of subplots
# some pairwise plots (scatterplots or side-by-side boxplots)
plot(ffarea,askprice,xlab="floorarea sqft/100",ylab="ask price/10000")
plot(factor(beds),askprice,xlab="#bedrooms",ylab="ask price/10000")
plot(factor(baths),askprice,xlab="#bathrooms",ylab="ask price/10000")
plot(sqfl,askprice)
plot(factor(view),askprice,xlab="view",ylab="ask price/10000")
plot(age,askprice)
plot(mfee,askprice,xlab="mon mgt fee/10")
plot(ffarea,mfee)
plot(factor(beds),ffarea,xlab="#bedrooms",ylab="floorarea")

options(digits=6)
bur7=lm(askprice~ ffarea+beds+baths+sqfl+view+age+mfee)
summ7=summary(bur7); sigma7=summ7$sigma  # estimated residual SD
# see  Appendix, Section 7, page 13, for explanations/definitions
# of quantities in the regression output
print(summ7)
pred=predict(bur7) # predicted values from regression equation 
res=resid(bur7)    # residuals
# residual plots
par(mfrow=c(2,2)) # 2x2 grid of subplots
qqnorm(res,main="normal Q-Q plot of residuals")
plot(pred,res,xlab="predicted value",ylab="residual")
abline(h=2*sigma7); abline(h=-2*sigma7)
title("const spread or pattern as yhat increases?",cex.main=0.8)
plot(ffarea,res,xlab="finished floor area/100",ylab="residual")
abline(h=2*sigma7); abline(h=-2*sigma7)
title("const spread or pattern as ffarea increases?",cex.main=0.8)
plot(sqfl,res,xlab="sqrt floor",ylab="residual")
abline(h=2*sigma7); abline(h=-2*sigma7)
title("const spread or pattern as sqfl increases?",cex.main=0.8)

# subsets of explanatory variables (models to be compared)
bur2=lm(askprice~ ffarea+sqfl)
print(summary(bur2))

bur3=lm(askprice~ ffarea+sqfl+age)
print(summary(bur3))

bur4=lm(askprice~ ffarea+sqfl+age+beds)
print(summary(bur4))

detach(b)
