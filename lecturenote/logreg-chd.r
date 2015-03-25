# Binary regression, response variable Y is binary (possible values 0 or 1)
# Model P(Y=1|xvec)=p(xvec)
# p(xvec)=exp(b0+beta%*%xvec)/[1+exp(b0+beta%*%xvec)]
# Example: coronary heart disease as a function of age, 
# source is Hosmer and Lemeshow, Applied Logistic Regression, 2nd ed.
# 100 subjects in a scientific study, 
# relevant population might be those with some heart disease.
chd=read.table("/Users/apple/Desktop/STAT306/lecturenote/chdage.dat.txt",head=T) #nrow=100
names(chd) # id age ichd; ichd=1 if significant coronary heart disease
chd=chd[,-1]  # omit index=id
print(summary(chd))
#      age             ichd     
# Min.   :20.00   Min.   :0.00  
# 1st Qu.:34.75   1st Qu.:0.00  
# Median :44.00   Median :0.00  
# Mean   :44.38   Mean   :0.43  
# 3rd Qu.:55.00   3rd Qu.:1.00  
# Max.   :69.00   Max.   :1.00

# correlation is useful for binary response but not scatterplot
print(cor(chd$age,chd$ichd))  # 0.5137987
# categorize to check for trend in proportion 
chd$agecateg=cut(chd$age,breaks=c(19.5,29.5,34.5,39.5,44.5,49.5,54.5,59.5,69.5))
attach(chd)
print(table(agecateg))
#(19.5,29.5] (29.5,34.5] (34.5,39.5] (39.5,44.5] (44.5,49.5] (49.5,54.5] 
#         10          15          12          15          13           8 
#(54.5,59.5] (59.5,69.5] 
#         17          10 

print(table(agecateg,ichd))
#             ichd
#agecateg       0  1
#  (19.5,29.5]  9  1
#  (29.5,34.5] 13  2
#  (34.5,39.5]  9  3
#  (39.5,44.5] 10  5
#  (44.5,49.5]  7  6
#  (49.5,54.5]  3  5
#  (54.5,59.5]  4 13
#  (59.5,69.5]  2  8

mnprop=tapply(ichd,agecateg,mean); print(mnprop)
#(19.5,29.5] (29.5,34.5] (34.5,39.5] (39.5,44.5] (44.5,49.5] (49.5,54.5] 
#  0.1000000   0.1333333   0.2500000   0.3333333   0.4615385   0.6250000 
#(54.5,59.5] (59.5,69.5] 
#  0.7647059   0.8000000 

agemid=c(25,32.5,37.5,42.5,47.5,52.5,57.5,65)
par(mar=c(0.1,0.1,0.1,0.1))
plot(agemid,mnprop)  # curve is sigmoid shape

# binary or logistic regression 
fit=glm(ichd~age,family="binomial")
print(summary(fit))
#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -5.30945    1.13365  -4.683 2.82e-06 ***
#age          0.11092    0.02406   4.610 4.02e-06 ***

#    Null deviance: 136.66  on 99  degrees of freedom
#Residual deviance: 107.35  on 98  degrees of freedom
#AIC: 111.35

# ============================================================
# explanation of outputted quantities
ysum=sum(ichd) # number of 1s
n=nrow(chd); phat=ysum/n
# binomial probability assuming probability(CHD;age)=constant
loglik0=ysum*log(phat)+(n-ysum)*log(1-phat)  # log binomial coef omitted
print(-2*loglik0) #  nulldev=136.663 (df=n-1 for 1-parameter model)
bhat=fit$coef; tem=bhat[1]+bhat[2]*age
loglik1=sum(ichd*tem) - sum(log(1+exp(tem)))
print(-2*loglik1) #  107.3531: residual deviance is -2*log-likelihood
#                    df=n-2 because there are 2 parameters beta0,beta1
npar=2; aic=-2*loglik1+2*npar
print(aic) # 111.3531: Akaike information criterion=-2*loglik+2*(#parameters)

# predicted probabilies
predpr=predict(fit,type="response")
# same as exp(tem)/(1+exp(tem))=1/(1+exp(-tem)); tem=bhat[1]+bhat[2]*age
print(predpr-1/(1+exp(-tem))) # zeros up to roundoff
# standard errors
Xmat=model.matrix(fit) # nx2 matrix with first column of 1s
hess=matrix(0,2,2)
# matrix of second order derivatives of negative log-likelihood at MLE
for(i in 1:n)
{ hess=hess+predpr[i]*(1-predpr[i])*outer(Xmat[i,],Xmat[i,]) }
invhess=solve(hess); print(invhess)
#            (Intercept)           age
#(Intercept)  1.28517284 -0.0266770195
#age         -0.02667702  0.0005788757
print(sqrt(diag(invhess)))
#  1.13365464  0.02405984  , the standard errors of the betahats
# The standard errors come from the sqrt of diagonal of the
#  inverse Hessian of the negative log-likelihood evaluated at 
#  maximum likelihood estimate.

# ============================================================
options(digits=4)
# For medical health studies, there are usually also other risk factors
# predicted probability of CHD at ages 42, 44, 46, 48, 50 (based on these data)
x<-NULL; x$age=seq(42,50,2)
predprfn=predict(fit,newdata=x,type="response")
print(predprfn)
#     1      2      3      4      5 
#0.3428 0.3944 0.4484 0.5037 0.5589
# logistic curve is increasing quite fast in this age interval
# ============================================================

# Hosmer-Lemeshow calibration check
prcateg=cut(predpr,breaks=seq(0,1,.2))
print(table(prcateg))
#  (0,0.2] (0.2,0.4] (0.4,0.6] (0.6,0.8]   (0.8,1] 
#       27        25        16        24         8 

HLsummary=tapply(ichd,prcateg,mean)
# prop of observed 1s with the logistic predicted prob in different intervals
print(HLsummary)
#  (0,0.2] (0.2,0.4] (0.4,0.6] (0.6,0.8]   (0.8,1] 
#   0.1111    0.3200    0.4375    0.7500    0.8750
# calibration is OK
detach(chd)
