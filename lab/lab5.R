# STAT306 Lab 5 Dummy variable and variable transformations
# example to illustrate categorical explanatory variable with more than 2
#   categories, also to show transforms
mov=read.table("moviegross.dat.txt",header=T,skip=2)
names(mov)
# year movie studio openweekendgross gross  ST

# Goal is to relate the opening weekend gross with the "final" gross
#   for the US market, one other explanatory variable might be the
#   studio that produced the movie;
mov$lngross=log(mov$gross)
mov$lnopen=log(mov$openweekendgross)
attach(mov)
print(cor(openweekendgross,gross))  # 0.827
print(cor(lnopen,lngross))          # 0.867

# Regression on the original scale
par(mfrow=c(2,3), mar=c(4, 4, 1, 1))
plot(openweekendgross,gross,type="n")
text(openweekendgross,gross,label=ST) 
fit=lm(gross~openweekendgross+ST)
print(summary(fit))

#                 Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       71.5602    22.9819   3.114  0.00324 ** 
#openweekendgross   2.9113     0.2854  10.202 3.59e-13 ***
#STf              -42.3190    24.3780  -1.736  0.08957 .  
#STs              -53.7543    27.4693  -1.957  0.05673 .  
#STw              -27.8388    24.9629  -1.115  0.27082    
#Residual standard error: 63.26 on 44 degrees of freedom
#Multiple R-squared: 0.7149,     Adjusted R-squared: 0.689 

residSE=sqrt(sum(fit$resid^2)/fit$df.resid)
plot(openweekendgross,fit$resid)
plot(fit$fitted,fit$resid)
abline(h=2*residSE)
abline(h=-2*residSE)

##Fit the model to the log transformed data.
plot(lnopen,lngross,type="n")
text(lnopen,lngross,label=ST)  
fit2=lm(lngross~lnopen+ST)
print(summary(fit2))

#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.933926   0.279079   6.930 1.45e-08 ***
#lnopen       0.874742   0.069323  12.618 3.23e-16 ***
#STf         -0.278467   0.136243  -2.044   0.0470 *  
#STs         -0.330307   0.153144  -2.157   0.0365 *  
#STw         -0.009481   0.139467  -0.068   0.9461    
#Residual standard error: 0.3531 on 44 degrees of freedom
#Multiple R-Squared: 0.7929,     Adjusted R-squared: 0.7741 
#The log transformed model has large R square

residSE2=sqrt(sum(fit2$resid^2)/fit2$df.resid)
plot(lnopen,fit2$resid)
plot(fit2$fitted,fit2$resid, ylim=c(-1, 1))
abline(h=2*residSE2)
abline(h=-2*residSE2)
#Compare the first column of the plots, a linear model is better in the log transformed data.
#In the second column, the variance is more homogeneous in the transformed data. 
#For the third column, we have less outliers in the residual plots


# Dummy variables
print(table(studio))
#studio
#20thcenturyfox         disney           sony         warner 
#            14             13              9             13
print(table(ST))
# d  f  s  w 
#13 14  9 13 
# ST is the abbreviation of studio 
# It is easier to use some short names in the presentation of the regression result
# but this will not affect coefficients estimates. 

class(ST)
#ST is a "factor" in the data frame.
#Sometimes if your factor is coded in numbers, i.e. 1, 2, .., R will treat them as numbers. 
#you can force them into factors by ST <- as.factor(ST)

levels(ST)
# disney is the "baseline" studio in the regression below,
#  because d < f, s, w
# By default, R will order the factor levels alphabetically. 

M <- model.matrix(fit)
#Obtain the design matrix of our regression model 

#Check the design matrix of the dummy variable.
head(mov, 10)
head(M, 10)
#Or you can compare the whole matrix.

#Or
which(as.numeric(M[,3])!=0)
which(ST=="f")

#Compare the STf STs STw columns of model.matrix(fit) to ST in the mov data, 
#what do you find?
