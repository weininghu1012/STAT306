###STAT306 Lab 6 
###Variable Selection. 

#For today's lab. You will need the R package "leaps". 
#To install this package on your own computer, 
install.packages("leaps")
#Or click "Package -> Install packages"
# and then choose "leaps" from the long list of R packages.
#load this package
library(leaps)

#Download the "richmondcondo" data set from the course website.
#This data set investigates the relationship between the asking price of a condo in Richmond (askprice)
#with some other variables, like 
#square footage (ffarea)
#number of bedrooms (beds) and bath rooms (baths)
#which floor the condo is on (floor)
#view (1 for indication of scenic view, 0 otherwise)
#how old the house is in years (age)
#monthly maintenance fee (mfee)
#the neighbourhood (region)


#Read in the data set
dat <- read.table("/Users/apple/Desktop/STAT306/lab/richmondcondo.txt", header=T, skip=2)
#Or dat <- read.table("Downloads/richmondcondo.txt", header=T, skip=2)

#Load the backwards elimination function written by Prof. Welch
source("/Users/apple/Desktop/STAT306/lab/ls.back.elim.R")
#source will put everything in the file into your R console 

#Have a look at the data
head(dat)
dat
dat <- dat[,-c(1, 10)] # to delete some part
#The first column (id of the house) should be discarded
#For illustration of the method, we will not work with the dummy variable "region"
par("mar")
par(mar=c(0.0001,0.0001,0.0001,0.0001)) # question, why the margin is too large
pairs(dat)

#Fit the model with all variables
fullModel <- lm(askprice~., data=dat)
summary(fullModel)

#Get the design matrix of the full linear model 
Xmat=model.matrix(fullModel)
Xmat
# ls.back.elim will delete the least significant variable 
# from the design matrix until all the variables are significant.
# We do not consider deleting the intercept for ls.back.elim()

X1mat=ls.back.elim(Xmat[,-1],  dat$askprice)
#baths is deleted in this step

X2mat=ls.back.elim(X1mat, dat$askprice)
#floor is deleted

X3mat=ls.back.elim(X2mat,  dat$askprice)
#All the coefficients are significant now.

finalModel <- lm(askprice~ffarea+ beds+ view+age+mfee, data=dat)
summary(finalModel)

#Consider other criterions for model selection
library(leaps)

#regsubsets will perform an exhaustive search of the best model 
# with different number of variables, i.e.
# It will find the best model with one variable, best model with two variables, etc.
# You might be wondering which criterion is used? AIC, BIC, R^2, adj-R^2?
# Does the criteria matter when we are only comparing models with the same number of variables, 
# e.g. askprice~ffarea+beds vs askprice~ffarea+view?
 
s1<- regsubsets(askprice~., data=dat, method="exhaustive")
ss1 <- summary(s1)

ss1
#The main results from ss1 is this table 
#Selection Algorithm: exhaustive
#         ffarea beds baths floor view age mfee
#1  ( 1 ) " "    " "  " "   " "   " "  " " "*" Best model with one variable is askprice~mfee 
#2  ( 1 ) " "    " "  " "   " "   " "  "*" "*" Best model with two variables is askprice~mfee+age
#3  ( 1 ) "*"    " "  " "   " "   " "  "*" "*"
#4  ( 1 ) "*"    " "  " "   " "   "*"  "*" "*" 
#5  ( 1 ) "*"    "*"  " "   " "   "*"  "*" "*" 
#6  ( 1 ) "*"    "*"  " "   "*"   "*"  "*" "*" 
#7  ( 1 ) "*"    "*"  "*"   "*"   "*"  "*" "*" 

#This table can be accessed directly via 
ss1$which
#where "TRUE" is in the place of "*"

#The criteria matters when we compare models with different numbers of variables
ss1$adjr2
which.max(ss1$adjr2)
#Best model is with 6 variables according to adj-R^2

ss1$cp
which.min(ss1$cp)
#Best model is with 5 variables according to Cp criterion

#When the number of variables are large, "exhaustive" search can be time consuming
#You can consider backward or forward stepwise methods.

#Backward stepwise
s2<- regsubsets(askprice~., data=dat, method="backward")
ss2 <- summary(s2)
ss2
# Selection Algorithm: backward
# ffarea beds baths floor view age mfee
# 1  ( 1 ) " "    " "  " "   " "   " "  " " "*" 
# 2  ( 1 ) " "    " "  " "   " "   " "  "*" "*" 
# 3  ( 1 ) "*"    " "  " "   " "   " "  "*" "*" 
# 4  ( 1 ) "*"    " "  " "   " "   "*"  "*" "*" 
# 5  ( 1 ) "*"    "*"  " "   " "   "*"  "*" "*" 
# 6  ( 1 ) "*"    "*"  " "   "*"   "*"  "*" "*" 
# 7  ( 1 ) "*"    "*"  "*"   "*"   "*"  "*" "*" 
#Forward stepwise
s3<- regsubsets(askprice~., data=dat, method="forward")
ss3 <- summary(s3)
ss3

# Selection Algorithm: forward
# ffarea beds baths floor view age mfee
# 1  ( 1 ) " "    " "  " "   " "   " "  " " "*" 
# 2  ( 1 ) " "    " "  " "   " "   " "  "*" "*" 
# 3  ( 1 ) "*"    " "  " "   " "   " "  "*" "*" 
# 4  ( 1 ) "*"    " "  " "   " "   "*"  "*" "*" 
# 5  ( 1 ) "*"    "*"  " "   " "   "*"  "*" "*" 
# 6  ( 1 ) "*"    "*"  " "   "*"   "*"  "*" "*" 
# 7  ( 1 ) "*"    "*"  "*"   "*"   "*"  "*" "*" 
#Are the results from these different methods the same?

# for the quiz part
install.packages("MASS")
library(MASS)
dat = cpus[,-c(1,9)] #do not want the "estperf" and "name" variables
dat
# Take "perf" as the response variable and all the other variables in the "dat" are the covariates
# find the best model according to Mallow's CP criterion

pairs(dat)
# get an initial full model
fullmodel = lm(perf~.,data = dat)
summary(fullmodel)
#perform an exhaustive search for the best model
p1= regsubsets(perf~., data=dat, method="exhaustive")
pp1 = summary(p1)
pp1
pp1$cp
which.min(pp1$cp)
#best model is with 5 variables according to cp
cpfit = lm(perf~syct+mmin+mmax+cach+chmax, data = dat)
summary(cpfit)
# lm(formula = perf ~ syct + mmin + mmax + cach + chmax, data = dat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -193.39  -24.94    5.77   26.65  389.66 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -5.608e+01  8.007e+00  -7.004 3.58e-11 ***
#   syct         4.912e-02  1.746e-02   2.813  0.00539 ** 
#   mmin         1.518e-02  1.788e-03   8.490 4.34e-15 ***
#   mmax         5.561e-03  6.397e-04   8.694 1.18e-15 ***
#   cach         6.296e-01  1.344e-01   4.686 5.11e-06 ***
#   chmax        1.460e+00  2.076e-01   7.032 3.05e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 59.86 on 203 degrees of freedom
# Multiple R-squared:  0.8648,  Adjusted R-squared:  0.8615 
# F-statistic: 259.7 on 5 and 203 DF,  p-value: < 2.2e-16
