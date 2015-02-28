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
dat <- read.table("richmondcondo.txt", header=T, skip=2)
#Or dat <- read.table("Downloads/richmondcondo.txt", header=T, skip=2)

#Load the backwards elimination function written by Prof. Welch
source("ls.back.elim.R.txt")
#source will put everything in the file into your R console 

#Have a look at the data
head(dat)
dat <- dat[,-c(1, 10)]
#The first column (id of the house) should be discarded
#For illustration of the method, we will not work with the dummy variable "region"
pairs(dat)

#Fit the model with all variables
fullModel <- lm(askprice~., data=dat)
summary(fullModel)

#Get the design matrix of the full linear model 
Xmat=model.matrix(fullModel)

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
#.....

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

#Forward stepwise
s3<- regsubsets(askprice~., data=dat, method="forward")
ss3 <- summary(s3)

#Are the results from these different methods the same?

