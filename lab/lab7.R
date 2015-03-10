###STAT306 Lab 7 
### Cross-validation. 

dat <- read.table("/Users/apple/Desktop/STAT306/lab/richmondcondo.txt", header=T, skip=2)
dat <- dat[,-c(1, 10)]

###Part 1 Leave-one-out CV
source("/Users/apple/Desktop/STAT306/lab/ls.cvrmse.R")
#This file is also provided by Prof. Welch. 
#It includes a function to calculate the leave-one-out CV RMSE

#Compare the full model and best model found by regsubsets
fullModel <- lm(askprice~., data=dat)
summary(fullModel)

selModel <- lm(askprice~ffarea+ beds+ view+age+mfee, data=dat)
#Recall that this is the best model based on Mallow's cp
summary(selModel)

#Calculate the leave-one-out CV RMSE for the full model
fullModel.cvrmse <- ls.cvrmse(fullModel)

#Calculate the leave-one-out CV RMSE for the selected model
selModel.cvrmse <- ls.cvrmse(selModel)

print(c(fullModel.cvrmse, selModel.cvrmse))
#The selected model has smaller cvrmse

###Part 2 Two-fold CV
n <- nrow(dat)

set.seed(1)  # set a seed for replicability
# here select half of the data randomly. 
id.sub1 <- sort(sample(1:n, round(n/2), replace = FALSE))

#Create the training and hold-out dat set 
dat.sub1 <- dat[id.sub1,]  # training
dat.sub2 <- dat[-id.sub1,] # holdout

fullModel.sub1 <- lm(askprice~., data = dat.sub1)

#Make predictions at the hold-out data set.
fullModel.pred1 <- predict(fullModel.sub1, dat.sub2)
fullModel.err1 <- sqrt(sum((dat.sub2$askprice - fullModel.pred1)^2)/length(fullModel.pred1))
fullModel.err1

#Compare to the selected model based on regsubsets
selModel.sub1 <- lm(askprice~ffarea+ beds+ view+age+mfee, data = dat.sub1)
selModel.pred1 <- predict(selModel.sub1, dat.sub2)
selModel.err1 <- sqrt(sum((dat.sub2$askprice - selModel.pred1)^2)/length(selModel.pred1))
selModel.err1
#Much smaller than that from the full model.


## reverse training and hold-out data ##
#dat.sub2 is now training, dat.sub1 is holdout

fullModel.sub2 <- lm(askprice~., data = dat.sub2)

#Make predictions at the hold-out data set.
fullModel.pred2 <- predict(fullModel.sub2, dat.sub1)
fullModel.err2 <- sqrt(sum((dat.sub1$askprice - fullModel.pred2)^2)/length(fullModel.pred2))
fullModel.err2

## average prediction error for the full model ##
(fullModel.err1  + fullModel.err2)/2

#For the selected model based o nregsubsets
selModel.sub2 <- lm(askprice~ffarea+ beds+ view+age+mfee, data = dat.sub2)
selModel.pred2 <- predict(selModel.sub2, dat.sub1)
selModel.err2 <- sqrt(sum((dat.sub1$askprice - selModel.pred2)^2)/length(selModel.pred2))
selModel.err2

## average prediction error ##
(selModel.err1  + selModel.err2)/2


#The lab7 quiz
library(MASS)
dat = cpus[,-c(1,9)]
names(dat)
fullModel = lm(perf~.,dat)
ls.cvrmse(fullModel)
set.seed(306)
n = nrow(dat)
n
id.sub1 <- sort(sample(1:n, round(n/2), replace = FALSE))
dat.train <- dat[id.sub1,]  
dat.holdout <- dat[-id.sub1,]
#What is the prediction error of the full model trained based on the 
#training data set on the hold-out data set?

fullModel.train <- lm(perf~., data = dat.train)
pred.holdout = predict(fullModel.train, dat.holdout)

pred.err = sqrt(sum((dat.holdout$perf - pred.holdout)^2)/length(pred.holdout))





