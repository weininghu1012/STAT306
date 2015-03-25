# STAT306 Lab9 
# Logistic Regression
# Continue with the low birth weight data in Lab 8.

#Read in the data
wt <- read.table("/Users/apple/Desktop/STAT306/lab/bwt.txt", header=T)
wt$race <- as.factor(wt$race)
wt$ftv <- as.factor(wt$ftv)

#Get the two logistic models. 
# The first model contains all the responsive variable
fit1=glm(low~., data=wt, family="binomial")
summary(fit1)
# The second model contains variables : 
fit2<-glm(low~lwt+race+smoke+ht+ptd, data=wt, family="binomial")
summary(fit2)

#Extract the AIC from above models.
fit1$aic
fit2$aic
#Smaller AIC is better.

##Two-fold cross-validation for out-of-sample misclassification rate.

set.seed(1)
ind <- sample(1:189, 90) #randomly generate 90 numbers from 1 to 189
wt.sub1 <- wt[-ind, ]
wt.sub2 <- wt[ind, ]


# wt.sub1 is used for training
fit1.sub1 <- glm(low~., data=wt.sub1, family="binomial")

#wt.sub2 is used for testing
pred1.prob1 <- predict(fit1.sub1, newdata=wt.sub2, type="response")

pt11.5 <- table(wt.sub2$low, as.numeric(pred1.prob1>0.5))
pt11.5
#Misclassification rate in the hold-out set.
(pt11.5[2,1] + pt11.5[1,2])/nrow(wt.sub2) 

#What will happen if we change the cut-off probability to 0.3?
pt11.3 <- table(wt.sub2$low, as.numeric(pred1.prob1>0.3))
pt11.3
#Misclassifications of the 0 (no low birth weight) cases increase while they decrease in the 1 cases. 
(pt11.3[2,1] + pt11.3[1,2])/nrow(wt.sub2) 

#If we increase the cut-off point to 0.7
pt11.7 <- table(wt.sub2$low, as.numeric(pred1.prob1>0.7))
pt11.7
(pt11.7[2,1] + pt11.7[1,2])/nrow(wt.sub2) 

#However, we do not use the misclassification rate to choose the cut-off point. 
#The cut-off point can be chosen based on the losses (costs) of misclassifications (at each case),
# which will not be covered in this course.


#Similarly analysis for fit2.  
fit2.sub1 <- glm(low~lwt+race+smoke+ht+ptd, data=wt.sub1, family="binomial")
pred2.prob1 <- predict(fit2.sub1, newdata=wt.sub2, type="response")
pt21.5 <- table(wt.sub2$low, as.numeric(pred2.prob1>0.5))
pt21.5

(pt21.5[2,1] + pt21.5[1,2])/nrow(wt.sub2) 

pt21.3 <- table(wt.sub2$low, as.numeric(pred2.prob1>0.3))
pt21.3
(pt21.3[2,1] + pt21.3[1,2])/nrow(wt.sub2) 

#############################################################
##Save and load objects in R ###############################
##For the term project, it might be useful to save some objects, like the output from lm, glm 
## and return to analyze them later or share them with teammates.

##For example, the following code will save everything in your workspace into the "Lab9.RData" file.
save.image("Lab9.RData") 
##You can find the "Lab9.RData" under your working directory.
##Now you can close this R section and open a new one
load("Lab9.RData")
ls()
##all the objects we created above, "fit1", "fit2", "pt11.5"....

##You will get all the variables (the whole working space) back.
##Notice that the save.image (respectively load) function by default only 
##saves file to (respectively load files from) the current working directory
##(Similar to read.table and write.table)
##You may need to provide the full path when loading, e.g. 
##load("E:/STAT306/Lab9.RData")
##Or 
##load("Documents/Lab9.RData")

##You can also only save the objects you need for future analysis. 
save(fit1, fit2, wt, file="Lab9-InUse.RData")
##Open a new R session or clear the current one by 
rm(list=ls())
##Load the saved objects back
load("Lab9-InUse.RData")
#See what is now included in the working space.
ls()
#Only "fit1" "fit2" "wt"

##You need to be careful when loading an ".RData" into a non-empty working space.
##The two work spaces should not have any variables of the same name.
##If you have a variable in the ".RData" of the same name as a variable in your current working space
##The current variable will be overwritten (replaced) by the one from the ".RData" you load.
##R will not give you any warning about this.
##An example is included in the Lab9 quiz.


#lab9 quiz
nb = read.table("/Users/apple/Desktop/STAT306/lab/newbie.txt", header=T)
head(nb)
nb1 = nb[1:1200,]
nb2 = nb[1201:1500,]
# fit the model with all responsive variables
fit1 = glm(Newbie~.,data = nb1, family = "binomial")
summary(fit1)

# Two fold cross-validation

# get the training one
fit.sub1 = glm(Newbie~.,data = nb1, family = "binomial")
# testing the model
predict.prob1 = predict(fit.sub1, newdata = nb2, type = "response")


#Classify a case as Newbie if the predicted probability exceeds 0.6 
# and otherwise classify it as a non-Newbie

pt = table(nb2$Newbie, as.numeric(predict.prob1>0.6))

# get the misclassification rate 
pt
rate = (pt[1,2]+pt[2,1])/nrow(nb2)
rate
nrow(nb2)
names(fit1$coefficients)
#Q1: 31
#Q2: 0.2483333

a = pi 
save.image("Lab9quiz.RData") 
a = 3 
load("Lab9quiz.RData") 

a
#Q3: 3.141593

