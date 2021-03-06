#webwork 4
#question1
askpr=c(33.7, 40.9, 47.8, 74.8, 53.9, 78.8, 54.8, 41.99, 57.8, 73.8, 56.8, 48.5, 46.8, 25.9, 73.9, 60.8, 58.68, 65.99, 56.88, 54.98, 51.99, 58.8, 65.8, 40.8, 50.8, 62.9, 68.5, 86.8, 48.8, 53.8, 59.8, 57.5, 55.2, 26.99, 68.8, 44.8, 50.5, 79.8, 54.8, 40.8, 57.8, 51.68, 77.8, 49.9, 79.99, 68.8, 62.8888, 52.4, 71.99, 58.39) 
#The explanatory variables are: 
#(i) finished floor area divided by 100 
ffarea=c(12, 16.06, 13.34, 17.48, 11.84, 19.48, 11.26, 12.9, 12.01, 17.54, 15.5, 14.8, 16.2, 6.1, 15.15, 13.2, 13.96, 22.78, 15.78, 13.06, 12.09, 17.37, 13.45, 14, 16.6, 14, 15.76, 15.08, 14.8, 10.95, 17.63, 13.46, 15.3, 10.5, 15.95, 9.4, 12.26, 15.25, 15.46, 12.26, 13.84, 15.1, 16.5, 15.6, 22, 16.9, 15.77, 16.22, 15.05, 15.09) 
#(ii) age 
age=c(28, 25, 32, 5, 15, 11, 0, 44, 0, 9, 23, 24, 30, 11, 0, 3, 9, 35, 17, 1, 7, 26, 1, 38, 23, 5, 4, 1, 50, 18, 26, 10, 9, 37, 18, 14, 3, 3, 41, 29, 10, 20, 3, 20, 20, 8, 6, 25, 8, 8) 
#(iii) monthly maintenance fee divided by 10 
mfee=c(25.9, 24.4, 24.5, 29.7, 21, 20.4, 24.8, 23.2, 14.2, 18.2, 17.4, 16.1, 16, 17.1, 22.2, 18.9, 22, 57.4, 17.3, 19.6, 18.1, 31, 18.2, 23, 19.9, 19.6, 22.1, 48.8, 25, 24.7, 32, 22.1, 16.9, 28, 23.6, 23.3, 18, 35, 31, 19.8, 16, 24.5, 25.4, 27, 26.7, 19.4, 35.7, 36.4, 22.3, 20.3) 
#(iv) number of bedrooms 
beds=c(2, 2, 3, 4, 2, 3, 2, 3, 3, 4, 3, 3, 
       4, 1, 4, 3, 3, 2, 4, 3, 3, 3, 3, 3, 
       4, 3, 4, 3, 3, 2, 5, 3, 3, 2, 3, 2, 
       3, 2, 3, 3, 3, 3, 4, 3, 3, 4, 3, 3, 3, 4) 
#(v) number of bathrooms 
baths=c(2.5, 3.5, 3.5, 4.5, 2.5, 3.5, 3.5, 2.5, 3.5, 3.5, 3.5, 3.5, 4.5, 1.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 2.5, 3.5, 3.5, 3.5, 3.5, 2.5, 2.5, 2.5, 3.5, 3.5, 1.5, 3.5, 2.5, 3.5, 3.5, 3.5, 2.5, 3.5, 2.5, 4.5, 3.5, 4.5, 4.5, 3.5, 2.5, 3.5, 3.5) 
richmondtownh=data.frame(cbind(askpr,ffarea,age,mfee,beds,baths)) 
install.packages("leaps")
library(leaps)
r1 = regsubsets(askpr~.,data = richmondtownh,method="exhaustive")
rr1 = summary(r1)
#   Selection Algorithm: exhaustive
#         ffarea age mfee beds baths
# 1  ( 1 ) " "    " " " "  " "  "*"  
# 2  ( 1 ) "*"    "*" " "  " "  " "  
# 3  ( 1 ) "*"    "*" "*"  " "  " "  
# 4  ( 1 ) "*"    "*" "*"  "*"  " "  
# 5  ( 1 ) "*"    "*" "*"  "*"  "*"  
#The values of adjusted R2 for the best models with 2, 3 and 4
#explanatory variables are respectively:
r2 = lm(askpr~ffarea+age,data = richmondtownh)
r3 = lm(askpr~ffarea+age+mfee, data = richmondtownh)
r4 = lm(askpr~ffarea+age+mfee+beds, data = richmondtownh)
summary(r2)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 19.96254    5.00610   3.988 0.000232 ***
#   ffarea       3.24601    0.33470   9.698 8.53e-13 ***
#   age         -0.63200    0.07345  -8.604 3.24e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.662 on 47 degrees of freedom
# Multiple R-squared:  0.7618,  Adjusted R-squared:  0.7517 
# F-statistic: 75.16 on 2 and 47 DF,  p-value: 2.28e-15
summary(r3)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  18.4434     4.9345   3.738 0.000512 ***
#   ffarea        2.9730     0.3555   8.362 8.70e-11 ***
#   age          -0.6565     0.0726  -9.043 9.01e-12 ***
#   mfee          0.2461     0.1287   1.913 0.061998 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.481 on 46 degrees of freedom
# Multiple R-squared:  0.7794,  Adjusted R-squared:  0.765 
# F-statistic: 54.16 on 3 and 46 DF,  p-value: 3.923e-15
summary(r4)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 15.64085    5.46843   2.860   0.0064 ** 
#   ffarea       2.63591    0.45664   5.772 6.79e-07 ***
#   age         -0.64497    0.07298  -8.837 2.16e-11 ***
#   mfee         0.32110    0.14331   2.241   0.0300 *  
#   beds         1.90628    1.63059   1.169   0.2485    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.456 on 45 degrees of freedom
# Multiple R-squared:  0.7859,  Adjusted R-squared:  0.7668 
# F-statistic: 41.29 on 4 and 45 DF,  p-value: 1.63e-14
rr1$cp
rr1$adjr2

#Question2
askpr=c(45.99, 81.9, 56.8, 56.88, 47.8, 60.8, 62.9, 54.8, 51.68, 53.9, 68.8, 47.9, 62.8888, 68.5, 53.8, 25.9, 79.8, 49.9, 54.98, 48.5, 48.8, 55.8, 55.2, 65.99, 71.99, 57.5, 51.99, 78.8, 54.8, 61.5, 77.8, 68.5, 57.8, 68.8, 40.8, 58.68, 73.8, 57.8, 40.9, 44.8, 26.99, 53.8, 50.5, 33.7, 79.99, 40.8, 58.8, 52.4, 65.8, 74.8) 
#Explanatory variables
ffarea=c(16.01, 20.95, 15.5, 15.78, 13.34, 13.2, 14, 15.46, 15.1, 11.84, 15.95, 12.1, 15.77, 13.59, 10.95, 6.1, 15.25, 15.6, 13.06, 14.8, 14.8, 13.06, 15.3, 22.78, 15.05, 13.46, 12.09, 19.48, 11.26, 14.5, 16.5, 15.76, 13.84, 16.9, 14, 13.96, 17.54, 12.01, 16.06, 9.4, 10.5, 12.22, 12.26, 12, 22, 12.26, 17.37, 16.22, 13.45, 17.48) 
age=c(25, 19, 23, 17, 32, 3, 5, 41, 20, 15, 18, 7, 6, 2, 18, 11, 3, 20, 1, 24, 50, 0, 9, 35, 8, 10, 7, 11, 0, 7, 3, 4, 10, 8, 38, 9, 9, 0, 25, 14, 37, 9, 3, 28, 20, 29, 26, 25, 1, 5)
mfee=c(33.7, 34.8, 17.4, 17.3, 24.5, 18.9, 19.6, 31, 24.5, 21, 23.6, 18, 35.7, 17, 24.7, 17.1, 35, 27, 19.6, 16.1, 25, 18.6, 16.9, 57.4, 22.3, 22.1, 18.1, 20.4, 24.8, 18.7, 25.4, 22.1, 16, 19.4, 23, 22, 18.2, 14.2, 24.4, 23.3, 28, 18.5, 18, 25.9, 26.7, 19.8, 31, 36.4, 18.2, 29.7) 
beds=c(3, 1, 3, 4, 3, 3, 3, 3, 3, 2, 3, 3, 3, 3, 2, 1, 2, 3, 3, 3, 3, 3, 3, 2, 3, 3, 3, 3, 2, 3, 4, 4, 3, 4, 3, 3, 4, 3, 2, 2, 2, 3, 3, 2, 3, 3, 3, 3, 3, 4) 

richmondtownh=data.frame(cbind(askpr,ffarea,age,mfee,beds)) 

askpr.ho=c(86.8, 58.39, 50.8, 108.8, 59.8, 73.9, 41.99, 50.8, 46.8) 
ffarea.ho=c(15.08, 15.09, 12.27, 23.98, 17.63, 15.15, 12.9, 16.6, 16.2) 
age.ho=c(1, 8, 17, 16, 26, 0, 44, 23, 30) 
mfee.ho=c(48.8, 20.3, 25.2, 36.9, 32, 22.2, 23.2, 19.9, 16) 
beds.ho=c(3, 4, 2, 3, 5, 4, 3, 4, 4) 
holdout=data.frame(cbind(askpr.ho,ffarea.ho,age.ho,mfee.ho,beds.ho)) 
names(holdout)=names(richmondtownh) # to make variables names the same as before 

fit4 = lm(askpr~ffarea+age+mfee+beds)
fit3 = lm(askpr~ffarea+age+mfee)
summ3 = summary(fit3)
print(summ3)
# Call:
#   lm(formula = askpr ~ ffarea + age + mfee)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -15.5706  -4.3817  -0.0942   3.4386  13.4063 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 16.89003    4.39408   3.844  0.00037 ***
#   ffarea       3.45265    0.34466  10.018 3.85e-13 ***
#   age         -0.59688    0.07894  -7.561 1.32e-09 ***
#   mfee        -0.03882    0.14910  -0.260  0.79577    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.145 on 46 degrees of freedom
# Multiple R-squared:  0.7881,  Adjusted R-squared:  0.7742 
# F-statistic: 57.01 on 3 and 46 DF,  p-value: 1.563e-15
summ4 = summary(fit4)
print(summ4)
# lm(formula = askpr ~ ffarea + age + mfee + beds)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -15.3894  -4.4285  -0.0987   3.4245  13.4857 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 16.48458    5.70500   2.889  0.00592 ** 
#   ffarea       3.42805    0.41061   8.349 1.08e-10 ***
#   age         -0.59608    0.08011  -7.440 2.28e-09 ***
#   mfee        -0.02945    0.17195  -0.171  0.86480    
# beds         0.18542    1.63719   0.113  0.91033    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.212 on 45 degrees of freedom
# Multiple R-squared:  0.7881,  Adjusted R-squared:  0.7693 
# F-statistic: 41.85 on 4 and 45 DF,  p-value: 1.287e-14

q1 = c(6.145,6.212)
#compute cross validation RMS prediction error using ls.cvrmse
ls.cvrmse <- function(ls.out)
  # Compute cross-validated root mean squared error of prediction.
  # Handles missing values.
  # ls.out is a fitted regression model from lsreg or lm.
{
  res.cv <- ls.out$residuals / (1.0 - ls.diag(ls.out)$hat)
  
  # Identify NA's and remove them.
  is.na.res <- is.na(res.cv)
  res.cv <- res.cv[!is.na.res]
  
  cvrmse <- sqrt(sum(res.cv^2) / length(res.cv))
  return(cvrmse)
}

cvrmse3 = ls.cvrmse(fit3)
cvrmse4 = ls.cvrmse(fit4)
q2 = c(cvrmse3,cvrmse4)
q2 = c(6.574330,6.795677)


# training set/holdout set
set.seed(123)
train = richmondtownh
holdout = holdout
fit3train = lm(askpr~ffarea+age+mfee,data = train)
fit4train = lm(askpr~ffarea+age+mfee+beds,data = train)
pred3hold = predict(fit3train,new = holdout)
pred4hold = predict(fit4train,new = holdout)
rmse3hold = sqrt(mean((holdout$askpr-pred3hold)^2))
rmse4hold = sqrt(mean((holdout$askpr-pred4hold)^2))
cat("\nSize of holdout set is ", nrow(holdout),"\n") # 9
print(c(rmse3hold,rmse4hold))
q3 = c(10.94156,10.93020)
