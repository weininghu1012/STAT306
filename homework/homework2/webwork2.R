#webwork
#problem for richmond townhouse
askpr=c(62.8888, 62.9, 25.9, 68.5, 51.99, 68.5, 41.99, 50.8,
        55.2, 40.9, 49.9, 51.68, 77.8, 47.9, 74.8, 73.8, 68.8, 53.8, 78.8,
        65.8, 33.7, 79.8, 52.4, 55.8, 81.9, 53.8, 56.88, 47.8, 68.8, 58.39,
        40.8, 40.8, 57.8, 108.8, 59.8, 50.8, 71.99, 50.5, 46.8, 73.9, 53.9,
        61.5, 48.8, 79.99, 58.68, 26.99, 48.5, 60.8, 57.5, 45.99)
ffarea=c(15.77, 14, 6.1, 13.59, 12.09, 15.76, 12.9, 16.6, 15.3,
         16.06, 15.6, 15.1, 16.5, 12.1, 17.48, 17.54, 16.9, 12.22, 19.48,
         13.45, 12, 15.25, 16.22, 13.06, 20.95, 10.95, 15.78, 13.34,
         15.95, 15.09, 12.26, 14, 12.01, 23.98, 17.63, 12.27, 15.05,
         12.26, 16.2, 15.15, 11.84, 14.5, 14.8, 22, 13.96, 10.5, 14.8, 13.2,
         13.46, 16.01)
age=c(6, 5, 11, 2, 7, 4, 44, 23, 9, 25, 20, 20, 3, 7, 5, 9, 8, 9, 11,
      1, 28, 3, 25, 0, 19, 18, 17, 32, 18, 8, 29, 38, 0, 16, 26, 17, 8, 3,
      30, 0, 15, 7, 50, 20, 9, 37, 24, 3, 10, 25)
mfee=c(35.7, 19.6, 17.1, 17, 18.1, 22.1, 23.2, 19.9, 16.9, 24.4,
       27, 24.5, 25.4, 18, 29.7, 18.2, 19.4, 18.5, 20.4, 18.2, 25.9, 35,
       36.4, 18.6, 34.8, 24.7, 17.3, 24.5, 23.6, 20.3, 19.8, 23, 14.2,
       36.9, 32, 25.2, 22.3, 18, 16, 22.2, 21, 18.7, 25, 26.7, 22, 28,
       16.1, 18.9, 22.1, 33.7)
beds=c(3, 3, 1, 3, 3, 4, 3, 4, 3, 2, 3, 3, 4, 3, 4, 4, 4, 3, 3, 3, 2, 2,
       3, 3, 1, 2, 4, 3, 3, 4, 3, 3, 3, 3, 5, 2, 3, 3, 4, 4, 2, 3, 3, 3, 3, 2, 3,
       3, 3, 3)
#You are to make a prediction of the response variable when
#ffarea=17, age=10, mfee=29, beds=3
#create a dataframe
richmondtownh = data.frame(cbind(askpr,ffarea,age,mfee,beds))
#(i) 2 explanatory variables ffarea, age
fit1<- lm(askpr~ffarea+age)
print(summary(fit1))
# 
#   Call:
#   lm(formula = askpr ~ ffarea + age)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -16.0413  -4.8843  -0.2669   3.7126  15.4935 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 10.55019    4.92972    2.14   0.0376 *  
#   ffarea       3.87298    0.31903   12.14 4.29e-16 ***
#   age         -0.63236    0.07836   -8.07 2.01e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.646 on 47 degrees of freedom
# Multiple R-squared:  0.8159,  Adjusted R-squared:  0.808 
# F-statistic: 104.1 on 2 and 47 DF,  p-value: < 2.2e-16


#(ii) 3 explanatory variables ffarea, age, mfee
fit2<- lm(askpr~ffarea+age+mfee)
print(summary(fit2))

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  8.72354    5.10857   1.708   0.0944 .  
# ffarea       3.65640    0.36056  10.141 2.60e-13 ***
#   age         -0.66162    0.08126  -8.142 1.83e-10 ***
#   mfee         0.23676    0.18770   1.261   0.2135    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.604 on 46 degrees of freedom
# Multiple R-squared:  0.822,  Adjusted R-squared:  0.8104 
# F-statistic: 70.82 on 3 and 46 DF,  p-value: < 2.2e-16


#(iii) 4 explanatory variables ffarea, age, mfee, beds
fit3<- lm(askpr~ffarea+age+mfee+beds)
print(summary(fit3))

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 11.01630    6.02182   1.829    0.074 .  
# ffarea       3.80717    0.41729   9.123 8.49e-12 ***
#   age         -0.66386    0.08173  -8.122 2.29e-10 ***
#   mfee         0.18030    0.20395   0.884    0.381    
# beds        -1.05345    1.44540  -0.729    0.470    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.638 on 45 degrees of freedom
# Multiple R-squared:  0.8241,  Adjusted R-squared:  0.8085 
# F-statistic: 52.71 on 4 and 45 DF,  p-value: < 2.2e-16


#according to the criteria of adjusted R, value close to 1 indicates a good fit of the model, in our case
#for fit1, adjusted R^2 = 0.808
#for fit2, adjusted R^2 = 0.8104
#for fit3, adjusted R^2 = 0.8085
#therefore, model 2 fits the best

#Now find the confidence interval for coeficient of ffarea
fit2$coef
b0 = fit2$coef[1]
b0
bffarea = fit2$coef[2]
bffarea
bage = fit2$coef[3]
bage
bmfee = fit2$coef[4]
bmfee
# get the confidence interval
n = length(age)
n
#compute the 0.975 quantile of the t distribution
t.1 = qt(0.975,n-2)
names(fit2)

#caculate the sum of residuals and sigma^2
res = fit2$residuals
ss.res = sum(res^2)
sigma2 = ss.res/(n-2)

#the default is 95%
confint(fit2)
prediction = b0 + 17*bffarea + 10*bage + 29*bmfee
prediction
b0
diag = lm.diag(fit2)
