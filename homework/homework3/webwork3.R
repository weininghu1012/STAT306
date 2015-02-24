#webwork 3
options(digits = 6)
# question1, read the data in
calories=c(100, 110, 100, 100, 100, 120, 110, 150, 120, 110,
           100, 70, 110, 100, 150, 110, 130, 110, 110, 90, 100, 80, 110,
           100, 90, 110, 90, 110, 140, 110, 90, 110, 90, 110, 110, 110,
           110, 110, 100, 120, 120, 110)

#explanatory variable
protein=c(3, 1, 2, 3, 3, 3, 1, 4, 3, 2, 2, 4, 3, 3, 4, 3, 3, 1, 2, 3, 3,
          2, 2, 2, 2, 2, 3, 6, 3, 1, 3, 3, 2, 1, 1, 2, 2, 1, 2, 1, 3, 2)
fat=c(1, 1, 0, 1, 1, 3, 1, 3, 2, 2, 0, 1, 0, 1, 3, 2, 2, 0, 1, 0, 2, 0, 1,
      1, 0, 1, 0, 2, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 3, 1, 0)
fiber=c(3, 0, 1, 3, 3, 3, 0, 3, 5, 1.5, 0, 10, 3, 3, 3, 2, 1.5, 0, 0, 3,
        2.5, 3, 1, 2, 3, 0, 4, 2, 4, 0, 5, 1.5, 4, 0, 0, 0, 0, 0, 2, 0, 6, 0)
carbo=c(15, 13, 18, 17, 16, 13, 12, 16, 12, 10.5, 11, 5, 17, 17,
        16, 13, 13.5, 14, 21, 20, 10.5, 16, 16, 15, 15, 21, 19, 17, 15, 13,
        13, 11.5, 15, 12, 23, 21, 12, 15, 11, 13, 11, 22)
sugars=c(5, 12, 5, 3, 3, 4, 13, 11, 10, 10, 15, 6, 3, 3, 11, 7, 10,
         11, 3, 0, 8, 0, 8, 6, 5, 3, 0, 1, 14, 12, 5, 10, 6, 13, 2, 3, 12, 9, 10,
         9, 14, 3)
mfr=c('P', 'P', 'R', 'G', 'G', 'P', 'G', 'R', 'P', 'G', 'P', 'N',
      'P', 'R', 'R', 'G', 'G', 'P', 'G', 'N', 'G', 'N', 'G', 'G', 'N', 'G',
      'N', 'G', 'G', 'G', 'P', 'G', 'R', 'G', 'R', 'G', 'G', 'G', 'G', 'G',
      'P', 'R')

# create a data frame
cereal = data.frame(cbind(calories,protein,fat,fiber,carbo,sugars))
cereal$mfr = mfr
levels(cereal$mfr)
print(table(cereal$mfr))
#categorical variable with 4 manufactures
n = nrow(cereal)
n
#fill in all the categories as "other"
submfr = rep("other",n)
#The estimate of the signed distance of the hyperplane for manufacturer
#G relative to P is and its SE is
#fill in "P" and "G" as contrast
imfr1 = (cereal$mfr == "P")
imfr2 = (cereal$mfr == "G")
submfr[imfr1] = "P"
submfr[imfr2] = "G"
cereal$submfr = submfr
factor(submfr)
# [1] P     P     other G     G     P     G     other P     G     P     other P     other other G    
# [17] G     P     G     other G     other G     G     other G     other G     G     G     P     G    
# [33] other G     other G     G     G     G     G     P     other
# Levels: G other P
# to make P the baseline
submfrp = C(factor(submfr),contr.treatment(3,base = 3))
cereal$submfrp = submfrp
attach(cereal)
calgp = lm(calories~protein+fat+fiber+carbo+sugars+submfrp)
print(summary(calgp))

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -11.930  -2.794   0.105   1.947  12.974 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    7.775      8.307    0.94    0.356    
# protein        2.939      1.169    2.52    0.017 *  
#   fat            9.856      1.113    8.86  2.4e-10 ***
#   fiber          0.560      0.703    0.80    0.431    
# carbo          4.020      0.391   10.29  5.6e-12 ***
#   sugars         3.470      0.291   11.94  1.0e-13 ***
#   submfrp1      -4.025      2.231   -1.80    0.080 .  
# submfrp2      -4.941      2.499   -1.98    0.056 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.07 on 34 degrees of freedom
# Multiple R-squared:  0.914,  Adjusted R-squared:  0.896 
# F-statistic: 51.3 on 7 and 34 DF,  p-value: 3.12e-16

#fill in "N" and "R" as contrast
submfr = rep("other",n)
imfr3 = (cereal$mfr == "N")
imfr4 = (cereal$mfr == "R")
submfr[imfr3] = "N"
submfr[imfr4] = "R"
cereal$submfr = submfr
factor(submfr)
# [1] other other R     other other other other R     other other other N     other R     R     other
# [17] other other other N     other N     other other N     other N     other other other other other
# [33] R     other R     other other other other other other R    
# Levels: N other R
submfrr = C(factor(submfr),contr.treatment(3,base = 3))
calnr = lm(calories~protein+fat+fiber+carbo+sugars+submfrr)
print(summary(calnr))

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -12.229  -3.187  -0.662   2.294  15.087 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.8098    10.5476    0.08    0.939    
# protein       2.8715     1.2262    2.34    0.025 *  
# fat           9.3853     1.1879    7.90  3.4e-09 ***
# fiber         0.9144     0.7101    1.29    0.207    
# carbo         4.0856     0.4418    9.25  8.3e-11 ***
# sugars        3.5583     0.3312   10.74  1.8e-12 ***
# submfrr1     -0.0453     3.9801   -0.01    0.991    
# submfrr2      2.6159     2.6175    1.00    0.325    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.31 on 34 degrees of freedom
# Multiple R-squared:  0.905,  Adjusted R-squared:  0.886 
# F-statistic: 46.4 on 7 and 34 DF,  p-value: 1.44e-15

detach(cereal)
cereal
reg6 = lm(calories~+protein+fat+fiber+carbo+sugars+mfr)
print(summary(reg6))

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -11.330  -2.673   0.069   1.829  13.130 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.3970     8.9976    0.27    0.792    
# protein       2.9729     1.1883    2.50    0.017 *  
# fat           9.9915     1.1976    8.34  1.2e-09 ***
# fiber         0.5233     0.7204    0.73    0.473    
# carbo         4.0748     0.4277    9.53  5.4e-11 ***
# sugars        3.5132     0.3216   10.92  1.7e-12 ***
# mfrN          0.0224     3.6652    0.01    0.995    
# mfrP          4.1737     2.3033    1.81    0.079 .  
# mfrR         -1.2986     2.6360   -0.49    0.626    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.14 on 33 degrees of freedom
# Multiple R-squared:  0.914,  Adjusted R-squared:  0.893 
# F-statistic: 43.7 on 8 and 33 DF,  p-value: 2.21e-15

#question2
# Assume s is a sample covariance or sample correlation matrix
# with (possible) row and column names, this function outputs
# partial correlation of first two variables given the rest.
pcor=function(s)
{ i=1; j=2
  i1=c(i, j)
  i2=1:nrow(s); i2=i2[c(-i, -j)]
  s11=s[i1,i1]; s12=s[i1,i2]; s21=s[i2,i1]; s22=s[i2,i2];
  condcov = s11 - s12 %*% solve(s22) %*% s21
  condcov[1,2]/sqrt(condcov[1,1] * condcov[2,2])
}
x1=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
x2=c(2, 6, 5, 4, 8, 0, 3, 1, 7, 9)
x3=c(2, 0, 7, 1, 5, 6, 3, 9, 8, 4)
y=c(4, 2, 2, 4, 10, 6, 6, 9, 7, 4)
data = data.frame(cbind(x1,x2,x3,y))
rmat = cor(data)
print (rmat)
rmat[1,1]

#caculate the part for Ryx1:x2
rsub1 = rmat[c(4,1,2),c(4,1,2)]
pcor(rsub1)

#caculate the part for Ryx2;x1x3
rsub2 = rmat[c(4,2,1,3),c(4,2,1,3)]
pcor(rsub2)
