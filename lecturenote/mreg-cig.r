# Example to illustrate effect of multicollinearity (Section 6.4)
#   or highly correlated explanatory variables 

# cigarette data, data set has 8 lines of documentation before header line
cig=read.table("cigarette.dat.txt",header=T,skip=8)  # n=25
cig=cig[,-1]  # first column has brand names
print(names(cig)) # "tar"    "nicot"  "weight" "CO"
pairs(cig); title("Cigarette data, y=CO (mg)")  # pairwise scatterplots plus title
print(summary(cig)) # means and quantiles
rmat=cor(cig); print(rmat[c(4,1,2,3),c(4,1,2,3)])  # sample correlation matrix
#              CO       tar     nicot    weight
#CO     1.0000000 0.9574853 0.9259473 0.4639592
#tar    0.9574853 1.0000000 0.9766076 0.4907654
#nicot  0.9259473 0.9766076 1.0000000 0.5001827
#weight 0.4639592 0.4907654 0.5001827 1.0000000

# some partial correlations for this data set are given in the Appendix of coursepack

attach(cig)  
fit1=lm(CO~tar); fit2=lm(CO~nicot); 
fit3=lm(CO~tar+weight); fit4=lm(CO~tar+nicot)
fit5=lm(CO~tar+nicot+weight)
detach(cig)  
# how many possible regression models on 3 variables?

print(summary(fit1))
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.74328    0.67521   4.063 0.000481 ***
#tar          0.80098    0.05032  15.918 6.55e-14 ***
#Residual standard error: 1.397 on 23 degrees of freedom
#Multiple R-squared: 0.9168,     Adjusted R-squared: 0.9132 

print(summary(fit2)) # beta for nicot changes a lot compared with below.
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   1.6647     0.9936   1.675    0.107    
#nicot        12.3954     1.0542  11.759 3.31e-11 ***
#Residual standard error: 1.828 on 23 degrees of freedom
#Multiple R-squared: 0.8574,     Adjusted R-squared: 0.8512 

print(summary(fit3))
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.11433    3.41620   0.912    0.372    
#tar          0.80419    0.05904  13.622 3.36e-12 ***
#weight      -0.42287    3.81299  -0.111    0.913    
#Residual standard error: 1.428 on 22 degrees of freedom
#Multiple R-squared: 0.9168,     Adjusted R-squared: 0.9093 

print(summary(fit4)) # betahat for nicot has changed a lot, its SE is larger than fit2
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   3.0896     0.8438   3.662 0.001371 ** 
#tar           0.9625     0.2367   4.067 0.000512 ***
#nicot        -2.6463     3.7872  -0.699 0.492035    
#Residual standard error: 1.413 on 22 degrees of freedom
#Multiple R-squared:  0.9186,    Adjusted R-squared:  0.9112 

print(summary(fit5))
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   3.2022     3.4618   0.925 0.365464    
#tar           0.9626     0.2422   3.974 0.000692 ***
#nicot        -2.6317     3.9006  -0.675 0.507234    
#weight       -0.1305     3.8853  -0.034 0.973527    
#Residual standard error: 1.446 on 21 degrees of freedom
#Multiple R-squared: 0.9186,     Adjusted R-squared: 0.907 
