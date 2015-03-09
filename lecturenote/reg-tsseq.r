# Example of Durbin-Watson test and diagnostics for serial dependence of 
# residuals when data are sequentially collected over time.

library(lmtest)
# Unemployment data
# 62x5 multivariate yearly time series from 1895 to 1956
#     UN = unemployment rate,
#     m = broad money supply,
#     p = implicit deflator of Gross National Product,
#     G = real purchases of goods and services,  
#     x = real exports.
myunemp=read.table("unemployment.txt",header=T)
attach(myunemp)
time = 1:62
logmprat=log(m/p)
logG=log(G)
logx=log(x)
print(cor(UN,logmprat)) # -0.209535
print(cor(UN,logG)) # -0.2595265
print(cor(UN,logx)) # -0.4411696
print(cor(UN,time)) # -0.1120659
print(cor(cbind(UN ,logmprat , logG ,logx ,time)))
#                 UN   logmprat       logG       logx       time
#UN        1.0000000 -0.2095350 -0.2595265 -0.4411696 -0.1120659
#logmprat -0.2095350  1.0000000  0.9053641  0.8076848  0.9833135
#logG     -0.2595265  0.9053641  1.0000000  0.6888852  0.9068027
#logx     -0.4411696  0.8076848  0.6888852  1.0000000  0.7984058
#time     -0.1120659  0.9833135  0.9068027  0.7984058  1.0000000
fit=lm(UN ~ logmprat + logG + logx + time)
print(summary(fit))
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  91.5258    10.2623   8.919 2.10e-12 ***
#logmprat    -13.8332     2.9894  -4.627 2.18e-05 ***
#logG         -5.7374     1.1378  -5.043 4.97e-06 ***
#logx         -9.5063     1.3060  -7.279 1.09e-09 ***
#time          0.9158     0.1126   8.132 4.15e-11 ***
#Residual standard error: 2.811 on 57 degrees of freedom
#Multiple R-squared:  0.6839,    Adjusted R-squared:  0.6617 

dwtest(UN ~ logmprat + logG + logx + time)
#data:  UN ~ logmprat + logG + logx + time
#DW = 0.7088, p-value = 1.399e-10
#alternative hypothesis: true autocorrelation is greater than 0
res=fit$resid
dwstat=0; n=length(res)
for(i in 1:(n-1)) { dwstat=dwstat+(res[i]-res[i+1])^2 }
dwstat=dwstat /sum(res^2)
print(dwstat) # 0.7088309
# 0<=dwstat<=4, dwstat=~2 for independence, 
# dwstat near 0 for strong positive serial correlation 
# dwstat near 4 for strong negative serial correlation 

par(mfrow=c(3,2))
plot(fit$resid) # -+-
abline(h=0)
plot(fit$resid[-62],fit$resid[-1],xlab="resid(t)",ylab="resid(t+1)"); abline(h=0); abline(v=0)
print(cor(fit$resid[-62],fit$resid[-1])) #0.644
plot(logmprat,UN,type="n"); text(logmprat,UN,label=1:62)
plot(logG,UN,type="n"); text(logG,UN,label=1:62)
plot(logx,UN,type="n"); text(logx,UN,label=1:62)
plot.ts(UN,xlab="time is years 1895:1956")
