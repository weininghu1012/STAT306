STAT 306-2015 Lab 2 
#Fit a Simple Regression Model
#Do Ex 2.1 using R

#Data: 
#stock, Table 2.3, in course notes 2-25
#predictor x : portfolio rate 
#response y : stock rate 
#number of observations : 20

#1. Create data matrix and plot the data

# create data
x = c( -2.0, 0.5, 1.3, 5.7, 10.4, 3.4, 7.6, 4.1, 5.0, 8.5, 12.3, 16.1, 13.2, 8.8, 7.9, 1.1, 3.0, 4.5, 2.7, 7.6)
y= c(2.7, 3.9, 4.5, 6.0, 8.7, 2.7, 3.2, 5.6, 9.2, 11.7, 6.8, 13.0, 11.9, 8.6, 6.1, 4.9, 1.0, 7.2, 2.7, 7.6)
stock = data.frame(cbind(x,y))

#Or you can read-in the data as we did in Lab 1.
#e.g.
#stock <-read.table("Downloads/stock.txt", header=T)
#stock <-read.tabl("F:/STAT306-2015/Lab2015/Lab1/stock.txt", header=T)

# have a look at the data
stock

# plot data
plot(x,y, xlab= " Portfolio rate (%)", ylab= " Stock rate (%) ")
# or
plot(stock, xlab= " Portfolio rate (%)", ylab= " Stock rate (%) ")

#2. Calculate beta0 hat (estimate of intercept term) and beta1 hat (estimate of slope coefficient)  

# fit the regression line and summarize the linear regression result using R function
reg = lsfit(x,y)
# to check what sorts of output components are available
names(reg)

# display coefficient estimations, with se, t statistic and P values
ls.print(reg) 
# extract estimated coefficients, beta0 hat and beta1 hat
reg$coef      
b0 = reg$coef[1]
b1= reg$coef[2]
# add one straight line into the current plot with the intercept b0 and slope b1 
plot(x,y, xlab= " Portfolio rate (%)", ylab= " Stock rate (%) ")
abline (b0, b1, col="red")

# or  
plot(stock, xlab= " Portfolio rate (%)", ylab= " Stock rate (%) ")
abline(reg)

#3. Calculate SS(Res) and sigma^2
n = length(y)
res = reg$res      
ss.res = sum(res^2)
sigma2 = ss.res/(n-2)

#4. Compute the standard errors of beta0 hat and beta1 hat 
# Computes basic statistics, including se, t- and p-values for the regression  
# coefficients
diag = ls.diag(reg)   
names(diag)
# some parts of this diag is not required in this course.

# extract standard errors for b0 and b1 
b0.se = diag$std.err[1]  
b1.se = diag$std.err[2]

#5. Test H0: beta1=0 
t.b1 = b1/b1.se
p.value = 2*(1-pt(t.b1, n-2))
p.value

#6. calculate 95% confidence intervals for b1
# compute 0.975 quantile of the t distribution with degrees of freedom n-2
t.1 = qt(0.975,n-2)  
#  calculate 95% confidence intervals for b1
b1.ci = c(b1-t.1*b1.se, b1+t.1*b1.se)
b1.ci

#7. Prediction at a new observation x=5.5 and calculate the confidence/prediction interval 
newx=5.5
newy=b0 + b1*newx
#confidence interval of newy
xss = sum((x-mean(x))^2) #sum of squares of x
newy.se=  sqrt(sigma2) * sqrt(1/n + (newx-mean(x))^2/xss)
newy.ci = c(newy - t.1*newy.se, newy + t.1*newy.se)
newy.ci

#prediction interval of newy
newy.pe=  sqrt(sigma2) * sqrt(1+1/n + (newx-mean(x))^2/xss)
newy.pi = c(newy - t.1*newy.pe, newy + t.1*newy.pe)
newy.pi

#8. Calculate the regression coefficients by yourself on a subset of the data.
# For example, we are now only interested in observations of x <10 in the stock data set
# get those x and call them x1
x1 <- x[x<10]
# get the corresponding y1
y1 <- y[x<10]
n1 <- length(x1)
#calculate the sufficient statistics as on Course note Page 2-9
mx1 <- mean(x1)
my1 <- mean(y1)
sxx1 <- sum(x1^2)
syy1 <- sum(y1^2)
sxy1 <- sum(x1*y1)

nb1 <- (sxy1 - n1*mx1*my1)/(sxx1 - n1*mx1^2)
#Or
nb1 <- sum((x1-mx1)*(y1-my1))/sum((x1-mx1)^2)
#Carry on to calculate the intercept for this subset data as well as their s.e. 
#Verify your results using the lsfit function.

#Lab Quiz is on Webwork. Please finish them before Friday 10pm.