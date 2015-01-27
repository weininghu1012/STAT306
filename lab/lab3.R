STAT306 Lab 03 :
## Fitting simple linear regression model using the statement "lm" in R.

#Read-in the data as we did in Lab 2.
#stock <-read.table("Downloads/stock.txt", header=T)
#stock <-read.table("F:/STAT306-2015/Lab2015/Lab1/stock.txt", header=T)

# To view the first 5 rows of the data file
stock[1:5,]

# To delete (if you want to do so) the first column of "stock" data
stock = stock[,-1]

# To check if the column numbered "1" is deleted (view the first 10 rows of the data)
stock[1:10,]

# To see the column names of the stock data
name = colnames(stock)
name
#[1]	"PortfolioRate"	"StockRate"

# To change the column names of the "stock" data to "PR" for Portfolio Rate and "SR" 
# for Stock Rate
colnames(stock)  = c ("PR", "SR")

# To check if it is changed (see first five rows of the data set)
stock[1:5,]

# Plot the data
plot(stock$PR , stock$SR , xlab="Portfolio Rate", ylab="Stock Rate")

# To fit a linear regression line (Stock Rate = Beta0 + Beta1 Portfolio Rate + Random Error)

reg = lm(stock$SR ~ stock$PR)
# more simply 
reg2 = lm(SR ~ PR , data=stock)

# To add the linear regression line to the plot (you did the same thing before!!!)
abline(reg)

# To see the summary of the result
summary(reg)

# To see what's included in the object reg
ls(reg)
# Or
names(reg)
# Where you can find coefficient, residuals, fitted.values, etc.
# Notice that not of all them are useful in this course.

# To extract the residuals (or errors) of the fit
reg$res
# or you may want to type the full name
reg$residuals
# When you do not type the full name, R will search for the items that matches what you typed.

#	or
summ = summary(reg)
summ$residuals

# To extract the fitted values
reg$fitted.values

# To extract the estimated coefficients
betatable=summ$coefficients
betatable[,1]

# To extract the standard error of the estimated coefficients
betatable[,2]

# To extract the test statistic "t" for the regression coefficients
betatable[,3]

# To extract the p-values of the test
betatable[,4]

# You can also check what's included in the summary of reg
ls(summ)


# To construct 95% confidence intervals for intercept and slope. 95% is the default.
confint(reg)

# To construct 99% confidence intervals for intercept and slope.
confint(reg, level = 0.99)

# 95% confidence interval for the mean response of Stock Rate for different values of Portfolio Rate, say for 7 and 11.
new <- data.frame(PR = c(7, 11)) 
# here is to create a data frame (matrix, if you like) with PR= 7, 11 and then put into the linear model.
# The data frame is required to make sure that "predict" works properly.

#predict(lm(SR ~ PR, data = stock), new,  se.fit = TRUE, interval="confidence", level=0.95)
# or  since "reg2" is saved, 
predict(reg2, new,  se.fit = TRUE, interval="confidence", level=0.95)

# 99% prediction interval of Stock Rate for different values of Portfolio Rate, say for 15 and 17.
new <- data.frame(PR = c(15, 17))
predict(reg2, new,  se.fit = TRUE, interval="prediction", level=0.99)
