#stock <- read.table("Documents/study/R/BikeSharingDemand/306lab/stock.txt", header = T)
setwd("/Users/apple/Documents/study/R/BikeSharingDemand/306lab")
stock <- read.table("stock.txt")
stock
stock[1:5,]
stock[,-1]
stock[1:10,]
name = colnames(stock)
name
colnames(stock) = c("PR", "SR")
stock[1:5,]
# webwork2, 1
v = c(505, 131, 38, 41,27,66,90,160,156,133,102,102,200,134,136,149,187,190,224,128,209,141,168,164)
# assign the data in a 4*6 matrix column wise
m = matrix(v,4,6)
m
x1 = m[4,6]
x1
x2 = m[4,5]
x3 = m[2,2]
x2
x3
s = c(x1^2, x2^2, x3^2)
sum(s)
s2 = c(x1,x2,x3)
a = sum(s2)
a^2
a
x = c( -2.0, 0.5, 1.3, 5.7, 10.4, 3.4, 7.6, 4.1, 5.0, 8.5, 12.3, 16.1, 13.2, 8.8, 7.9, 1.1, 3.0, 4.5, 2.7, 7.6)
y= c(2.7, 3.9, 4.5, 6.0, 8.7, 2.7, 3.2, 5.6, 9.2, 11.7, 6.8, 13.0, 11.9, 8.6, 6.1, 4.9, 1.0, 7.2, 2.7, 7.6)
reg1 = lsfit(x,y)
names(reg1)
reg2 = lsfit(y~x)
reg3 = lm(x,y)
reg4 = lm(y~x)
names(reg4)
