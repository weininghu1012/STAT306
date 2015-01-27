#STAT 306-2015 Lab1 

##############################################
# 1. Some R Basics
# assign a number to an argument
a<-5 
#or 
a=5
b<-10

# conduct some simple operations
a+b      # addition
a*b      # multiplication
a/b      # division
a^2      # exponentiation
sqrt(a)  # square root

# create a vector
cc<-c(1,3,5,7,9)     # c stands for combine values into a vector or list
d<-c(1:5)
cc
d

# extract compounds from a vector
cc[1]              # first element of the vector cc
d[3]               # third element of the vector d
d[c(1,3,5)]        # 1st, 3rd, and 5th elements of the vector d

# calculate summation of a vector
sum.1<-sum(cc)     # sum of the elements of the vector c
sum.2<-sum(cc*d)   # sum of componentwise products

# calculate mean of a vector
mean.1<-mean(cc)
mean.2<-mean(d)

# create a matrix
x<-c(1:20)
m<-matrix(x, 4, 5)     # elements will be assigned column wise
m

# extract row and column of a matrix
m1<-m[2, ]         # second row of the matrix m
m2<-m[, 3]         # third column of the matrix m

# extract components of the matrix
m[2,3]          # element corresponding to second row and third column

# calculate row summation 
sum.row<-apply(m, 1, sum)   # sum of the elements in each row of m
sum.row

# calculate column summation 
sum.col<-apply(m, 2, sum)    # sum of the elements in each column of m
sum.col

# Matrix multiplication
A <- matrix(1:10, ncol = 2)                        # order 5 by 2
B <- matrix(seq(2, 20, 2), nrow = 2, byrow = TRUE)       # order 2 by 5
A %*% B       # order 5 by 5
B %*% A       # order 2 by 2
BA <- B %*% A

# Inverse of the square matrix BA
solve(BA)

# Check
BA %*% solve(BA)                        # should be an identity matrix


#########################################
#2. Do Exercise 1.2 in course notes using R (just using as a calculator!)
# part 1: calculate pr(x1=0,x2=0)
p.1<- 135/600 
p.1
# part 2: calculate pr(Y=1,x1=1,x2=1)
p.2<- 25/600 
p.2
# part 3: calculate pr(Y=1|x1=0,x2=0)
p.3<- 134/135 
p.3
# part 4: calculate pr(x1=1,x2=0|Y=1)
p.4<- 127/325 
p.4
# part 5: calculate pr(Y=1|x1=0,x2=0)
p.5<- 134/135
p.5

##############################################################
#3. Reading data (Instruction for MAC in our lab ESB1042/1046)
#VERY IMPORTANT !!!!!
#Follow the instruction in the hand-outs to download the "stock" data set from our course website.
stock <-read.table("Downloads/stock.txt", header=T)
# look at the table
stock
# extract the first row of stock
stock.row.1<- stock[1,]
stock.row.1
#  extract the third column of stock
stock.col.3<- stock[,3]
stock.col.3
# another way to display the third column (StockRate)
stock.x3<- stock$StockRate
stock.x3

#It is similar to read in a file under the "Documents" folder
#stock <-read.table("Documents/data", header=T)
#Or similarly under Windows:
#stock <- read.table("E:/STAT306/stock.txt", header=T)
#Or you could set the R working directory to the folder that contains your data.
#setwd("E:/STAT306")
#To check if this is done correctly
#getwd()
#Read in the data directly
#stock <- read.table("stock.txt", header=T)

#######################################
#4. Create a simple plot
plot(stock$PortfolioRate, stock$StockRate)

######################################
#5. More about R

#5.1 R is a well-documented software. 
#You could see the help file of almost any function by
#Get the help of the function "read.table"
?read.table
#The help of matrix multiplication
help("%*%")

#5.2 Need some more functions from R? 
#Search for the function to plot the histogram
??histogram
#Or this can be done by Google with search keyword "R project XXX"

#5.3 History of your commands
#You can check the "short" history of your R commands by the direction buttons 
#on your keyboard.
#Or by the following function
history(20) #20 is the number of lines of code you want to retrieve.

#5.4 Save the results in R.
#To save a matrix or a table, you can use "write.table", 
# which is the counter part of read.table
write.table(stock, "stock.txt")
#The file by default will be saved to your working directory,
#which can be found by
getwd()
#Or you could save it to another place by specifying the full path, e.g.,
#write.table("E:/STAT306/stock")
#write.table("Documents/STAT306/Lab1/stock")

#Later on for your term project, you may want to save the whole working space,
#including your data, regression results, etc., by 
save.image("WorkSpace1.RData")

#5.5 Comments in R
#Find out by yourselves, what we are doing with the # in R?