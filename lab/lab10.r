############################################################
###STAT 306 2015 Lab 10: Principal Component Analysis (PCA)
############################################################

#Produce a similiar plot as in Figure 8.1(a) ofn the course notes
set.seed(1)
x <- sort(rnorm(20, mean=0, sd=2)) # sort from the minimum to maximum
y <- x + rnorm(20, mean=0, sd=1)
dat <- cbind(x, y)

par(mfrow=c(1, 2))
par("mar")
par(mar = c(1,1,1,1))
plot(x, y, type="n", main="The Original X and Y", xlim=c(-4, 4), ylim=c(-4, 4))
text(x, y, as.character(1:20))
arrows(-4, -4, 4, 4, lty=1, col="red")
arrows(2, -2, -2, 2, lty=2, col="red")




#You can find that the x and y are all around the line of Y=X and 
#most variability of the data is in the direction of this line, 
#while little variability is on the Y=-X direction. 
#But in practice, how could we find this direction? 
#This leads us to do a principal component analysis of the data.

dat.pc <- princomp(dat, cor = TRUE, scores = TRUE)
summary(dat.pc)
#Importance of components:
#                          Comp.1     Comp.2
#Standard deviation     1.3793090 0.31226068
#Proportion of Variance 0.9512466 0.04875336
#Cumulative Proportion  0.9512466 1.00000000
#The first principal component explains 95% of the variation in the data.

wMx <- loadings(dat.pc)
#Loadings:
#  Comp.1 Comp.2
#x  0.707 -0.707
#y  0.707  0.707

#The results of a PCA are usually discussed in terms of component scores, 
# sometimes called factor scores (the transformed variable values 
# corresponding to a particular data point), 
#and loadings (the weight by which each standardized original variable 
# should be multiplied to get the component score)

#The direction of the first principal component is
wMx[2,1]/wMx[1,1] # delta(y)/delta(x)

#The PCA can build a new coordinate system to present the data, 
#where the x-axis is the first principal component and the y-axis is the second principal component. 
#The new coordinates of the observations in this new system are called the scores (on the principal components). 
#Here is the plot of the scores of the original observations in the new corindate system
par(mar = c(2,2,2,2))
plot(dat.pc$scores, type="n", main="The New Coordinate System", xlim=c(-4, 4), ylim=c(-4, 4)) # this is a pure layer
text(dat.pc$scores[,1], dat.pc$scores[,2], as.character(1:20) )
arrows(-4, 0, 4, 0, lty=1, col="red")
arrows(0,-2, 0,  2, lty=2, col="red")

#compare these two plots carefully, you can see that the relative positions of the points 
#(e.g. position of first observation with respect to the position of the second observation) stay the same.
#The second plot is a rotation of the first plot.


# > x
# [1] -4.42939977 -1.67125722 -1.64093677 -1.25290762 -1.24248116 -0.61077677 -0.08986722
# [8] -0.03238053  0.36728665  0.65901554  0.77968647  0.97485810  1.15156270  1.18780264
# [15]  1.47664941  1.64244239  1.88767242  2.24986184  3.02356234  3.19056160
# > y
# [1] -3.51042240 -0.88912092 -1.56637178 -3.24225932 -0.62265541 -0.66690551 -0.24566272
# [8] -1.50313291 -0.11086341  1.07695710  2.13836602  0.87207038  1.53923431  1.13399760
# [15]  0.09958985  1.22744783  1.49338247  2.19054844  4.12358771  3.95373735

# > dat.pc$scores
# Comp.1      Comp.2
# [1,] -3.3038106  0.51754303
# [2,] -1.2683146  0.36200838
# [3,] -1.4991290  0.10710780
# [4,] -1.9459682 -0.64797647
# [5,] -1.0024550  0.28725416

# the comp1 is the result gained from standardized method

# try to do the pca analysis by myself
# first get the correlation of the matrix
cor.dat = cor(dat)
# x         y
# x 1.0000000 0.9024933
# y 0.9024933 1.0000000
dat.eigen = eigen(cor.dat)
# [,1]       [,2]
# [1,] 0.7071068 -0.7071068
# [2,] 0.7071068  0.7071068

# the variability explained by each PC
dat.eigen$values/sum(dat.eigen$values)
#same as 
summary(dat.pc)

# The loadings are the eigenvectors
dat.eigen$vectors
dat.pc$loadings

#caculate  the scores
dat.standardized = scale(dat)
dat.my.scores = dat.standardized%*%dat.eigen$vectors
dat.my.scores-dat.pc$scores # The difference is very small



#####A Real Principal Component Analysis Example
dat <- read.table("/Users/apple/Desktop/STAT306/lab/decathlon.txt", header=T)
#This data set studies athletes for the 10 events of the decathlon and 
#the columns 11 and 12 correspond respectively to the rank and the points obtained. 
#The last column is a categorical variable corresponding to the athletic meeting (2004 Olympic Game or 2004 Decastar)
#This data set can be found on http://factominer.free.fr/classical-methods/principal-components-analysis.html

#For this lab, we will discard the two non-numeric "Rank" and "Competition" column 
dat <- dat[,-c(11, 13)]
#Let's perform a principal component analysis on the correlation matrix of the athletes' performance in the 10 events.
cdat <- dat[,c(1:10)] 
pca <- princomp(cdat, cor=TRUE)

summary(pca)
#The first 7 PC's can explain over 90% of variability.

#See how the PC's are formed based on the original variables.
pca$loadings

#The biplot function can plot the PC's
biplot(pca)
#The location of the names indicate their scores on the first two PC's, i.e., 
#This plot is similar to second plot we produced above.
#It is not easy to study the PC's with the athlete names everywhere.
#To hide those names

biplot(pca, col=c(0, 2))
#The direction of the arrow of each variable in this plot is
#decided by their loadings on the first and second PC's.
#The first PC is the contrast between the short distance running events 
#like 400, 110m hurdle, 100m and the other events like high/long jump and javeline.  

####Try to do PC by yourself via the eigenvalues and vectors.
cor.cdat <- cor(cdat)
cdat.eigen <- eigen(cor.cdat)

#The variability explained by each PC
cdat.eigen$values/sum(cdat.eigen$values) # lambdai(sum(lambdai))
#Same as 
summary(pca)

#The loadings are the eigenvectors
cdat.eigen$vectors
pca$loadings
#They are rounded in the PCA 9and maybe with different sign).

#How the scores are calculated?
cdat.standardized <- scale(cdat)
#scale function standardizes the data.
cdat.my.scores <- cdat.standardized %*% cdat.eigen$vectors
cdat.my.scores - pca$scores  # comparing these two methods
#Negligible difference. 


#Lab10
burden<- read.csv("/Users/apple/Desktop/STAT306/lab/burden.csv", header = TRUE) 
burden<- burden[, -c(1, 26)] 
burden_pca <- princomp(burden, cor=TRUE)
summary(burden_pca)

burden_pca$loadings
