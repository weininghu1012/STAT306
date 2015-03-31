############################################################
###STAT 306 2015 Lab 10: Principal Component Analysis (PCA)
############################################################

#Produce a similiar plot as in Figure 8.1(a) ofn the course notes
set.seed(1)
x <- sort(rnorm(20, mean=0, sd=2))
y <- x + rnorm(20, mean=0, sd=1)
dat <- cbind(x, y)

par(mfrow=c(1, 2))
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

#The direction of the first principal component is
wMx[2,1]/wMx[1,1]

#The PCA can build a new coordinate system to present the data, 
#where the x-axis is the first principal component and the y-axis is the second principal component. 
#The new coordinates of the observations in this new system are called the scores (on the principal components). 
#Here is the plot of the scores of the original observations in the new corindate system

plot(dat.pc$scores, type="n", main="The New Coordinate System", xlim=c(-4, 4), ylim=c(-4, 4))
text(dat.pc$scores[,1], dat.pc$scores[,2], as.character(1:20) )
arrows(-4, 0, 4, 0, lty=1, col="red")
arrows(0,-2, 0,  2, lty=2, col="red")

#compare these two plots carefully, you can see that the relative positions of the points 
#(e.g. position of first observation with respect to the position of the second observation) stay the same.
#The second plot is a rotation of the first plot.

#####A Real Principal Component Analysis Example
dat <- read.table("decathlon.txt", header=T)
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
cdat.eigen$values/sum(cdat.eigen$values)
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
cdat.my.scores - pca$scores
#Negligible difference. 
