#lab4 for loop

f<-function(n,a,b){
  sum<-0
for (i in 1:n){
  if ((i%%a ==0)|(i%%b == 0)){
    sum <- sum + sqrt(i)
  }
}
return(sum)
}
f(63445,89,107)
#[1] 218009.1

#qqnorm
set.seed(1223)
n = 1000
x1 = (1:n) - (n+1)/2

#set (true) population parameters
b0 =1
b1 = 0.7

sigma = 0.5
par(mfrow=c(3,4),mar=c(3,1,1,1)) #set-up for 3*4 subplots on one page
nsim = 100  #number of replications
b1hat = rep(0,nsim)


b0hat = rep(0,nsim)
sigmahat = rep(0,nsim)


t1ratio = rep(0,nsim)
t0ratio = rep(0,nsim)

#simulation, replication loop
for (i in 1:nsim){
  err = rnorm(n,0,sigma)    #random form normal
  y = b0+b1*x1+err
  datareg = as.data.frame(cbind(y,x1))
  reg = lm(y~x1, data = datareg) #regression
  
  out = summary(reg)
  outcoeff = out$coeff
  b0hat[i] = outcoeff[1,1]
  b1hat[i] = outcoeff[2,1]
  
  
  
  
  se0hat = outcoeff[1,2]
  se1hat = outcoeff[2,2]
  sigmahat[i] = out$sigma
  
  t1ratio[i] = (b1hat[i]-b1)/se1hat
  
  
  t0ratio[i] = (b0hat[i]-b0)/se0hat
  
  if (i<=12){
    plot(x1,y,xlim=c(x1[1],x1[n]))
    
    abline (b0hat[i],b1hat[i],col =1)
    title(paste("Simulation",i))
  
  }

}


# Under the assumptions of the model ( epsilon_i ~ N(0,sigma^2) # independently)
# then

# (a) sampling distribution of b1hat is normal with mean b1
#     normal quantile plot of b1hat over different replications
#     shows points very close to a straight line

par(mfrow = c(1,2))
qqnorm(b1hat,main = "b1hat qqnorm")
qqline(b1hat)


# (b) sampling distribution of t-ratio is not normal because denominator 
#   involves a random sigmahat. The sampling distribution is Student t with
#   degrees of freedom n-k, where  k is the number of estimated betas.
# t-density has heavier tails than normal density, so normal quantile
#   plots shows points off the diagonal at the two ends.
qqnorm(t1ratio,main = "t1ratio qqnorm")
qqline(t1ratio)

# (c) sampling distribution of sigmahat^2 is right-skewed; when scaled
#   appropriately, it has a distribution called chi-squared.
par(mfrow=c(1,3))
hist(sigmahat^2,main="scaled chi2 dist.")

# (d) b1hat and b0hat has zero theoretical correlation with sigmahat
#     This property is involved in the mathematical derivation of the
#     Student t distribution for the t-ratio.
plot(b1hat,sigmahat) 

title("indep. under assumpt.")
plot(b0hat,sigmahat)
title(paste(nsim," replications"))

