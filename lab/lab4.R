# STAT306 2015 Lab 4
# simulation to show sampling distribution of beta1hat, sigmahat, t-ratios etc
# Sampling distribution means the distribution of beta1hat etc, when
#    replicating from the model: fixed true beta0, beta; fixed values
#     x_1,...,x_n of the explanatory variables; random N(0,sigma^2)
#     deviations epsilon_i from the line,  i=1,...,n
#    
set.seed(1223)  # set seed to get same thing if rerun again
n=9  # small sample size
x1 = (1:n)-(n+1)/2   # fixed x values at 1 to n; centred by subtracting average

# set (true) population parameters
b0=1; b1=0.7; sigma=0.5
par(mfrow=c(3,4), mar=c(3, 1, 1, 1))  # set-up for 3x4 subplots on one page
nsim=100   # number of replications
#nsim=1000   # uncomment this line to do more replications
b1hat=rep(0,nsim)           # pre-locate space to store replications
b0hat=rep(0,nsim)
sigmahat=rep(0,nsim)
t1ratio=rep(0,nsim)
t0ratio=rep(0,nsim)

# simulation/replication loop
for(i in 1:nsim)
{ err = rnorm(n,0,sigma)     # random from normal(mean=0,SD=sigma)
  y = b0+b1*x1+err           # generate y's with deviations in 'err'
  datareg = as.data.frame(cbind(y, x1))
  reg = lm(y~x1,data=datareg)  # regression 
  out=summary(reg)
  outcoeff=out$coeff           # get betas and their SEs
  b0hat[i]=outcoeff[1,1]       # coefficent beta0hat
  b1hat[i]= outcoeff[2,1]
  se0hat= outcoeff[1,2]
  se1hat= outcoeff[2,2]        # SE of beta1hat
  sigmahat[i]=out$sigma        # estimated residual SE or residual SD
  t1ratio[i] = (b1hat[i]-b1)/se1hat  # t-ratio for beta1hat
  t0ratio[i] = (b0hat[i]-b0)/se0hat
  if(i<=12)                     # plot the first few cases 
  { plot(x1,y,xlim=c(x1[1],x1[n]))
    abline(b0hat[i],b1hat[i],col=1)
    title(paste("Simulation ",i))
  }
}

# Under the assumptions of the model ( epsilon_i ~ N(0,sigma^2) # independently)
# then

# (a) sampling distribution of b1hat is normal with mean b1
#     normal quantile plot of b1hat over different replications
#     shows points very close to a straight line
par(mfrow=c(1,2))
qqnorm(b1hat,main="b1hat qqnorm")
qqline(b1hat)
# (b) sampling distribution of t-ratio is not normal because denominator 
#   involves a random sigmahat. The sampling distribution is Student t with
#   degrees of freedom n-k, where  k is the number of estimated betas.
# t-density has heavier tails than normal density, so normal quantile
#   plots shows points off the diagonal at the two ends.
qqnorm(t1ratio,main="t1ratio qqnorm") 
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
