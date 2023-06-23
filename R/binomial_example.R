# Example: Binomial Trial
# likelihood of getting 30 heads in 100 coin toses
curve(dbinom(30,100,x),0,0.75, ylab = 'L(p)', xlab = 'p')

# Simulate data and fit model
# Simulate 100 obs
set.seed(123)
y <- rbinom(n = 100, size = 100, prob = .3)

# model
mod <- function(theta) {
   -sum(dbinom(x = y, size = 100, prob = theta, log = TRUE))
}

# find theta (use Brent, the preferred method when fitting a single parameter)
# optim is a minimizer
opt <- optim(par = 0.5, fn = mod, hessian = TRUE, method = "Brent", 
             lower = 0, upper = 1)

## Wald Confidence Intervals 
#  Based on approximate normality:
#    Var(p) = 1 / -d^2(p), inverse of the negative second derivative
#    When p > 1, the Hessian is the matrix of partial negative second derivatives

# get variance using the inverse Hessian 
var.est <- 1/opt$hessian # estimated variance is the inverse of hessian
sqrt(var.est) # standard error (se): matches value from Likelihood Review
# Wald CI (assumes normal distribution)
upper.limit.wald <- opt$par + qnorm(.975)*sqrt(var.est) # calculates 95% confidence interval
lower.limit.wald <- opt$par - qnorm(.975)*sqrt(var.est) # calculates 95% confidence interval
wald.ci <- c(lower.limit.wald, upper.limit.wald)

# Calculate a profile likelihood by evaluating the log-likelihood for a range of theta
phat <- seq(0.001, .999, by = 0.001)
prof_loglike <- rep(0, length(phat))
for(i in seq_along(phat)){
   prof_loglike[i] = -mod(phat[i])
}
plot(phat, prof_loglike, ylim = c(-400,-250), xlim = c(.2, .4))

# Evaluate likelihood ratio (difference between the log-likelihood at the MLE and all of 
# the profile log-likelihoods that we just calculated) - We are doing this to calculate
# confidence intervals (an alternative to the ward CIs)
# log-likelihood at the MLE
mle_ll <- -mod(opt$par)
# evaluate the likelihood ratio on the log scale (take difference of the logs)
lr <- mle_ll - prof_loglike

# Want to hone in on where the difference is 1.92 (for a 95% CI)
# 1.92 comes from the chi squared distribution
upper.limit.plike <- max(phat[lr<1.92])
lower.limit.plike <- min(phat[lr<1.92])
proflike.ci <- c(lower.limit.plike, upper.limit.plike)

cbind(wald.ci, proflike.ci)

# The 2 methods produce similar CI's b/c we have a lg number of obs and the probability is 0.3,
# towards the center of the distribution. The likelihood surface is fairly symmetrical
# If we change the proability to 0.1, there will be more and lower the number of observations, 
# there will be a larger difference:


y <- 10 #n=1, prob = .1
opt2 <- optim(par = 0.5, fn = mod, hessian = TRUE, method = "Brent", 
             lower = 0, upper = 1)
var.est2 <- 1/opt2$hessian
upper.limit.wald2 <- opt2$par + qnorm(.975)*sqrt(var.est2) # calculates 95% confidence interval
lower.limit.wald2 <- opt2$par - qnorm(.975)*sqrt(var.est2) # calculates 95% confidence interval
wald.ci2 <- c(lower.limit.wald2, upper.limit.wald2)
prof_loglike2 <- rep(0, length(phat))
for(i in seq_along(phat)){
  prof_loglike2[i] = -mod(phat[i])
}
plot(phat, prof_loglike2, ylim = c(-10,-1), xlim = c(0, .25)) # this likelihood curve isn't symmetrical
lr2 <- -mod(opt2$par) - prof_loglike2
upper.limit.plike2 <- max(phat[lr2<1.92])
lower.limit.plike2 <- min(phat[lr2<1.92])
proflike.ci2 <- c(lower.limit.plike2, upper.limit.plike2)

cbind(wald.ci2, proflike.ci2)
#For this example, likelihood is slightly skewed
curve(dbinom(10,100,x),0,0.25, ylab = 'L(p)', xlab = 'p')
abline(v=.1, col='red')#MLE
#Wald CI:
abline(v=wald.ci2[1], lty=2); abline(v=wald.ci2[2], lty=2)
#Proflike CI: confidence intervals are shifted to the right, accounting for the skew
abline(v=proflike.ci2[1], lty=3, col='blue');abline(v=proflike.ci2[2], lty=3, col='blue')

# There alternative uses of profiling, e.g., as a model diagnostic.

# parameter transformations
mod_tranform <- function(theta) {
    p <- 1/(1+exp(-theta)) # inv logit transformation
   -sum(dbinom(x = y, size = 100, prob = p, log = TRUE))
}