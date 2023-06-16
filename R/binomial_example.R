curve(dbinom(30,100,x),0,0.75, ylab = 'L(p)', xlab = 'p')
# get 100 obs
y <- rbinom(n = 100, size = 100, prob = .1)

mod <- function(theta) {
   -sum(dbinom(x = y, size = 100, prob = theta, log = TRUE))
}

# find theta (use Brent, the prefered method when fitting a single parameter)
opt <- optim(par = 0.5, fn = mod, hessian = TRUE, method = "Brent", 
lower = 0, upper = 1)
# get confidence intervals
var.est <- 1/opt$hessian # estimated variance is the inverse of hessian
sqrt(var.est) # standard error (se): matches value from Likelihood Review
# Wald CI (assumes normal distribution)
upper.limit.wald <- opt$par + qnorm(.975)*sqrt(var.est) # calculates 95% confidence interval
lower.limit.wald <- opt$par - qnorm(.975)*sqrt(var.est) # calculates 95% confidence interval
c(lower.limit.wald, upper.limit.wald)

# Calculate a profile likelihood
phat <- seq(0.001, .999, by = 0.00001)
prof_loglike <- rep(0, length(phat))
for(i in seq_along(phat)){
   prof_loglike[i] = -mod(phat[i])
}
plot(phat, prof_loglike)

# Evaluate likelihood ratio (difference between the log-likelihood at the MLE and all of 
# the profile log-likelihoods that we just calculated) - We are doing this to calculate
# confidence intervals (an alternative to the ward CIs)
# log-likelihood at the MLE
mle_ll <- -mod(opt$par)
# evaluate the likelihood ratio on the log scale (take difference of the logs)
lr <- mle_ll - prof_loglike

# Want to hone in on where the difference is 1.92 (for a 95% CI)
upper.limit.plike <- max(phat[lr<1.92])
lower.limit.plike <- min(phat[lr<1.92])
c(lower.limit.plike, upper.limit.plike)

# The 2 methods produce similar CI's b/c we have a lg number of obs and the probability is 0.3,
# towards the center of the distribution. If we change the proability to 0.1, there will be more
# of a difference

# There alternative uses of profiling, e.g., as a model diagnostic.

# parameter transformations
mod_tranform <- function(theta) {
    p <- 1/(1+exp(-theta)) # inv logit transformation
   -sum(dbinom(x = y, size = 100, prob = p, log = TRUE))
}