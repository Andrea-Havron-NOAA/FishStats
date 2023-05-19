curve(dbinom(30,100,x),0,0.75, ylab = 'L(p)', xlab = 'p')
# get 100 obs
y <- rbinom(n = 100, size = 100, prob = .3)

mod <- function(theta) {
   -sum(dbinom(x = y, size = 100, prob = p, log = TRUE))
}

# find theta
opt <- optim(par = 0, fn = mod, hessian = TRUE)
# get confidence intervals
var.est <- 1/opt$hessian #estimated variance is the inverse of hessian
sqrt(var.est) #standard error (se): matches value from Likelihood Review
upper.limit <- opt$par + qnorm(.975)*sqrt(var.est) #calculates 95% confidence interval
lower.limit <- opt$par - qnorm(.975)*sqrt(var.est) #calculates 95% confidence interval
c(lower.limit, upper.limit)

# parameter transformations
mod <- function(theta) {
    p <- 1/(1+exp(-theta)) # logit transformation
   -sum(dbinom(x = y, size = 100, prob = p, log = TRUE))
}