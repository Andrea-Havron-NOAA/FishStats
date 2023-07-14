# Investigate identifiability of models, using anova as an example
# e.g., y = x^2 is not identifiable, b/c x could be neg or pos.
library(ggplot2)

# Simulated data and classic example of an unidentifiable model - 
# an anova example
n.obs <- 100
n.groups <- 5
set.seed(1)
group.dev <- rnorm(n.groups-1) # deviations for each group from the global mean
group.dev[n.groups] <- 0-sum(group.dev)
sum(group.dev)
sd <- 1
global.mean <- 3
group.mean <- global.mean + group.dev
true.par <- c(global.mean, group.dev, log(sd))

y <- lapply(1:n.groups, function(x) rnorm(n.obs, group.mean[x], sd))
df <- data.frame(y = unlist(y), group = rep(1:5, each = n.obs))

# model
#' @param theta a vector
mod <- function(theta) {
    y <- df$y
    grp.id <- df$group
    # beta are the effect terms
    beta0 <- theta[1]
    beta <- theta[2:6] # devation term
    sd <- exp(theta[7])

    nll <- 0
    for(i in 1:length(y)){
        group_mean <- beta0 + beta[grp.id[i]]
        nll = nll - dnorm(y[i], group_mean, sd, TRUE)
    }
    return(nll)
}

# nlminb() is an alternative to optim()
init.par <- c(rep(1, 6), 0)
opt <- nlminb(init.par, mod)

df.est <- data.frame(est = opt$par, true = true.par)
df.est$names <- c("b0", rep("beta", 5), "ln_sig")
df.est$est[df.est$names == "beta"] - df.est$true[df.est$names == "beta"]
df.est$est[df$names=="b0"] - df$true[df$names=="b0"]


cbind(opt$par[1] + opt$par[2:5], group.mean)
est.group.mean <- opt$par[1] + opt$par[2:6]

ggplot() + 
  geom_boxplot(data = df, mapping = aes(x=factor(group), y = y)) + 
  geom_point(aes(x = factor(1:5), y = group.mean, color = 'red')) + 
  theme_bw() + 
  theme(legend.position="none")

ggplot() + 
  geom_boxplot(data = df, mapping = aes(x=factor(group), y = y)) + 
  geom_point(aes(x = factor(1:5), y = group.mean), color = 'red') + 
  geom_point(aes(x = factor(1:5), y = est.group.mean), color = 'blue', shape = 4, size = 2) + 
  theme_bw() + 
  theme(legend.position="none")

plot(est.group.mean, group.mean);abline(0,1)
df.est
cbind(est.group.mean, group.mean)


# model with sum to zero constraints
# don't estimate all beta terms because assuming beta terms will sum to zero
# deviations around the mean are a normal distribution with a mean of zero
# removing a df (removing a parameter in the model)
#' @param theta a vector
mod_sum_to_zero <- function(theta) {
  y <- df$y
  grp.id <- df$group
  # beta are the effect terms
  beta0 <- theta[1]
  beta <- theta[2:5] # devation term
  sd <- exp(theta[6])
  
  nll <- 0
  beta[5] <- 0 - sum(beta) # last beta calculated from the first 4 beta estimates
  for(i in 1:length(y)){
    group_mean <- beta0 + beta[grp.id[i]]
    nll = nll - dnorm(y[i], group_mean, sd, TRUE)
  }
  return(nll)
}

# optimize the model
init.par <- c(rep(1, 5), 0)
opt <- nlminb(init.par, mod_sum_to_zero)
# See the results
est.beta <- opt$par[2:5] # 4 beta terms estimated
est.beta[5] <- 0 - sum(est.beta) #5th beta term estimated
opt$par[1]; true.par[1]
cbind(est.beta, true.par[2:6])
opt$par[6]; true.par[7] #log sd
sum2zero.est <- c(opt$par[1], est.beta, opt$par[6])

# may be able to sum to zero by subtracting from the mean instead
# this method keeps the same number of parameters of the non-identifiable model
# but constrains the space that beta can be in.
# hard to say under which circumstances which of the 2 methods performs
# better.

# model with sum to zero constraints (subtracting mean approach)
#' @param theta a vector (not the same as mod_sum_to_zero)
mod_sum2zero_subtractmean <- function(theta) {
    y <- df$y
    grp.id <- df$group
    # beta are the effect terms
    beta0 <- theta[1]
    beta <- theta[2:6] - mean(theta[2:6])# devation term
    sd <- exp(theta[7])
    
    nll <- 0
    for(i in 1:length(y)){
        group_mean <- beta0 + beta[grp.id[i]]
        nll = nll - dnorm(y[i], group_mean, sd, TRUE)
    }
    return(nll)
}

# nlminb() is an alternative to optim()
init.par <- c(rep(1, 6), 0)
opt_subtractmean <- nlminb(init.par, mod_sum2zero_subtractmean)
subtractmean.est <- c(opt_subtractmean$par[1], 
opt_subtractmean$par[2:6]-mean(opt_subtractmean$par[2:6]), 
opt_subtractmean$par[7])

# See the results
cbind(sum2zero.est, subtractmean.est, true.par)

# Compare to R anova
# this method is using a "baseline constraint". Drops the group mean.
# it is not estimating the global mean, only the group means.
# only estimating one group mean, and the rest are deviations around the group mean.
# This method is useful if you don't care about the global mean term. (often the case with anova
# where there is no pseudo replication)
# a scenario where you would want to know the global mean would be rec devs; we mostly care about the actual
# recruitment values, not just the devs.
r.anova <- lm(df$y ~ factor(df$group))
summary(r.anova)

mod.baseline <- function(theta){
  y <- df$y
  grp.id <- df$group
  # beta0 is the first group mean
  beta0 <- theta[1]
  # the other betas are the deviations off the first group mean
  beta <- theta[2:5]
  group_mean <- c(beta0, beta0 + beta)
  sd <- exp(theta[6])
  
  nll <- 0
  for(i in 1:length(y)){
    nll = nll - dnorm(y[i], group_mean[grp.id[i]], sd, TRUE)
  }
  return(nll)
}

# compare the built in R anove with the mod.baseline. There should be no difference.
# comparing to the other methods that use other constraints needs to be done carefully,
# because the parameterization of this model is different (no group mean estimated, only one group
# mean estimated and the rest of the group means are deviations around the first group mean)
init.par <- c(rep(1,5), 0)
opt.baseline <- nlminb(init.par, mod.baseline)
cbind(r.anova$coefficients[1:5], opt.baseline$par[1:5])
baseline.groupmean <- c(opt.baseline$par[1], 
  opt.baseline$par[1] + opt.baseline$par[2:5]) 
cbind(baseline.groupmean, group.mean)
