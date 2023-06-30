# Investigate identifiability of models
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
df.est$est[df$names == "beta"] - df$true[df$names == "beta"]
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


# model
#' @param theta a vector
mod_sum_to_zero <- function(theta) {
  y <- df$y
  grp.id <- df$group
  # beta are the effect terms
  beta0 <- theta[1]
  beta <- theta[2:5] # devation term
  sd <- exp(theta[6])
  
  nll <- 0
  beta[5] <- 0 - sum(beta)
  for(i in 1:length(y)){
    group_mean <- beta0 + beta[grp.id[i]]
    nll = nll - dnorm(y[i], group_mean, sd, TRUE)
  }
  return(nll)
}

init.par <- c(rep(1, 5), 0)
opt <- nlminb(init.par, mod_sum_to_zero)
est.beta <- opt$par[2:5]
est.beta[5] <- 0 - sum(est.beta)
opt$par[1]; true.par[1]
cbind(est.beta, true.par[2:6])
opt$par[6]; true.par[7]
