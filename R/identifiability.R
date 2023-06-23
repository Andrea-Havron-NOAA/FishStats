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


ggplot(df, aes(x=factor(group), y = y)) + geom_boxplot() + theme_bw()
