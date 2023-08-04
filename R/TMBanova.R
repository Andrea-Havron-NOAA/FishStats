install.packages("TMB")
library(TMB)
TMB::compile("R/anova.cpp")

# Simulated data and classic example of an unidentifiable model - 
# an anova example
# simulate data 
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
# set up for tmb
Data <- list(y = df$y, group = df$group-1)
Par <- list(beta = rep(0,5), ln_sd = 0)

dyn.load(TMB::dynlib("R/anova"))
obj <- TMB::MakeADFun(data = Data, parameters = Par, DLL = "anova")
# find MLE
opt <- nlminb(obj$par, obj$fn, obj$gr)
opt$par

