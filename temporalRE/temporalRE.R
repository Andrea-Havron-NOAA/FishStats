#AR1 model example using temporal.cpp

library(TMB)
compile("temporalRE/temporal.cpp")
#make TMB model available to the R environment
dyn.load(dynlib("temporalRE/temporal"))

# Simulate data
N <- 100
re_sd <- 1
obs_sd <- 1
growth_rate <- .7

set.seed(123)
expected_re <- rep(0,N) # fit with initial condition starting at one
re <- rep(1,N)
for(i in 2:N){
    expected_re[i] <- growth_rate * re[i-1]
    re[i] <- rnorm(1, expected_re[i], re_sd)
}

# plot time series
plot(1:N,re, type='l')
abline(h=0)

y <- rnorm(N, re, obs_sd)

# plot re with obs on top
plot(1:N,re, type='l')
lines(1:N, y, col = 'red')

# Set up TMB model using simulated data
Dat <- list(y = y)
Par <- list(growth_rate = 0.5,
            log_sigma_obs = 0,
            log_sigma_re = 0,
            re = rep(1,N)) # initialize parameters in TMB model; typically initiate re in AR1 models with 1 because initializing it with 0 can cause some issues

# Set up MakeADFun - set up AD part of TMB model
# this is the computational graph or "tape"
obj <- MakeADFun(Dat, Par, 
                random = "re", # the name of the random effect parameter
vector                DLL = "temporal")
obj$fn()
obj$gr() # gradients = slopes of likelihood at parameter values
obj$env$last.par.best # run though model based on initial conditions, calculate values, and return everything
obj$par # returns all initial conditions of the model; only returns fixed effects of the model (not the random effect), everything else is a random effect

# Optimize obj with nlminb (a minimizer - for each param, it's starts at initial conditions and randomly inserting values, re-eval likelihoods and seeing if there is improvement)
opt <- stats::nlminb(obj$par, obj$fn, obj$gr)  # starting param values, objective function, gradients (informs minimizer on which direction to go)
opt$par
exp(opt$par[2:3])
sdr <- sdreport(obj) # get estimated parameters
summary(sdr)
0.625043780 + 1.96* 0.1876653 
