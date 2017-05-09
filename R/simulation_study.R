library("poweRlaw") # install.packages("poweRlaw") ----
compiler::enableJIT(3) # Redundant in R 3.4.0

source("R/simulate_data.R")
source("R/mcmc.R")

## Simulation parameters alpha, lambda, mu, p ----
pars = c(2.2, 0.007, 0.05, 0.19)

## Simulate data ----
## Should match paper
simulation = simulate_data(n = 20000, pars, seed = 1)

## Total number of casulaties
colSums(simulation[[1]])[1]
## Total number of observed events
colSums(simulation[[1]])[2]

## Total number of observed casulaties
colSums(simulation[[2]])[3]

## Random walk parameters. 
## Obtained from pilot run
cov_mat = matrix(c(0.0042, -0.00032, -0.0011, -2.2e-05, 
                   -0.00032, 3e-05, 8.8e-05, -2.6e-07, 
                   -0.0011, 8.8e-05, 0.00041, 2.2e-05, 
                   -2.2e-05, -2.6e-07, 2.2e-05, 0.02), 
                           nrow=4)

## Only a single force
## The "force" needs to be unique. It could be called anything
obser = data.frame(cas = simulation[[2]][, "rounded"], force = "us")
N = 1100000; thin = 100
#N = 10000; thin = 1
## Discard first 100000
output = mcmc(pars, cov_mat, obser, N = N, thin = thin)

saveRDS(output, file="output/simulation.rds")

## Quick and dirty posteriors
par(mfrow=c(2, 2))
hist(output[[1]][,2], breaks = "fd"); points(pars[1], 0, col=2)
hist(output[[1]][,3], breaks = "fd",freq=F); points(pars[2], 0, col=2)
hist(output[[1]][,4], breaks = "fd"); points(pars[3], 0, col=2)
hist(output[[1]][,5], breaks = "fd"); points(pars[4], 0, col=2)

o = output
samples = 1:1000
est = data.frame(true=numeric(length(samples)), observed = 0)
for(i in 1:length(samples)){
  (s_pars = o[[1]][samples[i],2:5])
  y = o[[2]][[samples[i]]]
  freq = as.vector(y); value = as.numeric(names(y))
  (true = sum(1/(1-exp(-s_pars[2] - s_pars[3]*(value-1))) * freq*value))
  (observed = sum(add_noise(y=rep(value, freq),  p=s_pars[4])[,3]))
  est[i,] = c(true, observed) 
}

par(mfrow=c(1, 2))
hist(est[,1], breaks="fd"); points(colSums(simulation[[1]])[1], 0, bg=2, pch=21)
hist(est[,2], breaks="fd");points(colSums(simulation[[2]])[3], 0, bg=2, pch=21)

