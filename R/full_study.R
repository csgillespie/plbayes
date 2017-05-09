## Load packages and data ----
library("poweRlaw") # install.packages("poweRlaw")
compiler::enableJIT(3) # Redundant in R 3.4.0
source("R/mcmc.R")
#library("MASS")
data("native_american", package = "poweRlaw")
data("us_american", package = "poweRlaw")

## Use previous run for tuning ----
output_diff = readRDS(file="output/combined_mcmc.rds")[[1]]
colMeans(output_diff)
o1 = output_diff

## US forces
(p1 = colMeans(output_diff[,2:5]))
cov_mat = suppressWarnings(cov(output_diff[, 2:5])/4)
cov_mat[4,] = 0; cov_mat[,4] = 0; 
cov_mat[4, 4] = 0.005
c1 = cov_mat

## Native American
(p2 = colMeans(output_diff[,6:9]))
cov_mat = cov(output_diff[, 6:9])/5
cov_mat[4,] = 0; cov_mat[,4] = 0; 
cov_mat[4, 4] = 0.05
c2 = cov_mat

# c1 = matrix(c(0.0042, -0.00032, -0.0011, -2.2e-05, 
#                    -0.00032, 3e-05, 8.8e-05, -2.6e-07, 
#                    -0.0011, 8.8e-05, 0.00041, 2.2e-05, 
#                    -2.2e-05, -2.6e-07, 2.2e-05, 0.02), 
#                  nrow=4)

## Combine cov_mats into a single list
cov_mat = list(c1, c2)
#output_diff = o$output
## Combine data into single object
obser = data.frame(cas = c(sort(us_american$Cas), sort(native_american$Cas)), 
               force = rep(c("us", "nat"), c(NROW(us_american), NROW(native_american))))

## Initial starting values
set.seed(42) 
N = 3100000;  thin = 100
#N = 20000;  thin = 1
pars = c(p1, p2)
system.time(o <- mcmc(pars, cov_mat, obser,
         N = N, thin = thin,  verbose=TRUE) )

saveRDS(o, file="output/combined_mcmc.rds")
#o = readRDS(file="output/combined_mcmc.rds")
o1 = o[[1]]
## Sanity MCMC plots
source("graphics/R/helper.R")
setnicepar()
plot(o1[,1], type="l");

setnicepar(mfrow=c(2, 2))
plot(o1[,2], type="l");abline(h=mean(output_diff[,2]), col=2)
plot(o1[,6], type="l");abline(h=mean(output_diff[,6]), col=2)
plot(o1[,3], type="l");abline(h=mean(output_diff[,3]), col=2)
plot(o1[,7], type="l");abline(h=mean(output_diff[,7]), col=2)

setnicepar(mfrow=c(2, 2))
plot(o1[,4], type="l");abline(h=mean(output_diff[,4]), col=2)
plot(o1[,8], type="l");abline(h=mean(output_diff[,8]), col=2)
plot(o1[,5], type="l");abline(h=mean(output_diff[,5]), col=2)
plot(o1[,9], type="l"); abline(h=mean(output_diff[,9]), col=2)

setnicepar(mfrow=c(2, 2))
hist(o1[,2], breaks="fd");hist(o1[,6], breaks="fd")
hist(o1[,3], breaks="fd");hist(o1[,7], breaks="fd")

setnicepar(mfrow=c(2, 2))
hist(o1[,4], breaks="fd");hist(o1[,8], breaks="fd")
hist(o1[,5], breaks="fd");hist(o1[,9], breaks="fd")

