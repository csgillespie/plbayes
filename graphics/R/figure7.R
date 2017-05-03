library("poweRlaw")
data("us_american", package="poweRlaw")
data("native_american", package="poweRlaw")
source("graphics/R/helper.R")
source("R/simulate_data.R")

o = readRDS(file="output/combined_mcmc.rds")
o2 = o[[1]]
o1 = o2[1001:21000,]

set.seed(1)
samples = sample(1:NROW(o1), 1000)
i = 1
est = data.frame(true_us=numeric(length(samples)), obs_us = 0, true_nat=0, obs_nat=0, 
                 us_ntrue = 0, nat_ntrue = 0)
for(i in 1:length(samples)){
  (pars = o[[1]][samples[i],2:5])
  y = o[[2]][[samples[i]]]
  freq = as.vector(y); value = as.numeric(names(y))
  true_us = sum(1/(1-exp(-pars[2] - pars[3]*(value-1))) * freq*value)
  obs_us = sum(add_noise(rep(value, freq),  p=pars[4])[,3])
  us_ntrue = sum(1/(1-exp(-pars[2] - pars[3]*(value-1)))*freq)
  
  (pars = o[[1]][samples[i],6:9])
  y = o[[3]][[samples[i]]]
  freq = as.vector(y); value = as.numeric(names(y))
  true_nat = sum(1/(1-exp(-pars[2] - pars[3]*(value-1))) * freq*value)
  obs_nat = sum(add_noise(rep(value, freq),  p=pars[4])[,3])
  nat_ntrue = sum(1/(1-exp(-pars[2] - pars[3]*(value-1)))*freq)
  est[i,] = c(true_us, obs_us, true_nat, obs_nat, us_ntrue, nat_ntrue) 
}

# Actual vs observed battles
mean(est[,5]);nrow(us_american)
mean(est[,6]);nrow(native_american)

real_us = sum(us_american$Cas)
real_nat = sum(native_american$Cas)

fname = "graphics/output/figure7.pdf"
w= h = 7/4
pdf(fname, width=8, height=4)
mypalette(1)
setnicepar(mfrow=c(1, 2))
hist(est[,1], breaks="fd", 
     ylim=c(0, 150), xlim=c(10000, 25000), 
     main=NULL, xlab="Total Casualities", 
     col="grey60", border="white")
add_interval(est[,1], 140)
text(25000, 138, "(a)")

hist(est[,3], breaks="fd", 
     ylim=c(0, 150), xlim=c(35000, 150000), 
     main=NULL, xlab="Total Casualities", 
     col="grey60", border="white")
add_interval(est[,3], 140)
text(150000, 138, "(b)")
dev.off()
system(paste("pdfcrop", fname))


## Supplemental graphic 2 --------

fname = "graphics/output/supp2.pdf"
pdf(fname, width=8, height=4)
setnicepar(mfrow=c(1,2))
hist(est[,2], breaks="fd", xlim=c(10000, 11000), 
     main=NULL, xlab="Observed Casualities", 
     col="grey60", border="white", ylim=c(0, 160));
add_interval(est[,2], 160, real_us)
text(11000, 148, "(a)")

hist(est[,4], breaks="fd", xlim=c(24800, 26800), 
     main=NULL, xlab="Observed Casualities", 
     col="grey60", border="white", ylim=c(0, 150));
add_interval(est[,4], 140, real_nat)
text(26500, 138, "(b)")

dev.off()
system(paste("pdfcrop", fname))
