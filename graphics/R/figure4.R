library("ggplot2")
library("poweRlaw")
source("R/simulate_data.R")
source("graphics/R/helper.R")
o = readRDS(file="output/simulation.rds")
o1 = o[[1]]
o1 = o1[1001:11000,]

## Predictions
(pars = c(2.2, 0.007, 0.05, 0.19))

dd = simulate_data(20000, pars, seed=1)

set.seed(1)
samples = sample(1:NROW(o1))
i = 1
est = data.frame(true=numeric(length(samples)), observed = 0)
for(i in 1:length(samples)){
  (s_pars = o[[1]][samples[i],2:5])
  y = o[[2]][[samples[i]]]
  freq = as.vector(y); value = as.numeric(names(y))
  (true = sum(1/(1-exp(-s_pars[2] - s_pars[3]*(value-1))) * freq*value))
  (observed = sum(add_noise(y=rep(value, freq),  p=s_pars[4])[,3]))
  est[i,] = c(true, observed) 
}


fname = "graphics/output/figure4.pdf"
pdf(fname, width=4, height=4)
setnicepar(mfrow=c(1, 1))

hist(est[,1], breaks="fd", 
     ylim=c(0, 800), 
     xlim=c(30000, 120000), main=NULL, xlab="Total Casualities", 
     col="grey60", border="white");
true = sum(dd[[1]]$x)
add_interval(est[,1], 800, true)
dev.off()
system(paste("pdfcrop", fname))


## Supplemental graphic 1 --------

fname = "graphics/output/supp1.pdf"
pdf(fname, width=4, height=4)
setnicepar(mfrow=c(1,1))

hist(est[,2], breaks="fd" , 
     xlim=c(24000, 26000), 
     main=NULL, xlab="Observed Casualities", 
     col="grey60", border="white", ylim=c(0, 400));

add_interval(est[,2], 400, sum(dd[[2]]$rounded))

dev.off()
system(paste("pdfcrop", fname))



