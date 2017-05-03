library("ggplot2")
library("poweRlaw")
source("R/simulate_data.R")
source("graphics/R/helper.R")
o = readRDS(file="output/simulation.rds")
o1 = o[[1]]
dim(o1)
o1 = o1[1001:11000,]
mypalette(1)

## Predictions
(pars = c(2.2, 0.007, 0.05, 0.19))

dd = simulate_data(20000, pars, seed=1)
sum(dd[[2]][,3])

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

fname = "graphics/output/figure3a.pdf"
w= h = 7/4
pdf(fname, width=w*3, height=h*2)
setnicepar(mfrow=c(2, 2))

hist(o1[,2], breaks="fd", ylim=c(0, 500), 
     xlim=c(1.9, 2.5), main=NULL, xlab=expression(alpha), 
     col="grey60", border="white");
add_interval(o1[,2], 450, pars[1], TRUE)
text(2.45, 490, "(a)")

hist(o1[,3], breaks="fd", 
     ylim=c(0, 800),  xlim=c(0, 0.03), 
     main=NULL, xlab=expression(lambda), 
     col="grey60", border="white");
add_interval(o1[,3], 750, pars[2])
text(pars[2], 760, "True value", col=3, adj = c(0, 0), cex=0.9)
text(0.028, 750, "(b)")

hist(o1[,4], breaks=60, 
     ylim=c(0, 600), 
     xlim=c(0, 0.1), main=NULL, xlab=expression(mu), 
     col="grey60", border="white");
add_interval(o1[,4], 600, pars[3], FALSE)
text(0.09, 600, "(c)")


hist(o1[,5], breaks="fd", 
     ylim=c(0, 550), 
     xlim=c(0.13, 0.27), main=NULL, xlab="p", 
     col="grey60", border="white");
add_interval(o1[,5], 500, pars[4], FALSE)
text(0.255, 500, "(d)")
dev.off()
system(paste("pdfcrop", fname))

fname = "graphics/output/figure3b.pdf"
pdf(fname, width=4, height=4)
setnicepar(mfrow=c(1, 1))

hist(est[,1], breaks=80, 
     ylim=c(0, 1500), 
     xlim=c(30000, 150000), main=NULL, xlab="Total Casualities", 
     col="grey60", border="white");
true = sum(dd[[1]]$x)
add_interval(est[,1], 1500, true)

dev.off()
system(paste("pdfcrop", fname))


## Supplemental graphic 1 --------

fname = "graphics/output/supp1.pdf"
pdf(fname, width=4, height=4)
setnicepar(mfrow=c(1,1))

hist(est[,2], breaks="fd" , 
     xlim=c(30000,32000), 
     main=NULL, xlab="Observed Casualities", 
     col="grey60", border="white", ylim=c(0, 800));

add_interval(est[,2], 770, sum(dd[[2]]$rounded))

dev.off()
system(paste("pdfcrop", fname))





