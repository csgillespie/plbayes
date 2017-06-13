library("ggplot2")
library("poweRlaw")
source("R/simulate_data.R")
source("graphics/R/helper.R")
o = readRDS(file="output/simulation.rds")
o1 = o[[1]]
o1 = o1[1001:11000,]
mypalette(1)
## Predictions
(pars = c(2.2, 0.007, 0.05, 0.19))


fname = "graphics/output/figure3.pdf"
w= h = 7/4
pdf(fname, width=w*4, height=h*3)
setnicepar(mfrow=c(2, 2))

hist(o1[,2], breaks="fd", ylim=c(0, 600), 
     xlim=c(1.9, 2.5), main=NULL, xlab=expression(alpha), 
     col="grey60", border="white");
add_interval(o1[,2], 550, pars[1], TRUE)
text(2.45, 590, "(a)")

hist(o1[,3], breaks="fd", 
     ylim=c(0, 820),  xlim=c(0, 0.025), 
     main=NULL, xlab=expression(lambda), 
     col="grey60", border="white");
add_interval(o1[,3], 750, pars[2])
text(pars[2], 765, "True value",  adj = c(0, 0), cex=0.7)
text(0.023, 750, "(b)")

hist(o1[,4], breaks=60, 
     ylim=c(0, 600), 
     xlim=c(0, 0.12), main=NULL, xlab=expression(mu), 
     col="grey60", border="white");
add_interval(o1[,4], 600, pars[3], FALSE)
text(0.11, 600, "(c)")


hist(o1[,5], breaks="fd", 
     ylim=c(0, 550), 
     xlim=c(0.13, 0.27), main=NULL, xlab="p", 
     col="grey60", border="white");
add_interval(o1[,5], 500, pars[4], FALSE)
text(0.255, 500, "(d)")
dev.off()
system(paste("pdfcrop", fname))



