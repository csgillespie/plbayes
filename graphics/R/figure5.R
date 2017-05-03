library(ggplot2)
library("poweRlaw")
data("us_american", package="poweRlaw")
real = us_american$Cas
source("R/simulate_data.R")
source("graphics/R/helper.R")
mypalette(1)

## Load data
o = readRDS(file="output/combined_mcmc.rds")
o2 = o[[1]]
o1 = o2[1001:21000,]



w= h = 7/4
fname = "graphics/output/figure5a.pdf"
pdf(fname, width=w*3, height=h)
setnicepar(mfrow=c(1, 3), cex=0.6, cex.axis = 0.7, mgp=c(1.5, 0.3, 0))

hist(o1[,2], breaks=80, ylim=c(0, 1000), 
     xlim=c(1.8, 2.6), main=NULL, xlab=expression(alpha[U]), 
     col="grey60", border="white");
add_interval(o1[,2],  970, text=FALSE)
text(2.55, 990, "(a)", cex=0.5)

hist(o1[,3], breaks=60, ylim=c(0, 2750), 
     xlim=c(0, 1.5), main=NULL, xlab=expression(lambda[U]), 
     col="grey60", border="white");
add_interval(o1[,3], 2700)
text(1.4, 2640, "(b)", cex=0.5)

hist(o1[,4], breaks=100, ylim=c(0, 5000), 
     xlim=c(0, 1.5), main=NULL, xlab=expression(mu[U]), 
     col="grey60", border="white");
add_interval(o1[,4], 4800)
text(1.45, 4850, "(c)", cex=0.5)

dev.off()
system(paste("pdfcrop", fname))

fname = "graphics/output/figure5b.pdf"
pdf(fname, width=w*3, height=h)
setnicepar(mfrow=c(1, 3), cex=0.6, cex.axis = 0.7, mgp=c(1.5, 0.3, 0))
hist(o1[,6], breaks=40, 
     ylim=c(0, 2250), 
     xlim=c(1.8, 2.6), main=NULL, xlab=expression(alpha[N]), 
     col="grey60", border="white");
add_interval(o1[,6],  2150, text=FALSE)
text(2.55, 2200, "(d)", cex=0.5)


hist(o1[,7], breaks=50, ylim=c(0, 1500), 
     xlim=c(0, 0.03), main=NULL, xlab=expression(lambda[N]), 
     col="grey60", border="white");
add_interval(o1[,7], 1400)
text(0.028, 1450, "(e)", cex=0.5)

hist(o1[,8], breaks=40, ylim=c(0, 3000), 
     xlim=c(0, 0.15), main=NULL, xlab=expression(mu[N]), 
     col="grey60", border="white");
add_interval(o1[,8], 2850)
text(0.14, 2900, "(f)", cex=0.5)
dev.off()
system(paste("pdfcrop", fname))


fname = "graphics/output/figure5c.pdf"
pdf(fname, width=w*2, height=h)
setnicepar(mfrow=c(1, 2), cex=0.6, cex.axis = 0.7, mgp=c(1.5, 0.3, 0))
hist(o1[,5], breaks=60, ylim=c(0, 2000), 
     xlim=c(0, 0.20), main=NULL, xlab=expression(p[U]), 
     col="grey60", border="white");
add_interval(o1[,5],  1970)
text(0.2, 1990, "(g)", cex=0.5)

hist(o1[,9], breaks=60, ylim=c(0, 1000), 
     xlim=c(0.125, 0.275), main=NULL, xlab=expression(p[N]), 
     col="grey60", border="white");
add_interval(o1[,9],  970)
text(0.275, 990, "(h)", cex=0.5)
dev.off()
system(paste("pdfcrop", fname))

#########################
# Estimate Xmin
#########################
p = colMeans(o1)
sams = sample(1:NROW(o1), 1000)
i = 1
us = numeric(length(sams))
nat = numeric(length(sams))
for(i in 1:length(sams)){
  p = o1[sams[i],]  
  us[i] = which(1-exp(-p[3] - p[4]*(0:1000)) > 0.95)[1] + 1
  nat[i] = which(1-exp(-p[7] - p[8]*(0:1000)) > 0.95)[1] + 1
}
hist(us)
round(mean(us), 0)
round(mean(nat), 0)

