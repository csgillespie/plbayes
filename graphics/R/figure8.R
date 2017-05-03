library("poweRlaw")
library(ggplot2)
library(gridExtra)
source("R/simulate_data.R")
source("graphics/R/helper.R")
data("us_american", package="poweRlaw")
data("native_american",package="poweRlaw")
set.seed(1)
theme_set(theme_bw())
N = 1000

## Lognormal
o1 = readRDS(file="output/combined_ln_mcmc.rds")
samples = sample(1:NROW(o1[[1]]), N)
i = 1
est_ln = data.frame(true_us=numeric(length(samples)), true_nat=0)
for(i in 1:length(samples)){
  (pars = o1[[1]][samples[i],2:6])
  y = o1[[2]][[samples[i]]]
  freq = as.vector(y); value = as.numeric(names(y))
  (true_us = sum(1/(1-exp(-pars[2] - pars[3]*(value-1))) * freq*value))
  (obs_us = sum(add_noise(rep(value, freq),  p=pars[4])[,3]))
  
  (pars = o1[[1]][samples[i],7:11])
  y = o1[[3]][[samples[i]]]
  freq = as.vector(y); value = as.numeric(names(y))
  (true_nat = sum(1/(1-exp(-pars[2] - pars[3]*(value-1))) * freq*value))
  (obs_nat = sum(add_noise(rep(value, freq),p=pars[4])[,3]))
  est_ln[i,] = c(true_us, true_nat) 
}
mean(est[,1])
mean(est_ln[,1])

median(est[,3])
median(est_ln[,2])


setnicepar(mfrow=c(1, 2))
plot(est_ln[,1], type="l")
plot(est_ln[,2], type="l")


fname = "graphics/output/figure8.pdf"
pdf(fname, width=8, height=4)

## A bit of data manipulation for plotting ----
tmp = data.frame(values = c(est[,1], est[,3]), 
                 force = rep(c("US Forces", "Native American Forces"), each=nrow(est)),
                 type="Power law",
                 stringsAsFactors = FALSE)
tmp1 = data.frame(values = c(est_ln[,1], est_ln[,2]), 
                 force = rep(c("US Forces", "Native American Forces"), each=nrow(est_ln)),
                 type = "Lognormal",
                 stringsAsFactors = FALSE)
dd = rbind(tmp, tmp1)


g1 = ggplot(dd[dd$force == "US Forces",]) + 
  geom_boxplot(aes(type, values)) + 
  facet_wrap(~force) + ylim(c(0, 40000)) + 
  xlab(NULL) + ylab("Total Casualities")

g2 = ggplot(dd[dd$force == "Native American Forces",]) + 
  geom_boxplot(aes(type, values)) + 
  facet_wrap(~force) + 
  xlab(NULL) + ylab("Total Casualities") + 
  scale_y_log10(limits = c(1e2, 1e8), breaks=c(1e2, 1e4, 1e6, 1e8), 
                labels=c(expression(10^2), expression(10^4), expression(10^6), expression(10^8)))
grid.arrange(g1, g2, ncol=2)
dev.off()
system(paste("pdfcrop", fname))


mean(est[,1])
mean(est_ln[,1])
mean(est[,3])
mean(est_ln[,2])


median(est[,3])
median(est_ln[,2])




## Plots of p_u and p_n (not shown in the paper) ----
setnicepar(mfrow=c(1, 2))
hist(o1[[1]][,5], breaks=60, freq=FALSE, ylim=c(0, 22),
     main=NULL, xlab=expression(p[U]), 
     col="grey60", border="white")

hist(o1[[1]][,10], breaks=60, freq=FALSE, ylim=c(0, 22),
     main=NULL, xlab=expression(p[N]), 
     col="grey60", border="white")

