library("ggplot2")
theme_set(theme_bw())

mypalette(1, alpha=100)
o = readRDS(file="output/combined_mcmc.rds")
o2 = o[[1]]
o1 = o2[1001:21000,]

set.seed(1)
samples = sample(1:NROW(o1), 100)
x = (1:100)
i = 2
prob_u = numeric(length(x) * length(samples))
prob_n = numeric(length(x) * length(samples))

for(i in 1:length(samples)){
  (pars = o[[1]][samples[i],2:5])
  start = (i-1)*length(x)+1
  end = (i)*length(x)
  prob_u[start:end] = 1-exp(-pars[2] - pars[3]*(x-1))
  (pars = o[[1]][samples[i],6:9])

  prob_n[start:end] = 1-exp(-pars[2] - pars[3]*(x-1))
}

ms = colMeans(o1)
prob_u_mean = 1-exp(-ms[3] - ms[4]*(x-1))
prob_n_mean = 1-exp(-ms[7] - ms[8]*(x-1))
dd_mean = data.frame(prob=c(prob_u_mean, prob_n_mean), x=x, 
                     type=rep(c("US American", "Native American"), each=length(x)))

dd = data.frame(prob=c(prob_u, prob_n), x=x, 
                type=rep(c("US American", "Native American"), each=length(samples)*length(x)), 
                group=rep(1:length(samples), each=length(x)))


g= ggplot(dd) + 
  geom_line(aes(x, prob, group=group), alpha=0.05) + 
  facet_grid(~type)  
g1 = g + geom_line(data=dd_mean, aes(x, prob), col=3, lwd=1.5, alpha=1) + 
  ylab("Observation probability") + 
  xlab("No' of casualties")

fname = "graphics/output/figure4.pdf"
pdf(fname, width=8, height=4)
print(g1)
dev.off()
system(paste("pdfcrop", fname))
