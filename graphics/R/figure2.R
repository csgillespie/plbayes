source("graphics/R/helper.R")
f = function(theta, x)
  1-exp(-theta[1] - theta[2]*(x-1))

theta1  = c(0.1, 1)
theta2  = c(0.1, 0.1)
theta3  = c(0.1, 0.01)
x = seq(1, 100)
y1 = f(theta1, x)
y2 = f(theta2, x)
y3 = f(theta3, x)


fname = "graphics/output/figure2.pdf"
pdf(fname, width=6, height=4)

setnicepar()
mypalette(1)
plot(x, y1, type="l", col=4, 
     ylim=c(0, 1), panel.first=grid(), 
     ylab="Pr(Observing a conflict)", 
     xlab="No. of Casualties in a Conflict")
lines(x, y2, col=2, lty=2)
lines(x, y3, col=3, lty=3)

text(9.8, 0.9, expression(paste(mu, " = 1.0")), col=4)
text(18, 0.65, expression(paste(mu, " = 0.1")), col=2)
text(38, 0.24, expression(paste(mu, " = 0.01")), col=3)
dev.off()
system(paste("pdfcrop", fname))

