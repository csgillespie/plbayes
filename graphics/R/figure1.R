library("scales")
library("ggplot2")
library("poweRlaw")
source("graphics")
data("native_american")
data("us_american")
theme_set(theme_bw())

mypalette(1, alpha=100)
native_american$type = "Native American"
us_american$type = "US American"
dd = rbind(native_american, us_american)
g1 = ggplot(dd, aes(as.POSIXct(Date), Cas)) + geom_point(alpha=0.3) +
  facet_grid(~type) + 
  xlab("Date") + ylab("Casualties")
g1

m1 = displ$new(us_american$Cas)
dd_m1 = plot(m1); dd_m1$type ="US American" 

m2 = displ$new(native_american$Cas)
dd_m2 = plot(m2)
dd_m2$type = "Native American"
dd = rbind(dd_m1, dd_m2)
head(dd)

g2 = ggplot(dd, aes(x, y)) + 
  geom_point() + 
  scale_y_continuous(trans = 'log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x))) +
  scale_x_continuous(trans = 'log10',
                     breaks = trans_breaks('log10', function(x) 10^x, n=4),
                     labels = trans_format('log10', math_format(10^.x))) + 
  ylab("1-CDF") + facet_grid(~type)  + xlab("Casualties")
g2

fname = "graphics/output/figure1a.pdf"
pdf(fname, width=8, height=4)
g1
dev.off()
system(paste("pdfcrop", fname))

fname = "graphics/output/figure1b.pdf"
pdf(fname, width=8, height=4)
g2
dev.off()
system(paste("pdfcrop", fname))

