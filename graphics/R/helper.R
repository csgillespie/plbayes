est_xmin = function(pars) {
  x = 1:100
  p = 1 - exp(-pars[2] - pars[3]*(x-1))
  which(p > 0.95)[1]
}

add_interval = function(d, top,true=NULL,  text=FALSE){
  qs = quantile(d, c(0.025, 0.975))
  if(text)  text(qs[1], top+45, "95% interval", col=4, pos=4, cex=0.6)
  segments(qs[1], top, qs[2], top, col=4)
  segments(qs[1], top, qs[1], top-50, col=4, lty=3)
  segments(qs[1], 0, qs[1], 50, col=4, lty=3)
  segments(qs[2], top, qs[2], top-50, col=4, lty=3)
  segments(qs[2], 0, qs[2], 50, col=4, lty=3)
  
  if(!is.null(true))  points(true, top, pch=21, bg=3, col="white", cex=1.2)
}

setnicepar = function(mar=c(3,3,2,1), 
                      mgp=c(2,0.4,0), tck=-.01,
                      cex.axis=0.9, las=1, mfrow=c(1,1),...) {
  par(mar=mar, 
      mgp=mgp, tck=tck,
      cex.axis=cex.axis, las=las,mfrow=mfrow,...)
}


mypalette = function(set = 0, alpha=255) {
  if(set ==1 ) {#I want hue - pimp
    palette(c(rgb(200,79,178, alpha=alpha,maxColorValue=255), 
              rgb(105,147,45, alpha=alpha, maxColorValue=255),
              rgb(85,130,169, alpha=alpha, maxColorValue=255),
              rgb(204,74,83, alpha=alpha, maxColorValue=255),
              rgb(183,110,39, alpha=alpha, maxColorValue=255),
              rgb(131,108,192, alpha=alpha, maxColorValue=255),
              rgb(63,142,96, alpha=alpha, maxColorValue=255)))
  } else {
    message("Setting Default palette")
    palette("default")
  }
  
}