## Simulate from a zero truncated Poisson
rtpois = function(N, lambda) 
  qpois(runif(N, dpois(0, lambda), 1), lambda)
dtpois = function(x, lambda, log=FALSE) {
  if(log)
    dpois(x, lambda, log=log)-ppois(0, lambda, log=log, lower.tail = FALSE)
  else
    dpois(x, lambda, log=log)/ppois(0, lambda, log=log, lower.tail = FALSE)
}