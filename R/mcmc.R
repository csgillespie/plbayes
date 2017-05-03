library("MASS")
source("R/truncated_poisson.R")
source("R/ll.R")
source("R/get_freq.R")
source("R/output_matrix.R")
#' @export
mcmc = function(pars, cov_mat, obser, N, 
                thin=1, kern=function(theta) 1 - exp(-theta), 
                verbose=TRUE, initialise=NULL, ...) {
  ## rm is just me being defensive. Not actually needed.
  pars_cur = pars;rm(pars)

  if(!is.list(cov_mat)) cov_mat = list(cov_mat)
  if(is.null(initialise)) initialise = obser
  if(!is.data.frame(initialise) && NCOL(initialise) !=2) stop("Initialise error")

  x_cur = data.frame(force = initialise$force, true=initialise$cas, stringsAsFactors = FALSE)#, pois=x, rounded=x, types=types)
  x_prop = update_x(x_cur, obser)
  (log_ll_cur = ll(x_prop, pars_cur, obser))
  
  prior_cur = get_prior(pars_cur)
  
  output = get_output_matrix(pars_cur, N/thin)
  output[1,] = c(log_ll_cur, pars_cur)
  accept = 0
  
  proposed_us = list()
  proposed_nat = list()
  proposed_us[[1]] = table(x_cur[x_cur$force == "us", ]$true)
  proposed_nat[[1]] = table(x_cur[x_cur$force == "nat", ]$true)
  i = 2
  for(i in 2:N) {
    pars_prop = update_parameters(pars_cur, cov_mat)
    x_prop = update_x(x_cur, obser)
    
    log_ll = ll(x_prop, pars_prop, obser) 
    prior = get_prior(pars_prop)
    
    ratio = log_ll - log_ll_cur + prior - prior_cur + attr(x_prop, "trans")

    if(is.finite(ratio) && ratio > log(runif(1))) {
      log_ll_cur = log_ll
      pars_cur = pars_prop
      prior_cur = prior
      x_cur = x_prop
      accept = accept + 1
    } 
    
    if(!(i %% thin)) {
      output[i/thin,] = c(log_ll_cur, pars_cur)  
      proposed_us[[i/thin]] = table(x_cur[x_cur$force == "us", ]$true)
      proposed_nat[[i/thin]] = table(x_cur[x_cur$force == "nat", ]$true)
    }
    
    if(verbose && !(i %% ceiling(N/100)))
      message(i, ": ", paste(c(signif(pars_cur, 4), 
                               signif(log_ll_cur), signif(log_ll), 
                               signif(attr(x_prop, "trans"))), collapse=" "))
  }
  if(verbose) message("acceptance = ", signif(accept/N*100, 2), "%")
  class(output) = c("plmcmc_mat", class(output))
  attr(output, "kern") = kern
  
  if(verbose && require("beepr")) beep(6)
  return(list(output=output, proposed_us = proposed_us, proposed_nat = proposed_nat))
}

