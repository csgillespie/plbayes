
get_output_matrix = function(pars, N) {
  output = matrix(0, nrow=N, ncol=length(pars) + 1)
  
  col_names= c("ll", 
               "alpha1", paste0("lambda1", 1:2), "p1", 
               "alpha2", paste0("lambda2", 1:2), "p2")
  
  colnames(output) = col_names[1:(1 + length(pars))]
  class(output) = class(pars)
  output
}
