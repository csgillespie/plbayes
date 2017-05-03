# Estimating the number of casualties in the American Indian war

This repository contains the __R__ code for the paper, 

> Estimating the number of casualties in the American Indian war: a Bayesian analysis using the power law distribution

This paper is currently under revision in Annals of Applied Statistics.

## Data

The American Indian data set can be obtained from the `poweRlaw` package

```{r}
install.packages("poweRlaw")
data("us_american", package = "poweRlaw")
data("native_american", package = "poweRlaw")
```

The output directory contains the results from MCMC runs.

## Graphics

All code to reproduce the figures in the paper is contained in the `graphics` directory.

  * `R/` directory: R code. The files are named an obvious way.
  I've used the linux tool pdfcrop to crop the resulting pdf files. If
  you don't this tool, just comment out the line.
  
  * `output/` directory: pdf files. The create pdf files are saved in this directory.
  
## Reproducing the results

All code used in the paper is found in the R directory. The main files are

 * `R/simulation_study.R`
 * `R/full_study.R`