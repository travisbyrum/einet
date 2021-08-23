# einet

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/einet)](https://CRAN.R-project.org/package=einet)
[![R build status](https://github.com/travisbyrum/einet/workflows/R-CMD-check/badge.svg)](https://github.com/travisbyrum/einet/actions)
[![DOI](https://zenodo.org/badge/251196370.svg)](https://zenodo.org/badge/latestdoi/251196370)

<!-- badges: end -->

# Effective information and causal emergence in R

**Team members:** Travis Byrum, Anshuman Swain, Brennan Klein and William F Fagan

R code/package for calculating effective information in networks. This can then be used to search for macroscale representations of a network such that the coarse grained representation has more effective information than the microscale, a phenomenon known as causal emergence (see Klein and Hoel, 2020).

## Web App

A [shiny application](https://einet.shinyapps.io/einet/) is available for demonstration purposes.

## Basic usage in R

`library(devtools)`
`install_github("travisbyrum/einet") #installation`
`library(igraph)`
`library(einet)`
`set.seed(123)`
`karate_ce <- causal_emergence(karate) #using the karate club network provided within the package`
`karate_ce #displays all the relevant information about effective information at micro and macro scales, the effectiveness (normalized effective information at micro-scale and teh causal emergence`


