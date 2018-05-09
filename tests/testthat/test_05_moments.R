### -- This script checks for correct use of moments() -- ###

# Currently not available
testthat::skip("Currently not available")

## Remove everything
rm(list = ls())

## Load package(s) ##
library(distreg.vis)

## Load data ##
load("predictions.RData")

## - Tests - ##

# Gaussian - missing
beta_m <- moments(beta_p, family = beta_bamlss()$family)
