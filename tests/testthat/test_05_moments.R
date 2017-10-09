### -- This script checks for correct use of moments() -- ###

## Remove everything
rm(list = ls())

## Load package(s) ##
library(bamlss.vis)

## Load data ##
load("predictions.RData")

## - Tests - ##

# Gaussian - missing
beta_m <- moments(beta_p, family = beta_bamlss()$family)
