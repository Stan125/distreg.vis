### -- This script checks for correct use of moments() -- ###

## Load package(s) ##
library(bamlss.vis)

## - Tests - ##

# Gaussian
preds <- data.frame(mu = -2:2, sigma = 1:5, row.names = paste0("P", 1:5))
moms <- moments(preds, gaussian_bamlss()$family)
expect_equal(row.names(preds), row.names(moms))
