### -- This script checks for correct use of preds() -- ###

## Remove everything
rm(list = ls())

## Load package(s) ##
library(bamlss.vis)

## Load data ##
load("models_data.RData")

## --- Predictions --- ##
# Normal
normal_p <- preds(normal_model, expl)

# Beta
beta_p <- bamlss.vis:::preds(beta_model, expl)

# Binomial
binomial_p <- bamlss.vis:::preds(binomial_model, expl)

# Cnorm
cnorm_p <- bamlss.vis:::preds(cnorm_model, expl)

# Gamma
gamma_p <- bamlss.vis:::preds(gamma_model, expl)

# GPareto
gpareto_p <- bamlss.vis:::preds(gpareto_model, expl)

# Poisson
poisson_p <- bamlss.vis:::preds(poisson_model, expl)

# Multinomial model
multinomial_p <- bamlss.vis:::preds(multinomial_model, expl)

# MVnorm model
suppressWarnings({
  mvnorm_p <- bamlss.vis:::preds(mvnorm_model, expl)
})

## --- Save predictions --- ##
save(list = ls()[grepl("_p", ls())], file = "predictions.RData")
