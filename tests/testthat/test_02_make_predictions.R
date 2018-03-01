### -- This script checks for correct use of preds() -- ###

## Remove everything
rm(list = ls())

## Load package(s) ##
library(bamlss.vis)
library(gamlss)

# Warnings are errors
options(warn = 2)

## Load data ##
load("models_data.RData")

## --- Predictions --- ##
# All but mvnorm model
models <- ls()[grepl("model_", ls())]
models <- models[models != "mvnorm_model"] # MVnorm prediction separately
predictions <- list()
for (i in 1:length(models)) {
  predictions[[i]] <- bamlss.vis::preds(get(models[i]), newdata = expl)
  expect_true(any(class(predictions[[i]]) == "data.frame")) # Did drop = FALSE work?
}

# MVnorm model
suppressWarnings({
  mvnorm_p <- bamlss.vis:::preds(mvnorm_model, expl)
})

## --- Save predictions & other things --- ##
save(list = "predictions", file = "predictions.RData")

# Warnings are errors
options(warn = 0)
