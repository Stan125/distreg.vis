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
predictions <- lapply(models[names(models) != "mvnorm"], FUN = function(family_models) {
  lapply(family_models, FUN = function(model) {
    predictions <- bamlss.vis::preds(model, newdata = expl)
    expect_true(any(class(predictions) == "data.frame"))
    return(predictions)
  })
})

# # MVnorm model
# suppressWarnings({
#   mvnorm_p <- bamlss.vis:::preds(models$mvnorm$bamlss, expl)
#   expect_true(any(class(mvnorm_p) == "data.frame"))
#   predictions$mvnorm$bamlss <- mvnorm_p
# })

## --- Save predictions & other things --- ##
save(list = "predictions", file = "predictions.RData")

# Warnings are errors
options(warn = 0)
