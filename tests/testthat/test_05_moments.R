### -- This script checks for correct use of moments() -- ###

## Remove everything
rm(list = ls())

## Context
testthat::context("Compute Moments")

## Load package(s) ##
library(distreg.vis)

## Load data ##
load("predictions.RData")
load("models_data.RData")

## - Tests - ##
for (i in seq_len(length(predictions))) {
  for (j in seq_len(length(predictions[[i]]))) {
    if (fam_obtainer(models[[i]][[j]]) == "multinomial")
      next
    moments(predictions[[i]][[j]], fam_obtainer(models[[i]][[j]]))
  }
}
