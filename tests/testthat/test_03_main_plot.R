## Creates main distribution plots ##
# Based on all_models_demo.R

## Context
testthat::context("Make Plots")

# Remove everything
rm(list = ls())

# Libraries
library(distreg.vis)
library(dplyr)
library(gridExtra)

### --- Get models and data --- ###
load("models_data.RData")
load("predictions.RData")

## --- Plots --- ###

# Yes this is a loop - don't judge
plots <- list()
plots_cdf <- list()
for (i in seq_len(length(models))) {
  plots[[i]] <- list()
  plots_cdf[[i]] <- list()
  for (j in seq_len(length(models[[i]]))) {
    plots[[i]][[j]] <- plot_dist(models[[i]][[j]], predictions[[i]][[j]])
    plots_cdf[[i]][[j]] <- plot_dist(models[[i]][[j]], predictions[[i]][[j]], type = "cdf")
    cat(paste0(i, ",", j, "\n"))
  }
}

## --- Show da plots --- ###
expect_error(grid.arrange(grobs = do.call("c", plots)), regexp = NA) # normal
expect_error(grid.arrange(grobs = do.call("c", plots_cdf)), regexp = NA) # normal
