## Creates moments influence plots ##

# Remove everything
rm(list = ls())

## Context
testthat::context("Make Moments Plots")

# Libraries
library(distreg.vis)

### --- Get models and data --- ###
load("models_data.RData")

### --- Make plots --- ###
plots <- lapply(models, FUN = function(x) {
  lapply(x, FUN = function(y) {
    plot_moments(y, int_var = "norm1", expl)
  })
})
expect_error(gridExtra::grid.arrange(grobs = do.call("c", plots)), regexp = NA)

# suppressWarnings({
#   expect_error(plot_moments(mvnorm_model, int_var = "norm2", expl),
#                regexp = NA) # multivariate normal
# })
