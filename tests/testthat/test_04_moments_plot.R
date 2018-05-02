## Creates moments influence plots ##

# Currently not available
testthat::skip("Currently not available")

# Skip on CRAN
skip_on_cran()

# Remove everything
rm(list = ls())

# Libraries
library(bamlss.vis)

### --- Get models and data --- ###
load("models_data.RData")

### --- Make plots --- ###
expect_error(plot_moments(normal_model, int_var = "norm1", expl),
             regexp = NA) # Normal
expect_error(plot_moments(beta_model, int_var = "norm1", expl),
             regexp = NA) # Beta
expect_error(plot_moments(binomial_model, int_var = "norm1", expl),
             regexp = NA) # Binomial
expect_error(plot_moments(cnorm_model, int_var = "norm1", expl),
             regexp = NA) # Cnorm
expect_error(plot_moments(gamma_model, int_var = "norm1", expl),
             regexp = NA) # gamma
expect_error(plot_moments(gpareto_model, int_var = "norm1", expl),
             regexp = NA) # gpareto
expect_error(plot_moments(multinomial_model, int_var = "norm1", expl),
             regexp = NA) # multinomial
suppressWarnings({
  expect_error(plot_moments(mvnorm_model, int_var = "norm2", expl),
               regexp = NA) # multivariate normal
})
expect_error(plot_moments(poisson_model, int_var = "norm1", expl),
             regexp = NA) # poisson
