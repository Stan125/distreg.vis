## Creates main distribution plots ##
# Based on all_models_demo.R

# Skip on CRAN
skip_on_cran()

# Remove everything
rm(list = ls())

# Libraries
library(bamlss.vis)
library(dplyr)
library(gridExtra)

### --- Get models and data --- ###
load("models_data.RData")
load("predictions.RData")

## --- Plots --- ###
# Function because I'm lazy
plot_f <- function(model, preds)
  return(list(plot_dist(model, preds),
              plot_dist(model, preds, type = "cdf")))

normal_plots <- plot_f(normal_model, normal_p)
beta_plots <- plot_f(beta_model, beta_p)
binomial_plots <- plot_f(binomial_model, binomial_p)
cnorm_plots <- plot_f(cnorm_model, cnorm_p)
gamma_plots <- plot_f(gamma_model, gamma_p)
gpareto_plots <- plot_f(gpareto_model, gpareto_p)
poisson_plots <- plot_f(poisson_model, poisson_p)
multinomial_plots <- plot_dist(multinomial_model, multinomial_p)
suppressWarnings({
  mvnorm_pdf <- bamlss.vis:::pdfcdf_2d(mvnorm_p, mvnorm_model, type = "pdf")
  mvnorm_cdf <- bamlss.vis:::pdfcdf_2d(mvnorm_p, mvnorm_model, type = "cdf")
  mvnorm_pdf_palette <- bamlss.vis:::pdfcdf_2d(mvnorm_p, mvnorm_model, palette = "Spectral")
  mvnorm_pdf_contour_palette <- bamlss.vis:::pdfcdf_2d(
    mvnorm_p, mvnorm_model, palette = "Spectral",
    type = "pdf", display = "contour")
  mvnorm_pdf_image <- bamlss.vis:::pdfcdf_2d(mvnorm_p, mvnorm_model, type = "pdf",
                                             display = "image")
})

## --- Show da plots --- ###
expect_error(grid.arrange(grobs = normal_plots), regexp = NA) # normal
expect_error(grid.arrange(grobs = beta_plots), regexp = NA) # beta
expect_error(grid.arrange(grobs = binomial_plots), regexp = NA) # binomial
expect_error(grid.arrange(grobs = cnorm_plots), regexp = NA) # cnorm
expect_error(grid.arrange(grobs = gamma_plots), regexp = NA) # gamma
expect_error(grid.arrange(grobs = gpareto_plots), regexp = NA) # gpareto
expect_error(grid.arrange(grobs = poisson_plots), regexp = NA) # poisson
expect_error(grid.arrange(multinomial_plots), regexp = NA) # multinomial
suppressWarnings({
  expect_error(mvnorm_pdf, regexp = NA)
  expect_error(mvnorm_cdf, regexp = NA)
  expect_error(mvnorm_pdf_palette, regexp = NA)
  expect_error(mvnorm_pdf_contour_palette, regexp = NA)
  expect_error(mvnorm_pdf_contour, regexp = NA)
  expect_error(mvnorm_pdf_image, regexp = NA)
})


