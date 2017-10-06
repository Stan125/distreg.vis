## Main plot test ##
# Based on all_models_demo.R

# Skip on CRAN
skip_on_cran()

# Install packages if not installed
if (!require(gridExtra))
  install.packages("gridExtra")

# Libraries
library(bamlss.vis)
library(dplyr)
library(gridExtra)

# Set Seed
set.seed(1408)

## --- Generating data --- ##
data_fam <- model_fam_data()
art_data <- GAMart()

### --- Models --- ###

# Beta dist model
beta_model <- bamlss(list(beta ~ norm1 + norm2,
                          sigma2 ~ norm1 + norm2),
                     data = data_fam, family = beta_bamlss())

# Binomial model
binomial_model <- bamlss(list(binomial ~ norm1 + norm2),
                         data = data_fam, family = binomial_bamlss())

# Cnorm model
cnorm_model <- bamlss(list(cnorm ~ norm1 + norm2),
                      data = data_fam, family = cnorm_bamlss())

# Gamma model
gamma_model <- bamlss(list(gamma ~ norm1 + norm2,
                           sigma ~ norm1 + norm2),
                      data = data_fam, family = gamma_bamlss())

# GPareto model
gpareto_model <- bamlss(list(gpareto ~ norm1 + norm2),
                        data = data_fam, family = gpareto_bamlss())

# Poisson model
poisson_model <- bamlss(list(poisson ~ norm1 + norm2),
                        data = data_fam, family = poisson_bamlss())

# Multinomial model
multinomial_model <- bamlss(list(multinomial ~ norm1 + norm2),
                            data = data_fam,
                            family = multinomial_bamlss())

mvnorm_model <- bamlss(list(cbind(num, err) ~ x1 + x2 + x3,
                            sigma1 ~ 1,
                            sigma2 ~ 1), data = art_data,
                       family = mvnorm_bamlss(k = 2))

### --- Predictions --- ###

## Expl Variable
expl <- sample_n(data_fam, 5) %>%
  select(norm1:norm2) %>%
  mutate(intercept = TRUE)
expl_mvnorm <- sample_n(art_data, 5) %>%
  select(x1:x3) %>%
  mutate(intercept = TRUE)

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
mvnorm_p <- bamlss.vis:::preds(mvnorm_model, expl_mvnorm)

## --- Plots --- ###
# Function because I'm lazy
plot_f <- function(model, preds)
  return(list(plot_dist(model, preds),
              plot_dist(model, preds, type = "cdf")))

beta_plots <- plot_f(beta_model, beta_p)
binomial_plots <- plot_f(binomial_model, binomial_p)
cnorm_plots <- plot_f(cnorm_model, cnorm_p)
gamma_plots <- plot_f(gamma_model, gamma_p)
gpareto_plots <- plot_f(gpareto_model, gpareto_p)
poisson_plots <- plot_f(poisson_model, poisson_p)
multinomial_plots <- plot_dist(multinomial_model, multinomial_p)
mvnorm_pdf <- bamlss.vis:::pdfcdf_2d(mvnorm_p, mvnorm_model, type = "pdf")
mvnorm_cdf <- bamlss.vis:::pdfcdf_2d(mvnorm_p, mvnorm_model, type = "cdf")
mvnorm_pdf_contour <- bamlss.vis:::pdfcdf_2d(mvnorm_p, mvnorm_model,
                                             type = "pdf", display = "contour")
mvnorm_pdf_image <- bamlss.vis:::pdfcdf_2d(mvnorm_p, mvnorm_model, type = "pdf",
                                           display = "image")

## --- Show da plots --- ###
expect_error(grid.arrange(grobs = beta_plots), regexp = NA) # beta
expect_error(grid.arrange(grobs = binomial_plots), regexp = NA) # binomial
expect_error(grid.arrange(grobs = cnorm_plots), regexp = NA) # cnorm
expect_error(grid.arrange(grobs = gamma_plots), regexp = NA) # gamma
expect_error(grid.arrange(grobs = gpareto_plots), regexp = NA) # gpareto
expect_error(grid.arrange(grobs = poisson_plots), regexp = NA) # poisson
expect_error(grid.arrange(multinomial_plots), regexp = NA) # multinomial
expect_error(mvnorm_pdf, regexp = NA)
expect_error(mvnorm_cdf, regexp = NA)

