### Try out all models ###
### all-models demo ###
library(bamlss.vis)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Set Seed
set.seed(1408)

## --- Generating data --- ##
data_fam <- model_fam_data()
art_data <- GAMart()

### --- Models --- ###

# Normal model
normal_model <- bamlss(list(normal ~ norm1 + norm2,
                            sigma ~ norm1 + norm2),
                       data = data_fam, family = gaussian_bamlss())

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

# MVnorm model
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

# Normal
normal_p <- preds(normal_model, expl)

# Beta
beta_p <- preds(beta_model, expl)

# Binomial
binomial_p <- preds(binomial_model, expl)

# Cnorm
cnorm_p <- preds(cnorm_model, expl)

# Gamma
gamma_p <- preds(gamma_model, expl)

# GPareto
gpareto_p <- preds(gpareto_model, expl)

# Poisson
poisson_p <- preds(poisson_model, expl)

# Multinomial model
multinomial_p <- preds(multinomial_model, expl)

# MVnorm model
mvnorm_p <- preds(mvnorm_model, expl_mvnorm)

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
mvnorm_pdf <- bamlss.vis:::pdfcdf_2d(mvnorm_p, mvnorm_model, type = "pdf")
mvnorm_cdf <- bamlss.vis:::pdfcdf_2d(mvnorm_p, mvnorm_model, type = "cdf")
mvnorm_pdf_contour <- bamlss.vis:::pdfcdf_2d(mvnorm_p, mvnorm_model,
                                             type = "pdf", display = "contour")
mvnorm_pdf_image <- bamlss.vis:::pdfcdf_2d(mvnorm_p, mvnorm_model, type = "pdf",
                                     display = "image")

## --- Show da plots --- ###
grid.arrange(grobs = normal_plots) # normal
grid.arrange(grobs = beta_plots) # beta
grid.arrange(grobs = binomial_plots) # binomial
grid.arrange(grobs = cnorm_plots) # cnorm
grid.arrange(grobs = gamma_plots) # gamma
grid.arrange(grobs = gpareto_plots) # gpareto
grid.arrange(grobs = poisson_plots) # poisson
grid.arrange(multinomial_plots) # multinomial
mvnorm_pdf
mvnorm_cdf

