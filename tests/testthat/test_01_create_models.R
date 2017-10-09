## Creates all models, used for test_main_plot and test_moments ##
# Based on all_models_demo.R

# Remove everything
rm(list = ls())

# Skip on cran
skip_on_cran()

# Libraries
library(bamlss.vis)
library(dplyr)

# Warnings are errors
options(warn = 2)

# Set Seed
set.seed(1408)

## --- Generating data --- ##
data_fam <- model_fam_data()

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
suppressWarnings({
  mvnorm_model <- bamlss(list(normal ~ norm1 + norm2,
                              norm1 ~ norm2 + normal),
                         data = data_fam,
                         family = mvnorm_bamlss(k = 2))
})

### --- Predictions --- ###

## Expl Variable
expl <- sample_n(data_fam, 5) %>%
  select(norm1:norm2) %>%
  mutate(intercept = TRUE)

### --- Save models --- ###
save(file = "models_data.RData",
     list = c(ls()[grepl("model", ls())]), "data_fam", "expl")

