## Creates all models, used for test_main_plot and test_moments ##
# Based on all_models_demo.R

# Remove everything
rm(list = ls())

# Libraries
library(bamlss.vis)
library(dplyr)

# Warnings are errors
options(warn = 2)

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

suppressWarnings({
  mvnorm_model <- bamlss(list(cbind(num, err) ~ x1 + x2 + x3,
                            sigma1 ~ 1,
                            sigma2 ~ 1), data = art_data,
                       family = mvnorm_bamlss(k = 2))
})

### --- Predictions --- ###

## Expl Variable
expl <- sample_n(data_fam, 5) %>%
  select(norm1:norm2) %>%
  mutate(intercept = TRUE)
expl_mvnorm <- sample_n(art_data, 5) %>%
  select(x1:x3) %>%
  mutate(intercept = TRUE)


### --- Save models --- ###
save(file = "models_data.RData",
     list = c(ls()[grepl("model|expl", ls())]), "art_data", "data_fam")

