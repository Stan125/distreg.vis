## Creates all models, used for test_main_plot and test_moments ##
# Based on all_models_demo.R

# Remove everything
rm(list = ls())

# Skip on cran
skip_on_cran()

# Libraries
library(bamlss.vis)
library(gamlss)
library(dplyr)

# Set Seed
set.seed(1408)

## --- Generating data --- ##
data_fam <- model_fam_data()

### --- Models - bamlss w/ bamlss family --- ###

models <- list(
  normal = list(bamlss = bamlss(list(normal ~ norm1 + norm2,
                                     sigma ~ norm1 + norm2),
                                data = data_fam, family = gaussian_bamlss()),
                hybrid = bamlss(list(normal ~ norm1 + norm2,
                                     sigma ~ norm1 + norm2),
                                data = data_fam, family = gamlss.dist::NO()),
                gamlss = gamlss(normal ~ norm1 + norm2,
                                ~ norm1 + norm2, data = data_fam,
                                family = gamlss.dist::NO(),
                                trace = FALSE)),
  beta = list(bamlss = bamlss(list(beta ~ norm1 + norm2,
                                   sigma2 ~ norm1 + norm2),
                              data = data_fam, family = beta_bamlss())),
  binomial = list(bamlss = bamlss(list(binomial ~ norm1 + norm2),
                                  data = data_fam, family = binomial_bamlss())),
  cnorm = list(bamlss = bamlss(list(cnorm ~ norm1 + norm2),
                               data = data_fam, family = cnorm_bamlss())),
  gamma = list(bamlss = bamlss(list(gamma ~ norm1 + norm2,
                                    sigma ~ norm1 + norm2),
                               data = data_fam, family = gamma_bamlss())),
  gpareto = list(bamlss = bamlss(list(gpareto ~ norm1 + norm2),
                                 data = data_fam, family = gpareto_bamlss())),
  poisson = list(bamlss = bamlss(list(poisson ~ norm1 + norm2),
                                 data = data_fam, family = poisson_bamlss()),
                 hybrid = bamlss(list(poisson ~ norm1 + norm2),
                                 data = data_fam, family = gamlss.dist::PO()),
                 gamlss = gamlss(poisson ~ norm1 + norm2,
                                 data = data_fam, family = gamlss.dist::PO())),
  multinomial = list(bamlss = bamlss(list(multinomial ~ norm1 + norm2),
                                     data = data_fam,
                                     family = multinomial_bamlss())),
  mvnorm = list(bamlss = suppressWarnings({
    bamlss(list(normal ~ norm2,
                norm1 ~ norm2),
           data = data_fam,
           family = mvnorm_bamlss(k = 2))
  }))
)

### --- Expl variables for predictions --- ###

## Expl Variable
expl <- sample_n(data_fam, 5) %>%
  select(norm1:norm2)

### --- Save models --- ###
save(file = "models_data.RData",
     list = c("models", "expl", "data_fam"))

