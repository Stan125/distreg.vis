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

### --- Predictions --- ###

## Expl Variable
expl <- sample_n(data_fam, 5) %>%
  select(norm1:norm2) %>%
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

## --- Show da plots --- ###
grid.arrange(grobs = beta_plots) # beta
grid.arrange(grobs = binomial_plots) # binomial
grid.arrange(grobs = cnorm_plots) # cnorm
grid.arrange(grobs = gamma_plots) # gamma
grid.arrange(grobs = gpareto_plots) # gpareto
grid.arrange(grobs = poisson_plots) # poisson
grid.arrange(multinomial_plots) # multinomial
