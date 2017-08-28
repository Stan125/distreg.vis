### Try out all models ###
### all-models demo ###
library(bamlss.vis)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Set Seed
set.seed(1408)

## --- Generating data --- ##

## Get three equi-correlated uniform distributions
t2u <- function(x) ifelse(x<1, x^2, 2-(2-x)^2)/2
u_data <- data.frame(u1 = runif(500),
                     u2 = runif(500),
                     u3 = runif(500))
u_data <- with(u_data, data.frame(v1 = t2u(u1+u2),
                                v2 = t2u(u1+u3),
                                v3 = t2u(u2+u3)))


# Beta distribution, alpha = 2, beta = 5
beta_data <- data.frame(beta = qbeta(u_data$v1, 2, 5),
                        norm1 = qnorm(u_data$v2, 3, 5),
                        norm2 = qnorm(u_data$v3, 10, 15))

# Binomial distribution, pi = 0.8
binomial_data <- data.frame(binomial = qbinom(u_data$v1, 1, 0.8),
                            norm1 = qnorm(u_data$v2, 3, 5),
                            norm2 = qnorm(u_data$v3, 10, 15))

# Cnorm distribution, mu = 5, sigma = 2
cnorm_data <- data.frame(cnorm = cnorm_bamlss()$q(u_data$v1,
                                                  par = list(mu = 5, sigma = 2)),
                         norm1 = qnorm(u_data$v2, 3, 5),
                         norm2 = qnorm(u_data$v3, 10, 15))

# Gamma distribution, shape = 9, scale = 0.5
gamma_data <- data.frame(gamma = qgamma(u_data$v1, shape = 9, scale = 0.5),
                         norm1 = qnorm(u_data$v2, 3, 5),
                         norm2 = qnorm(u_data$v3, 10, 15))

# General Pareto distribution, sigma = 1, xi = 0.4
gpareto_data <- data.frame(gpareto = gpareto_bamlss()$q(u_data$v1,
                                                        list(sigma = 1, xi = 0.4)),
                           norm1 = qnorm(u_data$v2, 3, 5),
                           norm2 = qnorm(u_data$v3, 10, 15))

### --- Models --- ###

# Beta dist model
beta_model <- bamlss(list(beta ~ norm1 + norm2,
                          sigma2 ~ norm1 + norm2),
                     data = beta_data, family = beta_bamlss())

# Binomial model
binomial_model <- bamlss(list(binomial ~ norm1 + norm2),
                         data = binomial_data, family = binomial_bamlss())

# Cnorm model
cnorm_model <- bamlss(list(cnorm ~ norm1 + norm2),
                      data = cnorm_data, family = cnorm_bamlss())

# Gamma model
gamma_model <- bamlss(list(gamma ~ norm1 + norm2,
                           sigma ~ norm1 + norm2),
                      data = gamma_data, family = gamma_bamlss())

# GPareto model
gpareto_model <- bamlss(list(gpareto ~ norm1 + norm2),
                        data = gpareto_data, family = gpareto_bamlss())

### --- Predictions --- ###

## Expl Variable
expl <- sample_n(beta_data, 5) %>%
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

## --- Show da plots --- ###
grid.arrange(grobs = beta_plots) # beta
grid.arrange(grobs = binomial_plots) # binomial
grid.arrange(grobs = cnorm_plots) # cnorm
grid.arrange(grobs = gamma_plots) # gamma
grid.arrange(grobs = gpareto_plots) # gpareto
