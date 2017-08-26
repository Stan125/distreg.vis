### Try out all models ###
### all-models demo ###
library(bamlss.vis)
library(dplyr)
library(ggplot2)

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

### --- Models --- ###

# Beta dist model
beta_model <- bamlss(list(beta ~ norm1 + norm2,
                          sigma2 ~ norm1 + norm2),
                     data = beta_data, family = beta_bamlss())

# Binomial model
binomial_model <- bamlss(list(binomial ~ norm1 + norm2),
                         data = binomial_data, family = binomial_bamlss())

### --- Predictions --- ###
# Beta
beta_p <- bamlss.vis:::preds(beta_model, sample_n(beta_data, 3) %>%
                               select(norm1:norm2) %>%
                               mutate(intercept = TRUE))
# Binomial
binomial_p <- bamlss.vis:::preds(binomial_model, sample_n(binomial_data, 5) %>%
                                   select(norm1:norm2) %>%
                                   mutate(intercept = TRUE))


## --- Plots --- ###
plot_dist(beta_model, beta_p)
plot_dist(binomial_model, binomial_p)


