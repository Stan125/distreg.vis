## Demo: Visualise without Shiny App
library(bamlss.vis)
library(dplyr)
library(ggplot2)

# Data
art_data <- GAMart()

# Model 1 - only numeric covariates
model_num <- bamlss(list(num ~ s(x1) + s(x2) + s(x3),
                         sigma ~ x1 + x2 + x3), data = art_data)

# Predictions: pdf
p <- bamlss.vis:::plot_dist(model_num, sample_n(art_data, 5) %>%
                              mutate(intercept = TRUE), type = "pdf")
# Base Plot
p

# Predictions: cdf
p <- bamlss.vis:::plot_dist(model_num, sample_n(art_data, 5) %>%
                              mutate(intercept = TRUE), type = "cdf")
# Base Plot
p

# With colorbrewer palettes
p + scale_fill_brewer(palette = "Accent")
p + scale_fill_brewer(palette = "Dark2")
p + scale_fill_brewer(palette = "Paired")
p + scale_fill_brewer(palette = "Pastel1")
p + scale_fill_brewer(palette = "Pastel2")
p + scale_fill_brewer(palette = "Set1")
p + scale_fill_brewer(palette = "Set2")
p + scale_fill_brewer(palette = "Set3")
