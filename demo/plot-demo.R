## Demo: Visualise without Shiny App
library(bamlss.vis)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Data
art_data <- GAMart()

# Model 1: Gaussian family
model_num <- bamlss(list(num ~ x1 + x2 + x3), data = art_data)

# Predictions: pdf
predictions <- bamlss.vis:::preds(model_num, sample_n(art_data, 5) %>%
                       mutate(intercept = TRUE) %>%
                       select(x1:x3, intercept))
pdf <- plot_dist(model_num, predictions, type = "pdf")
cdf <- plot_dist(model_num, predictions, type = "cdf")

# Plots
grid.arrange(pdf, cdf)



