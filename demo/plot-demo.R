## Demo: Visualise without Shiny App
library(bamlss.vis)

# Data
art_data <- GAMart()

# Model 1 - only numeric covariates
model_num <- bamlss(list(num ~ s(x1) + s(x2) + s(x3),
                         sigma ~ x1 + x2 + x3), data = art_data)

# Predictions:
bamlss.vis:::plot_dist(model_num, select(art_data, x1:x3) %>%
                         summarise_all(funs(mean)) %>%
                         mutate(intercept = TRUE))
