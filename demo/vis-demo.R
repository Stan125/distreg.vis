library(bamlss.vis)
# Data
art_data <- GAMart()

# Model 1 - only numeric covariates
model_num <- bamlss(list(num ~ s(x1) + s(x2) + s(x3),
                     sigma ~ x1 + x2 + x3), data = art_data)

# Model 2 - including categorical covariates
model_cat <- bamlss(list(num ~ s(x1) + s(x2) + cat,
                         sigma ~ x1 + x2 + cat), data = art_data)

yo <- art_data %>% select(x1:x2) %>% summarise_all(funs(mean))
yo$cat <- "none"
yo <- fac_equ(base_df, yo)
bamlss.vis:::plot_dist(model_cat, cbind(yo, intercept = TRUE))

# Start the App
bamlss.vis::vis()
