## Demo: Shiny App
library(bamlss.vis)

# Data
data_fam <- model_fam_data()
art_data <- GAMart()

# Models
beta_model <- bamlss(list(beta ~ norm1 + norm2,
                          sigma2 ~ norm1 + norm2),
                     data = data_fam, family = beta_bamlss())
gamma_model <- bamlss(list(gamma ~ norm1 + norm2,
                           sigma ~ norm1 + norm2),
                      data = data_fam, family = gamma_bamlss())
mvnorm_model <- bamlss(list(cbind(num, err) ~ x1 + x2 + x3,
                            sigma1 ~ 1,
                            sigma2 ~ 1), data = art_data,
                       family = mvnorm_bamlss(k = 2))

# Start the App
bamlss.vis::vis()
