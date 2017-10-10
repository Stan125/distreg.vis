## Demo: Shiny App
library(bamlss.vis)

# Data
data_fam <- model_fam_data()

# Models
beta_model <- bamlss(list(beta ~ norm1 + norm2,
                          sigma2 ~ norm1 + norm2),
                     data = data_fam, family = beta_bamlss())
poisson_model <- bamlss(list(poisson ~ norm1 + norm2),
                        data = data_fam, family = poisson_bamlss())
mvnorm_model <- bamlss(list(normal ~ norm2,
                            norm1 ~ norm2), data = data_fam,
                       family = mvnorm_bamlss(k = 2))

# Start the App
bamlss.vis::vis()
