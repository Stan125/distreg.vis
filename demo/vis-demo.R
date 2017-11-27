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
mvnorm_model <- bamlss(list(normal ~ s(norm2),
                            norm1 ~ s(norm2)), data = data_fam,
                       family = mvnorm_bamlss(k = 2))
multinomial_model <- bamlss(list(multinomial ~ norm1 + norm2 + binomial1),
                            data = data_fam,
                            family = multinomial_bamlss())

# Start the App
bamlss.vis::vis()
