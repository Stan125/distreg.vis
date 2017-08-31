## Demo: Shiny App
library(bamlss.vis)

# Data
data_fam <- model_fam_data()

# Models
beta_model <- bamlss(list(beta ~ norm1 + norm2,
                          sigma2 ~ norm1 + norm2),
                     data = data_fam, family = beta_bamlss())
gamma_model <- bamlss(list(gamma ~ norm1 + norm2,
                           sigma ~ norm1 + norm2),
                      data = data_fam, family = gamma_bamlss())

# Start the App
bamlss.vis::vis()
