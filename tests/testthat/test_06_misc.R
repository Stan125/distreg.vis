## Misc Testing ##

# Remove everything
rm(list = ls())

# Libraries
library(dplyr)

### -- Mult_trans --- ###

# Get models
load("models_data.RData")
load("predictions.RData")

# Transform predictions of multinomial data
multinomial_p1 <- multinomial_p %>%
  sample_n(1)

m_one <- bamlss.vis:::mult_trans(multinomial_p1, multinomial_model)
m_two <- bamlss.vis:::mult_trans(multinomial_p, multinomial_model)
expect_equal(class(m_one), "data.frame")
expect_equal(class(m_two), "data.frame")

