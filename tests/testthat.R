library(testthat)
library(bamlss.vis)

test_check("bamlss.vis")

## -- Clean Up -- ##

# Remove plots file
file <- "tests/testthat/Rplots.pdf"
if (file.exists(file))
  file.remove(file)
file.remove("tests/testthat/models_data.RData")
