## -- Clean Up -- ##

# Options to old options
options(warn = 0)

## Context
testthat::context("Clean Up")

# Remove plots file
file.remove("models_data.RData")
file.remove("predictions.RData")
file.remove("Rplots.pdf")
