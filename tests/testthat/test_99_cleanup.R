## -- Clean Up -- ##

# Options to old options
options(warn = 0)

## Context
testthat::context("Clean Up")

## Now remove all plots
to_rm <- list.files()[grepl("plot_", list.files())]
file.remove(to_rm)
