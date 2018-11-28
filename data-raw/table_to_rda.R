dists <-
  read.table(
    "inst/extdata/dist_df.csv",
    sep = ",",
    dec = ".",
    header = TRUE,
    stringsAsFactors = FALSE
  )
save(list = "dists", file = "data/dists.rda")
