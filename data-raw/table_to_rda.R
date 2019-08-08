dists <-
  read.table(
    "inst/extdata/dist_df.csv",
    sep = ";",
    dec = ".",
    header = TRUE,
    stringsAsFactors = FALSE
  )
correct_readin <-
  sapply(dists$dist_name, FUN = function(x) {
    if (grepl("[ ]", x))
      return(FALSE)
    else
      return(TRUE)
  })
if (!all(correct_readin)) {
  stop("Distribution names not correctly read-in. Often problem with Excel.")
} else {
  save(list = "dists", file = "data/dists.rda")
}

