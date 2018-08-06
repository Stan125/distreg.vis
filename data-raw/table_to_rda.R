dists <- read.table("inst/extdata/dist_df.csv", header = TRUE, sep = ";")
save(list = "dists", file = "data/dists.rda")
