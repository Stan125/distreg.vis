dists <- readxl::read_excel("inst/extdata/dist_df.xlsx")
dists <- as.data.frame(dists)
save(list = "dists", file = "data/dists.rda")
