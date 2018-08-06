dists <- readxl::read_excel("inst/extdata/dist_df.xlsx")
save(list = "dists", file = "data/dists.rda")
