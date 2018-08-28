library("dplyr")
library("readxl")
suppressWarnings({
  dists <- read_excel("inst/extdata/dist_df.xlsx") %>%
    as.data.frame() %>%
    mutate_at(vars(l_limit:u_limit), .funs = as.numeric)
})
write.table(dists, file = "inst/extdata/dist_df.csv",
            sep = ";", dec = ".", row.names = FALSE)
save(list = "dists", file = "data/dists.rda")
