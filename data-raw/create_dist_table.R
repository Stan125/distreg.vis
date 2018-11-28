library(gamlss.dist)
library(bamlss)

### how many gamlss.dist families are there? ###
funs <- ls(envir = as.environment("package:gamlss.dist"))
funs <- funs[!grepl("d", funs)]
funs <- funs[!grepl("p", funs)]
funs <- funs[!grepl("q", funs)]
funs <- funs[!grepl("r", funs)]
funs <- funs[-c(1, 2)] # exclude helper functions
funs <- funs[!grepl("mean", funs)]
funs <- funs[!grepl("[.]", funs)]
funs <- funs[-which(funs %in% c("checklink", "flexDist", "get_C", "GetBI_C", "Family"))]
dist_df <- data.frame(dist_name = funs,
                      class = "gamlss",
                      implemented = rep(FALSE, length(funs)),
                      varying_limits = rep(NA, length(funs)),
                      l_limit = rep(NA, length(funs)),
                      u_limit = rep(NA, length(funs)),
                      type = rep(NA, length(funs)))
dist_df$dist_name <- as.character(dist_df$dist_name)
dist_df$type <- sapply(dist_df$dist_name, FUN = function(x) {
 f <- get(x, envir = as.environment("package:gamlss.dist"))
 try(fam <- f(), silent = TRUE)
 if (exists("fam"))
   return(fam$type)
 else
   return(NA)
})
test <- sapply(dist_df$dist_name, FUN = function(x) {
  f <- get(x, envir = as.environment("package:gamlss.dist"))
  try(fam <- f(), silent = TRUE)
  if (exists("fam"))
    return(fam$family[1] == x)
})
test <- unlist(test)
which(!test)

### bamlss families
funs_b <- ls(envir = as.environment("package:bamlss"))
funs_b <- funs_b[grepl("_bamlss", funs_b)]
funs_b_name <- sapply(funs_b, FUN = function(x)
  return(unlist(strsplit(x, "_"))[1]))
dist_df2 <- data.frame(dist_name = as.character(funs_b_name),
                       class = "bamlss",
                       implemented = rep(TRUE, length(funs_b)),
                       varying_limits = rep(TRUE, length(funs_b)),
                       l_limit = rep(NA, length(funs_b)),
                       u_limit = rep(NA, length(funs_b)),
                       type = rep(NA, length(funs_b)))

dist_compl <- rbind(dist_df, dist_df2)

## Write table
write.table(dist_compl, sep = ",", file = "dist_df.csv", row.names = FALSE)
