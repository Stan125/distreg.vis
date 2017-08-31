#' Internal: Function to check whether a family is continuous or not
#'
is.continuous <- function(family) {
  continuous_families <- c("beta", "cox", "cnorm", "gaussian",
                           "gaussian2", "gamma", "mvnorm",
                           "mvnormAR1", "Generalized Pareto")
  discrete_families <- c("poisson", "binomial", "multinomial")
  if (family %in% continuous_families)
    return(TRUE)
  else if (family %in% discrete_families)
    return(FALSE)
  else
    stop("Family not implemented")
}
