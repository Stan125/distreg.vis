#' Internal: Function to check whether a family is continuous or not
#'
#' @keywords internal
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

#' Internal: Function to check whether the modeled response is bivariate
#'
#' @keywords internal

is.2d <- function(family, links) {
  if (length(links) == 5 & family == ".mvnorm")
    return(TRUE)
  else
    return(FALSE)
}
