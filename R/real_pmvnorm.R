#' Internal: Real cumulative density function of multivariate normal dist
#'
#' @importFrom mvtnorm pmvnorm
#' @keywords internal

real_pmvnorm <- function(y, par) {
  return(pmvnorm(upper = as.numeric(y), mean = c(par$mu1, par$mu2),
                 sigma = matrix(c(par$sigma1^2, 0,
                                  0, par$sigma2^2), nrow = 2)))
}
