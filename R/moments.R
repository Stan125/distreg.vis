#' Return First two Moments of a distributions's parameters
#'
#' @importFrom stats pnorm dnorm
#' @export

moments <- function(par, family) {
  rnames <- row.names(par)
  if (family == "gaussian") {
    moments <- data.frame(Expected_Value = par$mu,
                          Variance = (par$sigma^2),
                          row.names = rnames)
  } else if (family == "gaussian2") {
    moments <- data.frame(Expected_Value = par$mu,
                          Variance = par$sigma2,
                          row.names = rnames)
  } else if (family == "beta") {
    a <- par$mu * (1 - par$sigma2) / (par$sigma2)
    b <- a * (1 - par$mu) / par$mu
    ex <- (1) / (1 + (b / a))
    vx <- (a * b) / (((a + b)^2) * (a + b + 1))
    moments <- data.frame(Expected_Value = ex,
                          Variance = vx,
                          row.names = rnames)
  } else if (family == "binomial") {
    par <- list(pi = unlist(par))
    moments <- data.frame(Expected_Value = par$pi,
                          Variance = par$pi * (1 - par$pi),
                          row.names = rownames)
  } else if (family == "cnorm") {
    mu <- par$mu
    sigma <- par$sigma
    ex <- pnorm(mu / sigma) * (mu + sigma * ((dnorm(mu / sigma) / pnorm(mu / sigma))))
    X <- pnorm(-mu / sigma)
    Y <- dnorm(mu / sigma) / (1 - X)
    Z <- Y^2 - Y * (-mu / sigma)
    vx <- (sigma^2) * (1 - X) * ((1 - Z) + (((-mu/sigma) - Y)^2)* X)
    moments <- data.frame(Expected_Value = ex,
                          Variance = vx,
                          row.names = rnames)
  } else if (family == "gamma") {
    a <- par$sigma
    s <- par$mu / par$sigma
    moments <- data.frame(Expected_Value = a * s,
                          Variance = a * s^2,
                          row.names = rnames)
  } else if (family == "poisson") {
    moments <- data.frame(Expected_Value = par$lambda,
                          Variance = par$lambda,
                          row.names = rnames)
  } else if (family == "Generalized Pareto") {
    ex <- par$sigma / (1 - par$xi)
    vx <- (par$sigma^2) / (1 - par$xi)^2 * (1 - 2 * par$xi)
    moments <- data.frame(Expected_Value = ex,
                          Variance = vx,
                          row.names = rnames)
  } else if (family == ".mvnorm") {
    moments <- data.frame(Expected_Value_1 = par$mu1,
                          Variance_1 = par$sigma1^2,
                          Expected_Value_2 = par$mu2,
                          Variance_2 = par$sigma2^2,
                          Rho_12 = par$rho12,
                          row.names = rnames)
  } else {
    moments <- NULL
  }
  # not yet implemented: cox, multinomial
  return(moments)
}
