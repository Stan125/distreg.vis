#' Return First two Moments of a distributions's parameters
#'

moments <- function(par, family) {
  par <- as.list(par)
  if (family == "gaussian") {
    moments <- data.frame(ex = par$mu, vx = (par$sigma^2))
  } else if (family == "gaussian2") {
    moments <- data.frame(ex = par$mu, vx = par$sigma2)
  } else if (family == "beta") {
    a <- par$mu * (1 - par$sigma2) / (par$sigma2)
    b <- a * (1 - par$mu) / par$mu
    ex <- (1) / (1 + (b / a))
    vx <- (a * b) / (((a + b)^2) * (a + b + 1))
    moments <- list(ex = ex, vx = vx)
  } else if (family == "binomial") {
    par <- list(pi = unlist(par))
    moments <- list(ex = par$pi, vx = par$pi * (1 - par$pi))
  } else if (family == "cnorm") {
    mu <- par$mu
    sigma <- par$sigma
    ex <- pnorm(mu / sigma) * (mu + sigma * ((dnorm(mu / sigma) / pnorm(mu / sigma))))
    X <- pnorm(-mu / sigma)
    Y <- dnorm(mu / sigma) / (1 - X)
    Z <- Y^2 - Y * (-mu / sigma)
    vx <- (sigma^2) * (1 - X) * ((1 - Z) + (((-mu/sigma) - Y)^2)* X)
    moments <- list(ex = ex, vx = vx)
  } else if (family == "gamma") {
    a <- par$sigma
    s <- par$mu / par$sigma
    moments <- list(ex = a * s, vx = a * s^2)
  } else if (family == "poisson") {
    moments <- list(ex = par$lambda, vx = par$lambda)
  } else if (family == "Generalized Pareto") {
    ex <- par$sigma / (1 - par$xi)
    vx <- (par$sigma^2) / (1 - par$xi)^2 * (1 - 2 * par$xi)
    moments <- list(ex = ex, vx = vx)
  } else {
    moments <- NULL
  }
  # not yet implemented: cox, multinomial
  return(moments)
}
