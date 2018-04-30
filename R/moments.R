#' Return First two Moments of a distributions's parameters
#'
#' @importFrom stats pnorm dnorm
#' @param par Parameters of the modeled distribution in a data.frame form.
#'   Can be Output of \code{\link{preds}}, for example.
#' @param family One of \code{\link{family.bamlss}} family objects' "family"
#'   description in character form. E.g. \code{"gaussian"} or \code{"beta"}.
#' @import gamlss.dist
#' @import bamlss
#' @export
moments <- function(par, fam_name) {
  # gamlss moments
  if (is.gamlss(fam_name)) {
    # Call the family
    fam <- get(fam_name, envir = as.environment("package:gamlss.dist"))
    fam_called <- fam()

    # Get moments for each row of par
    moms_raw <- apply(par, 1, function(x) {
      ex <- do.call(fam_called$ex, args = as.list(x)) # Expected value
      vx <- do.call(fam_called$vx, args = as.list(x)) # Variance
      return(c(ex = ex, vx = vx))
    })

    # Make into nice format
    moms <- as.data.frame(t(moms_raw))
  }

  # bamlss moments
  if (is.bamlss(fam_name)) {
    # Call the family
    fam <- get(paste0(fam_name, "_bamlss"), envir = as.environment("package:bamlss"))
    fam_called <- fam()

    # Get moments for each row of par
    moms_raw <- apply(par, 1, function(x) {
      ex <- fam_called$mean(as.list(x)) # Expected value
      vx <- fam_called$variance(as.list(x)) # Variance
      return(c(ex = ex, vx = vx))
    })

    # Make into nice format
    moms <- as.data.frame(t(moms_raw)) # lots of reshaping here... no me gusta
  }

  return(moms)
}



moments_old <- function(par, family) {
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
    moments <- data.frame(Expected_Value = par$pi,
                          Variance = par$pi * (1 - par$pi),
                          row.names = rnames)
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
