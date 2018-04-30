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
      ex <- do.call(fam_called$mean, args = as.list(x)) # Expected value
      vx <- do.call(fam_called$variance, args = as.list(x)) # Variance
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

