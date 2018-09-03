#' Return expected first two moments of a distribution, given the predicted parameters
#'
#' @importFrom stats pnorm dnorm
#' @param par Parameters of the modeled distribution in a data.frame form. Can
#'   be Output of \code{\link{preds}}, for example.
#' @param fam_name Name of the used family in character form. Can be one of
#'   \code{distreg.vis::dists$dist_name}. All gamlss.dist and exported bamlss families are
#'   supported. To obtain the family from a model in character form, use \code{\link{fam_obtainer}}.
#' @import bamlss
#' @export
moments <- function(par, fam_name) {
  # get rownames
  rnames <- row.names(par)

  # gamlss moments
  if (is.gamlss(fam_name)) {
    # Call the family
    fam <- get(fam_name, envir = as.environment("package:gamlss.dist"))
    fam_called <- fam()

    # Get moments for each row of par
    moms_raw <- apply(par, 1, function(x) {
      ex <- do.call(fam_called$mean, args = as.list(x)) # Expected value
      vx <- do.call(fam_called$variance, args = as.list(x)) # Variance
      return(c(Expected_Value = ex, Variance = vx))
    })

    # Make into nice format
    moms <- as.data.frame(t(moms_raw), row.names = rnames)
  }

  # bamlss moments
  if (is.bamlss(fam_name)) {
    # Call the family
    fam <- get(paste0(fam_name, "_bamlss"), envir = as.environment("package:bamlss"))
    fam_called <- fam()

    if (is.null(fam_called$mean) | is.null(fam_called$variance))
      stop("Not all momoment functions implemented")

    # Get moments for each row of par
    moms_raw <- apply(par, 1, function(x) {
      ex <- fam_called$mean(as.list(x)) # Expected value
      vx <- fam_called$variance(as.list(x)) # Variance
      return(c(Expected_Value = ex, Variance = vx))
    })

    # Make into nice format
    moms <- as.data.frame(t(moms_raw), row.names = rnames) # lots of reshaping here... no me gusta
  }

  return(moms)
}

