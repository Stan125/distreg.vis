#' Get quantile limits of a distribution
#'
#' Get the quantile limits of a distribution, depending on the predicted parameters.
#' @keywords internal
quants <- function(fam_name, pred_params) {

  # GAMLSS
  if (is.gamlss(fam_name)) {
    # Get quantile function
    q_raw_name <- paste0("q", fam_name)
    qfun <- function(p, par)
      return(do.call(get(force(q_raw_name), envir = as.environment("package:gamlss.dist")),
                     c(list(p = p), par)))
  }

  # BAMLSS
  if (is.bamlss(fam_name)) {
    # Get quantile function
    q_raw_name <- paste0(fam_name, "_bamlss")
    fam_called <- do.call(get(force(q_raw_name), envir = as.environment("package:bamlss")),
                          args = list())
    qfun <- fam_called$q
    if (is.null(qfun))
      stop("Quantile function not implemented.")
  }

  # Evaluate cdf at 0.001 and 0.999 quantile
  lower <- apply(pred_params, 1, function(x) qfun(p = 0.001, par = as.list(x)))
  upper <- apply(pred_params, 1, function(x) qfun(p = 0.999, par = as.list(x)))

  # Return results
  return(list(lower = lower, upper = upper))
}
