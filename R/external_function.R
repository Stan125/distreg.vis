#' External Function Implementer
#'
#' This function exists to extend \code{plot_moments} such that an external
#' function, which is user-written, can be included. Thus, the user can see the
#' impact of a variable on a self-defined measure, like the Gini Index.
#' @keywords internal

ex_f <- function(pred_params, ex_fun) {

  # This checks whether we have samples (list format) or not (data.frame format)
  if (is.list(pred_params) && !is.data.frame(pred_params))
    samples <- TRUE
  else if (is.data.frame(pred_params))
    samples <- FALSE
  else
    stop("par has to be either a data.frame or a list")

  # Obtain the function from the string
  fun <- get(ex_fun, envir = .GlobalEnv)

  # Stop if not function
  if (class(fun) != "function")
    stop("Argument 'ex-fun' has to be a function!")

  # Get values
  if (!samples)
    vals <- apply(pred_params, 1, FUN = fun)
  if (samples)
    vals <- lapply(pred_params, FUN = function(listparts) {
      apply(listparts, 1, FUN = function(x) {
        x <- as.list(x)
        return(fun(x))
      })
    })

  # Return values
  return(vals)
}
