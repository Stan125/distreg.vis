#' External Function Implementer
#'
#' This function exists to extend \code{plot_moments} such that an external
#' function, which is user-written, can be included. Thus, the user can see the
#' impact of a variable on a self-defined measure, like the Gini Index.
#' @keywords internal

ex_f <- function(pred_params, ex_fun) {

  # Obtain the function from the string
  fun <- get(ex_fun, envir = .GlobalEnv)

  # Stop if not function
  if (class(fun) != "function")
    stop("Argument 'ex-fun' has to be a function!")

  # Get values
  vals <- apply(pred_params, 1, FUN = fun)

  # Return values
  return(vals)
}
