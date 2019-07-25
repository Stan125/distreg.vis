#' Obtain mean values and reference categories of variables in a data.frame
#'
#' This function purely exists for the \code{set_mean} argument of
#' \code{\link{plot_moments}}. It takes a data.frame and obtains the mean values
#' (numeric variables) and reference categories (categorical covariates).
#'
#' @param input A \code{data.frame} object
#' @return A \code{data.frame} object with one row
#' @keywords internal

set_mean <- function(input) {
  if (!is(input, "data.frame"))
    stop("Argument `input` needs to be a data.frame object")

  # Do the operations
  new_df <- lapply(input, FUN = function(x) {

    # If variable is numeric take mean
    if (is.numeric(x))
      def_x <- mean(x, na.rm = TRUE)

    # If variable is character take the first observation
    if (is.character(x))
      def_x <- na.omit(x)[1]

    # If variable is factor take the first level
    if (is.factor(x)) {
      def_x <- as.factor(levels(x)[1])
      levels(def_x) <- levels(x)
    }

    # If variable is logical let it be FALSE
    if (is.logical(x))
      def_x <- FALSE

    # Return it
    return(def_x)
  })
  new_df <- as.data.frame(new_df, row.names = c("default_vals"))

  # Return new df
  return(new_df)
}
