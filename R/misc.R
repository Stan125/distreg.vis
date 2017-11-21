#' bamlss Searcher
#'
#' Function that searches the WD for a bamlss model
#' @keywords internal

search_ba <- function() {
  # Get components of Global Environment
  lsc <- ls(envir = .GlobalEnv)

  # Look for bamlss components
  bamlss_true <- sapply(lsc, FUN = function(x) {
    return(any(class(get(x)) == "bamlss"))
  })

  if (length(bamlss_true) > 0)
    if (sum(bamlss_true) > 0)
      return(lsc[bamlss_true])
  else
    return("")
}

#' Factor Equalizer
#'
#' Function that takes the levels of a df's factors and puts them to a second df
#'   (used for predictions). Returns a data.frame
#' @keywords internal

fac_equ <- function(base_df, pred_df) {
  # Factor Names
  varclass <- sapply(base_df, FUN = is.factor)
  varc_index <- which(varclass)

  # Make right factor levels
  for (i in varc_index)
    pred_df[, i] <- factor(pred_df[, i], levels = levels(base_df[, i]))

  # Return the pred_df
  return(pred_df)
}

#' Internal: Function to obtain all explanatory variables used to fit
#'   a model, without the dependent variables
#' @keywords internal

expl_vars <- function(model) {
  all_data <- model.frame(model)
  dep_names <- colnames(model$y)
  index <- !colnames(all_data) %in% dep_names
  return(all_data[, index, drop = FALSE])
}

#' Function for better use of formatR's tidy_source
#' @keywords internal
#' @importFrom formatR tidy_source
tidy_c <- function(x)
  return(tidy_source(text = x, output = FALSE, width.cutoff = 45))$text.tidy



