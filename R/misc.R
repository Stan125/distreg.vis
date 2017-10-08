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




