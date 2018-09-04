#' distreg Searcher
#'
#' Function that searches the WD for a distreg model
#' @keywords internal

search_distreg <- function() {
  # Get components of Global Environment
  lsc <- ls(envir = .GlobalEnv)

  # Look for bamlss & gamlss components
  distreg_true <- sapply(lsc, FUN = distreg_checker)

  # Return nothing if no bamlss or gamlss
  if (length(distreg_true) > 0)
    if (sum(distreg_true) > 0)
      return(lsc[distreg_true])
  else
    return("")
}

#' function Searcher
#'
#' Function that looks for objects of class 'function' in the working directory.

search_funs <- function() {
  # Get components of Global Envir
  lsc <- ls(envir = .GlobalEnv)

  # Look for functions
  functions_true <- sapply(lsc, FUN = function(x)
    return(is.function(get(x, envir = .GlobalEnv))))

  # Return only if there is a function
  if (length(functions_true) > 0)
    if (sum(functions_true) > 0)
      return(c("", lsc[functions_true]))
  else
    return("")
}

#' Checker if object is either gamlss or bamlss
#'
#' @keywords internal
distreg_checker <- function(x) {
  obj <- get(x, envir = .GlobalEnv)
  if (any(class(obj) == "bamlss"))
    return(TRUE)
  else if (any(class(obj) == "gamlss"))
    return(TRUE)
  else
    return(FALSE)
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

#' Factor Checker
#'
#' Checks whether some factor was unwantedly converted to an ordered factor
#' which rhandsontable sometimes does
#' @keywords internal
fac_check <- function(DF) {
  rn <- row.names(DF)
 DF <- lapply(DF, FUN = function(x) {
   if ("ordered" %in% class(x))
     return(factor(x, levels = levels(x), ordered = FALSE))
   else
     return(x)
 })
 DF <- data.frame(DF, row.names = rn)
 return(DF)
}

#' Function for better use of formatR's tidy_source
#' @keywords internal
#' @importFrom formatR tidy_source
tidy_c <- function(x)
  return(tidy_source(text = x, output = FALSE, width.cutoff = 45))$text.tidy


