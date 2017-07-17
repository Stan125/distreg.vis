#' bamlss Searcher
#'
#' Function that searches the WD for a bamlss model

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
