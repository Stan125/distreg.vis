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

  return(lsc[bamlss_true])
}
