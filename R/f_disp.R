#' Model distribution family display-er
#'
#' Prints the family and link functions of a model in a short way

f_disp <- function(model) {
  family <- family(model)
  dist <- family$family
  links <- family$links
  coefs <- names(family$links)
  return(paste0("Family: ", dist, "\n",
                "Links: ", paste(coefs, links, sep = ": ", collapse = ", ")))
}
