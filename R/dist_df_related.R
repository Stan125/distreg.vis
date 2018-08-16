#' Internal: Distribution Implementation Checker
#'
#' Is the distribution implemented in the distreg.vis framework?
#' @keywords internal
is.implemented <- function(name) {
  exists <- name %in% distreg.vis::dists$dist_name
  if (exists)
    return(distreg.vis::dists[distreg.vis::dists$dist_name == name, "implemented"])
  else
    return(FALSE)
}

#' Internal: Continuous/Mixed Distribution checker
#'
#' Check whether a given distribution is at least partly continuous (could be
#' mixed as well).
#' @keywords internal
is.continuous <- function(name) {
  type <- distreg.vis::dists[distreg.vis::dists$dist_name == name, "type"]
  if (type %in% c("Mixed", "Continuous"))
    return(TRUE)
  else
    return(FALSE)
}

#' Internal: Discrete Distribution Checker
#'
#' Check whether a given distribution is fully discrete.
#' @keywords internal
is.discrete <- function(name) {
  type <- distreg.vis::dists[distreg.vis::dists$dist_name == name, "type"]
  if (type == "Discrete")
    return(TRUE)
  else
    return(FALSE)
}

#' Internal: Is gamlss family?
#'
#' Check whether a given distribution comes from the gamlss.dist package
#' @keywords internal
is.gamlss <- function(name) {
  return(any(distreg.vis::dists[distreg.vis::dists$dist_name == name, "class"] == "gamlss"))
}

#' Internal: Is bamlss family?
#'
#' Check whether a given distribution comes from the bamlss package
#' @keywords internal
is.bamlss <- function(name) {
  return(any(distreg.vis::dists[distreg.vis::dists$dist_name == name, "class"] == "bamlss"))
}

#' Internal: Function to check whether the modeled response is bivariate
#'
#' @keywords internal
is.2d <- function(model) {
  fam_name <- fam_obtainer(model)
  is.mv <- as.logical(distreg.vis::dists[distreg.vis::dists$dist_name == fam_name, "is_multivariate"])
  links <- link_printer(model)
  if (is.mv && length(links) == 5)
    return(TRUE)
  else
    FALSE
}
