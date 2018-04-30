#' Internal: Distribution Implementation Checker
#'
#' Is the distribution implemented in the distreg.vis framework?
#' @keywords internal
is.implemented <- function(name) {
  exists <- name %in% dists$dist_name
  if (exists)
    return(dists[dists$dist_name == name, "implemented"])
  else
    return(FALSE)
}

#' Internal: Continuous/Mixed Distribution checker
#'
#' Check whether a given distribution is at least partly continuous (could be
#' mixed as well).
#' @keywords internal
is.continuous <- function(name) {
  type <- dists[dists$dist_name == name, "type"]
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
  type <- dists[dists$dist_name == name, "type"]
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
  return(any(dists[dists$dist_name == name, "class"] == "gamlss"))
}

#' Internal: Is bamlss family?
#'
#' Check whether a given distribution comes from the bamlss package
#' @keywords internal

is.bamlss <- function(name) {
  return(any(dists[dists$dist_name == name, "class"] == "bamlss"))
}

#' Internal: Function to check whether the modeled response is bivariate
#'
#' @keywords internal

is.2d <- function(name) {
  return(dists[dists$dist_name == name, "is_multivariate"])
}
