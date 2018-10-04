#' Internal: Distribution Implementation Checker
#'
#' Is the distribution generally implemented in the distreg.vis framework?
#' Meaning: Does \code{plot_dist()} work?
#' @keywords internal
is.implemented <- function(fam_name) {
  exists <- fam_name %in% distreg.vis::dists$dist_name
  if (exists)
    return(distreg.vis::dists[distreg.vis::dists$dist_name == fam_name, "implemented"])
  else
    return(FALSE)
}

#' Internal: Distributional Moments implementation checker
#'
#' Is the moment function of a given distribution family implemented? Meaning:
#' will \code{plot_moments()} work?
#' @keywords internal
has.moments <- function(fam_name) {
  exists <- fam_name %in% distreg.vis::dists$dist_name
  if (exists)
    return(distreg.vis::dists[distreg.vis::dists$dist_name == fam_name, "moment_funs"])
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
