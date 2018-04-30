#' Internal: Plot limit getter
#'
#' A function that heavily relies on the \code{distreg.vis::dists} data.frame to
#' obtain optimal plotting limits. Specifically, this function relies on the
#' columns \code{type_limits}, \code{l_limit}, \code{u_limit}.
#'
#' Three cases: categorical limits (\code{cat_limits}), no_limits, has_limits, both_limits
#' @keywords internal

limits <- function(fam_name, predictions) {

  # Get limit type
  lim_type <- type_getter(fam_name)

  # First case - no limits. Example: Normal distribution
  if (lim_type == "no_limits") {

    # Get moments
    moms <- moments(predictions, fam_name)

    # get upper and lower limit
    all_lims <- apply(moms, 1, FUN = function(x){
      ex <- x["ex"]
      sd <- sqrt(x["vx"])
      return(c(l = ex - 3 * sd, u = ex + 3 * sd)) # plot limits - 3 times the standard deviation
    })

    # Max and min for limits
    lims <- c(lower = min(all_lims), upper = max(all_lims))
  }

  # Second case: One Limit / Two Limits. Example: Beta distribution
  if (lim_type %in% c("one_limit", "both_limits")) {

    # Get moments
    moms <- moments(predictions, fam_name)

    # Get theoretical lims by dists data.frame
    theo_lims <- lims_getter(fam_name)

    # Get theoretical upper and lower limit
    all_lims <- apply(moms, 1, FUN = function(x){
      ex <- x["ex"]
      sd <- sqrt(x["vx"])
      return(c(l = ex - 3 * sd, u = ex + 3 * sd)) # plot limits - 3 times the standard deviation
    })

    # Find the min and max of both to get right limits
    all_combined <- na.omit(c(unlist(all_lims), theo_lims)) # the na.omit here takes care of the case when there is only one limit.... NA is input as the upper limit and then removed here.

    # Limits
    lims <- c(lower = min(all_combined), upper = max(all_combined))
  }

  # if we are categorical do this

  # (binomial should also be categorical, actually...)

  return(lims)
}

#' Internal: Limit type getter
#'
#' Get the limit type depending on \code{dists}.
#' @keywords internal
type_getter <- function(fam_name) {
  type <- dists[dists$dist_name == fam_name, "type_limits", drop = TRUE]
  return(as.character(type))
}

#' Internal: Upper- and lower limit of distribution getter
#'
#' Obtain the theoretical upper and lower limits of the distribution. Only
#' necessary if the distribution has limits
#' @keywords internal
lims_getter <- function(fam_name) {
  return(dists[dists$dist_name == fam_name, c("l_limit", "u_limit")])
}


#' Internal: Transform discrete predictions into a usable df
#'
#' @importFrom tidyr gather
#' @importFrom stats dpois ppois
#' @keywords internal

disc_trans <- function(predictions, family, type, model) {
  if (family == "binomial") {
    if (type == "pdf") {
      predictions$pi_inv <- 1 - predictions$pi
      predictions$rownames <- row.names(predictions)
      colnames(predictions) <- c("0", "1", "rownames")
      tf_df <- gather(predictions, "type", "value", -rownames)
    } else if (type == "cdf") {
      predictions$pi_inv <- 1
      predictions$rownames <- row.names(predictions)
      colnames(predictions) <- c("0", "1", "rownames")
      tf_df <- gather(predictions, "type", "value", -rownames)
      tf_df <- rbind(tf_df, data.frame(rownames = unique(tf_df$rownames),
                                       type = rep(-1e-100, (nrow(tf_df)/ 2)), # this is because starting point has to be left by just a little margin for plot...
                                       value = rep(0, (nrow(tf_df)/ 2))))
      tf_df$type <- as.numeric(tf_df$type)
    }
  } else if (family == "poisson") {
    if (type == "pdf"){
      limits <- 0:((max(predictions$lambda)*2) + 3) # what lim should preds be?
      tf_df <- apply(predictions, 1, FUN = function(x) return(dpois(limits, x)))
      tf_df <- cbind(tf_df, limits)
      colnames(tf_df) <- c(row.names(predictions), "type")
      tf_df <- gather(as.data.frame(tf_df), key = "rownames", "value", -type)
    } else if (type == "cdf") {
      limits <- 0:((max(predictions$lambda)*2) + 3) # what lim should preds be?
      tf_df <- apply(predictions, 1, FUN = function(x) return(ppois(limits, x)))
      tf_df <- cbind(tf_df, limits)
      colnames(tf_df) <- c(row.names(predictions), "type")
      tf_df <- gather(as.data.frame(tf_df), key = "rownames", "value", -type)
      tf_df <- rbind(data.frame(type = 0, rownames = row.names(predictions),
                                value = -1e-100), # this is because starting point has to be left by just a little margin for plot...
                     tf_df)
    }
  } else if (family == "multinomial") {
    if (type == "pdf") {
      tf_df_start <- mult_trans(predictions, model)
      tf_df <- tf_df_start
      tf_df$rownames <- row.names(tf_df)
      tf_df <- gather(tf_df, "type", "value", -rownames)
      tf_df$type <- factor(tf_df$type, labels = colnames(tf_df_start))
    }
  }
  return(tf_df)
}

#' Internal: Get colour palettes for 3D plots
#'
#' @importFrom RColorBrewer brewer.pal
#' @keywords internal

palette_getter <- function(name = "default") {
  if (name == "default")
    return(NULL)
  if (any(name == c("Blues", "Greens", "OrRd", "Purples")))
    return(brewer.pal(9, name)[5:9]) # omit white colours
  if (any(name == c("Spectral", "RdYlBu", "RdYlGn")))
    return(brewer.pal(9, name))
}

#' Internal: PDF and CDF getter
#'
#' Obtain the right PDF and CDF for the modeled distribution for a given model class and distribution
#'
#' @import gamlss.dist
#' @keywords internal

pdf_cdf_getter <- function(model) {
  # Stop if not gamlss or bamlss model
  if (!any(class(model) %in% c("gamlss", "bamlss")))
    stop("Only GAMLSS and BAMLSS classes supported.")

  # GAMLSS
  if (any(class(model) == "gamlss")) {
    fam_name <- fam_obtainer(model)

    # Probability distribution function
    d_raw_name <- paste0("d", fam_name)
    pdf <- function(x, par)
      return(do.call(get(force(d_raw_name), envir = as.environment("package:gamlss.dist")),
                     c(list(x = x), par))) # why does it preserve d_raw_name even if this function is used outside of this environment? http://adv-r.had.co.nz/Functions.html

    # Cumulative distribution function
    p_raw_name <- paste0("p", fam_name)
    cdf <- function(x, par)
      return(do.call(get(force(p_raw_name), envir = as.environment("package:gamlss.dist")),
                    c(list(x = x), par)))

    # Put p and d together
    dist_functions <- list(pdf = pdf, cdf = cdf)
  }

  # BAMLSS
  if (any(class(model) == "bamlss")) {
    family <- family(model)# here we need the whole family function
    pdf <- family$d
    cdf <- family$p

    # Stop when there is no pdf - AFAIK only in bamlss/multinomial case...
    if (is.null(p))
      stop(paste("Family", family, "does not have a CDF!"))

    # Put p and d together
    dist_functions <- list(pdf = pdf, cdf = cdf)
  }
  return(dist_functions)
}

#' Internal: Family obtainer
#'
#' Gets the right family (in character) from a given model
#' @keywords internal

fam_obtainer <- function(model) {
  # Check whether model is gamlss or bamlss
  if (!any(class(model) %in% c("gamlss", "bamlss")))
    stop("Cannot deal with model class if not bamlss or gamlss")

  # gamlss families
  if (any(class(model) == "gamlss"))
    fam <- model$family[1]

  # bamlss families
  if (any(class(model) == "bamlss"))
    fam <- model$family$family

  # Return it
  return(fam)
}
