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
    moms <- na.omit(moms)
    if (nrow(moms) == 0)
      stop("Cannot produce limits because predicted distribution does not have valid moments") # check whether there's problem with issue #53. can we display all distributions even if some moments are not able to be made?

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
    moms <- na.omit(moms)
    if (nrow(moms) == 0)
      stop("Cannot produce limits because predicted distribution does not have valid moments")

    # Get theoretical lims by dists data.frame - this means the support of the distribution
    theo_lims <- lims_getter(fam_name)

    # Get empirical upper and lower limits
    all_lims <- apply(moms, 1, FUN = function(x){
      ex <- x["ex"]
      sd <- sqrt(x["vx"])
      return(c(l = ex - 3 * sd, u = ex + 3 * sd)) # plot limits - 3 times the standard deviation
    })

    # Min/max of empirical limits (mean +- three times the sd)
    min_lim <- min(all_lims)
    max_lim <- max(all_lims)

    # Check here whether to use max/min or the theoretical limits
    if (isTRUE(min_lim < theo_lims$l_limit)) # this works even if there is an NA because then it won't be true as well... magic...
      lower <- theo_lims$l_limit
    else
      lower <- min_lim
    if (isTRUE(max_lim > theo_lims$u_limit))
      upper <- theo_lims$u_limit
    else
      upper <- max_lim

    # Limits
    lims <- c(lower = lower, upper = upper)
  }

  # Third case - categorical variables
  if (lim_type == "cat_limits") {
    # if we are categorical do this

    # (binomial should also be categorical, actually...)

    # probably have to write a function that catches the data from gamlss/bamlss models and uses it here...
  }
  return(lims)
}

#' Internal: Limit type getter
#'
#' Get the limit type depending on \code{distreg.vis::dists}.
#' @keywords internal
type_getter <- function(fam_name) {
  type <- distreg.vis::dists[distreg.vis::dists$dist_name == fam_name, "type_limits", drop = TRUE]
  return(as.character(type))
}

#' Internal: Upper- and lower limit of distribution getter
#'
#' Obtain the theoretical upper and lower limits of the distribution. Only
#' necessary if the distribution has limits
#' @keywords internal
lims_getter <- function(fam_name) {
  return(distreg.vis::dists[distreg.vis::dists$dist_name == fam_name, c("l_limit", "u_limit")])
}


#' Internal: Transform discrete predictions into a usable df
#'
#' @importFrom stats dpois ppois reshape
#' @keywords internal
disc_trans <- function(predictions, family, type, model) {
  if (family == "binomial") {
    if (type == "pdf") {
      predictions$pi_inv <- 1 - predictions$pi
      predictions$rownames <- row.names(predictions)
      colnames(predictions) <- c("v.0", "v.1", "rownames") # v. are the names that need transforming
      tf_df <- reshape(predictions, varying = seq_len(2), idvar = "rownames", direction = "long")
      colnames(tf_df) <- c("rownames", "type", "value")
      rownames(tf_df) <- seq_len(nrow(tf_df))
    } else if (type == "cdf") {
      predictions$pi_inv <- 1
      predictions$rownames <- row.names(predictions)
      colnames(predictions) <- c("v.0", "v.1", "rownames") # v. are the names that need transforming
      tf_df <- reshape(predictions, varying = seq_len(2), idvar = "rownames", direction = "long")
      colnames(tf_df) <- c("rownames", "type", "value")
      rownames(tf_df) <- seq_len(nrow(tf_df))
      tf_df <- rbind(tf_df, data.frame(rownames = unique(tf_df$rownames),
                                       type = rep(-1e-100, (nrow(tf_df)/ 2)), # this is because starting point has to be left by just a little margin for plot...
                                       value = rep(0, (nrow(tf_df)/ 2))))
      tf_df$type <- as.numeric(tf_df$type)
    }
  } else if (family %in% c("poisson", "PO")) {
    if (is.gamlss(family))
      lambda <- predictions$mu
    else
      lambda <- predictions$lambda
    # PDF
    if (type == "pdf"){
      limits <- 0:((max(lambda)*2) + 3) # what lim should preds be?
      tf_df_unshaped <- apply(predictions, 1, FUN = function(x) return(dpois(limits, x)))
      tf_df_unshaped <- cbind(tf_df_unshaped, limits)

      # New colnames for easier reshaping
      colnames(tf_df_unshaped) <- c(paste0("rn.", row.names(predictions)), "type")
      tf_df_unshaped <- as.data.frame(tf_df_unshaped)
      tf_df_unshaped$type <- as.character(tf_df_unshaped$type)

      # Reshape with base - to get rid of tidyr dependency
      tf_df <-
        reshape(
          tf_df_unshaped,
          varying = seq_len(nrow(predictions)),
          idvar = "type",
          direction = "long"
        )
      colnames(tf_df) <- c("type", "rownames", "value")
    }
    # CDF
    if (type == "cdf") {
      limits <- 0:((max(lambda)*2) + 3) # what lim should preds be?
      tf_df_unshaped <- apply(predictions, 1, FUN = function(x) return(ppois(limits, x)))
      tf_df_unshaped <- cbind(tf_df_unshaped, limits)

      # New colnames for easier reshaping
      colnames(tf_df_unshaped) <- c(paste0("rn.", row.names(predictions)), "type")
      tf_df_unshaped <- as.data.frame(tf_df_unshaped)
      tf_df_unshaped$type <- as.character(tf_df_unshaped$type)

      # Reshape with base - to get rid of tidyr dependency
      tf_df <-
        reshape(
          tf_df_unshaped,
          varying = seq_len(nrow(predictions)),
          idvar = "type",
          direction = "long"
        )
      colnames(tf_df) <- c("type", "rownames", "value")

      # Reshape again for plotting purposes
      tf_df <- rbind(data.frame(type = 0, rownames = row.names(predictions),
                                value = -1e-100), # this is because starting point has to be left by just a little margin for plot...
                     tf_df)
    }
  } else if (family == "multinomial") {
    if (type == "pdf") {
      ## Transform the predictions such that probabilities for all classes are given
      levels <- levels(model$model.frame[, 1])
      psums <- rowSums(predictions) + 1
      p0 <- 1 / psums
      trans_preds <- cbind(p0, matrix(apply(predictions, 2, FUN = function(x)
        return(x * p0)), ncol = length(levels) - 1)) # matrix because else it will not work with just one row
      trans_preds <- as.data.frame(trans_preds)
      colnames(trans_preds) <- paste0("lv.", levels)
      trans_preds$rownames <- row.names(trans_preds)
      tf_df <- reshape(trans_preds,
                       varying = seq_len(length(levels)),
                       idvar = "rownames",
                       direction = "long")
      colnames(tf_df) <- c("rownames", "type", "value")
      rownames(tf_df) <- seq_len(nrow(tf_df))
      tf_df$type <- factor(tf_df$type, labels = levels)
    }
    if (type == "cdf") {
      stop("CDF of Multinomial Family not feasible")
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
    cdf <- function(q, par)
      return(do.call(get(force(p_raw_name), envir = as.environment("package:gamlss.dist")),
                    c(list(q = q), par)))

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
#' @export
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
