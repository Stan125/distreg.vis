#' Plot predicted bamlss distribution families with ggplot2
#'
#' This function plots the parameters of a predicted distribution (e.g.
#'   obtained through \code{\link{preds}}) with ggplot2. You can use all
#'   implemented families in bamlss except two-dimensional distributions and
#'   the cox family.
#'
#' @param model A fitted bamlss object.
#' @param predictions A data.frame with rows for every model prediction and
#'   columns for every predicted parameter of the distribution. Is easily obtained
#'   with the \code{bamlss.vis} function \code{\link{preds}}.
#' @param palette The colour palette used for colouring the plot. You can use
#'   any of the ones supplied in \code{\link[ggplot2]{scale_fill_brewer}} though I
#'   suggest you use one of the qualitative ones: Accent, Dark2, etc.
#' @param type Do you want the probability distribution function ("pdf") or
#'   the cumulative distribution function ("cdf")?
#' @return A ggplot2 object.
#' @examples
#' # Generating data
#' data_fam <- model_fam_data()
#' # Compute model
#' beta_model <- bamlss(list(beta ~ norm1 + norm2,
#'   sigma2 ~ norm1 + norm2),
#'   data = data_fam, family = beta_bamlss())
#' # Get 3 predictions
#' pred_df <- data_fam[sample(1:nrow(data_fam), 3), c("norm1", "norm2")]
#' pred_df <- cbind(pred_df, intercept = rep(TRUE, 3))
#' param_preds <- preds(beta_model, pred_df)
#' # Create pdf, cdf plots
#' plot_dist(beta_model, param_preds)
#' plot_dist(beta_model, param_preds, type = "cdf")
#' plot_dist(beta_model, param_preds, palette = "Dark2")
#' @export

plot_dist <- function(model, predictions, palette = "default",
                      type = "pdf") {

  # Convert predictions to p_m
  p_m <- predictions

  # Get family and function for pdf
  fam_gen <- family(model)
  fam <- fam_gen$family
  funs_list <- list(pdf = fam_gen$d, cdf = fam_gen$p)

  # Hacky way to stop the function for multinomial cdf
  if (fam == "multinomial" & type == "cdf")
    stop("There is no cdf for the multinomial distribution!")

  # Get plot limits
  lims <- bamlss.vis:::limits(p_m, fam)

  # Different plots depending on type of distribution
  if (bamlss.vis:::is.continuous(fam))
    plot <- pdfcdf_continuous(lims, funs_list, type, p_m, palette)
  else if (!bamlss.vis:::is.continuous(fam))
    plot <- pdfcdf_discrete(p_m, palette, fam, type, model)

  # Return it
  return(plot)
}

#' Internal: Get plot limits for a predicted distribution.
#'
#' Returns a data.frame

limits <- function(predictions, family, times_sd = 3) {
  if (bamlss.vis:::is.continuous(family)) {
    if (family == "beta") {  # beta can only be from 0 to 1
      return(data.frame(x = c(0, 1)))
    } else if (family == "Generalized Pareto") {
      return(data.frame(x = c(0, 5)))
    } else {
      # Moments
      moments <- as.data.frame(bamlss.vis:::moments(predictions, family))

      # Limits for each 2 moments
      lims <- apply(moments, 1, function(x)
        return(c(x[1] - times_sd * sqrt(x[2]),
                 x[1] + times_sd * sqrt(x[2]))))

      return(data.frame(x = c(min(lims), max(lims))))
    }
  } else if (!bamlss.vis:::is.continuous(family)) {
    if (family == "binomial") {
      return(data.frame(x = c(0, 1)))
    } else if (family == "poisson") {
      # poisson
    } else if (family == "multinomial") {
      # multinomial here
    }
  }
}

#' Internal: Create the pdf/cdf for continuous covariates
#'
#' Returns a plot
#' @import ggplot2

pdfcdf_continuous <- function(lims, funs, type, p_m, palette) {
  if (type == "cdf") {
    # Assemble plot
    ground <- ggplot(data = data.frame(y = c(0, 1), x = lims), aes(x, y)) +
      ggtitle("Predicted distribution(s)") +
      labs(x = "y", y = "F(y)")

    # Add functions
    for (i in 1:nrow(p_m)) {
      args <- p_m[i, , drop = FALSE]
      ground <- ground +
        stat_function(fun = funs$cdf, args = list(par = as.list(args)),
                      geom = "line", aes_(col = row.names(args)))
    }
  } else if (type == "pdf") {
    # Assemble plot
    ground <- ggplot(data = lims, aes(x)) +
      ggtitle("Predicted distribution(s)") +
      labs(x = "y", y = "f(y)")

    # Add functions
    for (i in 1:nrow(p_m)) {
      args <- p_m[i, , drop = FALSE]
      ground <- ground +
        stat_function(fun = funs$pdf, args = list(par = as.list(args)),
                      geom = "area", aes_(fill = row.names(args)), alpha = 0.7)
    }
  }

  # Different theme
  ground <- ground + theme_classic()

  # Colour Palettes
  if (palette != "default")
    ground <- ground + scale_fill_brewer(palette = palette)

  # Make legend title
  if (type == "pdf") {
    ground$labels$fill <- "Predictions"
  } else if (type == "cdf") {
    ground$labels$colour <- "Predictions"
  }

  # Return plot
  return(ground)
}

#' Internal: Create the pdf/cdf for discrete covariates
#'
#' Returns a plot
#' @import ggplot2

pdfcdf_discrete <- function(p_m, palette, family, type, model) {

  # Transform discrete predictions
  pred_df <- disc_trans(p_m, family, type, model)

  if (type == "pdf") {
    # Assemble plot
    ground <- ggplot(pred_df, aes(type, value, fill = rownames)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "y", y = "f(y)") +
      ggtitle("Predicted distributions(s)")

    # Classic theme
    ground <- ground + theme_classic()

    # Palette
    if (palette != "default")
      ground <- ground + scale_fill_brewer(palette = palette)

    # Legend label
    ground$labels$fill <- "Predictions"

  } else if (type == "cdf") {
    # Assemble plot
    ground <- ggplot(pred_df, aes(type, value, col = rownames)) +
      geom_step(linetype = 2) +
      labs(x = "x", y = "F(x)") +
      ggtitle("Predicted distribution(s)")

    # Classic theme
    ground <- ground + theme_classic()

    # Palette
    if (palette != "default")
      ground <- ground + scale_fill_brewer(palette = palette)

    # Legend label
    ground$labels$colour <- "Predictions"
  }

  # Return plot
  return(ground)
}

#' Internal: Transform discrete predictions into a usable df
#'
#' @importFrom tidyr gather

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



