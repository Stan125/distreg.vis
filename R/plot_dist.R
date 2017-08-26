#' Function to plot the predicted distributions (with ggplot2) with
#' the intercept as another column of the dataframe
#'
#' @export

plot_dist <- function(model, predictions, palette = "default",
                      type = "pdf") {

  # Convert predictions to p_m
  p_m <- predictions

  # Get family and function for pdf
  fam_gen <- family(model)
  fam <- fam_gen$family
  funs_list <- list(pdf = fam_gen$d, cdf = fam_gen$p)

  # Get plot limits
  lims <- bamlss.vis:::limits(p_m, fam)

  # Different plots depending on type of distribution
  if (bamlss.vis:::is.continuous(fam))
    plot <- pdfcdf_continuous(lims, funs_list, type, p_m, palette)
  else if (!bamlss.vis:::is.continuous(fam))
    plot <- pdfcdf_discrete(p_m, palette, fam)

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
    } else {
      # Moments
      moments <- as.data.frame(moments(predictions, family))

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

pdfcdf_discrete <- function(p_m, palette, family) {
  # Transform discrete predictions
  pred_df <- disc_trans(p_m, family)

  # Assemble plot
  ground <- ggplot(data = pred_df, aes(rownames, value, fill = type)) +
    ggtitle("Predicted distribution(s)") +
    labs(x = "Predictions", y = "Probability distribution") +
    geom_bar(stat = "identity")

  # Classic theme
  ground <- ground + theme_classic()

  # Palette
  if (palette != "default")
    ground <- ground + scale_fill_brewer(palette = palette)

  # Return plot
  return(ground)
}

#' Internal: Transform discrete predictions into a usable df
#'
#' @importFrom tidyr gather

disc_trans <- function(predictions, family) {
  if (family == "binomial") {
    predictions$pi_inv <- 1 - predictions$pi
    predictions$rownames <- row.names(predictions)
    colnames(predictions) <- c("P(X = 0)", "P(X = 1)", "rownames")
    tf_df <- gather(predictions, "type", "value", -rownames)
  } else if (family == "poisson") {
    # do this
  } else if (family == "multinomial") {
    # do that
  }
  return(tf_df)
}



