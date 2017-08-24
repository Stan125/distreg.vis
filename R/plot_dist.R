#' Internal: Get plot limits for a predicted distribution.
#'
#' Returns a data.frame
#'
limits <- function(predictions, family, times_sd = 3) {
  if (family == "beta") { # beta can only be from 0 to 1
    return(data.frame(x = c(0, 1)))
  } else {
    # Moments
    moments <- as.data.frame(bamlss.vis:::moments(predictions, family))

    # Limits for each 2 moments
    lims <- apply(moments, 1, function(x)
      return(c(x[1] - times_sd * sqrt(x[2]),
               x[1] + times_sd * sqrt(x[2]))))

    return(data.frame(x = c(min(lims), max(lims))))
  }
}


#' Function to plot the predicted distributions (with ggplot2) with
#' the intercept as another column of the dataframe
#'
#' @import ggplot2
#' @export
plot_dist <- function(model, predictions, palette = "default",
                      type = "pdf") {

  # Convert predictions to p_m
  p_m <- predictions

  # Get family and function for pdf
  fam_gen <- family(model)
  fam <- fam_gen$family
  pdf <- fam_gen$d
  cdf <- fam_gen$p

  # Get plot limits
  lims <- limits(p_m, fam)

  if (type == "cdf") {
    # Assemble plot
    ground <- ggplot(data = data.frame(y = c(0, 1), x = lims), aes(x, y)) +
      ggtitle("Predicted distribution(s)") +
      labs(x = "y", y = "F(y)")

    # Add functions
    for (i in 1:nrow(p_m)) {
      args <- as.list(p_m[i, ])
      ground <- ground +
        stat_function(fun = cdf, args = list(par = as.list(p_m[i, ])),
                      geom = "line", aes_(col = row.names(p_m[i, ])))
    }
  } else if (type == "pdf") {
    # Assemble plot
    ground <- ggplot(data = lims, aes(x)) +
      ggtitle("Predicted distribution(s)") +
      labs(x = "y", y = "f(y)")

    # Add functions
    for (i in 1:nrow(p_m)) {
      args <- as.list(p_m[i, ])
      ground <- ground +
        stat_function(fun = pdf, args = list(par = as.list(p_m[i, ])),
                      geom = "area", aes_(fill = row.names(p_m[i, ])), alpha = 0.7)
    }
  }

  # Different theme
  ground <- ground + theme_classic()

  # Colour Palettes
  if (palette != "default")
    ground <- ground + scale_fill_brewer(palette = palette)

  # Make legend title
  if (type == "pdf")
    ground$labels$fill <- "Predictions"
  else if (type == "cdf")
    ground$labels$colour <- "Predictions"

  # Return it
  return(ground)
}
