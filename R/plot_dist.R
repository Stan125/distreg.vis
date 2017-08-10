#' Get plot limits for a predicted distribution
#'
limits <- function(x, times_sd = 3)
  return(c(x[1] - times_sd * x[2],
           x[1] + times_sd * x[2]))

#' Function to plot the predicted distributions (with ggplot2) with
#' the intercept as another column of the dataframe
#'
#' @import ggplot2
plot_dist <- function(model, newdata, palette = "default",
                      type = "pdf") {

  # Get predictions for newdata
  p_m <- preds(model, newdata)

  # Get family and function for pdf
  fam_gen <- family(model)
  fam <- fam_gen$family
  pdf <- fam_gen$d
  cdf <- fam_gen$p

  # Get plot limits
  lims <- apply(p_m, MARGIN = 1, FUN = limits)
  lims <- data.frame(x = c(min(lims), max(lims)))

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
                      geom = "line", aes_(col = paste("P", i)))
    }
  } else if (type == "pdf") {
    # Assemble plot
    ground <- ggplot(data = lims, aes(x)) +
      ggtitle("Predicted distribution(s)") +
      labs(x = "y", y = "f(x)")

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
