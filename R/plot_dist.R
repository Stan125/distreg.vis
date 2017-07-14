#' Get plot limits for a predicted distribution
#'
limits <- function(x, times_sd = 3)
  return(c(x[1] - times_sd * x[2],
           x[1] + times_sd * x[2]))

#' Function to plot the predicted distributions (with ggplot2)
#'
#' @import ggplot2
plot_dist <- function(model, newdata, intercept = TRUE) {
  # Get predictions
  p_m <- as.data.frame(predict(model, newdata, intercept = intercept,
                 type = "parameter"))

  # Get family and function for pdf
  fam_gen <- family(model)
  fam <- fam_gen$family
  pdf <- fam_gen$d

  # Get plot limits
  lims <- apply(p_m, MARGIN = 1, FUN = limits)
  lims <- data.frame(x = c(min(lims), max(lims)))

  # Assemble plot
  ground <- ggplot(data = lims, aes(x)) +
    ggtitle("Predicted distribution(s)") +
    labs(x = "y", y = "f(y)")

  # Put functions
  for (i in 1:nrow(p_m)) {
    args <- as.list(p_m[i, ])
    ground <- ground +
      stat_function(fun = pdf, args = list(par = as.list(p_m[i, ])),
                    geom = "area", aes_(fill = paste("P", i)), alpha = 0.7)
  }

  # Make legend title
  ground$labels$fill <- "Predictions"

  # Return it
  return(ground)
}
