#' Plot function: Display the influence of a covariate
#'
#' This function takes a dataframe of predictions with one row per
#'   prediction and one column for every explanatory variable.
#'   Then, those predictions are held constant while one specific variable
#'   is varied over it's whole range (min-max). Then, the constant
#'   variables with the varied interest variables are predicted and plotted
#'   against the expected value and the variance of the underlying
#'   distribution
#'
#' @param pred_data Predicted data, including the variable of interest,
#'   which will be ignored.
#' @importFrom magrittr %>% extract inset
#' @import ggplot2
#' @export

plot_moments <- function(model, int_var, pred_data) {

  # Get model data
  m_data <- model$model.frame

  # Ignore interested variable
  pred_data[[int_var]] <- NULL

  # Stop if int_var is not one of colnames of model frame
  if (!int_var %in% colnames(m_data))
    stop("int_var not one of explanatory variables")

  # What type does the variable have?
  if (is.numeric(m_data[[int_var]])) {
    coltype <- "num" # numeric
  } else {
    coltype <- "cat" # categorical
  }

  # Make a range of the variable
  if (coltype == "num") {
    vals_seq <- seq(min(m_data[[int_var]]), max(m_data[[int_var]]),
                    length.out = 100)
  } else {
    vals_seq <- unique(m_data[[int_var]])
  }

  # Now vary over all possible vals of interest variable
  pred_data <- pred_data %>%
    inset("prediction", value = rownames(.)) %>%
    extract(rep(row.names(.), each = length(vals_seq)), ) %>%
    inset(int_var, value = vals_seq) %>%
    inset("id", value = row.names(.))

  # Now make predictions and find out expected value and variance
  preds <- pred_data %>%
    subset(select = -c(id, prediction)) %>%
    preds(model, newdata = .) %>%
    bamlss.vis:::moments(par = ., family = family(model)$family) %>%
    inset("id", value = row.names(.))
  moments <- colnames(preds)[colnames(preds) != "id"]

  # Merge predictions with pred_data, transform into long and to numerics
  preds <- preds %>%
    merge(y = pred_data, by.x = "id") %>%
    extract(, c(int_var, "prediction", moments)) %>%
    tidyr::gather("moment", "value", moments) %>%
    inset("value", value = as.numeric(.[["value"]]))

  if (coltype == "num") { # if we have a numeric column trans to num
    preds[[int_var]] <- as.numeric(preds[[int_var]])
  } else if (coltype == "cat") { # if not then make character
    preds[[int_var]] <- as.character(preds[[int_var]])
  }

  # Now make plot
  ground <- ggplot(preds, aes_string(x = int_var, y = "value", col = "prediction")) +
    facet_wrap(~moment, scales = "free") +
    theme_bw() +
    labs(y = "Moment values") +
    ggtitle(paste("Influence of", int_var,
                  "on parameters of modeled distribution"))

  # Line if numeric
  if (coltype == "num") {
    plot <- ground +
      geom_line()
  } else if (coltype == "car") {
    plot <- ground +
      geom_bar(aes(fill = prediction),
               stat = "identity",
               position = position_dodge())
  }
  return(plot)
}
