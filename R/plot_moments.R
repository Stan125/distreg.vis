#' Plot function: Display the influence of a covariate
#'
#' This function takes a dataframe of predictions with one row per
#'   prediction and one column for every explanatory variable.
#'   Then, those predictions are held constant while one specific variable
#'   is varied over it's whole range (min-max). Then, the constant
#'   variables with the varied interest variables are predicted and plotted
#'   against the expected value and the variance of the underlying
#'   distribution
#' @param int_var The variable for which influences of the moments shall be
#'   graphically displayed. Has to be in character form.
#' @param pred_data Combinations of covariate data, sometimes also known as "newdata",
#'   including the variable of interest, which will be ignored in later
#'   processing.
#' @param model A fitted model on which the plots are based.
#' @param palette See \code{\link{plot_dist}}
#' @importFrom magrittr %>% extract inset set_colnames set_rownames
#' @importFrom viridis scale_fill_viridis scale_colour_viridis
#' @import ggplot2
#' @export

plot_moments <- function(model, int_var, pred_data, palette = "default") {

  # Get model data
  m_data <- model_data(model)

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

  # Use another function if you have multinomial family
  if (fam_obtainer(model) == "multinomial")
    return(plot_multinom_exp(model, int_var, pred_data, m_data, palette, coltype))

  # Now make predictions and find out expected value and variance
  preds <- pred_data %>%
    subset(select = -c(id, prediction)) %>%
    preds(model, newdata = .) %>%
    distreg.vis:::moments(par = ., fam_name = fam_obtainer(model)) %>%
    inset("id", value = row.names(.))
  moments <- colnames(preds)[colnames(preds) != "id"]

  # Merge predictions with pred_data, transform into long and to numerics
  preds <- preds %>%
    merge(y = pred_data, by.x = "id") %>%
    extract(, c(int_var, "prediction", moments)) %>%
    set_colnames(c(int_var, "prediction", paste0("mom.", moments))) %>%
    reshape(., direction = "long",
            varying = seq_along(moments) + 2, # because moments start after 2
            idvar = c("norm1", "prediction")) %>%
    set_rownames(seq_along(rownames(.))) %>%
    set_colnames(c(int_var, "prediction", "moment", "value"))

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

  # Palettes
  if (palette == "viridis") {
    ground <- ground +
      scale_fill_viridis(discrete = TRUE) +
      scale_colour_viridis(discrete = TRUE)
  } else if (palette != "default") {
    ground <- ground +
      scale_fill_brewer(palette = palette) +
      scale_colour_brewer(palette = palette)
  }

  # Line if numeric
  if (coltype == "num") {
    plot <- ground +
      geom_line()
  } else if (coltype == "cat") {
    plot <- ground +
      geom_bar(aes(fill = prediction),
               stat = "identity",
               position = position_dodge())
  }
  return(plot)
}


#' Internal: Plot function as sub-case to plot_moments for
#'   multinomial family
#'
#' @import ggplot2
#' @importFrom magrittr %>% inset extract set_rownames set_colnames
#' @importFrom viridis scale_fill_viridis scale_colour_viridis
#' @keywords internal

plot_multinom_exp <- function(model, int_var, pred_data, m_data, palette, coltype) {
    # Get predictions for each class dep on int_var
    preds <- pred_data %>%
      subset(select = -c(id, prediction)) %>%
      preds(model, newdata = .) %>%
      distreg.vis:::mult_trans(., model) %>%
      inset("id", value = row.names(.))
    classes <- as.character(unique(m_data[, 1]))
    preds <- preds %>%
      merge(y = pred_data, by.x = "id") %>%
      extract(, c(int_var, classes, "prediction")) %>%
      set_colnames(c(int_var, paste0("c.", classes), "prediction")) %>%
      reshape(., direction = "long",
              varying = seq_len(length(classes)) + 1,
              idvar = c("norm1", "prediction")) %>% # because classes start with second column
      set_colnames(c("norm1", "prediction", "class", "value")) %>%
      set_rownames(seq_len(nrow(.)))

    # Numerical influence plot
    if (coltype == "num") {
      ground <- ggplot(preds, aes_string(x = int_var, y = "value", fill = "class")) +
        geom_area() +
        facet_wrap(~prediction) +
        labs(y = "Expected value of class") +
        ggtitle(paste("Influence of", int_var,
                      "on expected values of every class' pi_i")) +
        theme_bw()
    }
    # Categorical influence plot
    if (coltype == "cat") {
      ground <- ggplot(preds, aes_string(x = int_var, y = "value", fill = "class")) +
        geom_bar(stat = "identity", position = position_dodge()) +
        facet_wrap(~prediction) +
        labs(y = "Expected value of class") +
        ggtitle(paste("Influence of", int_var,
                      "on expected values of every class' pi_i")) +
        theme_bw()
    }

    # Palettes
    if (palette == "viridis") {
      ground <- ground +
        scale_fill_viridis(discrete = TRUE) +
        scale_colour_viridis(discrete = TRUE)
    } else if (palette != "default") {
      ground <- ground +
        scale_fill_brewer(palette = palette) +
        scale_colour_brewer(palette = palette)
    }
    return(ground)
}
