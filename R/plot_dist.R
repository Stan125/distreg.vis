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
#' @param display Only specify this when creating plots for two-dimensional
#'   distributions. Can be either \code{"perspective"} for a perspective plot,
#'   \code{"contour"} for a contour plot or \code{"image"} for an
#'   image plot.
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
                      type = "pdf", display = "perspective") {

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
  if (!is.2d(fam, fam_gen$links))
    lims <- limits(p_m, fam)

  # Different plots depending on type of distribution
  if (is.2d(fam, fam_gen$links))
    plot <- pdfcdf_2d(p_m, model, type, display = display)
  else if (is.continuous(fam))
    plot <- pdfcdf_continuous(lims, funs_list, type, p_m, palette)
  else if (!is.continuous(fam))
    plot <- pdfcdf_discrete(p_m, palette, fam, type, model)

  # Return it
  return(plot)
}

#' Internal: Create the pdf/cdf for continuous covariates
#'
#' Returns a plot
#' @import ggplot2
#' @keywords internal

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
#' @keywords internal

pdfcdf_discrete <- function(p_m, palette, family, type, model) {

  # Transform discrete predictions
  pred_df <- disc_trans(p_m, family, type, model)

  if (type == "pdf") {
    # Assemble plot
    ground <- ggplot(pred_df, aes(type, y = value, fill = rownames)) +
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

#' Internal: Create 3D pdf/cdf plot
#'
#' @importFrom plotly plot_ly add_surface layout %>% colorbar
#' @importFrom magrittr set_colnames
#' @keywords internal

pdfcdf_2d <- function(p_m, model, type, display = "perspective") {
  # First we look whether cdf or pdf
  if (type == "pdf") {
    # Function that generates z values
    density_f <- mvnorm_bamlss()$d
    # Description for z values
    desc <- "f(x, y)"
  } else if (type == "cdf") {
    density_f <- real_pmvnorm
    desc <- "F(x, y)"
  }
  # Here possible values are computed
  p <- p_m[nrow(p_m), ] # only last predicton will be evaluated
  xval <- seq(p$mu1 - 3 * p$sigma1, p$mu1 + 3 * p$sigma1, length.out = 100)
  yval <- seq(p$mu2 - 3 * p$sigma2, p$mu2 + 3 * p$sigma2, length.out = 100)

  # Here these values are displayed in different ways
  if (display != "image") { # We have to first check for image because it uses different data structure (column), which is hella annoying
    z <- matrix(0, nrow = 100, ncol = 100)
    for (i in 1:100)
      for (j in 1:100)
        z[i, j] <- density_f(cbind(xval[i], yval[j]), par = p)
    if (display == "perspective") {
      plot <- plot_ly(x = xval, y = yval, z = z) %>%
        add_surface() %>%
        layout(title = "Predicted Distribution",
               scene = list(xaxis = list(title = "x"),
                            yaxis = list(title = "y"),
                            zaxis = list(title = desc)))
    } else if (display == "contour") {
      plot <- plot_ly(x = xval, y = yval, z = z, type = "contour") %>%
        colorbar(title = desc) %>%
        layout(xaxis = list(title = "x"),
               yaxis = list(title = "y"),
               title = "Predicted distribution")
    }
  } else if (display == "image") {
    comb_vals <- expand.grid(xval, yval) %>%
      set_colnames(c("x", "y"))
    comb_vals$z <- apply(comb_vals, 1, function(x)
      return(density_f(matrix(x, ncol = 2), par = p)))
    plot <- ggplot(comb_vals, aes(x, y, fill = z)) +
      geom_tile() +
      ggtitle("Predicted distribution") +
      theme_classic()
    plot$labels$fill <- desc
  }
  return(plot)
}
