#' Plot predicted bamlss distribution families with ggplot2
#'
#' This function plots the parameters of a predicted distribution (e.g. obtained
#' through \code{\link{preds}}) with ggplot2. You can use all implemented
#' families in bamlss except the cox family.
#'
#' @param model A fitted bamlss object.
#' @param pred_params A data.frame with rows for every model prediction and
#'   columns for every predicted parameter of the distribution. Is easily obtained
#'   with the \code{distreg.vis} function \code{\link{preds}}.
#' @param palette The colour palette used for colouring the plot. You can use
#'   any of the ones supplied in \code{\link[ggplot2]{scale_fill_brewer}} though I
#'   suggest you use one of the qualitative ones: Accent, Dark2, etc. Since 0.5.0
#'   \code{"viridis"} is included, to account for colour blindness. If you want to do
#'   3D plots, the accepted palettes are one of: \code{"default"}(viridis),
#'   \code{"Blues"}, \code{"Greens"}, \code{"OrRd"}, \code{"Purples"},
#'   \code{"Spectral"}, \code{"RdYlBu"}, \code{"RdYlGn"}.
#' @param type Do you want the probability distribution function ("pdf") or
#'   the cumulative distribution function ("cdf")?
#' @param display Only specify this when creating plots for two-dimensional
#'   distributions. Can be either \code{"perspective"} for a perspective plot,
#'   \code{"contour"} for a contour plot or \code{"image"} for an
#'   image plot.
#' @return A ggplot2 object.
#' @examples
#' # Generating data
#' data_fam <- model_fam_data(fam_name = "beta")
#' # Compute model
#' library("bamlss")
#' beta_model <- bamlss(list(beta ~ norm2 + binomial1,
#'   sigma2 ~ norm2 + binomial1),
#'   data = data_fam, family = beta_bamlss())
#' # Get 3 predictions
#' pred_df <- data_fam[sample(1:nrow(data_fam), 3), c("norm2", "binomial1")]
#' param_preds <- preds(beta_model, pred_df)
#' # Create pdf, cdf plots
#' plot_dist(beta_model, param_preds)
#' plot_dist(beta_model, param_preds, type = "cdf")
#' plot_dist(beta_model, param_preds, palette = "Dark2")
#' @export

plot_dist <- function(model, pred_params, palette = "default",
                      type = "pdf", display = "perspective") {

  # Check whether the function is even applied to the right classes
  if (!any(class(model) %in% c("bamlss", "gamlss")))
    stop("This tool only works for bamlss/gamlss classes")

  # Get right family
  fam_name <- fam_obtainer(model)

  # Check here whether distribution is even implemented
  if (!is.implemented(fam_name))
    stop("Family not implemented")

  # Get correct pdf and cdf functions - pdf_cdf_getter should also check whether a cdf is even available
  if (is.continuous(fam_name))
    funs_list <- list(pdf = fam_fun_getter(fam_name, "d"),
                      cdf = fam_fun_getter(fam_name, "p"))

  # Get correct limits
  lims <- limits(fam_name, pred_params)

  # Different plots depending on type of distribution
  if (is.2d(model))
    plot <- pdfcdf_2d(pred_params, model, type, display = display, palette = palette)
  else if (is.continuous(fam_name))
    plot <- pdfcdf_continuous(lims, funs_list, type, pred_params, palette)
  else if (!is.continuous(fam_name))
    plot <- pdfcdf_discrete(pred_params, palette, fam_name, type, model, lims)

  # Return it
  return(plot)
}

#' Internal: Create the pdf/cdf for continuous covariates
#'
#' Returns a plot
#' @import ggplot2
#' @importFrom viridis scale_fill_viridis scale_colour_viridis
#' @keywords internal

pdfcdf_continuous <- function(lims, funs, type, p_m, palette) {
  if (type == "cdf") {
    # Assemble plot
    ground <- ggplot(data = data.frame(y = c(0, 1), x = lims), aes_("x", "y")) +
      ggtitle("Predicted distribution(s)") +
      labs(x = "y", y = "F(y)")

    # Add functions
    for (i in seq_len(nrow(p_m))) {
      args <- p_m[i, , drop = FALSE]
      ground <- ground +
        stat_function(fun = funs$cdf, args = list(par = as.list(args)),
                      geom = "line", aes_(col = row.names(args)))
    }
  } else if (type == "pdf") {
    # Assemble plot
    ground <- ggplot(data = data.frame(x = lims), aes_("x")) +
      ggtitle("Predicted distribution(s)") +
      labs(x = "y", y = "f(y)")

    # Add functions
    for (i in seq_len(nrow(p_m))) {
      args <- p_m[i, , drop = FALSE]
      ground <- ground +
        stat_function(fun = funs$pdf, args = list(par = as.list(args)),
                      geom = "area", aes_(fill = row.names(args)), alpha = 0.7)
    }
  }

  # Different theme
  ground <- ground + theme_classic()

  # Colour Palettes - if not default or viridis
  if (palette == "viridis") {
    ground <- ground +
      scale_fill_viridis(discrete = TRUE) +
      scale_colour_viridis(discrete = TRUE)
  } else if (palette != "default") {
    ground <- ground +
      scale_fill_brewer(palette = palette) +
      scale_colour_brewer(palette = palette)
  }


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
#' @importFrom viridis scale_fill_viridis scale_colour_viridis
#' @keywords internal

pdfcdf_discrete <- function(pred_params, palette, fam_name, type, model, lims) {

  # Transform discrete predictions
  pred_df <- disc_trans(pred_params, fam_name, type, model, lims)

  if (type == "pdf") {
    # Assemble plot
    ground <- ggplot(pred_df, aes_("xvals", y = "value", fill = "rownames")) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "y", y = "f(y)") +
      ggtitle("Predicted distributions(s)")

    # Classic theme
    ground <- ground + theme_classic()

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

    # Legend label
    ground$labels$fill <- "Predictions"

  } else if (type == "cdf") {
    # Assemble plot
    ground <- ggplot(pred_df, aes("xvals", "value", col = "rownames")) +
      geom_step(linetype = 2) +
      labs(x = "x", y = "F(x)") +
      ggtitle("Predicted distribution(s)")

    # Classic theme
    ground <- ground + theme_classic()

    # Palette
    if (palette == "viridis") {
      ground <- ground +
        scale_fill_viridis(discrete = TRUE) +
        scale_colour_viridis(discrete = TRUE)
    } else if (palette != "default") {
      ground <- ground +
        scale_fill_brewer(palette = palette) +
        scale_colour_brewer(palette = palette)
    }

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
#' @importFrom viridis scale_colour_viridis scale_fill_viridis
#' @keywords internal

pdfcdf_2d <- function(p_m, model, type = "pdf", display = "perspective",
                      palette = "default") {
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

  # Make the palette
  cols <- palette_getter(palette)

  # Here these values are displayed in different ways
  if (display != "image") { # We have to first check for image because it uses different data structure (column), which is hella annoying
    z <- matrix(0, nrow = 100, ncol = 100)
    for (i in 1:100)
      for (j in 1:100)
        z[i, j] <- density_f(cbind(xval[i], yval[j]), par = p)
    if (display == "perspective") {
      plot <- plot_ly(x = xval, y = yval, z = z) %>%
        add_surface(colors = cols) %>%
        layout(title = "\nPredicted Distribution",
               scene = list(xaxis = list(title = "x"),
                            yaxis = list(title = "y"),
                            zaxis = list(title = desc)))
    } else if (display == "contour") {
      plot <- plot_ly(x = xval, y = yval, z = z, type = "contour",
                      colors = cols) %>%
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
    plot <- ggplot(comb_vals, aes_("x", "y", fill = "z")) +
      geom_tile() +
      ggtitle("Predicted distribution") +
      theme_classic() +
      scale_colour_viridis() +
      scale_fill_viridis()
    if (!is.null(cols))
      plot <- plot + scale_fill_gradientn(colors = cols)
    plot$labels$fill <- desc
  }
  return(plot)
}
