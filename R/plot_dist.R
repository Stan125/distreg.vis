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
#' @param rug If TRUE, creates a rug plot
#' @return A ggplot2 object.
#' @examples
#' # Generating data
#' data_fam <- model_fam_data(fam_name = "BE")
#' # Fit model
#' library("gamlss")
#' beta_model <- gamlss(BE ~ norm2 + binomial1,
#'   data = data_fam, family = BE())
#' # Get 3 predictions
#' pred_df <- data_fam[sample(1:nrow(data_fam), 3), c("norm2", "binomial1")]
#' param_preds <- preds(beta_model, pred_df)
#' # Create pdf, cdf plots
#' plot_dist(beta_model, param_preds, rug = TRUE)
#' @export

plot_dist <- function(model, pred_params, palette = "default", type = "pdf",
                      rug = FALSE) {

  # Check whether the function is even applied to the right classes
  if (!any(class(model) %in% c("bamlss", "gamlss")))
    stop("This tool only works for bamlss/gamlss classes")

  # Get right family
  fam_name <- fam_obtainer(model)

  # Obtain dependent variable
  if (rug)
    depvar <- model_data(model, dep = TRUE)
  else
    depvar <- NULL

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
  if (is.continuous(fam_name))
    plot <- pdfcdf_continuous(lims, funs_list, type, pred_params, palette, depvar)
  else if (!is.continuous(fam_name))
    plot <- pdfcdf_discrete(pred_params, palette, fam_name, type, model, lims, depvar)

  # Return it
  return(plot)
}

#' Internal: Create the pdf/cdf for continuous covariates
#'
#' Returns a plot
#' @import ggplot2
#' @importFrom viridis scale_fill_viridis scale_colour_viridis
#' @keywords internal

pdfcdf_continuous <- function(lims, funs, type, p_m, palette, depvar) {

  if (type == "cdf") {
    # Assemble plot
    ground <- ggplot(data = data.frame(y = c(0, 1), x = lims),
                     aes_string(x = "x", y = "y")) +
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
    ground <- ggplot(data = data.frame(x = lims), aes_string(x = "x")) +
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

  # This rug really ties the room together
  if (!is.null(depvar)) {
    ground <- ground +
      geom_rug(data = data.frame(depvar),
               aes_string(y = "0",
                          x = "depvar"),
               sides = "b")
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

pdfcdf_discrete <- function(pred_params, palette, fam_name, type, model, lims, depvar) {

  # Transform discrete predictions
  pred_df <- disc_trans(pred_params, fam_name, type, model, lims)

  if (type == "pdf") {
    # Assemble plot
    ground <- ggplot(pred_df, aes_string(x = "xvals", y = "value", fill = "rownames")) +
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

    # This rug really ties the room together
    if (!is.null(depvar) & fam_name != "multinomial") {
      suppressWarnings({
        ground <- ground +
          geom_rug(data = data.frame(depvar, fill = unique(pred_df$rownames)[1]), # weird bug with rownames not specified
                   aes_string(y = "1L",
                              x = "depvar",
                              fill = "fill"),
                   sides = "b", position = "jitter",
                   alpha = 0.5)
      })
    }

  } else if (type == "cdf") {

    if (fam_name == "multinomial")
      stop("CDF for multinomial dist not feasible")

    # Assemble plot
    ground <- ggplot(pred_df, aes_string("xvals", "value", col = "rownames")) +
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

    # This rug really ties the room together
    if (!is.null(depvar)) {
      suppressWarnings({
        ground <- ground +
          geom_rug(data = data.frame(depvar, colour = unique(pred_df$rownames)[1]), # weird bug with rownames not specified
                   aes_string(y = "1L",
                              x = "depvar",
                              colour = "colour"),
                   sides = "b", position = "jitter",
                   alpha = 0.5, colour = "black")
      })
    }
  }

  # Return plot
  return(ground)
}
