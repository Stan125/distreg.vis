###################################################
#######             distreg.vis            ########
#######         Distributional tests       ########
###################################################

########   ---    Preliminaries   ---    ########

## Libraries
library("distreg.vis")
library("bamlss")
library("gamlss")
library("testthat")
library("ggplot2")
library("gridExtra")
library("patchwork")

## Delete everything
rm(list = ls())

## Context
testthat::context("Test all dists")

##### Complete function #####
test_core <- function(fam_name) {

  ## Cat
  cat(paste0("Distribution Name: ", fam_name, "\n"))

  ########   ---    Creating data   ---    ########
  art_data <- model_fam_data(fam_name = fam_name)

  ########   ---    Creating models   ---    ########

  ## BAMLSS
  if (distreg.vis:::is.bamlss(fam_name)) {
    fam_called <- do.call(get(paste0(fam_name, "_bamlss"),
                              envir = as.environment("package:bamlss")),
                          args = list())

    # Different formulas depending on number of parameters
    form_list <- list(as.formula(paste0(fam_name, "~ norm2 + binomial1")))
    if (length(fam_called$names) > 1)
      form_list[[2]] <- ~ norm2 + binomial1

    # Create model
    model <- bamlss(form_list, data = art_data, family = fam_called, verbose = FALSE)
  }

  ## GAMLSS
  if (distreg.vis:::is.gamlss(fam_name)) {
    form <- as.formula(paste0(fam_name, "~ norm2 + binomial1"))
    model <- gamlss(form, sigma.formula = ~ .,
                    data = art_data, family = fam_name, trace = FALSE)
  }

  ########   ---    plot_dist()   ---    ########

  ## Predict parameters and plot distribution
  ndata <- art_data[sample(seq_len(nrow(art_data)), 5),
                    !colnames(art_data) %in% fam_name]
  pred_params <- preds(model, newdata = ndata)
  plots_dist <- plot_dist(model, pred_params) # pdf
  if (fam_name != "multinomial")
    plots_dist <- plots_dist + plot_dist(model, pred_params, type = "cdf") # cdf

  ## Save the plots
  ggsave(filename = paste0("plot_", fam_name, "_dist.png"), height = 6, width = 12,
         plot = plots_dist)

  ########   ---    moments()   ---    ########

  ## Get the moments
  if (fam_name != "multinomial") {
    if (distreg.vis:::has.moments(fam_name)) {
      moms <- moments(pred_params, fam_name)
    } else {
      expect_error(moms <- moments(pred_params, fam_name))
    }
  }

  ########   ---    moments()   ---    ########

  ## Create plots and save them if available
  if (distreg.vis:::has.moments(fam_name)) {
    # Create
    plots_moments <-
      plot_moments(model, "norm2", pred_data = ndata) +
      plot_moments(model, "binomial1", pred_data = ndata)

    if (fam_name == "LOGNO") {
      ineq <<- function(par) {
        2 * pnorm((par[["sigma"]] / 2) * sqrt(2)) - 1
      }
      plots_moments <- plots_moments +
        plot_moments(model, "norm2", pred_data = ndata, ex_fun = "ineq") +
        plot_moments(model, "binomial1", pred_data = ndata, ex_fun = "ineq")
    }

    # Save
    ggsave(filename = paste0("plot_", fam_name, "_moments.png"), height = 6, width = 12,
           plot = plots_moments)
  } else {
    expect_error(plot_moments(model, "norm2", pred_data = ndata))
  }
}

## Now test the function with all implemented distributions
families <- dists[dists$implemented, "dist_name"]
for (fam in families)
  test_core(fam)
