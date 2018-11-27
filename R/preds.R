#' Predict distributional parameters of a bamlss family with a bamlss model
#'
#' This function takes a fitted model and a dataframe with explanatory variables
#' and a column for the intercept to compute predicted parameters for the
#' specified distribution.
#'
#' @param model A fitted bamlss model object, created with \code{\link{bamlss}}.
#' @param newdata A data.frame with explanatory variables as columns, and rows
#'   with the combinations you want to do predictions for. Furthermore, whether
#'   or not to include the intercept has to be specified via a logical variable
#'   \code{intercept}.
#' @param what One of \code{mean}, \code{upperlimit}, \code{lowerlimit}. If it
#'   is \code{mean} (which is also the default), then the mean of the parameter
#'   samples is calculated. 2.5% and 97.5% quantiles are calculated for
#'   \code{lowerlimit} and \code{upperlimit}, respectively.
#' @examples
#' # Generating data
#' data_fam <- model_fam_data(fam_name = "BE")
#' # Fit model
#' library("gamlss")
#' beta_model <- gamlss(BE ~ norm2 + binomial1,
#'   data = data_fam, family = BE())
#' # Get 3 predictions
#' pred_df <- data_fam[sample(1:nrow(data_fam), 3), c("binomial1", "norm2")]
#' param_preds <- preds(beta_model, pred_df)
#' @return A data.frame with one column for every distributional parameter and a
#'   row for every covariate combination that should be predicted.
#' @importFrom stats na.omit predict
#' @importFrom gamlss predictAll
#' @export

preds <- function(model, newdata, what = "mean") {

  # Check and convert to data.frame
  if ("data.frame" %in% class(newdata))
    newdata <- as.data.frame(newdata)
  else
    stop("Newdata has to be in a data.frame format")

  # Omit NA's in prediction data
  newdata <- na.omit(newdata)

  # Rownames
  rnames <- row.names(newdata)

  if (is(model, "gamlss")) {

    # Only mean possible to be calculated
    if (what != "mean")
      stop("For gamlss models only mean can be calculated")

    # Predicted parameters - gamlss
    pred_par <-
      as.data.frame(predictAll(model, newdata = newdata,
                               output = "matrix", type = "response",
                               data = model_data(model)),
                    row.names = rnames)
    pred_par <- pred_par[, !colnames(pred_par) %in% "y", drop = FALSE] # goddamn

  } else if (is(model, "bamlss")) {

    # Mean
    if (what == "mean") {
      # Predicted parameters - bamlss - mean
      pred_par <-
        as.data.frame(predict(model, newdata = newdata, drop = FALSE,
                              type = "parameter", intercept = TRUE),
                      row.names = rnames)
    }

    # 2.5% Quantile
    if (what == "lowerlimit") {
      # Predicted parameters - bamlss - 2.5%
      pred_par <-
        as.data.frame(predict(model, newdata = newdata, drop = FALSE,
                              type = "parameter", intercept = TRUE,
                              FUN = function(x)
                                quantile(x, 0.025, na.rm = TRUE)),
                      row.names = rnames)
    }

    # 97.5% Quantile
    if (what == "lowerlimit") {
      # Predicted parameters - bamlss - 2.5%
      pred_par <-
        as.data.frame(predict(model, newdata = newdata, drop = FALSE,
                              type = "parameter", intercept = TRUE,
                              FUN = function(x)
                                quantile(x, 0.975, na.rm = TRUE)),
                      row.names = rnames)
    }

  } else {
    stop("Class is neither bamlss nor gamlss, so can't make predictions!")
  }

  # Return it here
  return(pred_par)
}

