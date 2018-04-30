#' Predict distributional parameters of a bamlss family with a bamlss model
#'
#' This function takes a fitted model and a dataframe with explanatory
#' variables and a column for the intercept to compute predicted parameters
#' for the specified distribution.
#'
#' @param model A fitted bamlss model object, created with \code{\link{bamlss}}.
#' @param newdata A data.frame with explanatory variables as columns,
#'   and rows with the combinations you want to do predictions for. Furthermore,
#'   whether or not to include the intercept has to be specified via a logical
#'   variable \code{intercept}.
#' @examples
#' # Generating data
#' data_fam <- model_fam_data()
#' # Fit model
#' beta_model <- bamlss(list(beta ~ norm1 + norm2,
#'   sigma2 ~ norm1 + norm2),
#'   data = data_fam, family = beta_bamlss())
#' # Get 3 predictions
#' pred_df <- data_fam[sample(1:nrow(data_fam), 3), c("norm1", "norm2")]
#' param_preds <- preds(beta_model, pred_df)
#' @return A data.frame with one column for every distributional parameter
#'   and a row for every covariate combination that should be predicted.
#' @importFrom stats na.omit predict
#' @importFrom gamlss predictAll
#' @export

preds <- function(model, newdata) {

  # Omit NA's in prediction data
  newdata <- na.omit(newdata)

  if (any(class(model) == "gamlss")) {
    # Predicted parameters - gamlss
    pred_par <-
      as.data.frame(predictAll(model, newdata = newdata,
                               output = "matrix", type = "response"))
    pred_par <- pred_par[, !colnames(pred_par) %in% "y", drop = FALSE] # goddamn
  } else if (any(class(model) == "bamlss")) {
    # Predicted parameters - bamlss
    pred_par <-
      as.data.frame(predict(model, newdata = newdata, drop = FALSE,
                            type = "parameter", intercept = TRUE))
  } else {
    stop("Class is neither bamlss nor gamlss, so can't make predictions!")
  }

  # Return it here
  return(pred_par)
}

