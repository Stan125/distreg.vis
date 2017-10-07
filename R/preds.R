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
#' pred_df <- cbind(pred_df, intercept = rep(TRUE, 3))
#' param_preds <- preds(beta_model, pred_df)
#' @return A data.frame with one column for every distributional parameter
#'   and a row for every covariate combination that should be predicted.
#' @importFrom stats na.omit predict
#' @export

preds <- function(model, newdata) {
  # Stop if no intercept
  if (is.null(newdata$intercept))
    stop("Intercept not specified")

  # Omit NA's
  newdata <- na.omit(newdata)

  # Two df's with and without intercept
  p_m_i <- data.frame()
  p_m_ni <- data.frame()

  # Get predictions for obs with intercept
  if (sum(newdata$intercept) != 0) {
    tempdata <- newdata[newdata$intercept == TRUE, ]
    p_m_i <- as.data.frame(predict(model, tempdata, type = "parameter",
                                   intercept = TRUE, drop = FALSE))
    row.names(p_m_i) <- row.names(tempdata)
  }
  # Get preds for obs without intercept
  if (sum(newdata$intercept) != nrow(newdata)) {
    tempdata <- newdata[newdata$intercept == FALSE, ]
    p_m_ni <- as.data.frame(predict(model, tempdata, type = "parameter",
                                    intercept = FALSE, drop = FALSE))
    row.names(p_m_ni) <- row.names(tempdata)
  }

  # Put both together
  p_m <- rbind(p_m_i, p_m_ni)
  p_m <- p_m[order(row.names(p_m)), , drop = FALSE]

  # Return it here
  return(p_m)
}
