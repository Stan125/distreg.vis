#' Model data getter
#'
#' Get the data from the models
model_data <- function(model) {

  # GAMLSS
  if (is(model, "gamlss")) {
    data_parts <- paste0(model$parameters, ".x")
    parts_combined <- do.call("cbind", model[data_parts])
    data_model <- parts_combined[, colnames(parts_combined) != "(Intercept)", drop = FALSE]
    all_data <- cbind(model$y, data_model)
    dep_name <- as.character(model$mu.formula)[2] # this works, because in gamlss we do not have multivariate responses
    colnames(all_data)[1] <- dep_name
    return(as.data.frame(all_data))
  }

  # BAMLSS
  if (is(model, "bamlss")) {
    return(model$model.frame)
  }
}

#' Internal: Function to obtain all explanatory variables used to fit
#'   a model, without the dependent variables
#' @keywords internal

expl_vars <- function(model) {
  all_data <- model_data(model)

  # GAMLSS
  if (is(model, "gamlss")) {
    dep_name <- as.character(model$mu.formula)[2]
    expl_vars <- all_data[, !colnames(all_data) %in% dep_name, drop = FALSE]
  }

  # BAMLSS
  if (is(model, "bamlss")) {
    dep_name <- colnames(model$y)
    expl_vars <- all_data[, !colnames(all_data) %in% dep_name, drop = FALSE]
  }
  return(expl_vars)
}
