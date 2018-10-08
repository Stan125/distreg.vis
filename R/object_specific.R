#' Model data getter
#'
#' Get the data from the models
#' @importFrom methods is
#' @importFrom stats model.frame
#' @param model A gamlss or bamlss object
#' @param dep If TRUE, then only the dependent variable is returned
#'
#' @keywords internal
model_data <- function(model, dep = FALSE, varname = NULL) {

  # GAMLSS
  if (is(model, "gamlss")) {

    # Put all together
    data_model <- model.frame(model)

    # Put dep variable in
    all_data <- cbind(model$y, data_model)
    dep_name <- as.character(model$mu.formula)[2] # this works, because in gamlss we do not have multivariate responses
    colnames(all_data)[1] <- dep_name

    # Here we check wether we have splines or identical columns
    all_data <- gamlss_data_cleaner(all_data)

    # Make data.frame out of this
    return_object <- as.data.frame(all_data)

    # If dep then return only dependent variable
    if (dep & is.null(varname))
      return_object <- return_object[[dep_name]]

    # If varname then return varname
    if (!dep & !is.null(varname))
      return_object <- return_object[[varname]]

  }

  # BAMLSS
  if (is(model, "bamlss")) {
    return_object <- model$model.frame

    # Return dependent variable if wanted
    if (dep & is.null(varname))
      return_object <- c(model$y)[[1]]

    # Return a specific variable
    if (!dep & !is.null(varname))
      return_object <- return_object[[varname]]
  }
  return(return_object)
}

#' Internal: Function to obtain all explanatory variables used to fit
#'   a model, without the dependent variables
#' @importFrom methods is
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

#' GAMLSS expl_data cleaner
#'
#' This checks whether we have spline column names and/or duplicate columns
#' @keywords internal
gamlss_data_cleaner <- function(temp_df) {
  cnames <- colnames(temp_df)

  # Clean of spline and other functions
  broken_up_list <- strsplit(cnames, "[(]|[)|,]")
  new_cnames <- sapply(broken_up_list, FUN = function(x) {
    if (length(x) != 1)
      return(x[2])
    else
      return(x[1])
  })

  # Assign new colnames
  new_df <- temp_df
  colnames(new_df) <- new_cnames

  # Only retain unique columns
  new_df <- new_df[, unique(new_cnames)]

  # Return it
  return(new_df)
}
