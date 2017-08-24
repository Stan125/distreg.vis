#' Handle predictions in bamlss case (including intercept as variable)
#'

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
    tempdata <- subset(newdata, intercept == TRUE)
    p_m_i <- as.data.frame(predict(model, tempdata, type = "parameter",
                                   intercept = TRUE))
    row.names(p_m_i) <- row.names(tempdata)
  }
  # Get preds for obs without intercept
  if (sum(newdata$intercept) != nrow(newdata)) {
    tempdata <- subset(newdata, intercept == FALSE)
    p_m_ni <- as.data.frame(predict(model, tempdata, type = "parameter",
                                    intercept = FALSE))
    row.names(p_m_ni) <- row.names(tempdata)
  }

  # Put both together
  p_m <- rbind(p_m_i, p_m_ni)
  p_m <- p_m[order(row.names(p_m)), ]

  # Return it here
  if (family(model)$family == "binomial") # always get a data.frame, even in 1d
    return(data.frame(pi = p_m))
  else
    return(p_m)
}
