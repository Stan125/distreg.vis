#' Internal: Function to transform multinomial predictions
#'
#' This function exists solely to transform predictions of the multinomial dist.
#' Transforms odds into probabilities to get into each class.

mult_trans <- function(predictions, model) {
  levels <- levels(model$model.frame[, 1])
  psums <- rowSums(predictions) + 1
  p0 <- 1 / psums
  trans_preds <- cbind(p0, apply(predictions, 2, FUN = function(x)
    return(x * p0)))
  trans_preds <- as.data.frame(trans_preds)
  colnames(trans_preds) <- levels
  return(trans_preds)
}
