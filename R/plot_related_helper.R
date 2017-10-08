#' Internal: Get plot limits for a predicted distribution.
#'
#' Returns a data.frame
#' @keywords internal

limits <- function(predictions, family, times_sd = 3) {
  if (family == ".mvnorm")
    return(NULL)
   else if (is.continuous(family)) {
    if (family == "beta") {  # beta can only be from 0 to 1
      return(data.frame(x = c(0, 1)))
    } else if (family == "Generalized Pareto") {
      return(data.frame(x = c(0, 5)))
    } else {
      # Moments
      moments <- as.data.frame(moments(predictions, family))

      # Limits for each 2 moments
      lims <- apply(moments, 1, function(x)
        return(c(x[1] - times_sd * sqrt(x[2]),
                 x[1] + times_sd * sqrt(x[2]))))

      return(data.frame(x = c(min(lims), max(lims))))
    }
  } else if (!is.continuous(family)) {
    if (family == "binomial") {
      return(data.frame(x = c(0, 1)))
    } else if (family == "poisson") {
      # poisson
    } else if (family == "multinomial") {
      # multinomial here
    }
  }
}


#' Internal: Transform discrete predictions into a usable df
#'
#' @importFrom tidyr gather
#' @importFrom stats dpois ppois
#' @keywords internal

disc_trans <- function(predictions, family, type, model) {
  if (family == "binomial") {
    if (type == "pdf") {
      predictions$pi_inv <- 1 - predictions$pi
      predictions$rownames <- row.names(predictions)
      colnames(predictions) <- c("0", "1", "rownames")
      tf_df <- gather(predictions, "type", "value", -rownames)
    } else if (type == "cdf") {
      predictions$pi_inv <- 1
      predictions$rownames <- row.names(predictions)
      colnames(predictions) <- c("0", "1", "rownames")
      tf_df <- gather(predictions, "type", "value", -rownames)
      tf_df <- rbind(tf_df, data.frame(rownames = unique(tf_df$rownames),
                                       type = rep(-1e-100, (nrow(tf_df)/ 2)), # this is because starting point has to be left by just a little margin for plot...
                                       value = rep(0, (nrow(tf_df)/ 2))))
      tf_df$type <- as.numeric(tf_df$type)
    }
  } else if (family == "poisson") {
    if (type == "pdf"){
      limits <- 0:((max(predictions$lambda)*2) + 3) # what lim should preds be?
      tf_df <- apply(predictions, 1, FUN = function(x) return(dpois(limits, x)))
      tf_df <- cbind(tf_df, limits)
      colnames(tf_df) <- c(row.names(predictions), "type")
      tf_df <- gather(as.data.frame(tf_df), key = "rownames", "value", -type)
    } else if (type == "cdf") {
      limits <- 0:((max(predictions$lambda)*2) + 3) # what lim should preds be?
      tf_df <- apply(predictions, 1, FUN = function(x) return(ppois(limits, x)))
      tf_df <- cbind(tf_df, limits)
      colnames(tf_df) <- c(row.names(predictions), "type")
      tf_df <- gather(as.data.frame(tf_df), key = "rownames", "value", -type)
      tf_df <- rbind(data.frame(type = 0, rownames = row.names(predictions),
                                value = -1e-100), # this is because starting point has to be left by just a little margin for plot...
                     tf_df)
    }
  } else if (family == "multinomial") {
    if (type == "pdf") {
      tf_df_start <- mult_trans(predictions, model)
      tf_df <- tf_df_start
      tf_df$rownames <- row.names(tf_df)
      tf_df <- gather(tf_df, "type", "value", -rownames)
      tf_df$type <- factor(tf_df$type, labels = colnames(tf_df_start))
    }
  }
  return(tf_df)
}
