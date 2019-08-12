#' distreg Searcher
#'
#' Function that searches the WD for a distreg model
#' @keywords internal

search_distreg <- function() {
  # Get components of Global Environment
  lsc <- ls(envir = .GlobalEnv)

  # Look for bamlss & gamlss components
  distreg_true <- sapply(lsc, FUN = distreg_checker)

  # Return nothing if no bamlss or gamlss
  if (length(distreg_true) > 0)
    if (sum(distreg_true) > 0)
      return(lsc[distreg_true])
  else
    return("")
}

#' function Searcher
#'
#' Function that looks for objects of class 'function' in the working directory.

search_funs <- function() {
  # Get components of Global Envir
  lsc <- ls(envir = .GlobalEnv)

  # Look for functions
  functions_true <- sapply(lsc, FUN = function(x)
    return(is.function(get(x, envir = .GlobalEnv))))

  # Return only if there is a function
  if (length(functions_true) > 0)
    if (sum(functions_true) > 0)
      return(c("NO FUNCTION", lsc[functions_true]))
  else
    return("NO FUNCTION")
}

#' Check if object is a distributional regression object
#'
#' @keywords internal
#' @details At the moment, the following classes are supported:
#' \itemize{
#' \item gamlss
#' \item bamlss
#' \item betareg
#' }

distreg_checker <- function(x) {
  if (is.character(x))
    obj <- get(x, envir = .GlobalEnv)
  else
    obj <- x
  if (is(obj, "bamlss"))
    return(TRUE)
  else if (is(obj, "gamlss"))
    return(TRUE)
  else if (is(obj, "betareg"))
    return(TRUE)
  else
    return(FALSE)
}

#' Factor Equalizer
#'
#' Function that takes the levels of a df's factors and puts them to a second df
#'   (used for predictions). Returns a data.frame
#' @keywords internal

fac_equ <- function(base_df, pred_df) {
  # Factor Names
  varclass <- sapply(base_df, FUN = is.factor)
  varc_index <- which(varclass)

  # Make right factor levels
  for (i in varc_index)
    pred_df[, i] <- factor(pred_df[, i], levels = levels(base_df[, i]))

  # Return the pred_df
  return(pred_df)
}

#' Factor Checker
#'
#' Checks whether some factor was unwantedly converted to an ordered factor
#' which rhandsontable sometimes does
#' @keywords internal

fac_check <- function(DF) {
  rn <- row.names(DF)
 DF <- lapply(DF, FUN = function(x) {
   if ("ordered" %in% class(x))
     return(factor(x, levels = levels(x), ordered = FALSE))
   else
     return(x)
 })
 DF <- data.frame(DF, row.names = rn)
 return(DF)
}

#' Function for better use of formatR's tidy_source
#' @keywords internal
#' @importFrom formatR tidy_source

tidy_c <- function(x)
  return(tidy_source(text = x, output = FALSE, width.cutoff = 45))$text.tidy


#' Obtain d&p&q functions
#'
#' Takes a family name and what kind of function you want and gives the right one back
#' @keywords internal
#' @importFrom stats dbeta pbeta

fam_fun_getter <- function(fam_name, type) {

  if (!is.distreg.fam(fam_name))
    stop("Quants only obtainable for distreg families.")

  # Test which type is wanted
  if (!type %in% c("d", "p", "q"))
    stop("Specified wrong type!")

  # GAMLSS
  if (is.gamlss(fam_name)) {
    raw_name <- paste0(type, fam_name)

    if (type %in% c("d", "p")) {# this is necessary because of argument matching
      fun <- function(x, par)
        return(do.call(get(force(raw_name), envir = as.environment("package:gamlss.dist")),
                       c(list(x), par))) # why does it preserve d_raw_name even if this function is used outside of this environment? http://adv-r.had.co.nz/Functions.html
    } else if (type == "q") {
      fun <- function(p, par)
        return(do.call(get(force(raw_name), envir = as.environment("package:gamlss.dist")),
                       c(list(p), par)))
    }
  }

  # BAMLSS
  if (is.bamlss(fam_name)) {
    raw_name <- paste0(fam_name, "_bamlss")
    fam_called <- do.call(get(force(raw_name), envir = as.environment("package:bamlss")),
                          args = list())
    fun <- fam_called[[type]]
    if (is.null(fun))
      stop(paste(type, "function not implemented."))
  }

  # betareg
  if (is.betareg(fam_name)) {
    if (type == "d") {
      fun <- function(x, par) {
        alpha <- par[["mu"]] * par[["phi"]]
        beta <- (1 - par[["mu"]]) * par[["phi"]]
        return(dbeta(x, alpha, beta))
      }
    }
    if (type == "p") {
      fun <- function(x, par) {
        alpha <- par[["mu"]] * par[["phi"]]
        beta <- (1 - par[["mu"]]) * par[["phi"]]
        return(pbeta(x, alpha, beta))
      }
    }
    if (type == "q") { # here we need to use p as argument in order to match everything correctly
      fun <- function(p, par) {
        alpha <- par[["mu"]] * par[["phi"]]
        beta <- (1 - par[["mu"]]) * par[["phi"]]
        return(qbeta(p, alpha, beta))
      }
    }
  }

  # Return the function
  return(fun)
}
