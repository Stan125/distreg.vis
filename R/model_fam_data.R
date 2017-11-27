#' Create a dataset to fit models with all possible families in bamlss
#'
#' @details This function creates a three-dimensional uniform distribution (with
#'   support from 0 to 1) which has a cross-correlation of 0.5. Then the first
#'   dimension is transformed into numerous other distributions via Inverse
#'   Transform Sampling \url{https://en.wikipedia.org/wiki/Inverse_transform_sampling}.
#'    The other two dimensions are transformed into Normal distributions
#'   (norm1, norm2). This procedure ensures that there is a dependency
#'   structure of the transformed first distribution and the other two.
#' @param nrow Number of observations of the exported dataset.
#' @param seed The seed which should be used, for reproducibility.
#' @import bamlss
#' @importFrom stats rmultinom qbeta qbinom qgamma runif qpois qnorm
#' @return A data.frame with columns for differently distributed data.
#' @export

model_fam_data <- function(nrow = 500, seed = 1408) {
  # Set seed
  set.seed(seed)

  ## Get three equi-correlated uniform distributions
  t2u <- function(x) ifelse(x<1, x^2, 2-(2-x)^2)/2
  u_data <- data.frame(u1 = runif(nrow),
                       u2 = runif(nrow),
                       u3 = runif(nrow))
  u_data <- with(u_data, data.frame(v1 = t2u(u1+u2),
                                    v2 = t2u(u1+u3),
                                    v3 = t2u(u2+u3)))

  ## Multinomial data
  probs <- with(u_data, cbind(cbind(v2, v3) %*% c(0, 0),
                              cbind(v2, v3) %*% rnorm(2),
                              cbind(v2, v3) %*% rnorm(2),
                              cbind(v2, v3) %*% rnorm(2)))
  probs <- exp(probs)
  choices <- t(apply(probs, 1, rmultinom, size = 1, n = 1))
  mult_data <- apply(choices, 1, FUN = function(x) which(x == 1))


  ## Families
  data <- data.frame(normal = qnorm(u_data$v1),
                     beta = qbeta(u_data$v1, 2, 5), # Beta distribution, alpha = 2, beta = 5
                     binomial = qbinom(u_data$v1, 1, 0.8), # Binomial distribution, pi = 0.8
                     cnorm = cnorm_bamlss()$q(u_data$v1, # Cnorm distribution, mu = 5, sigma = 2
                                              par = list(mu = 5, sigma = 2)),
                     gamma = qgamma(u_data$v1, shape = 9, scale = 0.5), # Gamma distribution, shape = 9, scale = 0.5
                     gpareto = gpareto_bamlss()$q(u_data$v1, # General Pareto distribution, sigma = 1, xi = 0.4
                                                  list(sigma = 1, xi = 0.4)),
                     poisson = qpois(u_data$v1, 3), # Poisson distribution, lambda = 3
                     multinomial = factor(mult_data,
                                          labels = c("one", "two",
                                                     "three", "four")),
                     norm1 = qnorm(u_data$v2, 3, 5),
                     norm2 = qnorm(u_data$v3, 10, 15),
                     binomial1 = factor(qbinom(u_data$v2, 1, 0.5), labels = c("yes", "no")))
  return(data)
}
