#' Calculate design effect
#'
#' Takes a vector of weights and calculates the design effect, or the proportional increase
#' in variance due to weighting. Calculated using the Kish (1965) approximation.
#'
#' @param weight A vector of weights. Non-numeric weights will be coerced to numeric.
#' @param include_zeroes Should zero weights be included when calculating \code{n},
#' \code{sd_wt}, and \code{deff}? Defaults to \code{FALSE}.
#'
#' @return A tibble containing the following columns:
#' @return \code{n} The sample size
#' @return \code{sd_wt} The standard deviation of the weights
#' @return \code{deff} The Kish approximation of an overall survey design effect
#' @return \code{ess} The effective sample size
#' @return \code{moe} The design-adjusted margin of error at 95% confidence
#'
#' @examples
#' calculate_deff(dec13_excerpt$weight)
#'
#' @import tibble
#' @export
calculate_deff <- function(weight, include_zeroes = FALSE) {
  weight <- as.numeric(weight)
  if (!include_zeroes) weight <- if_else(weight == 0, NA_real_, weight)
  n <- length(na.omit(weight))
  mean_wt <- mean(weight, na.rm = T)
  sd_wt <- sd(weight, na.rm = T)
  deff <- 1 + (sd_wt/mean_wt)^2
  ess <- n/deff
  variance <- (0.5^2/n) * deff
  std_err <- sqrt(variance)
  moe <- 1.96 * std_err * 100
  return(tibble(n = n,
                mean_wt = mean_wt,
                sd_wt = sd_wt,
                deff = deff,
                ess = ess,
                moe = moe))
}
