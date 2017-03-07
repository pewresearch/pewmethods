#' Trim weights
#'
#' Trim survey weights. Wrapper for the \code{trimWeights()} function in the \code{survey} package that eliminates
#' the need to create a \code{svydesign} object.
#'
#' @param weight A numeric vector of weights
#' @param lower_quantile The quantile at which the weights should be trimmed on the lower end
#' @param upper_quantile The quantile at which the weights should be trimmed on the upper end
#' @param minval NULL by default. If the lower quantile is less than the minimum value, minval will be used
#' for the trim.
#' @param maxval NULL by default. If the upper quantile is greater than the maximum value, maxval will be used
#' for the trim.
#' @param strict When the trimmed weights are reapportioned, some values may exceed the specificed threshold value.
#' If \code{strict = FALSE} this is permitted. Otherwise, weights are trimmed recursively until none exceed the limit.
#'
#' @return A numeric vector of trimmed weights
#'
#' @examples
#' calculate_deff(dec13_excerpt$weight)
#' trimmed <- trim_weights(dec13_excerpt$weight, lower_quantile = 0.1, upper_quantile = 0.9)

#' # Trimming reduces the standard deviation of the weights, reducing reducing the design effect
#' # and increasing the effective sample size.
#' calculate_deff(trimmed)
#'
#' @export
trim_weights <- function (weight, lower_quantile = 0.01, upper_quantile = 0.99, minval = NULL, maxval = NULL, strict = FALSE) {
  design <- svydesign(ids = ~1, data = data.frame(weight = weight), weights = ~weight)
  lower <- quantile(weights(design), lower_quantile)
  upper <- quantile(weights(design), upper_quantile)

  if (!is.null(minval)) {
    lower <- ifelse(lower >= minval, lower, minval)
  }

  if (!is.null(maxval)) {
    upper <- ifelse(upper <= maxval, upper, maxval)
    }

  trimmed <- trimWeights(design, lower = lower, upper = upper, strict = strict)
  return(weights(trimmed))
}

