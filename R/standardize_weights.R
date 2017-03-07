#' Standardize weights
#'
#' Divides each weight by the mean and converts missing weights to 0.
#' The resulting weights will have a mean of 1 and sum to either the nominal sample size
#' or the effective sample size, which is the nominal sample size divided by the Kish (1965)
#' approximation of an overall survey design effect
#'
#' @param weight A vector of weights. Coerced to \code{numeric} if a different type.
#' @param to_ess Should the weights sum to the effective sample size? Defaults to \code{FALSE}.
#' @param na_to_zero Should missing weights become zero weights? Defaults to \code{TRUE}.
#'
#' @return A vector of standardized weights.
#'
#' @examples
#' summary(dec13_excerpt$weight)
#' dec13_excerpt$weight <- standardize_weights(dec13_excerpt$weight)
#' summary(dec13_excerpt$weight)
#'
#' @export
standardize_weights = function(weight, to_ess = FALSE, na_to_zero = TRUE) {
  numweights <- as.numeric(weight)
  if (to_ess) {
    ess = calculate_deff(numweights)[["ess"]]
    std_weight = ess * numweights/sum(numweights, na.rm = T)
  } else {
    std_weight <- numweights/mean(numweights, na.rm = T)
  }
  if (na_to_zero) std_weight[is.na(std_weight)] = 0
  return(std_weight)
}
