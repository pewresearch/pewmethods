#' Replace if
#'
#' Takes a vector and replaces elements in the vector according to some condition.
#' (Equivalent to something like \code{df$var1[df$var2 == TRUE] <- 4} but less clunky.)
#' Unlike the base function \code{replace}, \code{replace_if} allows replacement of NAs
#'  with some other value and only accepts scalar values as replacements (preventing
#'  recycling value errors). As with \code{replace}, this function does not work with
#'  factors; use \code{forcats::fct_recode}.
#'
#' @param var A variable.
#' @param condition Either a scalar, a logical vector of the same length as \code{var},
#' or NA (in which case the function will look for and replace all NA values).
#' @param replacement A scalar.
#'
#' @return \code{var}, replaced.
#'
#' @examples
#' x <- c(1, 2, 3, 4)
#' replace_if(var = x, condition = x > 2, replacement = 99)
#'
#' x <- c(1, 2, NA, 4)
#' replace_if(var = x, condition = NA, replacement = 99)
#'
#' @export
replace_if <- function(var, condition, replacement) {

  if (length(replacement) != 1) {
    stop("replacement must be a scalar value")
  }

  if (length(condition) == 1) {
    # Check to make sure condition is not NA
    if (!is.na(condition)) {
      test <- var == condition

      # If it is, use the appropriate test
    } else {
      test <- is.na(var)
    }
  } else if (length(condition) != length(var) | !is.logical(condition)) {
    stop("condition must be either a scalar or a logical vector of the same length as var")
  } else {
    test <- condition
  }
  var[test] <- replacement
  return(var)
}
