#' Convert all categorical variables into dummy/indicator variables
#'
#' Takes a data frame and separates all columns containing categorical variables into
#' dummy variables (one column per category). Unlike model.matrix(), does not exclude any
#' categories from being converted into columns, and retains missing values.
#'
#' @param df A data frame containing the factors to be converted to dummies.
#' @param dummify_characters Should \code{character} variables be converted to factors and
#' dummified? Defaults to \code{TRUE}.
#' @param max_levels The maximum number of levels that a categorical variable can have in
#' order to be converted to dummies. This is to prevent converting variables such as
#' respondent IDs or open-ended responses to dummy variables. Defaults to 52, which is
#' slightly more than the number of U.S. states plus Washington, D.C.
#' @param sep A character string that will go in between the original variable name and the
#' corresponding factor level in the output, e.g. VARIABLE__Category. Defaults to \code{"__"}.
#'
#' @return A copy of the original data frame where all factor variables have been replaced
#' with dummies. Formatted as a tibble.
#'
#' @examples
#' test <- dummify_factors(dec13_excerpt)
#' head(test)
#'
#' @import dplyr tibble
#' @export
dummify_factors <- function(df, dummify_characters=TRUE, max_levels = 52, sep = "__") {
  if (dummify_characters) {
    df <- mutate_if(df, is.character, as.factor)
  }

  dummy_frame <- purrr::imap(df, function(f, fname) {

    if (!is.factor(f)) {
      return(tibble(!!fname := f))
    }

    if (length(levels(f)) > max_levels) {
      return(tibble(!!fname := f))
    }

    dum <- levels(f) %>%
      purrr::set_names() %>%
      purrr::map(~as.integer(f==.x)) %>%
      bind_cols()

    names(dum) <- sprintf("%s%s%s", fname, sep, names(dum))
    return(dum)
  }) %>%
    bind_cols()

  return(dummy_frame)

}
