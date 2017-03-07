#' Get SPSS label
#'
#' Returns variable labels from an SPSS dataset read in with either the \code{foreign}
#' or \code{haven} packages.
#'
#' @param df An SPSS dataset originally read into R via either \code{foreign} or
#'   \code{haven}.
#' @param var \code{character} vector containing the names of variables whose labels
#' should be returned.
#' @param null_label \code{character} value to return if there is no label for a variable.
#' Defaults to \code{"No label found"}.
#' @param unlist Should the output be converted from a list to a vector? Defaults to
#' \code{TRUE}. Set to \code{FALSE} to return a list instead.
#' @return The SPSS label/s, as a vector or a list.
#'
#' @examples
#' get_spss_label(dec13_excerpt, "q1")
#' get_spss_label(dec13_excerpt, c("q1", "party"))
#' get_spss_label(dec13_excerpt, c("q1", "sdfklsdf", "party"), null_label = "here be dragons")
#' get_spss_label(dec13_excerpt, c("q1", "sdfklsdf", "party"), unlist = FALSE)
#' @importFrom purrr map pluck attr_getter
#' @export
get_spss_label <- function(df, var, null_label = "No label found", unlist = TRUE) {
  # If the SPSS dataset was read in with the foreign package
  if ("variable.labels" %in% names(attributes(df))) {
    l <- map(var, ~pluck(df, attr_getter("variable.labels"), .x))
  }

  # If the SPSS dataset was read in with the haven package
  else {
    l <- map(var, ~pluck(df, .x, attr_getter("label")))
  }

  # If there are missing labels and an argument is passed to null_label
  label_missing <- which(unlist(map(l, is.null)))
  if (length(label_missing) > 0) {
    for (i in label_missing) {
      l[[i]] <- null_label
    }
  }

  # If unlist is TRUE
  if (unlist) l <- unlist(l)
  return(l)
}
