#' Set SPSS label
#'
#' Writes a label to an SPSS dataset read into R with either the \code{foreign} or \code{haven}
#' packages.
#'
#' @param df An SPSS dataset read into R via either \code{foreign} or \code{haven}.
#' @param var \code{character} vector containing the names of variables to be labeled.
#' @param label_text \code{character} vector containing the label or labels to be assigned.
#' @return A copy of \code{df} with variable labels updated.
#'
#' @examples
#' dec13_excerpt <- set_spss_label(dec13_excerpt, "q1", "Obama approval")
#' dec13_excerpt <- set_spss_label(dec13_excerpt,
#'                                c("q1", "q45"),
#'                                c("Obama approval", "Obamacare approval"))
#'
#' # Let's create a fake new variable for illustration
#' dec13_excerpt$newvar <- 1
#' dec13_excerpt <- set_spss_label(dec13_excerpt, "newvar", "New variable")
#' @export
set_spss_label <- function(df, var, label_text) {

  # var and label_text should be the same length
  if (length(var) != length(label_text)) stop("var and label_text should be the same length")

  # vars must be in df
  if (!all(var %in% names(df))) stop("some variable names are not present in dataframe")

  # If the SPSS dataset was read in with the foreign package
  if ("variable.labels" %in% names(attributes(df))) {
    attributes(df)$variable.labels[var] <- label_text
  }

  # If the SPSS dataset was read in with the haven package
  else {
    for (i in 1:length(var)) {
      attributes(df[[var[i]]])$label <- label_text[i]
    }
  }
  return(df)

}
