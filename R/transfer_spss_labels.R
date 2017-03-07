#' Transfer SPSS labels and attributes from \code{haven}
#'
#' Takes SPSS attributes from a \code{haven_labelled} variable and attaches them to a recoded
#' numeric variable, which preserves variable and value labels. This function only works
#' with SPSS datasets read in with the \code{haven} package.
#'
#' @param x A vector, or an expression that will output a vector.
#' @param reference_df Reference data.frame, that is to say, a data.frame read in using
#' \code{haven::read_sav()} without any further alterations.
#' @param varname \code{character}, indicating the name of the variable inside the reference
#' data.frame whose attributes should be transferred.
#' @param variable_label \code{character}, indicating the variable label (as displayed in the
#' "Label" column in SPSS).
#' Pass an argument here if you want the SPSS variable label to be different from what's in the
#' original variable.
#' @param value_labels Must be a named \code{numeric} or \code{integer} vector, indicating the
#' value labels (as displayed in the "Values" column in SPSS). The numeric values of the vector
#' indicate the numeric codes of a categorical variable (if applicable), while the names give the
#' text labels.
#' @param format.spss \code{haven} attribute that stores the variable format (as displayed in the
#' "Width" and "Decimals" columns in SPSS.
#' Pass an argument here if you want the SPSS variable format to be different from what's
#' in the original variable.
#' @param display_width \code{haven} attribute that stores the display width. (This is not what's
#' displayed in the "Width" column.)
#' Pass an argument here if you want the SPSS display width to be different from what's
#' in the original variable.
#' @param use_reference \code{logical} indicating whether a reference dataset and variable exists
#' to transfer SPSS labels from. Defaults to \code{TRUE}. Set to \code{FALSE} in order to jump
#' straight to passing in your own attributes.
#'
#' @return The vector with SPSS attributes set.
#'
#' @details Variables in an SPSS dataset read in with \code{haven::read_sav()} will contain
#' additional attributes affecting how they are displayed in SPSS, such as variable labels,
#' value labels, and display width. Recoding variables in R will frequently cause loss of
#' attributes, resulting in disappearing variable and value labels if the dataset with recoded
#' variables is written back to SPSS.
#' This function enables quick transfer of said attributes from a variable
#' from a reference dataset to one that is being recoded. This function also allows you to overwrite
#' selected attributes, for example as the result of a recode that collapses categories or
#' creates new ones.
#'
#' @examples
#' # library(dplyr)
#' # # do not run
#' # # these examples are illustrative, replace them with your own datasets
#' # spss_df <- read_sav("spss_df.sav")
#' # # An expression like this would take a recoded variable such as with dplyr::case_when(),
#' # # carry over the value labels, format.spss and display_width attributes,
#' # # and replace the variable label
#' # spss_df_recoded <- spss_df %>%
#' #   mutate(new_variable = transfer_spss_labels(case_when(old_variable == 1 ~ 3,
#' #                                                        old_variable == 3 ~ 1),
#' #                                              spss_df,
#' #                                              "old_variable",
#' #                                              variable_label = "Old variable reversed"))
#' # # check using the attributes() function
#' # attributes(spss_df_recoded$new_variable)
#' # tablena(spss_df_recoded$new_variable)
#' @importFrom haven labelled
#' @export
transfer_spss_labels <- function(x, reference_df = NULL, varname = NULL, variable_label = NULL,
                                 value_labels = NULL, format.spss = NULL, display_width = NULL,
                                 use_reference = TRUE) {
  if (!use_reference) {
    if (is.numeric(x) | is.character(x)) {
      out <- labelled(x, labels = NULL, label = NULL)
    }
  }
  else {
    if (is.numeric(x) | is.character(x)) {
      out <- labelled(x,
                      labels = attributes(reference_df[[varname]])[["labels"]],
                      label = attributes(reference_df[[varname]])[["label"]]) %>%
        structure(format.spss = attributes(reference_df[[varname]])[["format.spss"]],
                  display_width = attributes(reference_df[[varname]])[["display_width"]])
    }
    else {
      out <- structure(x,
                       label = attributes(reference_df[[varname]])[["label"]],
                       format.spss = attributes(reference_df[[varname]])[["format.spss"]],
                       display_width = attributes(reference_df[[varname]])[["display_width"]])
      }
  }
  if (!is.null(value_labels)) {
    if (!is.vector(value_labels, mode = "numeric")) stop("value_labels is not a numeric vector")
    else if (is.null(names(value_labels))) stop("value_labels has no names")
    out <- structure(out, labels = value_labels)
  }
  if (!is.null(variable_label)) {
    out <- structure(out, label = variable_label)
  }
  if (!is.null(format.spss)) {
    out <- structure(out, format.spss = format.spss)
  }
  if (!is.null(display_width)) {
    out <- structure(out, display_width = display_width)
  }
  return(out)
}

