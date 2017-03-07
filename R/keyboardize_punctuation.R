#' Keyboardize punctuation marks
#'
#' Takes a dataset and, for all categories in factor variables, standardizes stylized
#' typographic characters to ASCII versions that are easily typeable on a keyboard.
#'
#' @param df A \code{data.frame}.
#' @param convert_characters Should variables of class \code{character} also be converted,
#' in addition to variables of class "factor"? Defaults to \code{FALSE}.
#'
#' @return A copy of the original \code{data.frame}, with nonstandard characters replaced.
#'
#' @details
#' Text labels in datasets occasionally contain characters not easily typeable on a standard
#' keyboard which are almost impossible to distinguish from keyboard characters. (This often
#' happens when pasting text in from Microsoft Word). These include the curly apostrophe,
#' the en-dash, the em-dash and the ellipsis. This function converts these typographic
#' characters to characters that are on keyboards by default: the single straight apostrophe,
#' the hyphen-minus, and three periods.
#'
#' @examples
#' library(dplyr)
#' # Create an unwanted factor label containing a stylized apostrophe ’ for illustration.
#' dec13_excerpt <- dec13_excerpt %>% mutate(receduc =
#'                                            forcats::fct_recode(receduc,
#'                                                                `Don’t know/Refused` = "DK/Ref"))
#' table(dec13_excerpt$receduc)
#' dec13_excerpt <- keyboardize_punctuation(dec13_excerpt)
#' table(dec13_excerpt$receduc)
#'
#' @import dplyr
#' @export
keyboardize_punctuation <- function(df, convert_characters = FALSE) {
  orig_labels <- get_spss_label(df, names(df))

  df <- df %>%
    # Convert all single curly quotation marks to straight single quotation marks
    mutate_if(is.factor, .funs = function(x) factor(x, levels = levels(x), labels = str_replace_all(levels(x), "\u2018", "\u0027"))) %>%
    mutate_if(is.factor, .funs = function(x) factor(x, levels = levels(x), labels = str_replace_all(levels(x), "\u2019", "\u0027"))) %>%
    # Convert all en-dashes to hyphens
    mutate_if(is.factor, .funs = function(x) factor(x, levels = levels(x), labels = str_replace_all(levels(x), "\u2013", "\u002D"))) %>%
    # Convert all em-dashes to hyphens
    mutate_if(is.factor, .funs = function(x) factor(x, levels = levels(x), labels = str_replace_all(levels(x), "\u2014", "\u002D"))) %>%
    # Convert all ellipses to three periods
    mutate_if(is.factor, .funs = function(x) factor(x, levels = levels(x), labels = str_replace_all(levels(x), "\u2026", "\u002E\u002E\u002E"))) %>%
    # Convert all double curly quotation marks to straight double quotation marks
    mutate_if(is.factor, .funs = function(x) factor(x, levels = levels(x), labels = str_replace_all(levels(x), "\u201C", "\u0022"))) %>%
    mutate_if(is.factor, .funs = function(x) factor(x, levels = levels(x), labels = str_replace_all(levels(x), "\u201D", "\u0022")))

  if(convert_characters) {
    df <- df %>%
      # Convert all single curly quotation marks to straight single quotation marks
      mutate_if(is.character, .funs = function(x) str_replace_all(x, "\u2018", "\u0027")) %>%
      mutate_if(is.character, .funs = function(x) str_replace_all(x, "\u2019", "\u0027")) %>%
      # Convert all en-dashes to hyphens
      mutate_if(is.character, .funs = function(x) str_replace_all(x, "\u2013", "\u002D")) %>%
      # Convert all em-dashes to hyphens
      mutate_if(is.character, .funs = function(x) str_replace_all(x, "\u2014", "\u002D")) %>%
      # Convert all ellipses to three periods
      mutate_if(is.character, .funs = function(x) str_replace_all(x, "\u2026", "\u002E\u002E\u002E")) %>%
      # Convert all double curly quotation marks to straight double quotation marks
      mutate_if(is.character, .funs = function(x) str_replace_all(x, "\u201C", "\u0022")) %>%
      mutate_if(is.character, .funs = function(x) str_replace_all(x, "\u201D", "\u0022"))

  }
  if(!(all(str_detect(orig_labels, "No label found")))) {
    df <- set_spss_label(df, names(df), orig_labels)
  }
  return(df)
}
