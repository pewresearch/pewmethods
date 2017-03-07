#' DK to NA
#'
#' Takes a factor and converts "Don't know/Refused" responses to \code{NA}.
#' Matches a variety of common patterns such as "Don't know", "Refused"s, "DK/Ref", etc.
#'
#' @param f A factor variable.
#' @param pattern A regular expression that matches factor levels to convert to NA.
#'
#' @return The factor variable, with DK/Ref values set to NA.
#' @details Make sure to tabulate the variable after running this function and check
#' that levels weren't unintentionally removed.
#' @examples
#' demonstration <- factor(c(rep("Yes", 57), rep("No", 45), rep("Don't know", 4),
#'                         rep("Refused", 2), rep("DK/Ref", 10), rep("Not sure (VOL.)", 5)))
#' tablena(demonstration)
#' demonstration <- dk_to_na(demonstration)
#' tablena(demonstration) # success!
#'
#' @importFrom stringr str_detect
#' @export
dk_to_na <- function(f, pattern = "^[Dd]on['\u2019]t [Kk]|^[Dd][Kk]|^[Rr]efuse|^[Dd][Kk]/[Rr]ef|\\(VOL\\.\\)$") {

  f[str_detect(f, pattern = pattern)] = NA
  return(droplevels(f))

}
