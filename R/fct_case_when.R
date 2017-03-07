#' Factor case_when
#'
#' Wrapper around \code{dplyr::case_when} that converts the output to a factor and
#' preserves the order in which value labels were passed into the function.
#'
#' @param ... A sequence of two-sided formulas consistent with \code{dplyr::case_when}.
#' @return The output of \code{dplyr::case_when}, as class \code{"factor"} and ordered
#' however you wanted it.
#' @details Unlike case_when, fct_case_when allows factors to be passed in as
#' right-hand-side arguments - they are treated internally as characters, but the
#' resulting vector will preserve the order of the original factor levels.
#'
#' @examples
#' library(dplyr)
#' partysum <- with(dec13_excerpt, fct_case_when(party == "Republican" ~ "Rep/Lean Rep",
#'                           party == "Democrat"  ~ "Dem/Lean Dem",
#'                           partyln == "Republican" ~ "Rep/Lean Rep",
#'                           partyln == "Democrat" ~ "Dem/Lean Dem",
#'                           TRUE ~ partyln)
#'                           )
#'
#' # Compare to normal case_when() and then factor(), which will arrange the levels in
#' # unwanted alphabetical order
#'
#' partysum <- with(dec13_excerpt, factor(case_when(party == "Republican" ~ "Rep/Lean Rep",
#'                              party == "Democrat"  ~ "Dem/Lean Dem",
#'                              partyln == "Republican" ~ "Rep/Lean Rep",
#'                              partyln == "Democrat" ~ "Dem/Lean Dem",
#'                              TRUE ~ as.character(partyln))))
#'
#' @import rlang
#' @export
fct_case_when <- function(...)
{
  default_env <- caller_env()
  arguments <- list2(...)
  arguments <- Filter(function(elt) !is.null(elt), arguments)
  arg_len <- length(arguments)
  output_levels <- purrr::map(arguments, function(a) {
    out <- f_rhs(a) %>% eval_tidy(env = default_env)
    return(levels(out) %||% out)
  }) %>%
    squash_chr()
  for (i in 1:arg_len) {
    f_rhs(arguments[[i]]) <- as.character(f_rhs(arguments[[i]]) %>%
                                            eval_tidy(env = default_env))
  }
  cw <- do.call(case_when, arguments)
  cw <- factor(cw, levels = unique(output_levels))
  return(cw)
}

