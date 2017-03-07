#' Unify margins
#'
#' This is an internal function called by \code{rake_survey()} that makes sure that the raking targets and the survey data match up.
#'
#' @param pop_margins A list of tibbles giving the population margins for raking variables. \code{create_raking_targets()} outputs such a list.
#' @param .data The survey data to be raked.
#'
#' @details Ensures that the factor levels for each variable in the targets and in the data are in the same order,
#' and that the list of targets is not named.
#'
#' @import dplyr forcats rlang
#' @return Cleaned raking targets.
unify_margins <- function(pop_margins, .data) {
  res <- purrr::map(pop_margins,
                   function(pop_margin) {
                     vname = names(pop_margin[1])
                     # datavar = interaction(.data[, str_split(vname, pattern = "\\.", simplify = T)], sep = ":")
                     datavar = pluck(.data, vname)
                     # This ensures that the factor levels for each variable in the targets and in the data are in the same order
                     unified = fct_unify(list(pop_margin[[vname]], datavar), levels = levels(datavar))
                     pop_margin[[1]] = unified[[1]]
                     out <- as.data.frame(arrange(pop_margin, !!sym(vname)))
                     return(out)
                   })
  names(res) <- NULL
  return(res)
}
