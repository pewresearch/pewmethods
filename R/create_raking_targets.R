#' Create raking targets
#'
#' Given a dataset, creates a list of tibbles, each summarizing the marginal distribution
#' of categorical variables from that dataset. These can be used as raking targets by
#' passing them to the \code{pop_margins} argument in \code{rake_survey}. Each element in
#' the list will have two columns: the name of the raking target, which will by default have
#' the prefix "rk_" appended to indicate being a raking target, and the percentage of each
#' category in that variable.
#'
#' @param bm_data The name of the dataset to be used for calculating marginal distributions.
#' @param vars A character vector containing the names of all the variables that will be
#' used for raking targets.
#' Interactions between variables can be specified using the convention
#' \code{"variable1:variable2"}.
#' @param prefix A string containing the prefix to be prepended to the name of the first
#' column of each raking target. "rk_" by default.
#' @param new_sep The character separating interaction variables, if applicable. \code{"_"}
#' by default, does not do anything if no interaction variables present.
#' @param wt The weight to be used in calculating the targets. For unweighted targets,
#' use wt = 1.
#'
#' @return A list, with each element being a tibble returned by \code{get_totals} for
#' each raking target.
#'
#' @details Datasets used to create raking targets generally come from microdata describing
#' your population, which is taken to be the ground truth. For example, one frequently used
#' dataset for obtaining demographic raking targets for the population of U.S. adults is
#' the American Community Survey. If the dataset used is itself a survey, it may come with
#' ts own survey weights needed for the raking targets to accurately describe the target
#' population, in which case those weights need to be passed to the \code{wt} argument.
#' To prevent errors, a value must be supplied for \code{wt}. Use \code{wt = 1} if targets
#' are to be based on unweighted data.
#'
#' It is good practice to separate out variables used for raking from the raw variables,
#' because raking variables may be processed via recoding and imputation, among other things.
#' The \code{prefix} argument enforces this practice by adding a prefix to the names of the
#' raking variables. If the output of this function is subsequently passed to the
#' \code{pop_margins} argument of \code{rake_survey}, the code will search the sample data
#' file for variables with the same names. This is meant to ensure consistency.
#'
#' This function allows you to pass interactions between variables into the \code{vars}
#' argument by inserting a \code{:} between two variable names. When an interaction is
#' specified, the variable names will be concatenated using \code{new_sep}.
#'
#' @examples
#'
#' # Here we will use the acs_2017_excerpt dataset included wih the package
#'
#' # Notice that the names in the output are by default called rk_sex, rk_recage, rk_receduc,
#' # and rk_sex_receduc
#' create_raking_targets(acs_2017_excerpt,
#'                       vars = c("sex", "recage", "receduc", "sex:receduc"),
#'                       wt = "weight")
#'
#' @import stringr tibble
#' @export
create_raking_targets <- function(bm_data, vars, prefix = "rk_", new_sep = "_", wt = NULL) {

  if(is.null(wt)) stop("No weight specified. For unweighted targets, specify wt = 1")

  if(wt == 1) {
    targets <- vars %>% purrr::map(get_totals, df = bm_data)
    message("These are UNWEIGHTED targets")
  }
  else {
    targets <- vars %>% purrr::map(get_totals, df = bm_data, wt = wt)
  }
  targets <- lapply(targets, FUN = function(x) purrr::set_names(x,
                                                         nm = c(str_replace(sprintf("%s%s", prefix, names(x)[1]), ":", new_sep), "Freq")))
  targets <- purrr::map(targets, ~as_tibble(.x))
  return(targets)
}
