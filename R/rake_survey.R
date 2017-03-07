#' Rake survey
#'
#' Applies the raking algorithm to a survey using specified raking targets in order to obtain weights.
#' Wrapper around the \code{calibrate} function from the \code{survey} package with argument
#' \code{calfun = "raking"}.
#'
#' @param .data A \code{data.frame} containing the survey data
#' @param pop_margins A list of tibbles giving the population margins for raking variables.
#' \code{create_raking_targets} outputs such a list in the correct format.
#' @param base_weight The survey's base weight variable, if applicable. Can be ignored if the survey
#' data doesn't come with a base weight.
#' @param scale_to_n If \code{TRUE}, scales the resulting weights so so that they sum to the number of
#' observations in the survey.
#' @param epsilon A \code{numeric} variable indicating the tolerance for the raking algorithm. When the
#' proportions of the raking variables are within tolerance, the raking algorithm will stop.
#' @param maxit Maximum number of iterations before stopping if raking has not converged.
#' @param ... Other arguments passed to \code{survey::calibrate()}.
#'
#' @return A vector of survey weights.
#'
#' @details The variables in \code{.data} must exactly match the variables in \code{pop_margins} both
#' in name in the factor values. Additionally, they should contain no missing data.
#'
#' @examples
#' library(dplyr)
#' # Prepare variables from the survey data for raking
#' dec13_excerpt_raking <- dec13_excerpt %>%
#'   mutate(rk_sex = sex,
#'          rk_recage = dk_to_na(recage, pattern = "DK/Ref"),
#'          rk_receduc = dk_to_na(receduc, pattern = "DK/Ref")) %>%
#'   impute_vars(.) %>%
#'   mutate(rk_sex_receduc = interaction(rk_sex, rk_receduc, sep = ":"))
#' # Prepare population marginal distributions for raking
#' # Here we will use the acs_2017_excerpt dataset included wih the package
#' targets <- create_raking_targets(acs_2017_excerpt,
#'                                  vars = c("sex", "recage", "receduc", "sex:receduc"),
#'                                  wt = "weight")
#'
#' # Now that we have raking variables and population targets, we can create a raking weight
#' fake_weight <- rake_survey(dec13_excerpt_raking, targets)
#'
#' @import dplyr survey
#' @export
rake_survey <- function(.data, pop_margins, base_weight=1, scale_to_n = TRUE, epsilon=0.000005, maxit=100, ...) {
  .data <- as.data.frame(.data) %>%
    mutate_if(is.factor, factor, ordered = FALSE)

  pop_margins <- unify_margins(pop_margins, .data)%>%
    purrr::map(~mutate_if(., is.factor, factor, ordered = FALSE))

  nrow_data <- nrow(.data)

  # Add ids to data frame.  necessary for survey package
  .data$._id <-1:nrow_data
  ## If scale_to_n is true, rescale the population margins to the survey n
  if (scale_to_n) {
    pop_margins <- pop_margins %>% purrr::map(~mutate(., Freq = Freq * nrow_data/sum(Freq)))
  }

  # Create base weighted survey. If base weight is not supplied, base weight is
  # assumed to be 1
  design <- svydesign(ids = ~._id, data=.data, weights = make.formula(base_weight))

  # List of formulas containing names of raking variables
  # Captured from the list of pop totals that is passed in
  # Required that the first column of the tables contains the name of
  # the raking variable
  weight_formula <- pop_margins %>% purrr::map(~make.formula(names(.)[1]))

  # Rake the variable.
  x <- calibrate(design, formula = weight_formula, population = pop_margins, calfun="raking", epsilon=epsilon, maxit=maxit, ...)

  return(as.vector(weights(x)))
}
