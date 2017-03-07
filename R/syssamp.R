#' Systematic sample from a dataset
#'
#' Takes a dataset, sorts it by one or more variables, and draws samples at regular intervals.
#' A fractional skip interval is used so that each record still has an equal probability of selection
#' when the number of records in the frame is not evenly divisible by the sample size.
#'
#' @param .data A dataset, in data.frame or tibble format.
#' @param size The size of the desired sample.
#' @param stratvars Names of variables by which the sampling frame will be sorted
#' @param return_indices Return a vector containing the indices of the selected records. Defaults to \code{TRUE}.
#' If \code{FALSE}, a data.frame containing the sampled records will be returned.
#' @param seed Optional. Ensures that the same sample will be created if the code is rerun.
#'
#' @return Either a vector of indices or a data.frame/tibble.
#'
#'
#' @examples
#' dec13_sample_indices <- syssamp(dec13_excerpt, size = 10, stratvars = c("sex", "recage"))
#' dec13_sample <- syssamp(dec13_excerpt, size = 10, stratvars = c("sex", "recage"),
#'                         return_indices = FALSE)
#'
#' @references
#' Buskirk, T.D. (2008) Sampling interval. In P.J. Lavrakas, \emph{Encyclopedia of survey research methods}. Thousand Oaks, CA: Sage Publications.
#'
#' @import dplyr
#' @export
syssamp = function(.data, size, stratvars, return_indices = TRUE, seed = NA) {

  if (!is.na(seed)) set.seed(seed)

  skip = nrow(.data)/ size
  start = (runif(1) * skip) + 1
  rest = start + seq_len(size - 1) * skip
  selected = floor(c(start, rest))
  strat_samp = .data %>%
    mutate(.__rand = runif(n()),
           .__index = seq_len(n())) %>%
    arrange_at(.vars = c(stratvars, ".__rand")) %>%
    slice(selected)
  if (return_indices) {
    return(sort(strat_samp$.__index))
  } else {
    strat_samp %>% arrange(.__index) %>%
      select(-.__rand, -.__index)
  }
}
