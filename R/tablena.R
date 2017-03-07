#' Enhanced tables
#'
#' Displays tables with NAs, dimension names, and dimension classes by default
#'
#' @param ... Arguments passed to \code{table()}
#' @param show_class TRUE by default. Shows the class of the variable being tabled.
#'
#' @return A table
#'
#' @examples
#' tablena(dec13_excerpt$q2, dec13_excerpt$receduc)
#' # Also works with the with() function and other such functions that construct environments from data
#' with(dec13_excerpt, tablena(q2, receduc))
#'
#' @import rlang
#' @export
tablena <- function(..., show_class = TRUE) {
  default_env <- caller_env()
  arguments <- enexprs(...)

  if (!show_class) {
    return(table(..., useNA = "ifany", deparse.level = 2))
  }

  else if (show_class) {
    dnn_display = purrr::map(arguments, ~sprintf("%s, a %s", deparse(.x), Reduce(paste, class(eval_tidy(.x, env = default_env))))) %>% squash_chr()
  }

  table(..., useNA = "ifany", deparse.level = 2,
        dnn = dnn_display)
}
