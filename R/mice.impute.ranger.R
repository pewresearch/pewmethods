#' Random forest imputation using the ranger package
#'
#' Designed as an alternative to the the \code{"rf"} method in the \code{mice} package.
#' Imputes univariate missing data using random forests. It uses the same algorithm as
#' \code{"mice.impute.rf"} (included in the \code{mice} package, but is much faster because it uses
#' the \code{ranger} package.
#' This function should not be called directly.
#' Instead, call this function through \code{mice()} by specifying \code{method = "ranger"}.
#'
#' @param y Vector to be imputed
#' @param ry Logical vector of length \code{length(y)} marking observed values of y
#' with TRUE and missing values of y with FALSE.
#' @param x the matrix of predictors, without intercept
#' @param ntree the number of trees for the random forest algorithm. Default is 10.
#' @param wy Logical vector of length \code{length(y)}. A TRUE value indicates locations in y for which imputations are created.
#' @param type Not used. Needed for compatibility with mice
#' @param ... Other arguments passed to \code{ranger()}
#'
#' @return A vector of imputed values.
#'
#' @import dplyr ranger
#' @export
mice.impute.ranger <- function(y, ry, x, ntree = 10, wy = NULL, type = NULL, ...) {
  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop("Package \"ranger\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (is.null(wy)) wy <- !ry
  ntree <- max(1, ntree)
  nmis <- sum(wy)
  obs <- as.data.frame(x[ry, , drop = FALSE])
  mis <- x[wy, , drop = FALSE]
  obs$y <- y[ry]
  do_ordered <- is.numeric(y) | (is.factor(y) & length(levels(y)) ==
                                  2)
  f <- ranger(dependent.variable.name = "y", data = obs, replace = FALSE,
             num.trees = ntree, respect.unordered.factors = do_ordered,
             min.node.size = 5, ...)
  o_nodes <- data.frame(matrix(predict(f, data = obs, predict.all = T,
                                      type = "terminalNodes")$predictions, ncol=ntree)) %>%
    mutate(donor_id = 1:n()) %>%
    tidyr::gather(tree, node, -donor_id)

  p <- data.frame(matrix(predict(f, data = mis, predict.all = TRUE,
                                type = "terminalNodes")$predictions, ncol = ntree)) %>%
    mutate(id = 1:n()) %>%
    tidyr::gather(tree, node, -id) %>%
    group_by(id) %>%
    slice(sample(1:n(), size = 1)) %>%
    left_join(o_nodes, by = c("tree", "node")) %>%
    group_by(id) %>%
    slice(sample(1:n(), size = 1))

  impute <- obs$y[p$donor_id]
  return(impute)
}
