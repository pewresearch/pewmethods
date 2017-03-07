#' Get weighted percentages or totals
#'
#' Takes a categorical variable and returns the weighted or unweighted
#' percentage or total of each category. Can include a grouping variable. Can be
#' used to compare weights versus one another.
#'
#' @param var \code{character}, indicating the name of the variable to be tabulated.
#' Specify interactions by separating variable names with a colon.
#' @param df The \code{data.frame} containing the variables to be tabulated.
#' @param wt A \code{character} vector containing the name(s) of the weight variable(s)
#' to be used in tabulating results. If nothing is passed, results will be unweighted.
#' @param by For creating crosstabulations, an optional \code{character} variable
#' containing the name of the variable to be crossed with \code{var}. Can pass
#' multiple variables in as a vector.
#' @param by_total \code{logical} indicating whether a "total" column should be
#' returned in addition to columns defined by variables passed to \code{by}.
#' Defaults to \code{FALSE}.
#' @param percent Should the results be scaled as percentages? Defaults to \code{TRUE}.
#' If \code{FALSE}, weighted totals are returned.
#' @param include_unw Include unweighted frequencies in the output along with weighted.
#' Defaults to \code{FALSE}
#' @param digits The number of decimal points displayed. Defaults to value specified
#' in \code{options("digits")}.
#' @param complete TRUE/FALSE: Should factor levels with no
#' observations be included in the results? Defaults to \code{TRUE}.
#' @param na.rm If \code{FALSE}, \code{NA} values in \code{var} are included in the
#' results and included in the denominator for calculating percentages. If \code{TRUE},
#' they are excluded from any calculations. Defaults to \code{FALSE}.
#'
#' @return A \code{data.frame} with a column for the variable name, columns displaying
#' percentages or totals, and additional columns as specified by arguments to
#' this function.
#'
#' @details If no arguments are supplied to \code{by}, then the column names
#'   will be the weight names. If arguments are supplied to \code{by}, then the
#'   column names will be the categories of the grouping variable, and the
#'   output will have an additional column for the weight name.
#'
#' @examples
#' library(dplyr)
#' # Basic unweighted crosstab
#' get_totals("q1", dec13_excerpt)
#'
#' # Totals instead of percentages
#' get_totals("q1", dec13_excerpt, percent = FALSE)
#'
#' # Weighted crosstab
#' get_totals("receduc", dec13_excerpt, wt = "weight")
#'
#' # Weighted crosstab by grouping variable
#' get_totals("q1", dec13_excerpt, wt = "weight", by = "receduc")
#'
#' # Compare weights, including unweighted
#' # Let's make a fake weight by combining the landline and cellphone weights
#' dec13_excerpt <- dec13_excerpt %>% mutate(fake_weight = coalesce(llweight, cellweight))
#' get_totals("q1", dec13_excerpt, wt = c("weight", "fake_weight"), include_unw = TRUE)
#'
#' # Use dplyr::filter along with complete = FALSE to remove unwanted categories from the base
#' get_totals("q1", dec13_excerpt %>% filter(q1 != "Don't know/Refused (VOL.)"), wt = "weight",
#'            complete = FALSE)
#'
#' # Alternatively, filter unwanted categories out beforehand with dk_to_na
#' # and then use na.rm = TRUE
#' dec13_excerpt <- dec13_excerpt %>% mutate(q1 = dk_to_na(q1))
#' get_totals("q1", dec13_excerpt, wt = "weight", na.rm = TRUE)
#'
#' @import dplyr tibble forcats stringr rlang
#' @export
get_totals <- function(var, df, wt = NULL, by = NULL, by_total = FALSE, percent = TRUE, include_unw = FALSE, digits = NULL, complete = TRUE, na.rm = FALSE) {

  # Initial error handling

  if (!is.character(var)) stop("The var argument only accepts character strings")
  else if (length(var) > 1) stop("Only one string at a time can be passed to the var argument")
  else if (var == "") stop("Empty strings not allowed for var argument")

  # Pre-processing stage

  # Ungroup dataframe
  df <- ungroup(df)

  # If no argument is passed to wt, create a variable in the dataset called "unweighted" and set it to 1
  if (is.null(wt)) {
    df <- df %>%
      mutate(unweighted = 1)
    wt = "unweighted"
  }

  # If wt is not null and include_unw is TRUE, create a variable in the dataset called "unweighted", set it to 1, and add that variable to the vector of weight names
  if (!is.null(wt) & include_unw) {
    df <- df %>%
      mutate(unweighted = 1)
    wt <- c(wt, "unweighted")
  }

  # If var has a colon (`:`), var is presumed to be an interaction between two variables in the dataset
  if (str_detect(var, ":")) {
    var_split <- str_split(var, ":") %>% unlist()
  }

  # Make sure variable is a factor
  if (exists("var_split")) {
    df <- df %>%
      mutate_at(.vars = var_split, .funs = function(x) as.factor(x))
  }
  else {
    df <- df %>%
      mutate_at(.vars = var, .funs = function(x) as.factor(x))
  }

  # If by exists, make sure the corresponding variable is a factor
  if(!is.null(by)) {
    df <- df %>%
      mutate_at(.vars = by, .funs = function(x) droplevels(as.factor(x)))
  }

  # Main internal function
  make_weighted_crosstab <- function(var, df_in, wt_var) {

    # If the dataframe has 0 rows, manually construct a table indicating that the frequency has 0 observations and end execution
    # You might be wondering, why not just output some sort of message?
    # This is because one application of get_totals is to run it on multiple different combinations of arguments all at once
    # and compile the results into a list or tibble of its own
    # Some of those combinations might yield dataframes with 0 rows
    # This therefore ensures all the output is of the same class, allowing for functions such as bind_rows() to work
    if (nrow(df_in) == 0) {
      out <- tibble(!!sym(var) := factor(levels(pull(df_in, var))),
                    !!sym(wt_var) := rep(0, length(levels(pull(df_in, var)))))
      return(out)
    }

    # If na.rm = TRUE, filter out cases where var is NA
    if (na.rm) {
      df_in <- df_in %>%
        filter_at(.vars = var, .vars_predicate = all_vars(!is.na(.)))
    }

    # Else if na.rm = FALSE, make NA an explicit factor level
    else if (!(na.rm)) {
      if (exists("var_split")) {
        df_in <- df_in %>%
          mutate_at(.vars = var_split, fct_explicit_na)
      }

      else {
        df_in <- df_in %>%
          mutate_at(.vars = var, fct_explicit_na)
      }
    }

    # if wt_var contains NA values, filter df_in to not include them and print a warning
    # message indicating how many observations were removed

    if (anyNA(df_in[[wt_var]])) {
      num_NAs <- sum(is.na(df_in[[wt_var]]))
      df_in <- df_in %>%
        filter_at(.vars = wt_var, .vars_predicate = all_vars(!is.na(.)))
      warning(sprintf("Removed %s rows containing missing values for %s", num_NAs, wt_var))
    }

    # Convert totals to percent if applicable (scaling ._wt to add to 100)
    if (percent) {
      df_in <- df_in %>%
        mutate_at(.vars = wt_var, .funs = function(w) 100 * w / sum(w))
    }

    # This is the actual piece of code that does the weighted crosstab; everything else is formatting.

    # If var has a colon (`:`), var is presumed to be an interaction between two variables in the dataset
    if (exists("var_split")) {
      var_split <- var_split %>% as.list() %>% purrr::map(., sym)
      out <- df_in %>%
        group_by(do.call(`:`, var_split)) %>%
        summarise_at(.vars = wt_var, .funs = sum) %>%
        set_names(c(var, wt_var))
    }
    else {
      out <- df_in %>%
        group_by(!!sym(var)) %>%
        summarise_at(.vars = wt_var, .funs = sum)
    }

    # If digits = TRUE, round by that number of digits; otherwise, don't
    if (!is.null(digits)) {
      out <- out %>%
        mutate_at(.vars = wt_var, .funs = ~round(.x, digits = digits))
    }

    # If complete = TRUE, include all factor levels with no observations.
    # suppressWarnings() suppresses a "column has different attributes on LHS and RHS of join" message triggered by tidyr::complete
    # which will happen if var has additional attributes such as the "label" attribute that comes with files read in from SPSS
    # this is not an error and is annoying, thus the suppressWarnings() call
    suppressWarnings(if (complete) {
      out <- out %>%
        tidyr::complete(!!sym(var), fill = list(0) %>% set_names(wt_var))
    })

    return(out)
  }

  # If no arguments are passed to by, column names will be the weights themselves
  if (is.null(by)) {
    out <- purrr::map(wt, ~make_weighted_crosstab(var, df, .x)) %>%
      purrr::reduce(full_join, by = var)
  }

  # If one argument is passed to by, column names will be the categories, and there'll be an additional column for the weight name
  else if (length(by) >= 1) {
    out <- purrr::imap(df %>% split(.[, by], sep = ":"), function(df_by, category) {
      purrr::map(wt, function(w) {
        make_weighted_crosstab(var, df_by, w) %>%
          set_names(var, category) %>%
          mutate(weight_name = w)
      }) %>%
        bind_rows()
    })
    if(by_total) {
      out[["Total"]] <- purrr::map(wt, ~make_weighted_crosstab(var, df, .x) %>%
                              tidyr::gather(weight_name, Total, -matches(var))) %>%
        bind_rows() %>%
        select_at(.vars = c(var, "Total", "weight_name"))
    }
    out <- out %>%
      purrr::reduce(full_join, by = c(var, "weight_name")) %>%
      select(!!sym(var), weight_name, everything())
  }



  # Converting to a data.frame at the end so that different numbers of digits display properly
  out <- as.data.frame(out)

  # Replace NAs with zeroes
  out <- out %>% mutate_if(is.numeric, replace_if, NA, 0)
  return(out)
}
