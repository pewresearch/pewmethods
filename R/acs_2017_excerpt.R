#' American Community Survey 2017 Public Use Microdata Excerpt
#'
#' Contains a weight for each combination of sex, 6-category age, and 3-category education
#' proportional to its share in the non-institutionalized U.S. population, aged 18 and above.
#'
#' @docType data
#'
#' @usage data(acs_2017_excerpt)
#'
#' @format A data.frame with 36 rows and 3 variables:
#' \describe{
#'   \item{sex}{Male/Female}
#'   \item{agecat6}{Age, divided into 6 buckets}
#'   \item{educ3}{HS or less, some college, college graduate+}
#'   \item{weight}{A weight for each row proportional to the share of that row's variables in the population}
#'   }
#'
"acs_2017_excerpt"
