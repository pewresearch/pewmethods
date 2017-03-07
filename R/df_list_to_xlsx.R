#' Write a list of data frames to Excel
#'
#' Takes a list of data frames - for example, obtained by mapping get_totals()
#' over multiple variables - and writes them vertically into an Excel
#' spreadsheet.
#'
#' @param df_list A list of data frames, or tables, or tibbles, or anything
#'   rectangular-shaped. For writing to multiple sheets, a list of lists, with
#'   each top-level list corresponding to one sheet.
#' @param sheet_name The name/s of the sheet/s within the Excel file.
#' @param outfile The filename of the Excel file itself. Include the full
#'   directory path.
#' @param overwrite TRUE by default. If a file of the same name already exists,
#'   setting \code{overwrite} to TRUE will overwrite that file with the output
#'   of this function. Setting \code{overwrite} to FALSE will halt function
#'   execution if the file already exists.
#' @param label_list Optional list of labels that correspond to each of the
#'   elements of \code{df_list}. These are printed above the corresponding
#'   tables in the output. For writing to multiple sheets, a list of
#'   lists, with each top-level list corresponding to one sheet.
#' @param title Optional title to be printed on the first row of the Excel sheet.
#' For writing to multiple sheets, there must be one title per sheet.
#' @param borders Optional borders to be drawn around each table. Will create a
#'   border around the outside by default. See \code{openxlsx::writeData} for
#'   other options.
#'
#' @return This function does not return an object within R. Instead, it will
#'   write an Excel file to the filename specified in \code{outfile}.
#'
#' @examples
#' library(dplyr)
#' # Identify a list of variables I want to run crosstabs on
#' vars <- c("q1", "q45", "party")
#'
#' # Run weighted crosstabs by gender on each of the variables and shove them into a list
#' # get_totals() is a weighted crosstab function, while map() allows us to do get_totals()
#' # over and over again on everything in the vector called vars.
#' dec13_tabs <- purrr::map(vars, ~get_totals(.x, dec13_excerpt, by = "sex", wt = "weight"))
#'
#' # Get a corresponding list of labels for vars
#' dec13_labs <- get_spss_label(dec13_excerpt, vars, unlist = FALSE)
#'
#' # Write the weighted crosstabs to an Excel spreadsheet
#' # Note that if you run this example code on your own computer, then you will end up
#' # with an Excel spreadsheet written to your current working directory.
#' # Remove comment to run
#' # df_list_to_xlsx(df_list = dec13_tabs, sheet_name = "Dec 2013 crosstabs",
#' #                 outfile = "df_list_to_xlsx_example_output.xlsx", label_list = dec13_labs,
#' #                 title = "Dec 2013 crosstab example")
#'
#' # What if we want crosstabs across multiple sheets?
#' # Let's do one sheet for each education category.
#' # Recode the receduc variable to change DK/Ref to NA
#' dec13_excerpt <- dec13_excerpt %>% mutate(receduc = dk_to_na(receduc))
#'
#' # Convert the dataset into a list of three datasets, one per education category
#' dec13_list <- dec13_excerpt %>% split(.$receduc)
#'
#' # For each of the three datasets, get weighted crosstabs
#' # This will give us a list of three (one per education category),
#' # each with its own list of three (one per crosstab)
#' dec13_tabs <- purrr::map(dec13_list, function(df) {
#'   purrr::map(vars, ~get_totals(.x, df, wt = "weight"))
#'   })
#'
#' # Associate a label with each crosstab. The split() function removes labels,
#' # so we're going to call get_spss_label over the original dataset.
#' dec13_labs <- purrr::map(dec13_list, function(df) {
#'   get_spss_label(dec13_excerpt, vars, unlist = FALSE)
#'   })
#'
#' # Write the weighted crosstabs to an Excel spreadsheet with multiple sheets
#' # Notice that a warning about unsupported characters is printed
#' # Remove comment to run
#' # df_list_to_xlsx(dec13_tabs, sheet_name = names(dec13_tabs),
#' #                 outfile = "df_list_to_xlsx_example_output_2.xlsx", label_list = dec13_labs)
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @importFrom stringr str_detect str_replace_all str_sub
#' @export
df_list_to_xlsx <- function(df_list, sheet_name, outfile, overwrite = TRUE, label_list = NULL, title = NULL, borders = "surrounding") {

  wb <- createWorkbook()

  # If sheet_name contains characters that Excel considers to be invalid for sheet names (:, \, /, ?, *, [ or ]),
  # print a warning to the console and strip those characters from the sheet names
  if (any(str_detect(sheet_name, "\\/|\\\\|\\?|:|\\*|\\[|\\]"))) {
    warning("Excel sheet names cannot contain the characters :, /, \\, ?, *, [ or ]. These characters will automatically be removed from sheet names.")
    sheet_name <- str_replace_all(sheet_name, "\\/|\\\\|\\?|:|\\*|\\[|\\]", " ")
  }

  # If any element of sheet_name is longer than 31 characters,
  # print a warning to the console and truncate that element to 31 characters
  if (any(nchar(sheet_name) > 31)) {
    warning("Excel sheet names cannot contain more than 31 characters. These names will automatically be cut down to 31 characters.")
    sheet_name <- str_sub(sheet_name, start = 1, end = 31)
  }

  # If sheet_name is a vector of length 1, convert df_list into a list of length 1 containing a list of length(df_list).
  # otherwise, if sheet_name is longer than length 1, then df_list should be a list of lists to begin with
  # If label_list is given, and sheet_name is a vector of length 1, convert label_list into a list of length 1 containing a list of length(label_list)
  # otherwise, if sheet_name is longer than length 1, then label_list should be a list of lists to begin with

  if (length(sheet_name) == 1) {
    df_list <- list(df_list)
    if (!is.null(label_list)) {
      label_list <- list(label_list)
    }
  }

  # If title is given, it must be the same length as df_list
  if (!is.null(title)) {
    if (length(title) != length(df_list)) stop("df_list and title should both be as long as the number of sheets needed")
  }

  if (!is.null(label_list)) {
    if (length(label_list) != length(df_list)) stop("label_list and title should both be as long as the number of sheets needed")
  }

  for (i in 1:length(df_list)) {
    addWorksheet(wb, sheetName = sheet_name[i])

    if (!is.null(title)) {
      writeData(wb, sheet = sheet_name[i], x = title[i], startCol = 1, startRow = 1)
    }

    # If a title was placed in row 1, start writing dataframes to row 3; otherwise, write them to row 1
    sr <- ifelse(!is.null(title), 3, 1)

    if (is.null(label_list)) {

      for (j in 1:length(df_list[[i]])) {
        writeData(wb, sheet = sheet_name[i], x = df_list[[i]][[j]], startCol = 1, startRow = sr, borders = borders)
        sr <- sr + nrow(df_list[[i]][[j]]) + 2
      }
    }

    else if (!is.null(label_list)) {

      if (length(df_list[[i]]) != length(label_list[[i]])) {
        stop("The list of dataframes and list of labels in THIS sheet should be the same length.")
      }
      for (j in 1:length(df_list[[i]])) {
        writeData(wb, sheet = sheet_name[i], x = label_list[[i]][[j]], startCol = 1, startRow = sr)
        writeData(wb, sheet = sheet_name[i], x = df_list[[i]][[j]], startCol = 1, startRow = sr + 1, borders = borders)
        sr <- sr + nrow(df_list[[i]][[j]]) + 3
      }
    }
  }
  saveWorkbook(wb, file = outfile, overwrite = overwrite)
}
