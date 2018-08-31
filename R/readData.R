
# Copyright 2017-2018 John Gagnon
# This program is distributed under the terms of the GNU General Public License

#' A function to read an excel file and combine its sheets into a 
#' single dataframe.
#'
#' This function will read an excel file and combine its sheets into a single
#' dataframe.
#' @import shiny
#' @import shinythemes
#' @import dplyr
#' @import ggplot2
#' @rawNamespace import(Hmisc, except = c(summarize, src))
#' @importFrom rlang .data
#' @importFrom tibble as.tibble
#' @importFrom  gridExtra grid.arrange arrangeGrob
#' @importFrom egg set_panel_size
#' @importFrom readxl excel_sheets read_excel
#' @importFrom ggpubr compare_means get_legend
#' @importFrom gtable gtable_add_padding
#' @importFrom readr parse_number read_csv read_tsv
#' @importFrom scales trans_format math_format rescale_none
#' @importFrom stringr str_remove str_split word
#' @importFrom tidyr gather
#' @importFrom stats na.omit start
#' @importFrom colourpicker colourInput updateColourInput
#' @param file Takes an excel file to be read from
#' @param sheet Takes a vector of sheets to be read
#' @keywords readData
#' @return Tibble assembled from the sheets selected from the file
#' @examples
#' datasets <- readData_example("iris.xlsx")
#' readData(datasets, "iris")
#' @export
readData <- function(
    file = NULL,
    sheet = NULL) {
  for (i in seq_len(length(sheet))) {
    a <- readxl::read_excel(file,
      sheet = sheet[i],
      col_names = TRUE
    ) %>%
      dplyr::mutate("Sheet" = sheet[i]) %>%
      dplyr::select(.data$Sheet, dplyr::everything())

    column_names <- names(a)
    column_names <- stringr::str_replace_all(column_names, c(
      ",Freq. of Parent" = " %",
      ",Count" = " #",
      "\u2014" = "-",
      ",," = "",
      ",Median,<.*>," = " MFI "
    ))

    colnames(a) <- column_names

    if (i == 1) {
      f <- a
    } else {
      f <- dplyr::bind_rows(f, a)
    }
  }
  return(f)
}
