
# Copyright 2017-2018 John Gagnon
# This program is distributed under the terms of the GNU General Public License

#' A function to read the data to be plotted into the app
#'
#' This function will read each sheet listed and return a tibble
#' @param sheet Takes a vector of sheets to be read
#' @param file Takes an excel file to be read from
#' @keywords readData
#' @export
#' @examples
#' readData()

readData <- function(sheet, file) {
  for (i in 1:length(sheet)) {
    a <- readxl::read_excel(file,
      sheet = sheet[i],
      col_names = TRUE
    ) %>%
      mutate(Sheet = sheet[i]) %>%
      select(Sheet, everything())

    column_names <- names(a)
    column_names <- str_replace_all(column_names, c(
      ",Freq. of Parent" = " %",
      ",Count" = " #",
      "—" = "-",
      ",," = "",
      ",Median,<.*>," = " MFI "
    ))

    colnames(a) <- column_names

    if (i == 1) {
      f <- a
    } else {
      f <- bind_rows(f, a)
    }
  }
  return(f)
}
