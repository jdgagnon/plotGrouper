
# Copyright 2017-2018 John Gagnon
# This program is distributed under the terms of the GNU General Public License

#' A function to organize a tibble into tidy format and perform count transformations
#'
#' This function will organize a tibble into tidy format and perform count
#' transformations if appropriate columns are specified.
#' @import shiny
#' @import shinythemes
#' @import dplyr
#' @import ggplot2
#' @rawNamespace import(Hmisc, except = c(summarize, src))
#' @importFrom tibble as.tibble
#' @importFrom  gridExtra grid.arrange arrangeGrob
#' @importFrom egg set_panel_size
#' @importFrom readxl excel_sheets read_excel
#' @importFrom ggpubr compare_means get_legend
#' @importFrom gtable gtable_add_padding
#' @importFrom readr parse_number
#' @importFrom scales trans_format math_format rescale_none
#' @importFrom stringr str_remove str_split word
#' @importFrom tidyr gather
#' @importFrom stats na.omit start
#' @importFrom colourpicker colourInput updateColourInput
#' @param data A tibble
#' @param exclude A list of columns to exclude from gather
#' @param comp the name of comparison column
#' @param comps A vector of names of the comparisons
#' @param variables A vector of the variables to be plotted
#' @param id The name of unique identifier column
#' @param beadColumn The column name that has total number of beads/sample
#' @param dilutionColumn The column name that has dilution factor for each sample 1/x
#' @keywords organizeData
#' @return Tibble in tidy format based on columns chosen to be excluded.
#' Count data will be transformed if appropriate columns are present.
#' @examples
#' iris %>% dplyr::mutate(Species = as.character(Species)) %>%
#' dplyr::group_by(Species) %>%
#' dplyr::mutate(Sample = paste0(Species, "_", dplyr::row_number()), Sheet = "iris") %>%
#' dplyr::select(Sample, Sheet, Species, dplyr::everything()) %>%
#' plotGrouper::organizeData(data = .,
#' exclude = c("Sample", "Sheet", "Species"),
#' comp = "Species",
#' comps = c("setosa", "versicolor", "virginica"),
#' variables = "Sepal.Length",
#' id = "Sample",
#' beadColumn = "none",
#' dilutionColumn = "none")
#' @export
organizeData <- function(data = NULL,
                         exclude = NULL,
                         comp = NULL,
                         comps = NULL,
                         variables = NULL,
                         id = NULL,
                         beadColumn = NULL,
                         dilutionColumn = NULL) {
  if (!beadColumn %in% c("", "none") &
    !dilutionColumn %in% c("", "none") &
    stringr::str_detect(variables, "#")) {
    shiny::showNotification(
      ui = paste0("Count data is being transformed
                  by the equation:
                  (value/Bead)*", beadColumn, "*", dilutionColumn),
      type = "message",
      duration = 5
    )
  }

  d <- tidyr::gather(
    data,
    "variable",
    "value",
    -c(exclude)
  ) %>%
    dplyr::filter(get(comp) %in% comps)

  if (!beadColumn %in% c("", "none") &
    !dilutionColumn %in% c("", "none")) {
    d <- d %>%
      dplyr::group_by_(id) %>%
      dplyr::mutate("value" = ifelse(stringr::str_detect(variable, "#"),
        (value / value[variable == "Bead #"] *
          get(beadColumn) *
          get(dilutionColumn)),
        value
      )) %>%
      dplyr::ungroup() %>%
      dplyr::filter(variable %in% variables) %>%
      dplyr::filter(!grepl("Bead|Ungated", variable))
  } else {
    d <- d %>%
      dplyr::ungroup() %>%
      dplyr::filter(variable %in% variables) %>%
      dplyr::filter(!grepl("Bead|Ungated", variable))
  }
  return(d)
}
