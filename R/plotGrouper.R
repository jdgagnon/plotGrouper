
# Copyright 2017-2018 John Gagnon
# This program is distributed under the terms of the GNU General Public License

#' A function to run the plotGrouper shiny app
#'
#' This function runs the plotGrouper app
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
#' @param ... Any argument that you can pass to shiny::runApp
#' @examples
#' plotGrouper()
#' @return Runs the plotGrouper shiny app.
#' @export
plotGrouper <- function(...)
{
  # devtools::load_all()
  appDir <- system.file("application", package = "plotGrouper")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing plotGrouper.", call. = FALSE)
  }
  shiny::runApp(appDir, launch.browser = TRUE, ...)
}
