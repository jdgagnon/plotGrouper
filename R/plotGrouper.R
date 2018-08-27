
# Copyright 2017-2018 John Gagnon
# This program is distributed under the terms of the GNU General Public License

#' A function to run the shiny app
#'
#' This function allows you to run the app
#' @import magrittr
#' @import shiny
#' @import tidyverse
#' @import gridExtra
#' @import shinythemes
#' @import Hmisc
#' @import digest
#' @import egg
#' @import readxl
#' @param ... Any argument that you can pass to shiny::runApp
#' @export
#' @examples
#' plotGrouper()
plotGrouper <- function(...)
{
  # devtools::load_all()
  appDir <- system.file("application", package = "plotGrouper")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `plotGrouper`.", call. = FALSE)
  }
  shiny::runApp(appDir, launch.browser = TRUE, ...)
}
