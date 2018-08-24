
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
#' @export
plotGrouper <- function(...)
{
  # devtools::load_all()
  appDir <- system.file("application", package = "plotGrouper")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `plotGrouper`.", call. = FALSE)
  }
  shiny::runApp(appDir, launch.browser = TRUE, ...)
}
