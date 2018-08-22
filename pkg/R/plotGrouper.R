
#' A function to run the shiny app
#'
#' This function allows you to run the app
#' @param ... Takes any arguments that can be called with shiny::runApp()
#' @keywords plotGrouper
#' @export
#' @examples
#' plotGrouper()

plotGrouper <- function(...)
{
  devtools::load_all()
  appDir <- system.file("application", package = "plotGrouper")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `plotGrouper`.", call. = FALSE)
  }
  shiny::runApp(appDir, launch.browser = T, ...)
}
