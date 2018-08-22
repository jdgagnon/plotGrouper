
#' A function to run the shiny app
#'
#' This function allows you to run the app
#' @export
plotGrouper <- function(...)
{
  # devtools::load_all()
  appDir <- system.file("application", package = "plotGrouper")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `plotGrouper`.", call. = FALSE)
  }
  shiny::runApp(appDir, launch.browser = T, ...)
}
