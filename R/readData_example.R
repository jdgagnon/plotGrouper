
#' Get path to readData example
#'
#' readData comes bundled with an example file in its `inst/applications/www`
#' directory. This function make them easy to access.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @return Located example excel file in package
#' @examples
#' readData_example(path = "iris.xlsx")
#' @export
readData_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("application/www", package = "plotGrouper"))
  } else {
    system.file("application/www", 
                path, 
                package = "plotGrouper", 
                mustWork = TRUE)
  }
}
