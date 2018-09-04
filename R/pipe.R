# Copyright 2017-2018 John Gagnon
# This program is distributed under the terms of the GNU General Public License

#' Pipe graphics
#'
#' Like dplyr, ggvis also uses the pipe function, \code{\%>\%} to turn
#' function composition into a series of imperative statements.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A visualisation and a function to apply to it
#' @examples
#' # Instead of
#' dplyr::mutate(dplyr::filter(iris, Species == "versicolor"),
#' "Sample" = paste0(Species, dplyr::row_number()))
#' # You can write
#' dplyr::filter(iris, Species == "versicolor") %>%
#' dplyr::mutate("Sample" = paste0(Species, "_", dplyr::row_number()))
NULL
