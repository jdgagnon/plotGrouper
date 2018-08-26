
# Copyright 2017-2018 John Gagnon
# This program is distributed under the terms of the GNU General Public License

#' A function to organize the raw data to be plotted
#'
#' This function will organize the data and perform count calculations if appropriate
#' @param data Takes a tibble
#' @param exclude Takes list of columns to exclude from gather
#' @param comp Takes name of comparison column
#' @param comps Takes a vector of names of the comparisons
#' @param variables Takes vector of the variables to be plotted
#' @param id Takes name of unique identifier column
#' @param beadColumn Takes column name that has total number of beads/sample
#' @param dilutionColumn Takes column name that has dilution factor for each sample 1/x
#' @keywords organizeData
#' @export
#' @examples
#' organizeData()

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

  d <- gather(
    data,
    variable,
    value,
    -c(exclude)
  ) %>%
    filter(get(comp) %in% comps)

  if (!beadColumn %in% c("", "none") &
             !dilutionColumn %in% c("", "none")) {
    d <- d %>%
      group_by_(id) %>%
      mutate(value = ifelse(str_detect(variable, "#"),
        (value / value[variable == "Bead #"] *
           get(beadColumn) *
           get(dilutionColumn)),
        value
      )) %>%
      ungroup() %>%
      filter(variable %in% variables) %>%
      filter(!grepl("Bead|Ungated", variable))
  } else {
    d <- d %>%
      ungroup() %>%
      filter(variable %in% variables) %>%
      filter(!grepl("Bead|Ungated", variable))
  }
  return(d)
}
