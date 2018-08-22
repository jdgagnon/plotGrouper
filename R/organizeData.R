
#' A function to organize the raw data to be plotted
#'
#' This function will organize the data and perform count calculations if appropriate
#' @param data Takes a tibble
#' @param exclude Takes list of columns to exclude from gather
#' @param comp Takes name of comparison column
#' @param comps Takes a vector of names of the comparisons
#' @param variables Takes vector of the variables to be plotted
#' @param id Takes name of unique identifier column
#' @param bead Takes number of beads per sample
#' @param dilution Takes dilution factor
#' @param count True or False indicating presence of columns specifying Total Bead and Dilution
#' @keywords organizeData
#' @export
#' @examples
#' organizeData()

organizeData <- function(data,
                         exclude,
                         comp,
                         comps,
                         variables,
                         id,
                         bead,
                         dilution,
                         count) {
  d <- gather(
    data,
    variable,
    value,
    -c(exclude)
  ) %>%
    filter(get(comp) %in% comps)

  if (!is.na(bead) &
      !is.na(dilution) &
      str_detect(variables[1], "#")) {
    d <- d %>%
      group_by_(id) %>%
      mutate(value = ifelse(str_detect(variable, "#") &
                              !is.na(bead),
                            value / value[variable == "Bead #"] * bead * dilution,
                            value
      )) %>%
      ungroup() %>%
      filter(variable %in% variables) %>%
      filter(!grepl("Bead|Ungated", variable))
  } else if (count == T) {
    d <- d %>%
      group_by_(input$id) %>%
      mutate(value = ifelse(str_detect(variable, "#"),
                            (value / value[variable == "Bead #"] * `Total Bead` * `Dilution`),
                            value
      )) %>%
      ungroup() %>%
      filter(variable %in% variables) %>%
      filter(!grepl("Bead|Ungated", variable))
  } else {
    d <- d %>%
      filter(variable %in% variables) %>%
      filter(!grepl("Bead|Ungated", variable))
  }
  return(d)
}
