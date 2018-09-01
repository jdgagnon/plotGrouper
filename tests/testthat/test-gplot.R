context("test-gplot")
library(plotGrouper)

test_that("gplot works", {
  skip_on_bioc()
  gt <- iris %>% 
    dplyr::mutate(Species = as.character(Species)) %>%
    dplyr::group_by(Species) %>%
    dplyr::mutate(Sample = paste0(Species, "_", dplyr::row_number()),Sheet = "iris") %>%
    dplyr::select(Sample, Sheet, Species, dplyr::everything()) %>%
    tidyr::gather(variable, value, -c(Sample, Sheet, Species)) %>%
    dplyr::filter(variable == "Sepal.Length") %>%
    plotGrouper::gplot(
      comparison = "Species",
      group.by = "variable",
      shape.groups = c(19,21,17),
      color.groups = c(rep("black",3)),
      fill.groups = c("black","#E016BE", "#1243C9"))
  expect_is(gt, class = c("gtable", "gTree", "grob", "gDesc"))
})
