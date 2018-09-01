context("test-organizedata")
library(plotGrouper)

test_that("organizeData works", {
  skip_on_bioc()
  df <- iris %>% dplyr::mutate(Species = as.character(Species)) %>%
    dplyr::group_by(Species) %>%
    dplyr::mutate(Sample = paste0(Species, "_", dplyr::row_number()), Sheet = "iris") %>%
    dplyr::select(Sample, Sheet, Species, dplyr::everything()) %>%
    plotGrouper::organizeData(data = .,
                              exclude = c("Sample", "Sheet", "Species"),
                              comp = "Species",
                              comps = c("setosa", "versicolor", "virginica"),
                              variables = "Sepal.Length",
                              id = "Sample",
                              beadColumn = "none",
                              dilutionColumn = "none")
  expect_is(df, class = c("tbl_df", "tbl", "data.frame"))
})
