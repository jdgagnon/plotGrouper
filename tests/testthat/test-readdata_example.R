context("test-readdata_example")
library(plotGrouper)

testthat::test_that("readData_example works", {
  testthat::skip_on_os("windows")
  f <- plotGrouper::readData_example(path = "iris.xlsx")
  testthat::expect_is(f, class = "character")
})
