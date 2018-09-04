context("test-readdata")
library(plotGrouper)

testthat::test_that("readData works", {
  testthat::skip_on_os("windows")
  f <- plotGrouper::readData_example(path = "iris.xlsx")
  testthat::expect_is(readData(f, 1), class = c("tbl_df",
                                                "tbl",
                                                "data.frame"))
})
