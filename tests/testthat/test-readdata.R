context("test-readdata")
library(plotGrouper)

test_that("readData works", {
  f <- readData_example(path = "iris.xlsx")
  expect_is(readData(f,1), class = c("tbl_df", "tbl", "data.frame"))
})
