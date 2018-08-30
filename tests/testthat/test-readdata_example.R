context("test-readdata_example")
library(plotGrouper)

test_that("readData_example works", {
  f <- readData_example(path = "iris.xlsx")
  expect_is(f, class = "character")
})
