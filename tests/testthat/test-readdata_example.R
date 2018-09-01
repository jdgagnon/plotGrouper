context("test-readdata_example")
library(plotGrouper)

test_that("readData_example works", {
  skip_on_bioc()
  f <- readData_example(path = "iris.xlsx")
  expect_is(f, class = "character")
})
