library(testthat)
library(shinytest)
library(plotGrouper)

test_that("Application works", {
  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  expect_pass(testApp(".", compareImages = FALSE))
})