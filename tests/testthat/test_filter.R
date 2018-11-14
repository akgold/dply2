library(dply2)

context("Check Filter")
testthat::test_that("filter2 works", {
  expect_equal(capture.output(df <- filter2(dplyr::starwars, gender == "female")),
               c("", "Filtering for gender == \"female\".", "\tRows Dropped: 68"))
})
