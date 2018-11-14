library(dply2)

context("Check Filter")

testthat::test_that("recode2 works", {
  f <- fct_recode2(dplyr::starwars$gender,
                   m_or_f = c("male", "female"),
                   other = c("hermaphrodite"),
                   .ordered = TRUE, .other_to_na = TRUE)
  expect_true(is.ordered(f))
  expect_equal(unique(f), factor(c("m_or_f", NA, "other"), ordered = TRUE))
})


