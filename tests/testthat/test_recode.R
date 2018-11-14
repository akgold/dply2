library(dply2)

context("Check Filter")

expect_equal(capture.output(df <- filter2(dplyr::starwars, gender == "female")), 
             c("", "Filtering for gender == \"female\".", "\tRows Dropped: 68"))
