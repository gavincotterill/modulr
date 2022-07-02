test_that("clean_vector works", {

  cell_df <- data.frame("vector" = c("13-100-1",
                                      "4-11-9",
                                      "1-2",
                                      "5"
  )
  )
  exp_1 <- "1-13-100"
  exp_2 <- "4-9-11"
  exp_3 <- "1-2"
  exp_4 <- "5"
  clean1 <- clean_vector(cell_df$vector[1])
  clean2 <- clean_vector(cell_df$vector[2])
  clean3 <- clean_vector(cell_df$vector[3])
  clean4 <- clean_vector(cell_df$vector[4])

  expect_equal(clean1, exp_1)
  expect_equal(clean2, exp_2)
  expect_equal(clean3, exp_3)
  expect_equal(clean4, exp_4)


})
