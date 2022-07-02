test_that("clean_holding works", {

  cell_df <- data.frame("holding" = c("13_4/100_1/1_2-1_3-1_4-1_0",
                                      "4_1-4_3/9_0-9_1/11_6",
                                      "1_1//2_1",
                                      "5_0"
                                      )
  )
  exp_1 <- "1_2-1_3-1_4/13_4/100_1"
  exp_2 <- "4_1-4_3/9_1/11_6"
  exp_3 <- "1_1/2_1"
  exp_4 <- "5_0"
  clean1 <- clean_holding(cell_df$holding[1])
  clean2 <- clean_holding(cell_df$holding[2])
  clean3 <- clean_holding(cell_df$holding[3])
  clean4 <- clean_holding(cell_df$holding[4])

  expect_equal(clean1, exp_1)
  expect_equal(clean2, exp_2)
  expect_equal(clean3, exp_3)
  expect_equal(clean4, exp_4)


})
