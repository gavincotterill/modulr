test_that("extract_group works", {

  cell_df <- data.frame("holding" = c("13_4/100_1/1_2-1_3-1_4-1_0",
                                      "4_1-4_3/9_0-9_1/11_6",
                                      "1_1//2_1",
                                      "5_0"
  )
  )
  exp_1 <- c("13", "100", "1")
  exp_2 <- c("4", "9", "11")
  exp_3 <- c("1", NA, "2")
  exp_4 <- "5"
  clean1 <- stringr::str_split(cell_df$holding[1], "/")[[1]] %>% extract_group()
  clean2 <- stringr::str_split(cell_df$holding[2], "/")[[1]] %>% extract_group()
  clean3 <- stringr::str_split(cell_df$holding[3], "/")[[1]] %>% extract_group()
  clean4 <- stringr::str_split(cell_df$holding[4], "/")[[1]] %>% extract_group()

  expect_equal(clean1, exp_1)
  expect_equal(clean2, exp_2)
  expect_equal(clean3, exp_3)
  expect_equal(clean4, exp_4)


})
