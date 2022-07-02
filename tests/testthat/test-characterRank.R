test_that("characterRank works", {
  list1 <- c("13", "100", "1")
  list2 <- c("4", "9", "11")
  list3 <- c("1", NA, "2")
  list4 <- c("5")

  exp_1 <- c(2, 3, 1)
  exp_2 <- c(1, 2, 3)
  exp_3 <- c(1, 3, 2)
  exp_4 <- 1
  clean1 <- characterRank(list1)
  clean2 <- characterRank(list2)
  clean3 <- characterRank(list3)
  clean4 <- characterRank(list4)

  expect_equal(clean1, exp_1)
  expect_equal(clean2, exp_2)
  expect_equal(clean3, exp_3)
  expect_equal(clean4, exp_4)

})
