test_that("sample_vec works", {
  first <- sample_vec(10)
  second <- sample_vec(c(1, 2, 3), 1)

  expect_equal(first, 10)
  expect_equal(length(second), 1)
})
