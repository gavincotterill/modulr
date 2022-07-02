test_that("rand_vect performs a multinomial draw", {
  N = 12
  M = 5
  out <- rand_vect(N, M, sd = 1, pos.only = TRUE)
  expect_equal(M, sum(out))
})
