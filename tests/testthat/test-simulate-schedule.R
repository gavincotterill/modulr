library(testthat)
library(modulr)

test_that("simulate_schedule works", {
  na <- 10
  s1 <- simulate_schedule(n_animals = na, n_groups = 3, n_splits = NA, time_to_leave = 5, time_to_return = 2, travel_time = c(0.001, 0.002), sampling_duration = 14, simulator = "independent")
  s2 <- simulate_schedule(n_animals = na, n_groups = 3, n_splits = NA, time_to_leave = 5, time_to_return = 2, travel_time = c(0.001, 0.002), sampling_duration = 14, simulator = "non-independent")
  s3 <- simulate_schedule(n_animals = na, n_groups = 3, n_splits = NA, time_to_leave = 5, time_to_return = 2, travel_time = c(0.001, 0.002), sampling_duration = 14, simulator = "group-think")

  expect_equal(length(s1), na)
  expect_equal(length(s2), na)
  expect_equal(length(s3), na)

})
