library(testthat)
library(modulr)

test_that("plot_transmissions works", {
  na <- 10
  s1 <- simulate_schedule(n_animals = na, n_groups = 3, n_splits = NA, time_to_leave = 5, time_to_return = 2, travel_time = c(0.001, 0.002), sampling_duration = 14, simulator = "independent")
  gc <- graph_crossing(schedule = s1, exposure_time = 2, infectious_time = 2, index_case = names(s1)[[1]])
  expect_silent(plot_transmissions(gc))
})
