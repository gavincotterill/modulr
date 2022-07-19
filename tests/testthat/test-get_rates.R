test_that("get_rates works for independent", {
  schedule1 <- simulate_schedule(n_animals = 10, n_groups = 2, n_splits = NA, time_to_leave = 5, time_to_return = 2, travel_time = c(0.001, 0.2), sampling_duration = 30, simulator = "independent")
  schedule2 <- simulate_schedule(n_animals = 10, n_groups = 2, n_splits = NA, time_to_leave = 5, time_to_return = 2, travel_time = c(0.001, 0.2), sampling_duration = 30, simulator = "independent")
  schedule_list <- list(schedule1, schedule2)
  average_rates_list <- purrr::map(schedule_list, ~get_rates(.x, sim = "independent"))
  expect_equal(length(average_rates_list), 2)
  expect_equal(length(average_rates_list[[1]]), 2)
  expect_equal(length(average_rates_list[[2]]), 2)

})

test_that("get_rates works for independent", {
  schedule1 <- simulate_schedule(n_animals = 10, n_groups = 2, n_splits = NA, time_to_leave = 5, time_to_return = 2, travel_time = c(0.001, 0.2), sampling_duration = 30, simulator = "group-think")
  schedule2 <- simulate_schedule(n_animals = 10, n_groups = 2, n_splits = NA, time_to_leave = 5, time_to_return = 2, travel_time = c(0.001, 0.2), sampling_duration = 30, simulator = "group-think")
  schedule_list <- list(schedule1, schedule2)
  average_rates_list <- purrr::map(schedule_list, ~get_rates(.x, sim = "group-think"))
  expect_equal(length(average_rates_list), 2)
  expect_equal(length(average_rates_list[[1]]), 2)
  expect_equal(length(average_rates_list[[2]]), 2)

})
