test_that("q_rel works", {
  schedule <- simulate_schedule(n_animals = 10, n_groups = 2, n_splits = NA, time_to_leave = 5, time_to_return = 2, travel_time = c(0.001, 0.2), sampling_duration = 30, simulator = "independent")
  g <- graph_from_schedule(schedule)
  val <- q_rel(g)
  expect_vector(val, size = 1)
  })
