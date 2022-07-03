test_that("graph_from_schedule produces a graph", {
  at <- simulate_independence(n_animals = 12,
                              n_groups = 2,
                              time_to_leave = 3,
                              time_to_return = 1,
                              travel_time = c(0.001, 1),
                              sampling_duration = 7)
  g <- graph_from_schedule(at)
  expect_equal(is(g, "igraph"), TRUE)
})
