
test_that("graph_crossing using first element of simulate_independence output", {
  at <- simulate_independence(n_animals = 12,
                              n_groups = 2,
                              time_to_leave = 3,
                              time_to_return = 1,
                              travel_time = c(0.001, 1),
                              sampling_duration = 7)
  gc <- graph_crossing(at, 1, 1, names(at)[[1]])

  expect_equal(nrow(gc), 12)

})
