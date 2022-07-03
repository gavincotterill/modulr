test_that("simulate_graph discrete creates list", {
  expect_error(simulate_graph(n_animals = 3,
                              n_groups = 1,
                              time_to_leave = 5,
                              time_to_return = 2,
                              travel_time = c(0.001, 0.002),
                              sampling_duration = 7,
                              sampler = "discrete",
                              samples_per_day = 1)
  )
  g_d <- simulate_graph(n_animals = 6,
                        n_groups = 2,
                        time_to_leave = 5,
                        time_to_return = 2,
                        travel_time = c(0.001, 0.002),
                        sampling_duration = 7,
                        sampler = "discrete",
                        samples_per_day = 1)
  expect_equal(length(g_d), 6)
  expect_output(str(g_d), "Class \'igraph\'  hidden list of 10")
})

test_that("simulate_graph continuous creates list", {
  expect_error(simulate_graph(n_animals = 3,
                              n_groups = 1,
                              time_to_leave = 5,
                              time_to_return = 2,
                              travel_time = c(0.001, 0.002),
                              sampling_duration = 7,
                              sampler = "continuous",
                              samples_per_day = 1)
  )
  g_c <- simulate_graph(n_animals = 10,
                        n_groups = 2,
                        time_to_leave = 5,
                        time_to_return = 2,
                        travel_time = c(0.001, 0.002),
                        sampling_duration = 7,
                        sampler = "continuous",
                        samples_per_day = 1)
  expect_equal(length(g_c), 10)
})
