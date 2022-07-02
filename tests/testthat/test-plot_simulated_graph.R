test_that("plotting simulated graph works", {

  g_d <- simulate_graph(n_animals = 12,
                        n_groups = 2,
                        time_to_leave = 5,
                        time_to_return = 2,
                        travel_time = c(0.001, 2),
                        sampling_duration = 7,
                        sampler = "discrete",
                        samples_per_day = 1)

  expect_silent({plot_simulated_graph(g_d)})

})
