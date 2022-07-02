test_that("plotting sampled graph works", {

  g_d <- simulate_graph(n_animals = 12,
                        n_groups = 2,
                        time_to_leave = 3,
                        time_to_return = 2,
                        travel_time = c(0.001, 2),
                        sampling_duration = 7,
                        sampler = "discrete",
                        samples_per_day = 1)

  g_obs <- sample_graph(graph = g_d,
                        sample_nNodes = 6,
                        prop_hi_res = 0.5,
                        sampling_duration = 7,
                        hi_res = 12,
                        lo_res = 1/7,
                        regime = "grab-two",
                        alg = "netcarto")

  expect_silent({plot_sampled_graph(g_obs = g_obs, g = g_d)})

})
