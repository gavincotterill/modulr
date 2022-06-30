test_that("when there are more than two edges in sampled graph with fast_greedy, all memberships are assigned", {
  library(igraph)
  g_d <- simulate_graph(n_animals = 5,
                        n_groups = 2,
                        time_to_leave = 5,
                        time_to_return = 2,
                        travel_time = c(0.001, 2),
                        sampling_duration = 7,
                        sampler = "discrete",
                        samples_per_day = 1)

  # this is a corner case where NA in membership because fewer than two edges, no qrel calculation possible
  g_obs <- sample_graph(graph = g_d,
                        sample_nNodes = 4,
                        prop_hi_res = 0.5,
                        hi_res = 12,
                        lo_res = 1/7,
                        sampling_duration = 7,
                        regime = "random",
                        alg = "fast_greedy")

  expect_equal(length(g_obs), 10)
  expect_output(str(g_obs), "Class \'igraph\'  hidden list of 10")
  expect_silent(plot_sampled_graph(g_obs, g_d))
})

test_that("sample_graph creates list", {

  na <- 10
  s1 <- simulate_schedule(n_animals = na, n_groups = 3, n_splits = NA, time_to_leave = 5, time_to_return = 2, travel_time = c(0.001, 0.002), sampling_duration = 14, simulator = "independent")
  g0 <- graph_from_schedule(s1)
  g1 <- sample_graph(graph = g0,
                    sample_nNodes = 6,
                    prop_hi_res = 0.5,
                    hi_res = 12,
                    lo_res = 1/7,
                    sampling_duration = 7,
                    regime = "even",
                    alg = "netcarto")

  g2 <- sample_graph(graph = g0,
                        sample_nNodes = 6,
                        prop_hi_res = 0.5,
                        hi_res = 12,
                        lo_res = 1/7,
                        sampling_duration = 7,
                        regime = "random",
                        alg = "netcarto")

  g3 <- sample_graph(graph = g0,
                        sample_nNodes = 6,
                        prop_hi_res = 0.5,
                        hi_res = 12,
                        lo_res = 1/7,
                        sampling_duration = 7,
                        regime = "grab-two",
                        alg = "netcarto")

  expect_equal(length(g1), 10)
  expect_output(str(g1), "Class \'igraph\'  hidden list of 10")
  expect_equal(length(igraph::V(g1)), 6)
  expect_equal(length(igraph::V(g2)), 6)
  expect_equal(length(igraph::V(g3)), 6) # this is a good test because it caught an issue with grab-two line 94

  plot_sampled_graph(g3, g0)

})
