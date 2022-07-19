test_that("when there are more than two edges in sampled graph with fast_greedy, all memberships are assigned", {
  # library(igraph)
  g_d <- simulate_graph(n_animals = 10,
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

  expect_equal(length(g_obs), 4)
  expect_output(str(g_obs), "Class \'igraph\'  hidden list of 10")
  expect_silent(plot_sampled_graph(g_obs, g_d))
})

test_that("sample_graph grab-two works with multiple conditions", {

  na <- 10
  s1 <- simulate_schedule(n_animals = na, n_groups = 3, n_splits = NA, time_to_leave = 5, time_to_return = 2, travel_time = c(0.001, 0.002), sampling_duration = 14, simulator = "independent")
  g0 <- graph_from_schedule(s1)

  # sample_nNodes <= 2
  expect_error(sample_graph(graph = g0,
                            sample_nNodes = 2,
                            prop_hi_res = 0.5,
                            hi_res = 12,
                            lo_res = 1/7,
                            sampling_duration = 7,
                            regime = "grab-two",
                            alg = "netcarto"))

  # regime grab-two
    # sample nNodes is fewer than twice the number of groups and even, remainder 0, 1, >1
  g1 <- sample_graph(graph = g0,
                     sample_nNodes = 4,
                     prop_hi_res = 0.5,
                     hi_res = 12,
                     lo_res = 1/7,
                     sampling_duration = 7,
                     regime = "grab-two",
                     alg = "netcarto")
  expect_equal(length(igraph::V(g1)), 4)
    # sample nNodes is fewer than twice the number of groups and odd, remainder 0, 1, >1
  g2 <- sample_graph(graph = g0,
                     sample_nNodes = 5,
                     prop_hi_res = 0.5,
                     hi_res = 12,
                     lo_res = 1/7,
                     sampling_duration = 7,
                     regime = "grab-two",
                     alg = "netcarto")
  expect_equal(length(igraph::V(g2)), 5)

    # sample nNodes is greater than twice the number of groups
  g3 <- sample_graph(graph = g0,
                     sample_nNodes = 7,
                     prop_hi_res = 0.5,
                     hi_res = 12,
                     lo_res = 1/7,
                     sampling_duration = 7,
                     regime = "grab-two",
                     alg = "netcarto")
  expect_equal(length(igraph::V(g3)), 7)

  g4 <- sample_graph(graph = g0,
                     sample_nNodes = 8,
                     prop_hi_res = 0.5,
                     hi_res = 12,
                     lo_res = 1/7,
                     sampling_duration = 7,
                     regime = "grab-two",
                     alg = "netcarto")
  expect_equal(length(igraph::V(g4)), 8)

  g5 <- sample_graph(graph = g0,
                     sample_nNodes = 9,
                     prop_hi_res = 0.5,
                     hi_res = 12,
                     lo_res = 1/7,
                     sampling_duration = 7,
                     regime = "grab-two",
                     alg = "netcarto")
  expect_equal(length(igraph::V(g5)), 9)
})

test_that("sample_graph even works with multiple conditions", {

  na <- 12
  s1 <- simulate_schedule(n_animals = na, n_groups = 4, n_splits = NA, time_to_leave = 5, time_to_return = 2, travel_time = c(0.001, 0.002), sampling_duration = 14, simulator = "independent")
  g0 <- graph_from_schedule(s1)

  # regime even
  # regime even floor(sample_nNodes / nGroups) == 0
  g1 <- sample_graph(graph = g0,
                     sample_nNodes = 3, # floor(3/4) == 0
                     prop_hi_res = 0.5,
                     hi_res = 12,
                     lo_res = 1/7,
                     sampling_duration = 7,
                     regime = "even",
                     alg = "netcarto")
  expect_equal(length(igraph::V(g1)), 3)

  # regime even floor(sample_nNodes / nGroups) > 0
  g2 <- sample_graph(graph = g0,
                     sample_nNodes = 4, # floor(4/4) == 1
                     prop_hi_res = 0.5,
                     hi_res = 12,
                     lo_res = 1/7,
                     sampling_duration = 7,
                     regime = "even",
                     alg = "netcarto")
  expect_equal(length(igraph::V(g2)), 4)

  # sample nNodes is greater than twice the number of groups
  g3 <- sample_graph(graph = g0,
                     sample_nNodes = 5,
                     prop_hi_res = 0.5,
                     hi_res = 12,
                     lo_res = 1/7,
                     sampling_duration = 7,
                     regime = "even",
                     alg = "netcarto")
  expect_equal(length(igraph::V(g3)), 5)

  g4 <- sample_graph(graph = g0,
                     sample_nNodes = 6,
                     prop_hi_res = 0.5,
                     hi_res = 12,
                     lo_res = 1/7,
                     sampling_duration = 7,
                     regime = "even",
                     alg = "netcarto")
  expect_equal(length(igraph::V(g4)), 6)

  g5 <- sample_graph(graph = g0,
                     sample_nNodes = 9,
                     prop_hi_res = 0.5,
                     hi_res = 12,
                     lo_res = 1/7,
                     sampling_duration = 7,
                     regime = "even",
                     alg = "netcarto")
  expect_equal(length(igraph::V(g5)), 9)

})


test_that("sample_graph random works with multiple conditions", {

  na <- 10
  s1 <- simulate_schedule(n_animals = na, n_groups = 6, n_splits = NA, time_to_leave = 5, time_to_return = 2, travel_time = c(0.001, 0.002), sampling_duration = 14, simulator = "independent")
  g0 <- graph_from_schedule(s1)

  # regime random netcarto
  g1 <- sample_graph(graph = g0,
                     sample_nNodes = 3,
                     prop_hi_res = 0.5,
                     hi_res = 12,
                     lo_res = 1/7,
                     sampling_duration = 7,
                     regime = "random",
                     alg = "netcarto")
  expect_equal(length(igraph::V(g1)), 3)

  # regime random walktrap
  g2 <- sample_graph(graph = g0,
                     sample_nNodes = 10,
                     prop_hi_res = 0.5,
                     hi_res = 12,
                     lo_res = 1/7,
                     sampling_duration = 7,
                     regime = "random",
                     alg = "walktrap")
  expect_equal(length(igraph::V(g2)), 10)

  # regime random fast_greedy
  g3 <- sample_graph(graph = g0,
                     sample_nNodes = 7,
                     prop_hi_res = 0.5,
                     hi_res = 12,
                     lo_res = 1/7,
                     sampling_duration = 7,
                     regime = "grab-two",
                     alg = "fast_greedy")
  expect_equal(length(igraph::V(g3)), 7)

  expect_output(str(g1), "Class \'igraph\'  hidden list of 10")


})
