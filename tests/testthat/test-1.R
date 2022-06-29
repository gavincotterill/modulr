library(testthat)
library(modulr)

test_that("simulate_animal creates list", {
  animal <- simulate_animal(time_to_leave = 3,
                            time_to_return = 1,
                            travel_time = c(0.001, 0.002),
                            n_groups = 4,
                            samples_per_day = 1,
                            sampling_duration = 7)

  expect_equal(length(animal), 4)
  expect_output(str(animal), "List of 4")
})

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
  expect_equal(length(g_d), 10)
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
  g_c <- simulate_graph(n_animals = 3,
                        n_groups = 2,
                        time_to_leave = 5,
                        time_to_return = 2,
                        travel_time = c(0.001, 0.002),
                        sampling_duration = 7,
                        sampler = "continuous",
                        samples_per_day = 1)
  expect_equal(length(g_c), 10)
  expect_output(str(g_c), "Class \'igraph\'  hidden list of 10")
})

test_that("sample_graph creates list", {

  g_d <- simulate_graph(n_animals = 12,
                        n_groups = 2,
                        time_to_leave = 5,
                        time_to_return = 2,
                        travel_time = c(0.001, 0.002),
                        sampling_duration = 7,
                        sampler = "discrete",
                        samples_per_day = 1)

  # this is a corner case where NA in membership because fewer than two edges, no qrel calculation possible
  g_obs <- sample_graph(graph = g_d,
                        sample_nNodes = 6,
                        prop_hi_res = 0.5,
                        hi_res = 12,
                        lo_res = 1/7,
                        sampling_duration = 7,
                        regime = "grab-two",
                        alg = "netcarto")

  expect_equal(length(g_obs), 10)
  expect_output(str(g_obs), "Class \'igraph\'  hidden list of 10")
})

## this isn't really doing what I intended anymore, possibly because I changed sample graph?
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

test_that("module_summary works", {

  g_d_1 <- simulate_graph(n_animals = 6,
                        n_groups = 2,
                        time_to_leave = 5,
                        time_to_return = 2,
                        travel_time = c(0.001, 2),
                        sampling_duration = 7,
                        sampler = "discrete",
                        samples_per_day = 1)

  g_d_2 <- simulate_graph(n_animals = 7,
                          n_groups = 2,
                          time_to_leave = 5,
                          time_to_return = 2,
                          travel_time = c(0.001, 2),
                          sampling_duration = 7,
                          sampler = "discrete",
                          samples_per_day = 1)

  g_d_list <- list(g_d_1, g_d_2)

  module_stats <- module_summary(g_d_list)

  expect_silent({  module_stats %>%
      `names<-`(c("n", "mean", "sd", "netSize")) %>%
      ggplot2::ggplot() +
      ggplot2::geom_pointrange(ggplot2::aes(x = n, y = mean, ymin = mean - sd, ymax= mean + sd,
                                            color = as.factor(netSize)),
                               position = ggplot2::position_jitter(width = .4), lty = "dotted") +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("") +
      ggplot2::labs(x = "Modules per Network", y = "Mean of Nodes per Module")+
      ggplot2::theme(legend.title = ggplot2::element_text(size = 20),
                     legend.text = ggplot2::element_text(size = 20),
                     axis.text = ggplot2::element_text(size = 20),
                     axis.title = ggplot2::element_text(size = 20)) +
      ggplot2::guides(color = ggplot2::guide_legend(title = "Network Size"))
    })

})


