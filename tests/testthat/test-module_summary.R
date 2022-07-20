test_that("module_summary works", {

  g_d_1 <- simulate_graph(n_animals = 10,
                          n_groups = 2,
                          time_to_leave = 5,
                          time_to_return = 2,
                          travel_time = c(0.001, 2),
                          sampling_duration = 7,
                          sampler = "discrete",
                          samples_per_day = 1)

  g_d_2 <- simulate_graph(n_animals = 12,
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
