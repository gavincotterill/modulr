test_that("single id rate works", {
  schedule <- simulate_schedule(n_animals = 10, n_groups = 2, n_splits = NA,
                                time_to_leave = 5, time_to_return = 2,
                                travel_time = c(0.001, 0.2),
                                sampling_duration = 30,
                                simulator = "independent")

  id <- names(schedule)[[1]]

  t1 <- modulr:::single_id_rate(schedule[[1]], id, sim = "independent")

  expect_equal(length(t1), 2)
})
