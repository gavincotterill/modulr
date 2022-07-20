test_that("get_times works", {
  na <- 10
  s1 <- simulate_schedule(n_animals = na, n_groups = 3, n_splits = NA, time_to_leave = 5, time_to_return = 2, travel_time = c(0.001, 0.002), sampling_duration = 14, simulator = "independent")

  ids <- names(s1)

  out <- get_times(schedule = s1[[1]], id = ids[[1]], simulator = "independent")

  for (i in 2:length(ids)) {
    out[i, ] <- get_times(s1[[i]], ids[[i]], simulator = "independent")
  }

  expect_equal(length(s1), nrow(out))

  s2 <- simulate_schedule(n_animals = na,
                          n_groups = 3,
                          n_splits = NA,
                          time_to_leave = 5,
                          time_to_return = 2,
                          travel_time = c(0.001, 0.002),
                          sampling_duration = 14,
                          simulator = "group-think")

  ids2 <- names(s2)

  out2 <- get_times(schedule = s2[[1]], id = ids2[[1]], simulator = "group-think", option = "co-located")

  for (i in 2:length(ids2)) {
    out2[i, ] <- get_times(s2[[i]], ids2[[i]], simulator = "group-think", option = "co-located")
  }

  expect_equal(length(s2), nrow(out2))

  out3 <- get_times(schedule = s2[[1]], id = ids2[[1]], simulator = "group-think", option = "attached")

  for (i in 2:length(ids2)) {
    out3[i, ] <- get_times(s2[[i]], ids2[[i]], simulator = "group-think", option = "attached")
  }

  expect_equal(length(s2), nrow(out3))

})
