test_that("simulate_groups does its thing", {
  sg <- simulate_groups(animals_home = 1,
                        n_groups = 3,
                        time_to_leave = 4,
                        time_to_return = 2,
                        travel_time = c(0.01, 2),
                        sampling_duration = 7)
  expect_equal(names(sg), c("inputs", "locations", "animals_home"))
})

