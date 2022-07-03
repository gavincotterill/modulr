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
