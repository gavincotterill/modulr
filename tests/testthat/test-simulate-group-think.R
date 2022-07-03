test_that("group-think has all individuals at all intervals", {
  # group-think - this should get some group sizes >= 11 to test character order issues and same for ngroups
  na = 150
  t2 <- simulate_non_independence(n_animals = na,
                                  n_groups = 13,
                                  time_to_leave = 5,
                                  time_to_return = 2,
                                  travel_time = c(0.001,0.25),
                                  sampling_duration = 30)

  t3 <- t2 %>%
    dplyr::group_by(start) %>%
    dplyr::summarise(mems = paste(members, collapse = "-")) %>%
    dplyr::mutate(mems = stringr::str_replace_all(mems, "/", "-")) %>%
    dplyr::mutate(mems = stringr::str_split(mems, "-"))
  t4 <- lapply(t3$mems, unlist)
  t5 <- purrr::map(t4, sort)
  list_of_duplicates <- purrr::map(t5, ~.x[which(duplicated(.x) == TRUE)])
  list_of_duplicates[which(purrr::map(list_of_duplicates, ~all(.x == "")) == TRUE)] <- NULL # this might be a better test than previous

  expect_equal(length(list_of_duplicates), 0)

  t6 <- purrr::map2(t5, "", ~ stringr::str_subset(., .y) )

  allPresent <- function(x, y){all(lengths(x) == y) }

  expect_equal(allPresent(t6, na), TRUE)
})

