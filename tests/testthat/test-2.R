library(testthat)
library(modulr)

test_that("simulate_independence creates list", {
  at <- simulate_independence(n_animals = 12,
                            n_groups = 2,
                            time_to_leave = 3,
                            time_to_return = 1,
                            travel_time = c(0.001, 1),
                            sampling_duration = 7)

  expect_equal(length(at), 12)
  expect_output(str(at), "List of 12")
})

test_that("graph_crossing using first element of simulate_independence output", {
  at <- simulate_independence(n_animals = 12,
                            n_groups = 2,
                            time_to_leave = 3,
                            time_to_return = 1,
                            travel_time = c(0.001, 1),
                            sampling_duration = 7)
  gc <- graph_crossing(at, 1, 1, names(at)[[1]])

  expect_equal(nrow(gc), 12)

})

test_that("rand_vect performs a multinomial draw", {
  N = 12
  M = 5
  out <- rand_vect(N, M, sd = 1, pos.only = TRUE)
  expect_equal(M, sum(out))
})

test_that("simulate_groups does its thing", {
  sg <- simulate_groups(animals_home = 1,
                        n_groups = 3,
                        time_to_leave = 4,
                        time_to_return = 2,
                        travel_time = c(0.01, 2),
                        sampling_duration = 7)
  expect_equal(names(sg), c("inputs", "locations", "animals_home"))
})

test_that("simulate_non_independence has all individuals at all intervals", {
  t2 <- simulate_non_independence(n_groups = 4,
                                  time_to_leave = 5,
                                  time_to_return = 2,
                                  travel_time = c(0.001,2),
                                  sampling_duration = 7)

  t3 <- t2 %>%
    dplyr::group_by(start) %>%
    dplyr::summarise(mems = paste(members, collapse = "-")) %>%
    dplyr::mutate(mems = stringr::str_replace_all(mems, "/", "-")) %>%
    dplyr::mutate(mems = stringr::str_split(mems, "-"))
  t4 <- lapply(t3$mems, unlist)
  t5 <- purrr::map(t4, sort)

  list_of_duplicates <- purrr::map(t5, ~.x[which(duplicated(.x) == TRUE)])
  expect_equal(all(lapply(list_of_duplicates, identical, character(0)) == TRUE), TRUE)

  allSame <- function(x) length(unique(x)) == 1
  t6 <- purrr::map2(t5, "", ~ stringr::str_subset(., .y) )

  expect_equal(allSame(t6), TRUE) # should return true
})

test_that("simulate_non_independence2 has all individuals at all intervals", {
  # this test is probably adequate, but there can be empty characters returned in the list "" from dropping dummy animals
  t2 <- simulate_non_independence2(n_groups = 4,
                                  time_to_leave = 5,
                                  time_to_return = 2,
                                  travel_time = c(0.001,2),
                                  sampling_duration = 7)

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


  allSame <- function(x) length(unique(x)) == 1
  t6 <- purrr::map2(t5, "", ~ stringr::str_subset(., .y) )

  expect_equal(allSame(t6), TRUE) # should return true
})

test_that("graph_from_schedule produces a graph", {
  at <- simulate_independence(n_animals = 12,
                            n_groups = 2,
                            time_to_leave = 3,
                            time_to_return = 1,
                            travel_time = c(0.001, 1),
                            sampling_duration = 7)
  g <- graph_from_schedule(at)
  expect_equal(is(g, "igraph"), TRUE)
})

test_that("netcarto_modules returns expected", {
  adjmat <- matrix(sample(seq(0, 1, .01), 16), nrow = 4)
  diag(adjmat) <- 0
  adjmat[lower.tri(adjmat)] <- 0
  rownames(adjmat) <- colnames(adjmat) <- paste0("Animal_", 1:4)
  y1 <- netcarto_modules(adjmat)

  expect_equal(is.vector(y1), TRUE)
  expect_equal(length(y1), 4)

  # g_d <- simulate_graph(n_animals = 5,
  #                       n_groups = 2,
  #                       time_to_leave = 5,
  #                       time_to_return = 2,
  #                       travel_time = c(0.001, 2),
  #                       sampling_duration = 7,
  #                       sampler = "discrete",
  #                       samples_per_day = 1)
  #
  # # this is a corner case where NA in membership because fewer than two edges, no qrel calculation possible
  # g_obs <- sample_graph(graph = g_d,
  #                       sample_nNodes = 4,
  #                       prop_hi_res = 0.5,
  #                       hi_res = 12,
  #                       lo_res = 1/7,
  #                       sampling_duration = 7,
  #                       regime = "random",
  #                       alg = "fast_greedy")
  #
  # y2 <- netcarto_modules(g_obs)
  # expect_equal(length(y2), 4)

})






