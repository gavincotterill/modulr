library(testthat)
library(modulr)

test_that("modulr plot works with multiple algorithms", {

  # many edges
  adjmat <- matrix(sample(seq(0, 1, .01), 16), nrow = 4)
  diag(adjmat) <- 0
  adjmat[lower.tri(adjmat)] <- 0
  rownames(adjmat) <- colnames(adjmat) <- paste0("Animal_", 1:4)

  expect_silent({  modulr_plot(adjmat, alg = "fast_greedy") })
  expect_silent({  modulr_plot(adjmat, alg = "walktrap") })
  expect_silent({  modulr_plot(adjmat, alg = "netcarto") })

  g_obs <- igraph::graph_from_adjacency_matrix(adjmat, weighted = T, mode = "undirected", diag = F)
  expect_silent({ modulr_plot(g_obs, alg = "netcarto")})

  # single edge
  adjmat <- matrix(sample(seq(0, 1, .01), 4), nrow = 2)
  diag(adjmat) <- 0
  adjmat[lower.tri(adjmat)] <- 0
  rownames(adjmat) <- colnames(adjmat) <- paste0("Animal_", 1:2)
  expect_silent({  modulr_plot(adjmat, alg = "netcarto") })

  g <- igraph::graph_from_adjacency_matrix(adjmat, weighted = T, mode = "undirected", diag = F)
  expect_silent({ modulr_plot(g, alg = "netcarto")})

  # no edges
  adjmat <- matrix(rep(0, 4), nrow = 2)
  rownames(adjmat) <- colnames(adjmat) <- paste0("Animal_", 1:2)
  expect_error({  modulr_plot(adjmat, alg = "netcarto") })

  # wrong algorithm
  expect_error({  modulr_plot(adjmat, alg = "fake") })


})
