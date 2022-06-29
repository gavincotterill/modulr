test_that("netcarto_modules returns expected", {

  # many edges
  adjmat <- matrix(sample(seq(0, 1, .01), 16), nrow = 4)
  diag(adjmat) <- 0
  adjmat[lower.tri(adjmat)] <- 0
  rownames(adjmat) <- colnames(adjmat) <- paste0("Animal_", 1:4)
  y1 <- netcarto_modules(adjmat)

  expect_equal(is.vector(y1), TRUE)
  expect_equal(length(y1), 4)

  g <- igraph::graph_from_adjacency_matrix(adjmat, weighted = T, mode = "undirected", diag = F)

  y2 <- netcarto_modules(g)
  expect_equal(length(y2), 4)

  # single edge
  adjmat <- matrix(sample(seq(0, 1, .01), 4), nrow = 2)
  diag(adjmat) <- 0
  adjmat[lower.tri(adjmat)] <- 0
  rownames(adjmat) <- colnames(adjmat) <- paste0("Animal_", 1:2)
  y3 <- netcarto_modules(adjmat)
  expect_equal(length(y3), 2)


  # no edges
  adjmat <- matrix(rep(0, 4), nrow = 2)
  rownames(adjmat) <- colnames(adjmat) <- paste0("Animal_", 1:2)
  expect_error({  netcarto_modules(adjmat) })

})
