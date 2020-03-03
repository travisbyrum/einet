context("Testing Causal Emergence")

### Testing Markov Blanket ---------------------------------------------
test_that("Markov Blanket is calculated correctly", {
  adjacency_matrix <- matrix(
    cbind(
      c(0.0, 1.0, 0.0, 0.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 0.0)
    ),
    nrow = 4
  )

  graph <- igraph::graph.adjacency(adjacency_matrix,  mode = "directed")

  blanket <- mb(graph, 2)

  expect_true(1 %in% blanket[[1]])
  expect_true(3 %in% blanket[[1]])
})
