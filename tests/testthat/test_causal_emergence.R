context("Testing Causal Emergence")

### Testing Markov Blanket ---------------------------------------------
test_that("Markov Blanket is calculated correctly", {
  graph <- matrix(
    cbind(
      c(0.0, 1.0, 0.0, 0.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 0.0)
    ),
    nrow = 4
  ) %>%
    igraph::graph.adjacency(mode = "directed")

  blanket <- mb(graph, 2)

  expect_true(1 %in% blanket[[1]])
  expect_true(3 %in% blanket[[1]])
})

### Testing Causal Emergence --------------------------------------------
test_that("Causal Emergence is calculated correctly", {
  graph <- matrix(
    cbind(
      c(0.0, 1.0, 0.0, 0.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 0.0)
    ),
    nrow = 4
  ) %>%
    igraph::graph.adjacency(mode = "directed")

 ce <- causal_emergence(graph)

 expect_true(length(ce) > 0)
})

