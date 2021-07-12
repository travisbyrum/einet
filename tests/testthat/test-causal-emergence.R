context("Testing Causal Emergence")

MAX_ERROR <- 0.001

with_seed <- function(seed, code) {
  code <- substitute(code)
  orig.seed <- .Random.seed
  on.exit(.Random.seed <<- orig.seed)
  set.seed(seed)
  eval.parent(code)
}

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
      c(0.0, 2.0, 1.0),
      c(0.0, 0.0, 0.0),
      c(0.0, 0.0, 0.0)
    ),
    nrow = 3
  ) %>%
    igraph::graph.adjacency(mode = "directed")

  ce <- causal_emergence(graph)

  expect_lte(ce$ei_macro - 0, MAX_ERROR)
  expect_lte(ce$ei_micro - 0, MAX_ERROR)
  expect_lte(ce$ce - 0, MAX_ERROR)
})

### Testing Causal Emergence --------------------------------------------
test_that("Causal Emergence is calculated with and_and", {
  and_and <- matrix(
    rbind(
      c(1.0, 0.0, 0.0, 0.0),
      c(1.0, 0.0, 0.0, 0.0),
      c(1.0, 0.0, 0.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0)
    ),
    nrow = 4
  )

  ce <- causal_emergence(and_and)

  expect_true(length(ce) > 0)
})

### Testing Causal Emergence Equivalence --------------------------------
test_that("Causal Emergence has equivalent vertices and edges with same mapping", {
  with_seed(123, {
    complete <- igraph::make_full_graph(10, directed = FALSE, loops = FALSE)
    complete_ce <- causal_emergence(complete)

    equal_v <- all(igraph::V(complete_ce[["g_macro"]]) == igraph::V(complete_ce[["g_micro"]]))
    equal_e <- all(igraph::E(complete_ce[["g_macro"]]) == igraph::E(complete_ce[["g_micro"]]))

    expect_true(equal_v)
    expect_true(equal_e)
  })
})
