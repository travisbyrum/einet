#' @export
effective_information <- function(x, ...) UseMethod("effective_information")

#' @export
effective_information.matrix <- function(x, ...) {
  assertthat::assert_that(is.matrix(x))

  graph <- igraph::graph.adjacency(x,  mode = "directed")

  effective_information.igraph(graph, ...)
}

#' @export
effective_information.igraph <- function(graph, normalized = FALSE) {
  assertthat::assert_that(igraph::is.igraph(graph))

  nodes <- igraph::V(graph)
  out_edges <- igraph::incident_edges(graph, igraph::V(graph), mode = "out")

  n_out <- out_edges %>%
    Filter(function(v) length(v) > 0, .) %>%
    length

  out_edge_len <- out_edges %>%
    sapply(function(v) length(v)) %>%
    sum

  non_zero <- Filter(function(oe) length(oe) > 0, out_edges) %>%
    Reduce(function(a, b) append(a, b), .) %>%
    as.numeric

  weight_value <- 1 / length(out_edges)

  graph <- graph %>%
    igraph::set_edge_attr("weight", igraph::E(.), 0) %>%
    igraph::set_edge_attr(
      "weight",
      index = non_zero,
      value = weight_value
    )

  # TODO CHECK IF HAS WEIGHTS

  if (length(out_edges) == 0) {
    return(0)
  }

  w_out <- vector(mode = "numeric", length = length(nodes))
  w_in <- vector(mode = "numeric", length = length(nodes))

  set_win <- function(i) {
    edges <- graph %>%
      igraph::incident(nodes[i], mode = "out")

    targets <- edges %>% igraph::head_of(graph, .) %>%
      unique

    weight <- rep(0, length(targets))

    for (e in seq_along(edges)) {
      wij <- match(igraph::head_of(graph, edges[[e]]), targets)

      weight[wij] <- weight[wij] + (
        edges[[e]] %>%
          igraph::edge_attr(graph, "weight", .)%||%
          0
      )
    }

    if (sum(weight) > 0) {
      w_out[i] <<- entropy::entropy(weight, unit = "log2")
    }

    out_edges_i <- igraph::incident(graph, nodes[i], mode = "out")
    j <- igraph::ends(graph, out_edges_i)[, 2] %>%
      as.numeric

    for (node_value in j) {
      if (!is.na(node_value)) {
        w_in[node_value] <<- w_in[node_value] +
          (out_edges_i %>%
             igraph::edge_attr(graph, "weight", .) %>%
             sum)
      }
    }
  }

  lapply(seq_along(nodes), set_win)

  w_out_average <- sum(w_out) / n_out
  win_entropy <- entropy::entropy(w_in, unit = "log2")

  if (normalized) {
    return(
      (win_entropy - w_out_average) / log2(length(nodes))
    )
  }

  return(win_entropy - w_out_average)
}
