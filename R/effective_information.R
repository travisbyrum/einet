#' @export
effective_information <- function(x, ...) UseMethod("effective_information")

#' @export
effective_information.matrix <- function(x) {
  assertthat::assert_that(is.matrix(x))

  graph <- igraph::graph.adjacency(x,  mode = "directed")

  out_edges <- igraph::incident_edges(graph, igraph::V(graph), mode = "out")

  # TODO CHECK IF HAS WEIGHTS

  graph <- graph %>%
    igraph::set_edge_attr("weight", out_edges, 1 / length(out_edges))

  number_of_nodes <- igraph::gsize(graph)

  if (length(out_edges) == 0) {
    return(0)
  }

  w <- lapply(
    igraph::V(graph),
    function(x) {
      weight <- igraph::edge_attr(graph, "weight", x)

      list(
        w_out = entropy::entropy(weight, unit = "log2"),
        w_in  = igraph::edge_attr(graph, "weight", x) / length(out_edges)
      )
    }
  )

  w_out_average <- sapply(w, function(t) t$w_out) %>%
    sum / length(out_edges)

  win_entropy <- sapply(w, function(t) t$w_in) %>%
    entropy::entropy(unit = "log2")

  win_entropy - w_out_average
}
