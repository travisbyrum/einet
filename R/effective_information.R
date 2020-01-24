#' @export
effective_information <- function(x, ...) UseMethod("effective_information")

#' @export
effective_information.matrix <- function(x = 0) {
  assertthat::assert_that(is.matrix(x))

  graph <- igraph::graph.adjacency(x,  mode = "directed")
  nodes <- igraph::V(graph)
  out_edges <- igraph::incident_edges(graph, igraph::V(graph), mode = "out")

  graph <- graph %>%
    igraph::set_edge_attr("weight", igraph::E(.), 0) %>%
    igraph::set_edge_attr("weight", out_edges, 1 / length(out_edges))

  # TODO CHECK IF HAS WEIGHTS

  if (length(out_edges) == 0) {
    return(0)
  }

  w_out <- vector(mode = "numeric", length = length(nodes))
  w_in <- vector(mode = "numeric", length = length(nodes))

  set_win <- function(i) {
    weight <- igraph::edge_attr(graph, "weight", nodes[i])
    w_out[i] <<- entropy::entropy(weight, unit = "log2")

    out_edges <- igraph::incident(graph, nodes[i], mode = "out")
    j <- igraph::ends(graph, out_edges)[2]

    w_in[j] <<- w_in[j] +
      out_edges %>%
      igraph::edge_attr(graph, "weight", .)
  }

  lapply(seq_along(nodes), set_win)

  w_out_average <- sum(w_out) / length(out_edges)
  win_entropy <- entropy::entropy(w_in, unit = "log2")

  win_entropy - w_out_average
}
