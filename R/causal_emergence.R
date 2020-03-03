#'  Create Markov Blanket
#'
#' @export
mb <- function(graph, nodes) {
  lapply(
    seq_along(nodes),
    function(i) {
      node <- nodes[i]
      predecessors <- igraph::tail_of(graph, igraph::E(graph)[to(node)])
      children <- igraph::head_of(graph, igraph::E(graph)[from(node)])
      unique(c(children, predecessors))
    }
  )
}

#' create_macro
create_macro <- function(graph, ...) {
  e_values <- eigen(igraph::as_adjacency_matrix(graph))
  right <- e_values$vectors
  left <- MASS::ginv(e_values$vectors)

  return(graph)
}

#'  Causal Emergence
#'
#' @export
causal_emergence <- function(x, ...) UseMethod("causal_emergence")

#' @export
causal_emergence.list <- function(graphs, ...) {
  valid <- sapply(graphs, igraph::is_igraph)

  assertthat::assert_that(all(valid))

  lapply(graphs, causal_emergence.igraph)
}


#'  Update Markov Blanket
update_blanket <- function(blanket, removal = NULL) {
  lapply(
    seq_along(blanket),
    function(i) setdiff(blanket[[i]], removal)
  )
}

#' @export
causal_emergence.igraph <- function(graph, span = -1, thresh = 1e-4, types = FALSE, ...) {
  assertthat::assert_that(igraph::is.igraph(graph))

  # Outgoing edges
  w_out <- igraph::incident_edges(graph, igraph::V(graph), mode = "out") %>%
    sapply(function(v) {
      if (length(v) > 0) {
        return(igraph::tail_of(graph, v[[1]]))
      }
    }) %>%
    Filter(Negate(is.null), .) %>%
    as.numeric

  nodes_left <- mb(graph, w_out)

  span <- ifelse(span > 0, span, length(nodes_left))
  shuffle <-  sample(seq_along(nodes_left), size = span)

  ei_micro <- effective_information.igraph(graph)
  ei_current <- ei_micro

  checked_macros <- c()
  macro_types <- list()
  current_mapping <- lapply(seq_along(nodes_left), function(i) i)

  for (node_i in shuffle) {
    macros_to_check <- update_blanket(blanket, checked_macros)[[node_i]]
    queue <- macros_to_check

    if (length(macros_to_check) < 1) {
      next
    }

    node_i_macro <- ifelse(
      current_mapping[[node_i]] == node_i,
      max(sapply(current_mapping, function(i) i)) + 1,
      current_mapping[[node_i]]
    )

    has_compare <- TRUE

    # now start a loop of EI comparisons
    while (has_compare) {
      shuffled_m <- macros_to_check[sample(seq_along(macros_to_check))]

      possible_macro <- tail(shuffled_m, 1)
      possible_mapping <- current_mapping
      possible_mapping[c(node_i, possible_macro)] <- node_i_macro

      queue <<- queue[1:(length(queue) - 1)]

      if (types) {

      } else {
        macro_types[[node_i_macro]] <- "spatem1"
        graph_macro = create_macro(graph, possible_mapping, macro_types)
      }

      ei_macro = effective_information(graph_macro)

      if (is.infinite(ei_macro)) {
        return(graph_macro)
      }

      # G_macro = check_network(G_macro)
      # EI_macro = effective_information(G_macro)
      # if np.isinf(EI_macro):
      #   return G_macro

      if ((ei_macro - ei_current) > thresh) {

      }
    }
  }

  structure(
    list(
      graph       = graph,
      macro_types = macro_types,
      ei          = ei_current
    ),
    class = "CE"
  )

  shuffle
}

# causal_emergence(graph, span = 2)
# causal_emergence(list(graph))
