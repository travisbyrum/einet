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
create_macro <- function(graph, mapping, macro_types, ...) {
  N_micro <- length(igraph::V(graph))
  w_out <- graph
  stationary_dist <- stationary(graph)

  assertthat::assert_that(length(mapping) > 0, msg = 'Macro mapping missing.')

  nodes_micro <- sapply(mapping, function(value) value$node) %>%
    unique

  nodes_macro <- sapply(mapping, function(value) value$macro) %>%
    unique

  non_spatem2_max_index <- max(nodes_macro) + 1

  n_macros_spatem2 <- sapply(macro_types, function(v) v == "spatem2") %>%
    as.numeric %>%
    sum(na.rm = TRUE)

  new_max_index_tmp <- non_spatem2_max_index + n_macros_spatem2

  macro_id_spatem2 <- seq(
    non_spatem2_max_index,
    new_max_index_tmp - 1,
    length = max(0, new_max_index_tmp - non_spatem2_max_index)
  )

  nodes_in_macro_network <- append(nodes_macro, macro_id_spatem2)
  N_macro <- length(nodes_in_macro_network)
  n_TOO_BIG_MACRO = max(nodes_in_macro_network) + 1

  nodes_in_macro_network_mapping <- lapply(
    seq_along(nodes_in_macro_network),
    function(i) list(key = nodes_in_macro_network[[i]], value = i )
  )

  TOO_BIG_MACRO <- matrix(0L, nrow = n_TOO_BIG_MACRO, ncol = n_TOO_BIG_MACRO)

  all_final_node_types <- lapply(
    nodes_in_macro_network,
    function(i) list(key = i, value = ifelse(i < N_micro, 'micro', NA))
  ) %>%
    Filter(function(v) !is.na(v$value), .)

  for (key in keys(nodes_in_macro_network_mapping)) {
    if (!(key %in% keys(all_final_node_types))) {
      all_final_node_types[[length(all_final_node_types) + 1]] <- list(key = key, value = 'spatem1')
    }
  }

  macro_mumu_pairings <- list()
  spt2_ind_tmp <- 1
  mu_mu <- NULL

  for (i in seq_along(macro_types)) {
    value <- macro_types[[i]]

    if (!is.null(value) && !is.na(value) && value == 'spatem2') {
      mu_mu <- macro_id_spatem2[[spt2_ind_tmp]]
      all_final_node_types[[mu_mu]]$value <- 'mu_mu'
      macro_mumu_pairings[[i]] <- 'mu_mu'
      spt2_ind_tmp <- spt2_ind_tmp + 1
    }
  }

  for (i in seq_along(nodes_macro)) {
    final_node_i <- nodes_macro[[i]]
    W_i_out_final <- rep(0, n_TOO_BIG_MACRO)

    final_node_i_type <- all_final_node_types %>%
      Filter(function(v) v$key == final_node_i, .) %>%
      .[[1]] %>%
      .$value

    if (final_node_i_type == 'micro') {
      out_indices <- igraph::incident(graph, 2, mode = "out") %>%
        igraph::head_of(graph, .) %>%
        as.numeric

      out_weights <- graph %>%
        igraph::edge_attr(
          'weight',
          index = igraph::incident(graph, 2, mode = "out")
        )

      new_indices <- lapply(
        mapping,
        function(m) {
          ifelse(m$node %in% out_indices, m$macro, NA)
        }
      ) %>%
        Filter(function(o) !is.na(o), .)

      W_i_out_final[out_indices] <- W_i_out_final[out_indices] + out_weights
      TOO_BIG_MACRO[final_node_i, ] <- W_i_out_final
    } else if (final_node_i_type == 'spatial') {
      micros_in_macro_i <- mapping %>%
        Filter(function(v) v$macro == final_node_i, .) %>%
        sapply(function(v) v$node)

      macro_row_sum <- rep(0, NROW(igraph::as_adjacency_matrix(w_out)))

      Wout_macro_subgraph <- igraph::as_adjacency_matrix(w_out) %>%
        as.matrix %>%
        .[micros_in_macro_i, ]

      nodes_outside_macro_i <- nodes_in_macro_network %>%
        Filter(function(v) !(v %in% micros_in_macro_i) && i != final_node_i, .)

      nodes_outside_macro_mic_index <- mapping %>%
        Filter(function(v) v$macro %in% nodes_outside_macro_i, .) %>%
        sapply(function(v) v$node)

      input_probs_to_macro <- w_out %>%
        igraph::as_adjacency_matrix(.) %>%
        as.matrix %>%
        t %>%
        .[micros_in_macro_i, ] %>%
        t

      for (i in seq_along(macro_row_sum)) {
        added <- Wout_macro_subgraph  %*%
          t(input_probs_to_macro[i])

        macro_row_sum <- macro_row_sum + added
      }

      if (sum(macro_row_sum) == 0) {
        W_i_out_final[final_node_i] <- 1
        TOO_BIG_MACRO[final_node_i, ] <- W_i_out_final
      } else {
        out_indices <- w_out %>%
          igraph::as_adjacency_matrix(.) %>%
          .[micros_in_macro_i, ] %>%
          .[as.numeric(nodes_outside_macro_mic_index)] %>%
          Filter(function(v) v > 0, .) %>%
          as.numeric

        new_indices <- out_indices %>%
          lapply(
            function(v) {
              if (is.null(nodes_outside_macro_mic_index[v][[1]])) {
                return(NA)
              } else {
                return(nodes_outside_macro_mic_index[v][[1]])
              }
            }
          ) %>%
          Filter(function(v) !is.na(v), .) %>%
          as.numeric

        for (j in seq_along(new_indices)) {
          value <- new_indices[[j]]
          old_i <- out_indices[[j]]

          wij_out <- mapping %>%
            Filter(function(v) v$node == value, .) %>%
            sapply(function(v) v$node) %>%
            as.numeric

          acc <- W_i_out_final[wij_out]
          W_i_out_final[wij_out] <- W_i_out_final[wij_out] + acc

          selfloop <- sum(macro_row_sum[micros_in_macro_i])

          if (selfloop < 0) {
            selfloop <- 0
          }

          W_i_out_final[final_node_i] <- selfloop
          TOO_BIG_MACRO[final_node_i, ] <- W_i_out_final / sum(W_i_out_final)
        }
      }
    } else if (final_node_i_type == 'spatem1') {
      micros_in_macro_i <- mapping %>%
        Filter(function(v) v$macro == final_node_i, .) %>%
        sapply(function(v) v$node) %>%
        as.numeric

      Wout_macro_subgraph <- igraph::as_adjacency_matrix(w_out) %>%
        as.matrix %>%
        .[micros_in_macro_i, ]

      macro_i_stationary <- stationary_dist[micros_in_macro_i]
      macro_i_stationary_sum <- sum(macro_i_stationary)

      Wout_macro_subgraph_weighted <- Wout_macro_subgraph

      for (j in seq_along(micros_in_macro_i)) {
        if (is.numeric(Wout_macro_subgraph)) {
          value <- Wout_macro_subgraph
          Wout_macro_subgraph_weighted <- value * macro_i_stationary[j]
        } else {
          value <- Wout_macro_subgraph[j, ]
          Wout_macro_subgraph_weighted[j, ] <- value * macro_i_stationary[j]
        }
      }

      nodes_outside_macro_i <- nodes_in_macro_network %>%
        Filter(function(v) !(v %in% micros_in_macro_i) && v != final_node_i, .) %>%
        as.numeric

      nodes_outside_macro_mic_index <- mapping %>%
        Filter(function(v) v$macro %in% nodes_outside_macro_i, .) %>%
        sapply(function(v) v$node) %>%
        as.numeric

      Wout_macro_i_exitrates <- Wout_macro_subgraph_weighted[nodes_outside_macro_mic_index] %>%
        sum

      Wout_macro_i_exitrates_sum <- Wout_macro_i_exitrates %>%
        sum

      if (Wout_macro_i_exitrates_sum == 0) {
        W_i_out_final[final_node_i] <- 1
        TOO_BIG_MACRO[final_node_i, ] <- W_i_out_final
      } else {
        Wout_macro_i_exitrates_norm <- Wout_macro_i_exitrates

        for (j in seq_along(Wout_macro_i_exitrates)) {
          wij <- Wout_macro_i_exitrates[[j]]
          Wout_macro_i_exitrates_norm[j] <- wij / macro_i_stationary_sum
        }

        out_indices <- w_out %>%
          igraph::as_adjacency_matrix(.) %>%
          .[micros_in_macro_i, ] %>%
          .[as.numeric(nodes_outside_macro_mic_index)] %>%
          Filter(function(v) v > 0, .) %>%
          as.numeric

        new_indices <- out_indices %>%
          sapply(function(v) nodes_outside_macro_mic_index[v]) %>%
          as.numeric

        for (j in seq_along(new_indices)) {
          old_i <- out_indices[[j]]
          wij_ind <- new_indices[[j]]
          mapping[[wij_ind]]$macro
          W_i_out_final[wij_ind] <- W_i_out_final[wij_ind] + Wout_macro_i_exitrates_norm[old_i]
        }

        selfloop <- 1 - sum(W_i_out_final)

        if (selfloop < 0) {
          selfloop <- 0
        }

        W_i_out_final[final_node_i] <- selfloop
        TOO_BIG_MACRO[final_node_i, ] <- W_i_out_final
      }
    } else if (final_node_i_type == 'spatem2') {
      mu_mu_index <- macro_mumu_pairings[[final_node_i]]
      W_mu_out_final <- rep(0, n_TOO_BIG_MACRO)

      micros_in_macro_i <- mapping %>%
        Filter(function(v) v$macro == final_node_i, .) %>%
        sapply(function(v) v$node) %>%
        as.numeric

      Wout_macro_subgraph <- igraph::as_adjacency_matrix(w_out) %>%
        as.matrix %>%
        .[micros_in_macro_i, ]

      macro_i_stationary <- stationary_dist[micros_in_macro_i]
      macro_i_stationary_sum <- sum(macro_i_stationary)

      Wout_macro_subgraph_weighted <- Wout_macro_subgraph

      for (j in seq_along(micros_in_macro_i)) {
        if (is.numeric(Wout_macro_subgraph)) {
          value <- Wout_macro_subgraph
          Wout_macro_subgraph_weighted <- value * macro_i_stationary[j]
        } else {
          value <- Wout_macro_subgraph[j, ]
          Wout_macro_subgraph_weighted[j, ] <- value * macro_i_stationary[j]
        }
      }

      nodes_outside_macro_i <- nodes_in_macro_network %>%
        Filter(function(v) !(v %in% micros_in_macro_i) && i != final_node_i, .)

      nodes_outside_macro_mic_index <- mapping %>%
        Filter(function(v) v$macro %in% nodes_outside_macro_i, .) %>%
        sapply(function(v) v$node)

      Wout_macro_i_exitrates <- Wout_macro_subgraph_weighted[nodes_outside_macro_mic_index] %>%
        sum

      Wout_macro_i_exitrates_sum <- Wout_macro_i_exitrates %>%
        sum

      if (Wout_macro_i_exitrates_sum == 0) {
        W_i_out_final[final_node_i] <- 1
        W_mu_out_final[as.numeric(mu_mu_index)] <- 1

        TOO_BIG_MACRO[final_node_i, ] <- W_i_out_final
        TOO_BIG_MACRO[as.numeric(mu_mu_index), ] <- W_mu_out_final
      } else {
        out_macro_i_exitrates_norm <- Wout_macro_i_exitrates.copy

        for (j in seq_along(Wout_macro_i_exitrates)) {
          wij <- Wout_macro_i_exitrates[[j]]
          denom <- macro_i_stationary_sum - Wout_macro_i_exitrates_sum
          Wout_macro_i_exitrates_norm[i] <- wij / denom
        }

        out_indices <- w_out %>%
          igraph::as_adjacency_matrix(.) %>%
          .[micros_in_macro_i, ] %>%
          .[as.numeric(nodes_outside_macro_mic_index)] %>%
          Filter(function(v) v > 0, .) %>%
          as.numeric

        new_indices <- out_indices %>%
          sapply(function(v) nodes_outside_macro_mic_index[v]) %>%
          as.numeric

        for (j in seq_along(new_indices)) {
          old_i <- out_indices[[j]]
          wij_ind <- new_indices[[j]]
          wij_ind <- macro_mapping[[wij_ind]]$macro
          W_mu_out_final[wij_ind] <- W_mu_out_final[wij_ind] + Wout_macro_i_exitrates_norm[old_i]
        }

        W_i_out_final[as.numeric(mu_mu_index)] <- 1

        mu_selfloop <- 1 - sum(W_mu_out_final, na.rm = TRUE)

        if (mu_selfloop < 0) {
          mu_selfloop <- 0
        }

        W_mu_out_final[as.numeric(mu_mu_index)] <- mu_selfloop

        TOO_BIG_MACRO[final_node_i, ] <- W_i_out_final
        TOO_BIG_MACRO[mu_mu_index, ] <- W_mu_out_final
      }
    } else if (final_node_i_type == 'mu_mu') {
      next
    }
  }

  TOO_BIG_MACRO[nodes_in_macro_network, ][, nodes_in_macro_network]
}

#' Outgoing edges
out_edges <- function(graph) {
  igraph::incident_edges(graph, igraph::V(graph), mode = "out") %>%
    sapply(function(v) {
      if (length(v) > 0) {
        return(igraph::tail_of(graph, v[[1]]))
      }
    }) %>%
    Filter(Negate(is.null), .) %>%
    as.numeric
}

#'  Stationary Distribution
stationary <- function(graph, zero_cutoff = 1e-10) {
  A <- igraph::as_adjacency_matrix(graph) %>%
    as.matrix

  rows <- NROW(A)

  I <- diag(nrow = rows)

  A <- rbind(I - A, rep(1, rows))
  B <- c(rep(0, rows), 1)

  P <- lm(B ~ 0 + A)$coefficients
  P[P < zero_cutoff] <- 0
  P[is.na(P)] <- 0

  if (sum(P) != 0 && sum(P) != 1) {
    P <- P / sum(P)
  }

  as.numeric(P)
}

#'  Update Markov Blanket
update_blanket <- function(blanket, removal = NULL) {
  lapply(
    seq_along(blanket),
    function(i) setdiff(blanket[[i]], removal)
  )
}

#'  Causal Emergence
#'
#' @export
causal_emergence <- function(x, ...) UseMethod("causal_emergence")

#' @export
causal_emergence.matrix <- function(x, ...) {
  assertthat::assert_that(is.matrix(x))

  graph <- igraph::graph.adjacency(x,  mode = "directed")

  causal_emergence.igraph(graph, ...)
}

#' @export
causal_emergence.list <- function(graphs, ...) {
  valid <- sapply(graphs, igraph::is_igraph)

  assertthat::assert_that(all(valid))

  lapply(graphs, causal_emergence.igraph)
}

#' @export
causal_emergence.igraph <- function(graph,
                                    span            = -1,
                                    thresh          = 1e-4,
                                    types           = FALSE,
                                    max_iterations  = 2,
                                    ...) {
  assertthat::assert_that(igraph::is.igraph(graph))

  out_edges <- igraph::incident_edges(graph, igraph::V(graph), mode = "out")

  graph <- graph %>%
    igraph::set_edge_attr("weight", igraph::E(.), 0) %>%
    igraph::set_edge_attr(
      "weight",
      Filter(function(oe) length(oe) > 0, out_edges), 1 / length(out_edges)
    )

  # w_out <-out_edges(graph) why do we not have to take out edges
  w_out <- igraph::V(graph)
  blanket <- mb(graph, w_out)
  nodes_left <- w_out

  span <- ifelse(span > 0, span, length(nodes_left))
  shuffle <-  sample(seq_along(nodes_left), size = span)

  ei_micro <- effective_information.igraph(graph)
  eff_micro <- effective_information.igraph(graph, effectiveness = TRUE)

  ei_current <- ei_micro
  eff_current <- eff_micro

  checked_macros <- c()
  macro_types <- list()
  macro_types_tmp <- macro_types

  current_mapping <- lapply(
    seq_along(nodes_left),
    function(i) {
      list(
        macro = w_out[[i]],
        node  = w_out[[i]]
      )
    }
  )

  for (i in seq_along(shuffle)) {
    node_i <- w_out[[shuffle[i]]]
    progress <- (i / length(shuffle)) * 100
    cat(sprintf('[%.1f%%] Checking node %d\n', progress, node_i))

    macros_to_check <- update_blanket(blanket, checked_macros)[[node_i]]
    queue <- macros_to_check

    if (length(macros_to_check) < 1) {
      next
    }

    node_i_macro <- ifelse(
      current_mapping[[node_i]]$macro == node_i,
      max(sapply(current_mapping, function(i) i$macro)) + 1,
      current_mapping[[node_i]]
    )

    iteration <- 0
    while (length(queue) > 0) {
      iteration <- iteration + 1

      queue <- queue[sample(seq_along(queue))]

      possible_macro <- head(queue, 1) %>%
        as.numeric

      queue <- tail(queue, -1)

      possible_mapping <- current_mapping
      possible_mapping[[possible_macro]]$macro <- node_i_macro
      possible_mapping[[node_i]]$macro <- node_i_macro

      if (types) {

      } else {
        macro_types_tmp[[node_i_macro]] <- "spatem1"
        graph_macro <- create_macro(graph, possible_mapping, macro_types)
      }

      ei_macro = effective_information(graph_macro)
      eff_macro <- effective_information.igraph(graph, effectiveness = TRUE)

      if (is.infinite(ei_macro)) {
        return(graph_macro)
      }

      if ((ei_macro - ei_current) > thresh) {
        ei_current <- ei_macro
        eff_current <- eff_macro

        macro_mapping <- possible_mapping
        macro_types <- macro_types_tmp

        checked_macros <- checked_macros %>%
          append(c(as.numeric(node_i), possible_mapping))

        nodes_in_macro_i <-  macro_mapping %>%
          Filter(function(v) v$macro == node_i_macro, .) %>%
          sapply(function(v) v$node)

        for (new_micro_i in seq_along(nodes_in_macro_i)) {
          neighbors_i_M <- mb(graph, new_micro_i)[[1]] %>%
            as.numeric

          for (node_j_M in neighbors_i_M) {
            if (!(node_j_M %in% queue) && node_j_M != node_i) {
              queue <- append(queue, node_j_M)
            }
          }
        }
      }

      if (iteration > max_iterations) {
        break
      }
    }
  }

  structure(
    list(
      g_micro     = graph,
      macro_types = macro_types,
      g_macro     = create_macro(graph, current_mapping, macro_types),
      mapping     = current_mapping,
      ei_macro    = ei_current,
      ei_micro    = ei_micro,
      ce          = eff_micro - eff_micro
    ),
    class = "CE"
  )
}
