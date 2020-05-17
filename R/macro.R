#' create_macro
#'
#' @description
#' Coarse-grains a network according to the specified macro_mapping and
#' the types of macros that each macro is associated with.
#'
#' @param graph igraph
#' @param mapping List mapping from micro to macro nodes.
#' @param macro_types List of node distribution types.
#' @param ... Passed arguments.
#'
#' @return
#' Directed igraph graph object corresponding to a coarse-grained network
#' according to the \code{mapping} of micro nodes onto macro nodes, given by
#' \code{mapping}.
create_macro <- function(graph, mapping, macro_types, ...) {
  assertthat::assert_that(
    length(mapping) > 0,
    msg = "Macro mapping missing."
  )

  assertthat::assert_that(
    !is.null(names(mapping)),
    msg = "Macro mapping missing."
  )

  graph_micro <- check_network(graph)
  N_micro <- length(igraph::V(graph_micro))

  w_out <- graph_micro %>%
    igraph::as_adj(attr = "weight")

  stationary_dist <- stationary(graph_micro)

  # nodes_micro <- mapping %>%
  #   as.numeric %>%
  #   unique %>%
  #   sort

  nodes_macro <- mapping %>%
    get_macro %>%
    unique %>%
    sort

  non_spatem2_max_index <- max(nodes_macro) + 1
  n_macros_spatem2 <- sum(names(macro_types) == "spatem2", na.rm = TRUE)
  new_max_index_tmp <- non_spatem2_max_index + n_macros_spatem2

  macro_id_spatem2 <- seq(
    non_spatem2_max_index,
    new_max_index_tmp - 1,
    length = max(0, new_max_index_tmp - non_spatem2_max_index)
  )

  nodes_in_macro_network <- append(nodes_macro, macro_id_spatem2)

  # N_macro <- length(nodes_in_macro_network)
  n_TOO_BIG_MACRO <- max(nodes_in_macro_network)

  nodes_in_macro_network_mapping <- lapply(
    seq_along(nodes_in_macro_network),
    function(i) list(key = nodes_in_macro_network[[i]], value = i)
  )

  TOO_BIG_MACRO <- matrix(0L, nrow = n_TOO_BIG_MACRO, ncol = n_TOO_BIG_MACRO)

  all_final_node_types <- lapply(
    nodes_in_macro_network,
    function(i) list(key = i, value = ifelse(i < N_micro, "micro", NA))
  ) %>%
    Filter(function(v) !is.na(v$value), .)

  for (key in keys(nodes_in_macro_network_mapping)) {
    if (!(key %in% keys(all_final_node_types))) {
      all_final_node_types[[length(all_final_node_types) + 1]] <-
        list(key = key, value = "spatem1")
    }
  }

  macro_mumu_pairings <- list()
  spt2_ind_tmp <- 1
  mu_mu <- NULL

  for (i in seq_along(macro_types)) {
    value <- names(macro_types)[[i]]

    if (!is.null(value) && !is.na(value) && value == "spatem2") {
      mu_mu <- macro_id_spatem2[[spt2_ind_tmp]]
      all_final_node_types[[mu_mu]]$value <- "mu_mu"
      macro_mumu_pairings[[i]] <- "mu_mu"
      spt2_ind_tmp <- spt2_ind_tmp + 1
    }
  }

  for (i in seq_along(nodes_in_macro_network)) {
    final_node_i <- nodes_in_macro_network[[i]]
    W_i_out_final <- rep(0, n_TOO_BIG_MACRO)

    final_node_i_type <- all_final_node_types %>%
      Filter(function(v) v$key == final_node_i, .) %>%
      .[[1]] %>%
      .$value

    if (final_node_i_type == "micro") {
      out_indices <- igraph::incident(
        graph_micro,
        final_node_i,
        mode = "out"
      ) %>%
        igraph::head_of(graph_micro, .) %>%
        as.numeric

      out_weights <- graph_micro %>%
        igraph::edge_attr(
          "weight",
          index = igraph::incident(graph_micro, final_node_i, mode = "out")
        )

      new_indices <- get_macro(mapping, out_indices)

      w_ij <- new_indices[seq_along(out_weights)]
      W_i_out_final[w_ij] <- W_i_out_final[w_ij] + out_weights

      TOO_BIG_MACRO[final_node_i, ] <- W_i_out_final
    } else if (final_node_i_type == "spatial") {
      micros_in_macro_i <- mapping[which(get_macro(mapping) %in% final_node_i)]
      macro_row_sum <- rep(0, NROW(w_out))

      Wout_macro_subgraph <- w_out %>%
        .[micros_in_macro_i, ]

      nodes_outside_macro_i <- nodes_in_macro_network %>%
        Filter(function(v) !(v %in% micros_in_macro_i) && i != final_node_i, .)

      nodes_outside_macro_mic_index <- mapping[
        which(get_macro(mapping) %in% nodes_outside_macro_i)
        ]

      input_probs_to_macro <- t(w_out) %>%
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
    } else if (final_node_i_type == "spatem1") {
      micros_in_macro_i <- mapping[which(get_macro(mapping) %in% final_node_i)] %>%
        as.numeric

      Wout_macro_subgraph <- w_out %>%
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

      nodes_outside_macro_mic_index <- mapping[
        which(get_macro(mapping) %in% nodes_outside_macro_i)
        ] %>%
        as.numeric

      if (any(c("dgCMatrix", "matrix") %in% class(Wout_macro_subgraph_weighted))) {
        Wout_macro_i_exitrates <- Wout_macro_subgraph_weighted[, nodes_outside_macro_mic_index] %>%
          as.matrix %>%
          colSums
      } else {
        Wout_macro_i_exitrates <- Wout_macro_subgraph_weighted[nodes_outside_macro_mic_index] %>%
          as.matrix %>%
          t
      }

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
          .[micros_in_macro_i, ] %>%
          as.matrix %>%
          colSums %>%
          .[as.numeric(nodes_outside_macro_mic_index)] %>%
          sapply(function(v) v > 0) %>%
          which

        new_indices <- out_indices %>%
          sapply(function(v) nodes_outside_macro_mic_index[v]) %>%
          as.numeric

        old_i <- out_indices[seq_along(new_indices)]
        wij_ind <- get_macro(mapping)[which(mapping %in% new_indices)]
        W_i_out_final[wij_ind] <- W_i_out_final[wij_ind] + Wout_macro_i_exitrates_norm[old_i]

        selfloop <- 1 - sum(W_i_out_final)

        if (selfloop < 0) {
          selfloop <- 0
        }

        W_i_out_final[final_node_i] <- selfloop
        TOO_BIG_MACRO[final_node_i, ] <- W_i_out_final
      }
    } else if (final_node_i_type == "spatem2") {
      mu_mu_index <- macro_mumu_pairings[[final_node_i]]
      W_mu_out_final <- rep(0, n_TOO_BIG_MACRO)
      micros_in_macro_i <- mapping[which(get_macro(mapping) %in% final_node_i)] %>%
        as.numeric

      Wout_macro_subgraph <- w_out %>%
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

      nodes_outside_macro_mic_index <- mapping[
        which(get_macro(mapping) %in% nodes_outside_macro_i)
        ] %>%
        as.numeric

      if (any(c("dgCMatrix", "matrix") %in% class(Wout_macro_subgraph_weighted))) {
        Wout_macro_i_exitrates <- Wout_macro_subgraph_weighted[, nodes_outside_macro_mic_index] %>%
          as.matrix %>%
          colSums
      } else {
        Wout_macro_i_exitrates <- Wout_macro_subgraph_weighted[
          nodes_outside_macro_mic_index
          ] %>%
          as.matrix %>%
          t
      }

      if (Wout_macro_i_exitrates_sum == 0) {
        W_i_out_final[final_node_i] <- 1
        W_mu_out_final[as.numeric(mu_mu_index)] <- 1

        TOO_BIG_MACRO[final_node_i, ] <- W_i_out_final
        TOO_BIG_MACRO[as.numeric(mu_mu_index), ] <- W_mu_out_final
      } else {
        Wout_macro_i_exitrates_norm <- Wout_macro_i_exitrates

        for (j in seq_along(Wout_macro_i_exitrates)) {
          wij <- Wout_macro_i_exitrates[[j]]
          denom <- macro_i_stationary_sum - Wout_macro_i_exitrates_sum
          Wout_macro_i_exitrates_norm[i] <- wij / denom
        }

        out_indices <- w_out %>%
          .[micros_in_macro_i, ] %>%
          as.matrix %>%
          colSums %>%
          .[as.numeric(nodes_outside_macro_mic_index)] %>%
          sapply(function(v) v > 0) %>%
          which

        new_indices <- out_indices %>%
          sapply(function(v) nodes_outside_macro_mic_index[v]) %>%
          as.numeric

        old_i <- out_indices[seq_along(new_indices)]
        wij_ind <- get_macro(mapping)[which(mapping %in% new_indices)]
        W_i_out_final[wij_ind] <- W_i_out_final[wij_ind] +
          Wout_macro_i_exitrates_norm[old_i]

        W_i_out_final[as.numeric(mu_mu_index)] <- 1

        mu_selfloop <- 1 - sum(W_mu_out_final, na.rm = TRUE)

        if (mu_selfloop < 0) {
          mu_selfloop <- 0
        }

        W_mu_out_final[as.numeric(mu_mu_index)] <- mu_selfloop

        TOO_BIG_MACRO[final_node_i, ] <- W_i_out_final
        TOO_BIG_MACRO[mu_mu_index, ] <- W_mu_out_final
      }
    } else if (final_node_i_type == "mu_mu") {
      next
    }
  }

  M <- TOO_BIG_MACRO[nodes_in_macro_network, ][, nodes_in_macro_network]

  M %>%
    igraph::graph.adjacency(mode = "directed", weighted = TRUE)
}

#' select_macro
#'
#' @description
#' Given a potential mapping of the network and a new macro node,
#' this function assigns a new macro node to the candidate node optimized for
#' maximal accuracy.
#'
#' @param graph igraph object of micro network
#' @param macro numeric of possible macro node
#' @param mapping List mapping from micro to macro nodes.
#' @param macro_types List of node distribution types.
#' @param ... Passed arguments.
#'
#' @return
#' List with igraph graph object corresponding to a coarse-grained network
#' and updated node types.
select_macro <- function(graph, macro, mapping, macro_types, ...) {
  assertthat::assert_that(length(macro) == 1, msg = "Macro mapping missing.")

  graph_micro <- check_network(graph)

  #micro nodes within macro
  nodes_in_new_macro <- mapping[sapply(names(mapping), function(x) x == macro)]
  rest_of_nodes <- mapping[sapply(names(mapping), function(x) x != macro)]

  edges_from_micro_in_macro <- nodes_in_new_macro %>%
    as.numeric %>%
    lapply(
      function(v) {
        igraph::adjacent_vertices(graph_micro, v, mode = "out")[[1]] %>%
          as.numeric
      }
    )

  nodes_with_outside_output <- seq_along(edges_from_micro_in_macro) %>%
    Filter(function(v) {
      length(intersect(edges_from_micro_in_macro[[v]], rest_of_nodes)) > 1
    }, .) %>%
    nodes_in_new_macro[[.]] %>%
    as.numeric

  edges_to_micro_in_macro <- nodes_in_new_macro %>%
    as.numeric %>%
    lapply(
      function(v) {
        igraph::adjacent_vertices(graph_micro, v, mode = "in")[[1]] %>%
          as.numeric
      }
    )

  nodes_with_outside_input <- seq_along(edges_to_micro_in_macro) %>%
    Filter(function(v) {
      length(intersect(edges_to_micro_in_macro[[v]], rest_of_nodes)) > 1
    }, .) %>%
    nodes_in_new_macro[[.]] %>%
    as.numeric

  tmp_type_nms <- names(macro_types)

  macro_types_spatem1 <- append(macro_types, macro)
  macro_types_spatem2 <- macro_types_spatem1

  macro_types_spatem1 <- macro_types_spatem1 %>%
    setNames(
      c(tmp_type_nms, "spatem1")
    )

  macro_types_spatem2 <- macro_types_spatem2 %>%
    setNames(
      c(tmp_type_nms, "spatem2")
    )

  macro_intersect <- intersect(
    nodes_with_outside_input,
    nodes_with_outside_output
  )

  if (length(macro_intersect) > 0) {
    graph_macro <- create_macro(graph_micro, mapping, macro_types_spatem2)
  } else {
    graph_macro <- create_macro(graph_micro, mapping, macro_types_spatem1)
  }

  list(
    g_macro     = graph_macro,
    macro_types = macro_types
  )
}
