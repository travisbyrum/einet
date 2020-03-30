options(shiny.maxRequestSize = 100*1024^2)

library(magrittr)
library(entropy)
library(einet)

copy_copy <- matrix(
  rbind(
    c(1.0, 0.0, 0.0, 0.0),
    c(0.0, 0.0, 1.0, 0.0),
    c(0.0, 1.0, 0.0, 0.0),
    c(0.0, 0.0, 0.0, 1.0)
  ),
  nrow = 4
)

and_and <- matrix(
  rbind(
    c(1.0, 0.0, 0.0, 0.0),
    c(1.0, 0.0, 0.0, 0.0),
    c(1.0, 0.0, 0.0, 0.0),
    c(0.0, 0.0, 0.0, 1.0)
  ),
  nrow = 4
)

or_or <- matrix(
  rbind(
    c(1.0, 0.0, 0.0, 0.0),
    c(0.0, 0.0, 0.0, 1.0),
    c(0.0, 0.0, 0.0, 1.0),
    c(0.0, 0.0, 0.0, 1.0)
  ),
  nrow = 4
)

server <- function(input, output) {
  dataset <- reactive({
    if (is_truthy(input$file_graph)) {
      graph_out <- igraph::read_graph(
        input$file_graph$datapath,
        format = input$graph_type
      )

      return(graph_out)
    } else {
      return(
        switch(
          input$graph,
          "copy_copy" = copy_copy,
          "and_and"   = and_and,
          "or_or"     = or_or
        )
      )
    }
  })

  causal <- reactive({
    dataset() %>%
      causal_emergence
  })

  output$graph <- renderPlot({
    graph <- dataset()
    ce <- causal()

    if (is.matrix(graph)) {
      graph <- igraph::graph.adjacency(graph, mode = "directed")
    }

    wc <- sapply(ce$mapping, function(m) m$macro) %>%
      igraph::make_clusters(graph, .)

    new_cols <- RColorBrewer::brewer.pal(n = wc$vcount, name = "RdBu")[igraph::membership(wc)]
    par(mfrow=c(1,1))

    plot(
      wc,
      graph,
      col             = new_cols,
      mark.border     ="black",
      vertex.label    = NA,
      vertex.size     = 9,
      edge.arrow.size = .4
    )
  })

  output$ei <- renderText({
    ei <- dataset() %>%
      effective_information

    sprintf('Effective Information: %.2f\n', ei)
  })

  output$ce <- renderText({
    # ce <- dataset() %>%
    #   causal_emergence

    # sprintf('Causal Emergence EI Micro: %.2f\n', ce$ei_micro)
  })
}
