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

  output$graph <- renderPlot({
    graph <- dataset() %>%
      igraph::graph.adjacency(mode = "directed")

    plot(graph)
  })

  output$ei <- renderText({
    ei <- dataset() %>%
      effective_information

    ce <- dataset() %>%
      causal_emergence

    sprintf('Effective Information: %.2f\n', ei)
  })

  output$ce <- renderText({
    ce <- dataset() %>%
      causal_emergence

    sprintf('Causal Emergence EI Micro: %.2f\n', ce$ei_micro)
  })
}
