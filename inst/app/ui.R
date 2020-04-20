ui <- fluidPage(

  # App title ----
  titlePanel("Causal Emergence and Effective Information"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      fileInput(
        "file_graph", "Choose Graph File",
        multiple = FALSE,
        accept = c(
          "text/plain"
        )
      ),
      tags$hr(),
      radioButtons(
        "graph_type",
        "Graph File Type",
        choices = c(
          EdgeList = "edgelist",
          GraphML  = "graphml"
        ),
        selected = "graphml"
      ),
      selectInput(
        inputId  = "graph",
        label    = "Choose example dataset",
        selected = "copy_copy",
        choices  = c("copy_copy", "and_and", "or_or")
      ),
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      h1("Graph"),
      plotOutput(outputId = "graph"),
      h1("Summary"),
      textOutput("ei"),
      verbatimTextOutput("ce"),
    )
  )
)
