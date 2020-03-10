#' @export
run_example <- function() {
  app_dir <- system.file("app", package = "einet")

  if (app_dir == "") {
    stop("Could not find shiny app. Try re-installing `einet`.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
